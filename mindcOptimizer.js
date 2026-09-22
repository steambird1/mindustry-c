import { ASTNodeType, AttributeClass, ASTNode, CompilationPhase,
	ProgramNode, FunctionDeclarationNode, FunctionCallNode, AssignmentExpressionNode, 
    WhileStatementNode, ASTBuilder, ASTVisitor, __convert, objectList,
	liquidList, unitList, buildingList, ConstantManager,
	CastExpression,
	ForStatementNode,
	VariableDeclarationNode,
	TypeSpecifierNode,
	IdentifierNode, dynamicBuiltins
 } from "./mindcBase.js";
import { InstructionBuilder } from "./mindcGeneratorBase.js";

import { SymbolEntry, Scope, TypeInfo, MemberInfo, SemanticAnalyzer, VariableLinker } from "./mindcSemantic.js";

// Auxiliary type to handle break/continue
class BreakException {
	constructor(node) {
		this.node = node;
		this.type = 'BreakException';
	}
}

class ContinueException {
	constructor(node) {
		this.node = node;
		this.type = 'ContinueException';
	}
}

class ReturnRegistry {
	constructor() {
		this.counter = 0;
	}

	record() {
		this.counter++;
	}
}

/**
 * Compatibility problem exists when array B is assigned to array A (explicitly, or implicity by function call)
 * 
 * @typedef {{type: 'traditional' | 'embedded' | 'encoded', mutable: boolean, implement: null | 'regular' | 'divided' | 'lookup'}} arrayStyle
 */
export class ArrayStateReferrer {
	constructor() {
		/**
		 * @type {Map<string, string>}
		 */
		this.arrayParent = new Map();
		/**
		 * @type {Map<string, arrayStyle>}
		 */
		this.arrayState = new Map();

		/**
		 * @type {arrayStyle}
		 */
		this.defaultState = {
			type: 'traditional',
			mutable: true,
			implement: null
		};
	}

	/**
	 * 
	 * @param {arrayStyle?} x 
	 * @param {arrayStyle?} y 
	 * @returns {arrayStyle?}
	 */
	static compatibleOf(x, y) {
		if ((!x) && (!y)) return undefined;
		if (!x) return y;
		if (!y) return x;
		if (x.type === 'traditional' || y.type === 'traditional') return {
			type: 'traditional',
			mutable: x.mutable || y.mutable,
			implement:	null
		};
		if (x.type == 'embedded' || y.type == 'embedded') return {
			type: 'embedded',
			mutable: x.mutable || y.mutable,
			implement: (x.type == y.type) ? x.type.implement : 'divided'
		};
		return x.type;
	}

	/**
	 * 
	 * @param {string} x 
	 * @param {arrayStyle} style 
	 * @param {boolean} [compatible=false] 
	 */
	load(x, style, compatible = false) {
		const head = this.query(x);
		this.arrayState.set(head.root, compatible ? ArrayStateReferrer.compatibleOf(style, head.status) : style);
	}

	/**
	 * 
	 * @param {string} x 
	 */
	downgrade(x) {
		this.load(x, {
			type: 'traditional',
			mutable: false,
			implement: null
		}, true);
	}

	/**
	 * @param {string} x 
	 * @param {arrayStyle | undefined} [defaults=undefined] 
	 * @returns {{root: string, status: arrayStyle?}}
	 */
	query(x, defaults = undefined) {
		if (!this.arrayParent.has(x)) {
			this.arrayParent.set(x, x);
		} else if (this.arrayParent.get(x) !== x) {
			this.arrayParent.set(x, this.query(this.arrayParent.get(x)).root);
		}
		return {
			root: this.arrayParent.get(x), status: this.arrayState.get(x) ?? (defaults === undefined ? this.defaultState : defaults)
		};
	}

	/**
	 * 
	 * @param {string} x 
	 * @param {string} y 
	 */
	assign(x, y) {
		const rx = this.query(x, null), ry = this.query(y, null);
		this.arrayParent.set(ry.root, rx.root);
		this.arrayState.set(rx.root, ArrayStateReferrer.compatibleOf(rx.status, ry.status));
	}

	clear() {
		this.arrayParent.clear();
		this.arrayState.clear();
	}

	/**
	 * The style recorded for an array symbol, falling back to the default
	 * (traditional) layout when nothing has been recorded.
	 * 
	 * @param {import('./mindcSemantic.js').SymbolEntry | string} symbol 
	 * @returns {arrayStyle}
	 */
	styleOf(symbol) {
		const name = (typeof symbol === 'string') ? symbol
			: (symbol && typeof symbol.getAssemblySymbol === 'function') ? symbol.getAssemblySymbol() : `${symbol}`;
		return this.query(name).status ?? this.defaultState;
	}
}

/* ------------------------------------------------------------------ *
 * Array representations -- classification
 *
 * The array styles above only say *which* layout an array has been
 * given; the table below says what that means for the code generator
 * (see the `ArrayRet` family in mindcInfrastructure.js).
 * ------------------------------------------------------------------ */

/**
 * Every representation an array can have.
 *
 * `supportsPointer` tells whether the array may be converted into a
 * pointer -- only the traditional layout lives in memory, so it is the
 * only one that can.
 *
 * @type {Map<string, {type: string, implement: string?, representation: string, supportsPointer: boolean}>}
 */
export const ARRAY_IMPLEMENTATIONS = new Map([
	['traditional', { type: 'traditional', implement: null, representation: 'TraditionalArrayRet', supportsPointer: true }],
	['embedded.regular', { type: 'embedded', implement: 'regular', representation: 'PseudoListArrayRet', supportsPointer: false }],
	['embedded.divided', { type: 'embedded', implement: 'divided', representation: 'PackedArrayRet', supportsPointer: false }],
	['embedded.lookup', { type: 'embedded', implement: 'lookup', representation: 'ConstantArrayRet', supportsPointer: false }],
	['encoded', { type: 'encoded', implement: null, representation: 'ConstantArrayRet', supportsPointer: false }]
]);

/**
 * The key of an array style inside `ARRAY_IMPLEMENTATIONS`.
 * 
 * @param {arrayStyle?} style 
 * @returns {string}
 */
export function keyOfArrayStyle(style) {
	if (!style || !style.type) return 'traditional';
	return style.implement ? `${style.type}.${style.implement}` : style.type;
}

/**
 * Decide which representation an array symbol should get.
 *
 * This is a pure mirror of `loadArrayForSymbol` (which writes into the
 * referrer): the same conditions, but returning the style instead of
 * storing it. It exists so that the code generator can ask "how is this
 * array represented?" without running a full analysis pass.
 *
 * @param {import('./mindcSemantic.js').SymbolEntry} symbol
 * @param {any?} optimizeConfig The `optimizeConfig` of the optimizer
 * @returns {arrayStyle}
 */
export function classifyArrayStyle(symbol, optimizeConfig = null) {
	const traditional = { type: 'traditional', mutable: !symbol.isConst, implement: null };
	const extracted = symbol.extractType();
	// Arrays that are "no longer pointerImpl() but still have pointerTo"
	if (!extracted.pointerTo) return traditional;
	// Volatile arrays must stay in memory
	if (extracted.isVolatile()) return traditional;
	// Only arrays of simple (one word) elements can use the special layouts
	if ((!['basic', 'device', 'null'].includes(extracted.pointerTo.kind)) || extracted.pointerTo.isPointerImpl()) {
		return traditional;
	}
	const config = optimizeConfig ?? {};
	if (!config["doArrayOptimize"]) return traditional;
	if (symbol.isConst) {
		return config["constArrayOptimize"] ?? config["arrayOptimize"] ?? { type: 'encoded', mutable: false, implement: null };
	}
	return config["arrayOptimize"] ?? { type: 'embedded', mutable: true, implement: null };
}

export class Optimizer extends ASTVisitor {
	/**
		 * @typedef {{
		 * 	calls: integer,
		 *  hasSideEffects: boolean,
		 *  isAddressed: boolean?,
		 *  isDefinition: boolean,
		 *  isInline: boolean?,
		 *  canInline: boolean,
		 *  node: FunctionDeclarationNode?,
		 *  scope: Scope?,
		 *  size: number?,
		 *  writtenVarsUnknown: boolean,
		 *  writtenVars: SymbolEntry[]
		 * }} functionInfoEntry
		 * 
		 * @typedef {number | string | VariableLinker | extBuiltinConstant} constantInfo
		 * @typedef {{varName: string, scope: Scope, symbol: SymbolEntry?}} variableModification
		 */
    constructor(compiler = null) {
		super(compiler);
		/**
		 * @type {SemanticAnalyzer}
		 */
		this.semantic = compiler.semanticAnalyzer;
        this.modified = false;
        this.errors = [];
        this.warnings = [];
		this.inlinedScopeId = 0;
		this.inlineCounter = 0;
		/**
		 * Functions that must not be inlined AUTOMATICALLY.
		 *
		 * `soft` holds what `#pragma noinline` asked for, which the `inline`
		 * keyword outranks. `hard` holds what correctness requires (a function
		 * the generated stack walker jumps to, say) and nothing outranks it.
		 * `all` means "every function".
		 *
		 * @type {{soft: Set<string>, hard: Set<string>, all: boolean, allHard: boolean}}
		 */
		this.autoInlineForbidden = {
			soft: new Set(), hard: new Set(), all: false, allHard: false
		};
        
        // 优化状态
        this.functionCallGraph = new Map();
		
		/**
		 * @type {Map<string, functionInfoEntry>}
		 */
        this.functionInfo = new Map();
        this.constants = new Map(); // 全局常量表
		/**
		 * 
		 * @type {Map<string, Map<string, constantInfo>>}
		 */
        this.localConstants = new Map(); // 局部常量表
		/**
		 * @type {Map<string, Map<string, {reads: number, writes: number, canOptimize: boolean?}>>}
		 */ 
        this.variableUses = new Map(); // 变量使用统计 {scopePath: Map<varName, {reads, writes}>}
		//this.visitedSideEffects = false;

        // 作用域分析
		/**
		 * @type {Scope}
		 */
        this.currentScope = null;
		this.currentFunction = null;
		/**
		 * @type {(Scope | null)[]}
		 */
        this.scopeStack = [];
		/**
		 * @type {(string | null)[]}
		 */
		this.callStack = [];
        this.analyzedScopes = new Set();

		this.arrayState = new ArrayStateReferrer();

		this.optimizeConfig = {};
    }

	/**
	 * An optimization NOTE -- what the optimizer DID: inlined a call, removed
	 * a variable, deleted dead code, unrolled a loop, ...
	 *
	 * These are not remarks about the program (the way "condition is always
	 * true" is), but a trace of the compilation, so they belong on the console
	 * instead of in the warning list a user is supposed to read.
	 *
	 * @param {string} message
	 */
	logOptimization(message) {
		console.log(`[Optimizer] ${message}`);
	}

	/**
	 * 
	 * @param {{
	 * 	globalScope: Scope,
	 *  typeTable: Map<string, TypeInfo>,
	 *  symbolTable: *,
	 *  structTable: *,
	 *  typedefTable: *
	 * }} analysisResult 
	 * @param {ProgramNode} ast 
	 * @returns 
	 */
    optimize(analysisResult, ast) {
        const { globalScope, symbolTable, typeTable, structTable, typedefTable } = analysisResult;
        
        this.ast = ast;
		/**
		 * @type {Scope}
		 */
        this.globalScope = globalScope;
        this.symbolTable = symbolTable;
        this.typeTable = typeTable;
        this.structTable = structTable;
        this.typedefTable = typedefTable;
        
        this.modified = false;
        this.errors = [];
        this.warnings = [];
        this.analyzedScopes.clear();

		if (this.compiler && this.compiler.extraConfig) {
			this.optimizeConfig = this.compiler.extraConfig.getAttribute('optimizeOption') ?? {};
		}
        
		/*
		Every `for` becomes a `while` before anything looks at the program, so
		that there is only one kind of loop to reason about.
		*/
		this.convertForLoopsToWhile();

        // 多轮优化
        let iteration = 0;
        const maxIterations = 10;
        
        do {
            this.modified = false;
			console.log(`=== Begin optimization round ${iteration} ===`);
            this.optimizationPass();
			this.analyzedScopes.clear();
			console.log(`=== End optimization round ${iteration} ===`)
            iteration++;
			// debug:
			//console.log(iteration + ' -th optimize:');
			//console.log(this);
        } while (this.modified && iteration < maxIterations);

		// Once everything is known, promote fully determined arrays
		this.finalizeConstantArrays();

        return {
            success: this.errors.length === 0,
            ast: this.ast,
            errors: this.errors,
            warnings: this.warnings,
            optimized: iteration > 1,
			total_iterations: iteration	/* Manually added for debug */
        };
    }

    optimizationPass() {
		this.arrayState.clear();
		// A fresh simulation budget for this round
		this.grossSimulationBudget = null;

        // 1. 收集全局信息（函数调用图等）
		this.visitedFunction = new Set();
		/**
		 * 
		 * @param {Scope} scope 
		 */
		const initializeScopes = scope => {
			scope.symbols.forEach(child => {
				child.isConstant = false;
			})
			scope.children.forEach(child => initializeScopes(child));
			this.localConstants.set(scope.getPath(), new Map());
			this.variableUses.set(scope.getPath(), new Map());
		};
		initializeScopes(this.globalScope);

        this.collectGlobalInfo();

        // 3. 按作用域递归分析
        this.analyzeScope(this.globalScope);

		// 4. Decide which pointers may store a NAME instead of an address
		this.analyzeNamePointers();
		// Needs the verdict of the pass above: `int *p = &x` is fine only when p
		// is a name pointer.
		this.markBuiltinOutputTargets();

		// Scope optimizing done in global analyses
		/*
		const optimizingScope = scope => {
			// First optimize children scopes!
			scope.children.forEach(child => optimizingScope(child));
			this.optimizeScope(scope);
		};
		optimizingScope(this.globalScope);
		*/
		
		// 2. 删除无用函数
        this.removeUnusedFunctions();
        
        // 4. 函数内联
        this.inlineFunctions();
        
        // 5. 全局死代码消除
        this.globalDeadCodeElimination();
    }

    // =============== 作用域分析 ===============

	// ! Manually adjusted !
	// This actually direct finds program entry
	/**
	 * Decide which pointers may store the NAME of what they point at
	 * (`read`/`write @this <name>`) instead of a `block`/`pos` pair.
	 *
	 * Off unless `#pragma optimize(pointer)` is given, and it needs v150.
	 * Even then a pointer only qualifies when
	 *
	 *  - everything ever stored into it is the address of a plain register
	 *    variable, or of a member of a register struct (which is a register
	 *    too), or of another qualifying pointer;
	 *  - what it points at fits in ONE register -- a traditional pointer takes
	 *    two, so `&p` is only allowed when p is itself a name pointer;
	 *  - it is never involved in pointer arithmetic (`p + 2`, `p[2]`, `p++`),
	 *    which only makes sense for arrays;
	 *  - it is never assigned from an array, nor into an array;
	 *  - and it does not live inside a recursive function, where a register
	 *    would not survive the nested call.
	 *
	 * Pointers that are assigned to one another share a verdict, so a pointer
	 * can never end up holding a name while a related one holds an address.
	 */
	/**
	 * Find every variable whose address is only ever taken by a builtin that
	 * WRITES through it -- `ulocate` and `ucontrol` take their output
	 * parameters as register NAMES (`ulocate ... outx outy found building`).
	 *
	 * For such a variable the name is all that is needed, so it can stay a
	 * register. This is not the name pointer optimisation: nothing is stored
	 * anywhere and no `read @this` is involved, so it applies at every version,
	 * with or without `#pragma optimize(pointer)` -- which only governs
	 * pointers that keep a name in a variable.
	 *
	 * One ordinary `&x` (`int *p = &x`, say) needs a real address, and then x
	 * goes to memory as usual.
	 */
	markBuiltinOutputTargets() {
		/** @type {Map<string, {total: number, builtin: number, symbol: any}>} */
		const uses = new Map();
		const isOutputBuiltin = name => name === 'ulocate' || name === 'ucontrol';
		const symbolOf = (expr, scope) => {
			if (!expr || expr.type !== 'Identifier') return null;
			return scope?.lookup?.(expr.name) ?? null;
		};
		const note = (symbol, builtin, pointerSymbol = null, label = '?') => {
			if (!symbol) return;
			const key = symbol.getAssemblySymbol?.() ?? symbol.name ?? null;
			if (!key) return;
			if (!uses.has(key)) uses.set(key, { occurrences: [], symbol });
			uses.get(key).occurrences.push({
				builtin: builtin === true,
				pointerSymbol,
				label: arguments.length > 3 ? arguments[3] : '?'
			});
		};

		const walk = (node, scope) => {
			if (!node || typeof node !== 'object') return;
			if (Array.isArray(node)) { node.forEach(child => walk(child, scope)); return; }
			if (node.type === 'FunctionCall' || node.type === 'BuiltinCall') {
				// A builtin keeps its name on `functionName`, a user call on `callee`
				const callee = node.callee?.name ?? node.functionName ?? node.name ?? null;
				for (const arg of node.arguments ?? []) {
					if (arg && arg.type === 'UnaryExpression' && arg.operator === '&') {
						/*
						The walk reaches this node again when it descends into
						`arguments`; that pass must not count it a second time as
						an ordinary address use.
						*/
						arg.__outputParameter = true;
						note(symbolOf(arg.argument, scope), isOutputBuiltin(callee), null, 'call:' + callee);
					}
				}
			} else if (node.type === 'UnaryExpression' && node.operator === '&'
				&& !node.__outputParameter) {
				note(symbolOf(node.argument, scope), false, null, 'bare:' + node.argument?.type);
			}
			/*
			`int *p = &x` and `p = &x`: whether x needs a REAL address depends on
			whether p keeps names, which the pointer pass above has just decided
			-- so remember the pointer and judge it afterwards.
			*/
			const pointerTarget = name => {
				if (!name || typeof name !== 'string') return null;
				const target = scope?.lookup?.(name) ?? null;
				if (!target) return null;
				const kind = target.myType?.()?.kind ?? target.type?.type?.kind ?? null;
				return kind === 'pointer' ? target : null;
			};
			if (node.type === 'VariableDeclarator' && node.initializer
				&& node.initializer.type === 'UnaryExpression'
				&& node.initializer.operator === '&') {
				const target = pointerTarget(node.name);
				if (target) {
					node.initializer.__outputParameter = true;
					note(symbolOf(node.initializer.argument, scope), false, target, 'decl');
				}
			}
			if (node.type === 'AssignmentExpression' && node.left?.type === 'Identifier'
				&& node.right?.type === 'UnaryExpression' && node.right.operator === '&') {
				const target = pointerTarget(node.left);
				if (target) {
					node.right.__outputParameter = true;
					note(symbolOf(node.right.argument, scope), false, target);
				}
			}
			if (node.children) node.children.forEach(child => walk(child, scope));
			const fields = ['functions', 'globalDeclarations', 'typeDefinitions', 'statements',
				'expression', 'test', 'consequent', 'alternate', 'body', 'init', 'update',
				'argument', 'left', 'right', 'declarators', 'arguments', 'callee', 'initializer'];
			for (const field of fields) {
				if (Array.isArray(node[field])) node[field].forEach(child => walk(child, scope));
				else if (node[field] && typeof node[field] === 'object') walk(node[field], scope);
			}
		};
		this.ensureFunctionDeclarations();

		const walkScope = scope => {
			if (scope?.astNode) walk(scope.astNode, scope);
			scope?.children?.forEach(walkScope);
		};
		walkScope(this.globalScope);

		this.recursiveFunctions ??= this.findRecursiveFunctions?.(this.functionCallGraph) ?? new Set();
		for (const use of uses.values()) {
			/*
			Every single `&x` has to be nameable: either a builtin output
			parameter (the instruction takes the name anyway), or stored into a
			pointer that itself keeps names (then the name is what p holds and
			nothing needs a real address).
			*/
			const nameable = use.occurrences.every(o =>
				o.builtin
				// The pointer never survived (dead code) -- nothing reads the address
				|| o.pointerSymbol === null && o.dead === true
				|| o.pointerSymbol?.usesNamePointer === true);
			if (!nameable) continue;
			const symbol = use.symbol;
			const type = symbol?.myType?.() ?? symbol?.type?.type ?? null;
			if (!type) continue;
			// Only something that fits a single register can be named
			if (['pointer', 'array', 'struct', 'union'].includes(type.kind)) continue;
			// These have to live in memory, so there is no register to name
			if (symbol.isVolatile || symbol.isExtern || symbol.isStatic) continue;
			if (this.isRecursiveSymbol?.(symbol)) continue;
			symbol.addressedAsBuiltinOutput = true;
		}
	}

	/**
	 * Index every function declaration by name, so a call site can find the
	 * PARAMETER symbol an argument lands in.
	 */
	ensureFunctionDeclarations() {
		if (this.functionDeclarations) return this.functionDeclarations;
		/** @type {Map<string, any>} */
		this.functionDeclarations = new Map();
		const collect = node => {
			if (!node || typeof node !== 'object') return;
			if (Array.isArray(node)) { node.forEach(collect); return; }
			if (node.type === 'FunctionDeclaration' && node.name) {
				this.functionDeclarations.set(node.name, node);
			}
			for (const key of ['functions', 'globalDeclarations', 'statements',
				'declarations', 'children', 'body']) {
				const child = node[key];
				if (Array.isArray(child)) child.forEach(collect);
				else if (child && typeof child === 'object') collect(child);
			}
		};
		collect(this.ast ?? this.globalScope?.astNode ?? null);
		const collectFromScope = scope => {
			if (scope?.astNode?.type === 'FunctionDeclaration' && scope.astNode.name) {
				this.functionDeclarations.set(scope.astNode.name, scope.astNode);
			}
			scope?.children?.forEach(collectFromScope);
		};
		collectFromScope(this.globalScope);
		return this.functionDeclarations;
	}

	analyzeNamePointers() {
		const config = this.optimizeConfig ?? {};
		if (config.doPointerOptimize !== true) return;
		const version = this.compiler?.config?.getAttribute('targetVersion') ?? 0;
		if (version < 150) return;
		// Which functions call themselves (directly or not)? Nothing local to
		// one can be kept in a register.
		this.recursiveFunctions = this.findRecursiveFunctions?.(this.functionCallGraph) ?? new Set();

		/** @type {Map<string, any>} */
		const pointers = new Map();
		/** @type {Map<string, Set<string>>} */
		const addressTaken = new Map();

		/**
		 * Does a value of this type fit in a single register?
		 * @param {any} type
		 * @returns {boolean}
		 */
		const isSingleRegister = (type) => {
			if (!type || !type.isTypeInfo) return false;
			switch (type.kind) {
				case 'pointer':
					// A traditional pointer takes two words (`_block` / `_pos`);
					// only a name pointer takes one.
					return false;
				case 'array':
				case 'struct':
				case 'union':
					return false;
				default:
					return true;
			}
		};

		const keyOf = symbol => symbol?.getAssemblySymbol?.() ?? symbol?.name ?? null;

		/**
		 * Look a name up in `scope`, falling back to a search over every scope:
		 * the pass runs on a scope tree where a call site may be visited from a
		 * scope that does not chain up to the one the variable lives in.
		 * @param {any} scope
		 * @param {string?} name
		 */
		const lookupName = (scope, name) => {
			if (!name) return null;
			const direct = scope?.lookup?.(name) ?? null;
			if (direct) return direct;
			let found = null;
			const visit = sc => {
				if (found) return;
				sc?.symbols?.forEach(sym => {
					if (!found && (sym.name === name)) found = sym;
				});
				if (!found) sc?.children?.forEach(visit);
			};
			visit(this.globalScope);
			return found;
		};

		const typeOfSymbol = symbol => symbol?.myType?.() ?? symbol?.type?.type ?? null;

		const isPointerSymbol = symbol => typeOfSymbol(symbol)?.kind === 'pointer';

		const isArraySymbol = symbol => {
			const type = typeOfSymbol(symbol);
			return !!type && (type.kind === 'array');
		};

		/**
		 * @param {any} symbol
		 * @returns {any} the entry describing this pointer
		 */
		const register = symbol => {
			const key = keyOf(symbol);
			if (!key) return null;
			if (!pointers.has(key)) {
				pointers.set(key, {
					key, symbol,
					sources: new Set(),	// what may be stored into it
					peers: new Set(),	// pointers assigned to / from it
					bad: false
				});
			}
			return pointers.get(key);
		};

		/**
		 * Classify `&expr`, i.e. whether the target is a single register whose
		 * name can be stored.
		 * @param {any} expr
		 * @param {any} scope
		 * @returns {{kind: string, target: string?, targetSymbol: any?}}
		 */
		const classifyAddress = (expr, scope) => {
			if (!expr) return { kind: 'invalid', target: null, targetSymbol: null };
			if (expr.type === 'UnaryExpression' && expr.operator === '*') {
				// `&*q` is just `q`
				return classify(expr, scope);
			}
			if (expr.type === 'Identifier') {
				const symbol = lookupName(scope, expr.name);
				if (!symbol) return { kind: 'invalid', target: null, targetSymbol: null };
				if (isPointerSymbol(symbol)) {
					// Pointing at a pointer: only allowed if that one is a name pointer
					return { kind: 'pointer', target: keyOf(symbol), targetSymbol: symbol };
				}
				if (isArraySymbol(symbol)) return { kind: 'invalid', target: null, targetSymbol: symbol };
				volatileGuard(symbol);
				if (symbol.isVolatile || symbol.isExtern || symbol.isStatic) {
					return { kind: 'invalid', target: null, targetSymbol: symbol };
				}
				if (!isSingleRegister(typeOfSymbol(symbol))) {
					return { kind: 'invalid', target: null, targetSymbol: symbol };
				}
				return { kind: 'register', target: keyOf(symbol), targetSymbol: symbol };
			}
			if (expr.type === 'MemberExpression') {
				// `&s.m`: allowed when s is a register struct, because then the
				// member is a register of its own
				const object = expr.children?.[0] ?? expr.object ?? null;
				if (!object || object.type !== 'Identifier') {
					return { kind: 'invalid', target: null, targetSymbol: null };
				}
				const owner = scope?.lookup(object.name) ?? null;
				if (!owner) return { kind: 'invalid', target: null, targetSymbol: null };
				const type = typeOfSymbol(owner);
				if (!type || !['struct', 'union'].includes(type.kind)) {
					return { kind: 'invalid', target: null, targetSymbol: null };
				}
				if (type.isPointerImpl?.()) return { kind: 'invalid', target: null, targetSymbol: owner };
				return { kind: 'register', target: null, targetSymbol: owner };
			}
			return { kind: 'invalid', target: null, targetSymbol: null };
		};

		/**
		 * Classify a value that is stored into a pointer.
		 * @param {any} expr
		 * @param {any} scope
		 * @returns {{kind: string, target: string?, targetSymbol: any?}}
		 */
		const classify = (expr, scope) => {
			if (!expr) return { kind: 'invalid', target: null, targetSymbol: null };
			switch (expr.type) {
				case 'UnaryExpression':
					if (expr.operator !== '&') return { kind: 'invalid', target: null, targetSymbol: null };
					return classifyAddress(expr.argument, scope);
				case 'Identifier': {
					const symbol = scope?.lookup(expr.name) ?? null;
					if (!symbol) return { kind: 'invalid', target: null, targetSymbol: null };
					if (isPointerSymbol(symbol)) {
						return { kind: 'pointer', target: keyOf(symbol), targetSymbol: symbol };
					}
					// `p = arr` : an array decays to a real address
					return { kind: 'invalid', target: null, targetSymbol: symbol };
				}
				case 'BinaryExpression':
				case 'MemberExpression':
				case 'FunctionCall':
				case 'CastExpression':
				default:
					// Arithmetic on a pointer walks an array; anything unknown is
					// refused rather than guessed at.
					return { kind: 'invalid', target: null, targetSymbol: null };
			}
		};

		const volatileGuard = symbol => { };

		/**
		 * Note that `expr` is stored into `pointerSymbol`.
		 * @param {any} pointerSymbol
		 * @param {any} expr
		 * @param {any} scope
		 * @param {boolean} [extraBad]
		 */
		const note = (pointerSymbol, expr, scope, extraBad = false) => {
			const entry = register(pointerSymbol);
			if (!entry) return;
			if (extraBad) entry.bad = true;
			// Anything inside a recursive function must live in memory
			if (this.isRecursiveSymbol?.(pointerSymbol)) entry.bad = true;
			const info = classify(expr, scope);
			entry.sources.add(info.kind);
			if (info.kind === 'pointer' && info.target) {
				entry.peers.add(info.target);
			} else if (info.kind !== 'register') {
				entry.bad = true;
			}
			if (info.kind === 'register' && info.target) {
				if (!addressTaken.has(info.target)) addressTaken.set(info.target, new Set());
				addressTaken.get(info.target).add(entry.key);
			}
			if (info.kind === 'register' && info.targetSymbol && this.isRecursiveSymbol?.(info.targetSymbol)) {
				// The target itself lives in a recursive function: not a register
				entry.bad = true;
			}
		};

		/**
		 * @param {any} node
		 * @param {any} scope
		 */
		const walk = (node, scope) => {
			if (!node || typeof node !== 'object') return;
			if (Array.isArray(node)) { node.forEach(child => walk(child, scope)); return; }
			switch (node.type) {
				case 'AssignmentExpression': {
					const left = node.left;
					if (left && left.type === 'Identifier') {
						const symbol = scope?.lookup(left.name) ?? null;
						if (isPointerSymbol(symbol)) {
							note(symbol, node.right, scope, node.operator !== '=');
						} else if (isArraySymbol(symbol)) {
							// `arr = p` : the pointer has to be a real address
							const source = scope?.lookup(node.right?.name ?? '') ?? null;
							if (isPointerSymbol(source)) register(source).bad = true;
						}
					}
					break;
				}
				case 'VariableDeclarator': {
					if (node.name && node.initializer) {
						const symbol = scope?.lookup(node.name) ?? null;
						if (isPointerSymbol(symbol)) note(symbol, node.initializer, scope);
					}
					break;
				}
				case 'FunctionCall': {
					/*
					`oper(&x)` stores the address into the PARAMETER. The
					parameter is a pointer too, so it is judged by the same rules
					-- if it can hold a name, x never needs a real address.
					*/
					const declaration = this.ensureFunctionDeclarations()
						.get(node.functionName ?? node.callee?.name ?? null) ?? null;
					if (declaration) {
						(declaration.parameters ?? []).forEach((parameter, i) => {
							const argument = node.arguments?.[i];
							if (!argument) return;
							const name = parameter?.name?.name ?? parameter?.name ?? parameter;
							const symbol = declaration.scope?.lookup?.(name)
								?? declaration.body?.scope?.lookup?.(name) ?? null;
							if (isPointerSymbol(symbol)) note(symbol, argument, scope);
						});
					}
					break;
				}
				case 'BinaryExpression': {
					/*
					`p + 1` (and `p - q`) need a real ADDRESS to move. A pointer
					that holds a name has none, so such a pointer must not be
					given the name representation -- otherwise the emitted code
					copies its (never assigned) `_block` / `_pos` and walks off
					to a link that does not exist.
					*/
					const op = node.operator;
					if (op === '+' || op === '-') {
						for (const side of [node.left, node.right]) {
							if (!side || side.type !== 'Identifier') continue;
							const symbol = scope?.lookup(side.name) ?? null;
							if (isPointerSymbol(symbol)) register(symbol).bad = true;
						}
					}
					break;
				}
				case 'AssignmentExpression': {
					/*
					`p += 1` is the same thing written another way.
					*/
					if (node.operator === '+=' || node.operator === '-=') {
						const left = node.left;
						if (left && left.type === 'Identifier') {
							const symbol = scope?.lookup(left.name) ?? null;
							if (isPointerSymbol(symbol)) register(symbol).bad = true;
						}
					}
					break;
				}
				case 'MemberExpression': {
					// `p[i]` is pointer arithmetic
					const object = node.children?.[0] ?? node.object ?? null;
					if (object && object.type === 'Identifier') {
						const symbol = scope?.lookup(object.name) ?? null;
						if (isPointerSymbol(symbol)) register(symbol).bad = true;
					}
					break;
				}
				case 'UnaryExpression': {
					if ((node.operator === '++' || node.operator === '--') && node.argument) {
						const symbol = scope?.lookup(node.argument.name) ?? null;
						if (isPointerSymbol(symbol)) register(symbol).bad = true;
					}
					break;
				}
				default: break;
			}
			if (node.children) node.children.forEach(child => walk(child, scope));
			const fields = ['functions', 'globalDeclarations', 'typeDefinitions', 'statements',
				'expression', 'test', 'consequent', 'alternate', 'body', 'init', 'update',
				'argument', 'left', 'right', 'declarators', 'arguments', 'callee', 'initializer'];
			for (const field of fields) {
				if (Array.isArray(node[field])) node[field].forEach(child => walk(child, scope));
				else if (node[field] && typeof node[field] === 'object') walk(node[field], scope);
			}
		};

		const walkScope = scope => {
			if (scope?.astNode) walk(scope.astNode, scope);
			scope?.children?.forEach(walkScope);
		};
		walkScope(this.globalScope);

		/*
		`&x` handed straight to a builtin that writes through it (`ulocate`,
		`ucontrol "within"`). The processor can write into the register a name
		pointer names, so x may stay a register -- as long as EVERY `&x` in the
		program is such an output parameter.
		*/
		if (pointers.size === 0) return;

		/* Group pointers that are assigned to one another: they share a verdict. */
		const byName = new Map();
		for (const [key, entry] of pointers) byName.set(entry.symbol?.name ?? key, key);
		const groups = [];
		const seen = new Set();
		const collect = (key, group) => {
			if (group.has(key)) return;
			group.add(key);
			const entry = pointers.get(key);
			if (!entry) return;
			for (const peer of entry.peers) {
				const peerKey = byName.get(peer) ?? peer;
				if (pointers.has(peerKey)) collect(peerKey, group);
			}
			// Anything pointing AT this pointer shares its fate too
			for (const [otherKey, other] of pointers) {
				if (other.peers.has(key)) collect(otherKey, group);
			}
		};
		for (const key of pointers.keys()) {
			if (seen.has(key)) continue;
			const group = new Set();
			collect(key, group);
			group.forEach(k => seen.add(k));
			groups.push(group);
		}

		/* Fixpoint: a group qualifies only if every source of every member is a
		   register or an already qualified pointer. */
		const eligible = new Set();
		let changed = true;
		while (changed) {
			changed = false;
			for (const group of groups) {
				if (group.size === 0) continue;
				let ok = true;
				for (const key of group) {
					const entry = pointers.get(key);
					/*
					`auto` version made for real addresses: it must not be given
					the name representation, even if every call site would allow
					it -- the body may move the pointer.
					*/
					if (entry?.symbol?.noNamePointer === true) { ok = false; break; }
					if (!entry || entry.bad) { ok = false; break; }
					for (const source of entry.sources) {
						if (source !== 'register' && source !== 'pointer') { ok = false; break; }
					}
					if (!ok) break;
					for (const peer of entry.peers) {
						const peerKey = byName.get(peer) ?? peer;
						if (!pointers.has(peerKey)) continue;
						// A peer inside the same group shares this group's verdict,
						// so it cannot block it -- otherwise `int **pp = &p` and
						// `int *p = &x` would deadlock.
						if (group.has(peerKey)) continue;
						if (!eligible.has(peerKey)) { ok = false; break; }
					}
					if (!ok) break;
				}
				if (ok && ![...group].every(k => eligible.has(k))) {
					group.forEach(k => eligible.add(k));
					changed = true;
				}
			}
		}

		for (const key of eligible) {
			const entry = pointers.get(key);
			if (entry) entry.symbol.usesNamePointer = true;
		}

		/*
		A variable whose address is only ever taken by name pointers does not
		have to live in memory at all -- that is the whole point.
		*/
		for (const [target, takers] of addressTaken) {
			if (takers.size === 0) continue;
			const allNames = [...takers].every(t => eligible.has(t));
			if (allNames) this.markNameOnlyTarget(target);
		}
	}

	/**
	 * Whether the symbol belongs to a recursive function.
	 * @param {any} symbol
	 * @returns {boolean}
	 */
	isRecursiveSymbol(symbol) {
		if (!symbol || symbol.isStatic) return false;
		const region = symbol.getFunctionRegion?.() ?? null;
		return !!region && this.recursiveFunctions?.has(region);
	}

	/**
	 * Record that the variable called `assemblyName` is only ever reached
	 * through name pointers, so it may stay in a register.
	 * @param {string} assemblyName
	 */
	markNameOnlyTarget(assemblyName) {
		const visitScope = scope => {
			if (!scope) return;
			scope.symbols?.forEach(symbol => {
				if ((symbol.getAssemblySymbol?.() ?? null) === assemblyName) {
					symbol.namePointersOnly = true;
				}
			});
			scope.children?.forEach(visitScope);
		};
		visitScope(this.globalScope);
	}

    analyzeScope(scope) {
        if (this.analyzedScopes.has(scope)) {
            return;
        }
        
        this.analyzedScopes.add(scope);
		
		// 进入当前作用域
        this.enterScope(scope);
        /*
        // 首先分析子作用域（从内到外）
        scope.children.forEach(childScope => {
            this.analyzeScope(childScope);
        });
        */
        // 分析当前作用域的AST节点
        if (scope.astNode) {
            this.analyzeNode(scope.astNode);
        }
        
        // 进行作用域特定的优化
        this.optimizeScope(scope);
        
        // 退出作用域
        this.exitScope();
    }

    enterScope(scope) {
        this.scopeStack.push(this.currentScope);
        this.currentScope = scope;
		// 初始化当前作用域的常量表和变量使用统计
        const scopePath = scope.getPath();
        // this.localConstants.set(scopePath, new Map());	// Can be removed
        // this.variableUses.set(scopePath, new Map());
    }

    exitScope() {
		const scopePath = this.currentScope.getPath();
        this.currentScope = this.scopeStack.pop();
    }

	/**
	 * 
	 * @param {string} name 
	 */
	enterFunction(name) {
		this.callStack.push(this.currentFunction);
		this.currentFunction = name;
	}

	exitFunction() {
		this.currentFunction = this.callStack.pop();
	}

    getCurrentScopePath() {
        return this.currentScope ? this.currentScope.getPath() : 'g';
    }

	/**
	 * 
	 * @param {Scope} scope 
	 * @param {string} varName 
	 * @param {Scope} targetScope
	 * @param {string} targetName 
	 * @remarks Duplicating a variable does not mean duplicating its variable name reference.
	 */
	duplicateVariable(scope, varName, targetScope, targetName) {
		let varRef = scope.lookupCurrent(varName);
		if (!varRef) {
			return;
		}
		varRef = varRef.duplicate();
		varRef.name = targetName;
		varRef.variableReferrer = new VariableLinker(targetName, varRef);
		targetScope.addSymbol(varRef);
		const scopePath = scope.getPath();
		const targetPath = targetScope.getPath();
		const variableScope = this.variableUses.get(scopePath);
		let targetVariableScope = this.variableUses.get(targetPath);
		if (!variableScope) {
			return;
		}
		if (!targetVariableScope) {
			targetVariableScope = new Map();
			this.variableUses.set(targetPath, targetVariableScope);
		}
		let varState = variableScope.get(varName);
		if (!varState || !varState.reads || !varState.writes ) {
			varState = {
				reads: 1,
				writes: 1,
				canOptimize: true
			};	// Prevent unexpected optimizing
		}
		targetVariableScope.set(targetName, { ...varState });
	}

    // =============== 节点分析 ===============

	/**
	 * 
	 * @param {ASTNode} node 
	 * @param {*} info 
	 * @returns 
	 */
    analyzeNode(node, info = null) {
        if (!node) return;

		const inheritances = ['hasBreak', 'hasContinue', 'returnCount'];	// Use OR-merge
		const copies = ['isLoop', 'isUnsure', 'firstLoopRun', 'inFunction', 'actualCalling', 'optimizingLoop', 'cannotCascade', 'fromCallNodeAnalysis', 'strangeLoop', 'modifiedVars',
				'atLoop', 'atUnsure', 'atFunction'	// at-components are about the scopes.
		];
		const clarifications = ['hasBreak', 'hasContinue', 'isLoop', 'isUnsure', 'firstLoopRun', 'inFunction', 'actualCalling', 'optimizingLoop', 'cannotCascade', 'fromCallNodeAnalysis', 'strangeLoop'];
        
		if (info && info.parentInfo) {
			for (const att of copies) {
				info[att] = info[att] ?? info.parentInfo[att];
			}
			for (const att of inheritances) {
				info[att] = info[att] ?? info.parentInfo[att];
			}
			for (const att of clarifications) {
				info[att] = Boolean(info[att]);
			}
		}
		if (info && info.disposeReturn) {
			node.setAttribute('disposeReturn', true);
			let copiedInfo = { ...info };
			copiedInfo.disposeReturn = false;
			copiedInfo.parentInfo = info;
			info = copiedInfo;
		}
		
		// Make replacements before analysis
		const assessment = (info && info.strangeLoop) ? false : this.shouldRunEvaluation(node, info);
		// !!! TODO: Bugs here !!!
		this.restrictiveVariables = null;
		if (!(info && (info.optimizingLoop))) {
			if (assessment) {
				this.replaceConstantAtPresent(node);
			} else if (assessment === null) {
				if (info.firstLoopRun) {
					if (info.modifiedVars && !info.modifiedVars.has("unrecognized")) {
						this.replaceConstantAtPresent(node, false, info.modifiedVars);
					} else {
						this.replaceConstantAtPresent(node, true);
					}
				} else {
					this.replaceConstantAtPresent(node);
				}
			} else {
				this.replaceConstantAtPresent(node, true);
			}
		}

		try {
			switch (node.type) {
				case 'Program':
					// this.functionInfo.clear();	// Reserved for further use
					if (node.globalDeclarations) {
						node.globalDeclarations.forEach(decl => this.analyzeNode(decl, info));
					}
					if (node.functions) {
						node.functions.forEach(func => {
							if (func.scope) {
								func.scope.recursivelyGetAllSymbols().forEach(
									/**
									 * 
									 * @param {SymbolEntry} symb 
									 */
								symb => {
									if (symb.isArray()) this.loadArrayForSymbol(symb);	// Do this first, and sth else later!
								});
							}
						});
						node.functions.forEach(func => {
							if (func.name === 'main') {
								this.analyzeNode(func, {
									actualCalling: true,
									parentInfo: info
								});
							}
						});	// Starting just from main function to prevent all those strange problems!
						const recursiveGraph = this.findRecursiveFunctions(this.functionCallGraph);
						node.functions.forEach(func => {
							const relevantSymbol = this.globalScope.lookup(func.name);
							/*
							Gone means it was inlined everywhere and dropped:
							there is nothing left to analyse. (A function built
							by the compiler -- an `auto` instantiation -- is
							dropped the same way once every call was inlined.)
							*/
							if (!relevantSymbol) return;
							if (relevantSymbol.isAddressed || recursiveGraph.has(func.name)) {
								this.analyzeNode(func, {
									actualCalling: false,
									parentInfo: info
								});
							}
						});
					}
					break;
				
				case 'FunctionDeclaration':
					this.analyzeFunction(node, info);
					break;
					
				case 'VariableDeclaration':
					this.analyzeVariableDeclaration(node, info);
					break;
					
				case 'CompoundStatement':
					this.analyzeCompoundStatement(node, info);
					break;
					
				case 'ExpressionStatement':
					//this.analyzeExpressionStatement(node, info);
					break;
					
				case 'AssignmentExpression':
					this.analyzeAssignmentExpression(node, info);
					break;
					
				case 'Identifier':
					this.analyzeIdentifier(node, info);
					break;
					
				case 'BinaryExpression':
					this.analyzeBinaryExpression(node, info);
					break;
					
				case 'UnaryExpression':
					this.analyzeUnaryExpression(node, info);
					break;
					
				case 'IfStatement':
				case 'ConditionalExpression':
					this.analyzeIfStatement(node, info);
					break;
					
				case 'WhileStatement':
					this.analyzeWhileStatement(node, info);
					break;
					
				case 'ForStatement':
					this.analyzeForStatement(node, info);
					break;
					
				case 'ReturnStatement':
					this.analyzeReturnStatement(node, info);
					break;
					
				case 'FunctionCall':
					this.analyzeFunctionCall(node, info);
					break;
					
				case 'BreakStatement':
					this.analyzeBreakStatement(node, info);
					break;
					
				case 'ContinueStatement':
					this.analyzeContinueStatement(node, info);
					break;

				case 'CastExpression':
					this.analyzeCastExpression(node, info);
					break;
				
				case 'BuiltinCall':
					const builtins = info ? { ...info } : {};
					builtins.callNode = node;
					node.arguments.forEach(args => {
						this.analyzeNode(args, builtins);
					});
					break;
					
				case 'MemberExpression':
					// Don't further analyze them!!!
					this.analyzeNode(node.children[0], info);
					if (node.getAttribute('computed')) {
						this.analyzeNode(node.children[1], info);
					}
					// Otherwise it is NOT a valid variable!!
					return;
					break;
					
				default:
					// 递归分析子节点
					
					break;
			}
		} catch (err) {
			if (err instanceof RangeError) {
				console.log(`Exceeding maximum range of iteration: `, err);
			} else {
				throw err;
			}
		}

		if (node.declarators) {
			this.analyzeVariableDeclaration(node, info);
		}
		if (node.children) {
			this.analyzeStatementList(node, node.children, info,
				node.type === 'ExpressionStatement');
		}
		if (info && info.parentInfo) {
			for (const att of inheritances) {
				info.parentInfo[att] = info[att] || info.parentInfo[att];
			}
		}
    }

    // =============== 具体分析函数 ===============
	
	/**
	 * Analyse a list of child nodes.
	 *
	 * The list is snapshotted first: analysing a statement can REMOVE it (a
	 * folded `if`, a dead assignment) and `replaceNode` splices the array we
	 * are iterating. Iterating the live array then skips the statement that
	 * moved into the removed one's place -- which is how the `while (true)` in
	 * `main` of example7a went unanalysed, leaving the call graph empty and
	 * every other function looking unused.
	 *
	 * @param {ASTNode} owner
	 * @param {ASTNode[]} list
	 * @param {any} info
	 * @param {boolean} [disposeReturnAllButLast]
	 */
	analyzeStatementList(owner, list, info, disposeReturnAllButLast = false) {
		if (!list) return;
		const snapshot = [...list];
		const last = snapshot[snapshot.length - 1];
		for (const child of snapshot) {
			if (!child) continue;
			// Gone in the meantime (removed, or replaced by something else)
			if (!list.includes(child)) continue;
			this.analyzeNode(child, {
				parentInfo: info,
				disposeReturn: disposeReturnAllButLast && child !== last
			});
		}
	}

	// These functions are manually implemented as I have reached maximum conversation length of Deepseek:
	
	// ! Manually added scope entrance !
	analyzeCompoundStatement(node, info = null) {
		let enteredScope = false;
		if (node.scope && node.scope.getPath() !== this.currentScope.getPath()) {
			enteredScope = true;
			this.enterScope(node.scope);
		}
		if (node.statements) {
			this.analyzeStatementList(node, node.statements, info);
		}
		if (enteredScope) {
			this.exitScope();
		}
	}
	
	/**
	 * 
	 * @param {ASTNode} node 
	 * @param {*} info 
	 * @deprecated
	 */
	analyzeExpressionStatement(node, info = null) {
		if (node.children) {
			node.children.forEach(child => this.analyzeNode(child, info));
		}
	}
	
	/**
	 * 
	 * @param {CastExpression} node 
	 * @param {*} info 
	 */
	analyzeCastExpression(node, info = null) {
		this.analyzeNode(node.expression, info);

		if (node.dataType && (node.dataType.isVolatile() || node.dataType.isPointerImpl())) {
			this.markNodeAsWithSideEffects(node);
		}
	}

	// TODO: Value assignment might be a kind of writing !!!
	analyzeBinaryExpression(node, info = null) {
		//const scopePath = this.getCurrentScopePath();
		//const localConstants = this.localConstants.get(scopePath);
		this.analyzeNode(node.left, info);
		this.analyzeNode(node.right, info);

		const downgradeArray = subNode => {
			if (subNode.symbol && subNode.symbol.isArray()) {
				this.arrayState.downgrade(subNode.symbol.getAssemblySymbol());
			}
		};

		downgradeArray(node.left);
		downgradeArray(node.right);

		if (node.operator.includes('=') && node.operator !== '==' && node.operator !== '>=' && node.operator !== '<=' && node.operator !== '!=') {
			// Assignment
			if (node.left.type === 'Identifier') {
				this.addWriteForCurrentScope(node.left.name, 
					(info.isUnsure || !info.actualCalling) ? null : this.evaluateConstantExpression(node), 
					this.shouldRunEvaluation(node, info));
			} else {
				// Something special -- this component will have side effect
				// ! New attribute label !
				node.setAttribute('maybeHasSideEffects', true);
			}
		}
	}
	
	// Return whether value should be updated right away (the exception is in the pre-analysis of loop)
	// This is a manually-added function
	/**
	 * 
	 * @param {ASTNode} node 
	 * @param {*} info 
	 * @param {SymbolEntry?} [symbol=null] 
	 * @returns {boolean?} `null` means 'do not make changes on variable states'.
	 */
	/**
	 * --------------------------------------------------------------------
	 * How much is known about an expression?
	 * --------------------------------------------------------------------
	 *
	 * Every place that decides "may I rely on what this expression does?"
	 * answers one of exactly three things:
	 *
	 *   EVAL_DEFINITE -- the value is known for sure, and recording it is safe.
	 *                    Constant folding, branch pruning, loop unrolling.
	 *
	 *   EVAL_TRACKED  -- the VALUE is not known, but the ACCESSES are: we know
	 *                    that a read/write happens and to which symbol. Good
	 *                    enough for usage counting and dead-store removal, NOT
	 *                    good enough to write a value into the constant table.
	 *
	 *   EVAL_UNKNOWN  -- nothing can be relied upon at all: the code may not
	 *                    even run (an unsure branch), or it runs an unknown
	 *                    number of times (a loop). Not even "this variable is
	 *                    written here" is trustworthy.
	 *
	 * `shouldRunEvaluation` below answers with one of these; the helpers
	 * `isDefinite` / `isTracked` are what callers should test with, so that the
	 * three states are never conflated with true/false.
	 */
	static EVAL_DEFINITE = 'definite';
	static EVAL_TRACKED = 'tracked';
	static EVAL_UNKNOWN = 'unknown';

	/**
	 * @param {any} state One of EVAL_DEFINITE / EVAL_TRACKED / EVAL_UNKNOWN
	 * @returns {boolean} Whether the value is known and may be recorded.
	 */
	isDefinite(state) {
		return state === Optimizer.EVAL_DEFINITE;
	}

	/**
	 * @param {any} state One of EVAL_DEFINITE / EVAL_TRACKED / EVAL_UNKNOWN
	 * @returns {boolean} Whether the accesses are known, even if the value is not.
	 */
	isTracked(state) {
		return state === Optimizer.EVAL_DEFINITE || state === Optimizer.EVAL_TRACKED;
	}

	/**
	 * Turn an evaluation state into the `doUpdate` argument of
	 * `addWrittenSymbol`: DEFINITE records the value, TRACKED records only that
	 * a write happens, UNKNOWN records nothing at all.
	 *
	 * @param {any} state
	 * @returns {boolean?}
	 */
	updateFlagFor(state) {
		if (state === Optimizer.EVAL_DEFINITE) return true;
		if (state === Optimizer.EVAL_TRACKED) return null;
		return false;
	}

	/**
	 * How much may be relied upon for `node`.
	 *
	 * @param {ASTNode} node
	 * @param {any} info The analysis context (loop / unsure / function state)
	 * @param {SymbolEntry?} symbol The symbol being touched, if any -- a symbol
	 *        declared *outside* the unsure region keeps its state even inside one
	 * @returns {string} EVAL_DEFINITE, EVAL_TRACKED or EVAL_UNKNOWN
	 */
	evaluationState(node, info = null, symbol = null) {
		const state = Optimizer;
		if (info) {
			/*
			Inside an "unsure" region (a branch we cannot decide): we know the
			code may run, so accesses to symbols declared OUTSIDE the region are
			still known -- but nothing has a definite value.
			*/
			if (info.isUnsure) {
				if (symbol && symbol.scope.isParent(info.atUnsure)) return state.EVAL_DEFINITE;
				return state.EVAL_UNKNOWN;
			}
			/*
			A function that is not actually called (or is called from somewhere
			we do not track): accesses inside it are known, values are not --
			unless the symbol lives outside that function.
			*/
			if (info.inFunction && !info.actualCalling) {
				if (symbol && symbol.scope.isParent(info.atFunction)) return state.EVAL_DEFINITE;
				return state.EVAL_UNKNOWN;	// i.e. uncertain calls
			}
			if (info.optimizingLoop) return state.EVAL_DEFINITE;	// High priority!
			/*
			Inside a loop: a symbol declared outside survives every iteration,
			so its accesses are known; on the first run we can even fold values,
			afterwards nothing is definite.
			*/
			if (info.isLoop) {
				/*
				A symbol declared outside the loop survives every iteration, so
				its ACCESSES are known -- but its VALUE is not, as soon as the
				loop writes to it: `i = i + 1` is not `i = 1`. Matching by name as
				well as by symbol, because a body moved here by inlining may
				still name the symbol it used to have.
				*/
				if (symbol && this.loopModifies(info, symbol)) {
					return info.firstLoopRun ? state.EVAL_TRACKED : state.EVAL_UNKNOWN;
				}
				if (symbol && symbol.scope.isParent(info.atLoop)) return state.EVAL_DEFINITE;
				if (info.firstLoopRun) return state.EVAL_TRACKED;	// Priority relevantly lower
				return state.EVAL_UNKNOWN;
			}
		}
		return state.EVAL_DEFINITE;
	}

	/**
	 * Whether this loop writes to `symbol`.
	 *
	 * @param {any} info the analysis context of the loop
	 * @param {import('./mindcSemantic.js').SymbolEntry} symbol
	 * @returns {boolean}
	 */
	loopModifies(info, symbol) {
		const modified = info?.modifiedVars;
		if (!modified || !symbol) return false;
		for (const entry of modified) {
			if (!entry || typeof entry !== 'object') continue;
			if (entry.symbol === symbol) return true;
			// Fall back on the name: an inlined body may carry a stale symbol
			if (entry.varName && symbol.name && entry.varName === symbol.name) return true;
		}
		return false;
	}

	/**
	 * Legacy boolean entry point, kept because it is used in many places:
	 * `true` means "the value can be trusted".
	 *
	 * @param {ASTNode} node
	 * @param {any} [info]
	 * @param {SymbolEntry?} [symbol]
	 * @returns {boolean}
	 */
	shouldRunEvaluation(node, info = null, symbol = null) {
		const state = this.evaluationState(node, info, symbol);
		/*
		`EVAL_TRACKED` used to come back as `null` from here, and several callers
		distinguish it from `false` (see `addWrittenSymbol`). Keep that: return
		the flag that says how much may be recorded.
		*/
		return this.updateFlagFor(state);
	}

	// Some unary expressions have side	effects of writing (++ and --):
	// ! Manually adjusted !
	analyzeUnaryExpression(node, info = null) {
		const curStat = this.shouldRunEvaluation(node, info);
		if ((node.operator === '++' || node.operator === '--')) {
			if (node.argument.type === 'Identifier') {
				const scopePath = this.currentScope.lookupScopeOf(node.argument.name).getPath();
				const localConstants = this.localConstants.get(scopePath);
				let newConstant = localConstants.get(node.argument.name);
				if (this.canHaveConstantEval(newConstant)) {
					// ! TODO: Only do this for: !
					if (node.operator === '++') newConstant++;
					if (node.operator === '--') newConstant--;
				}
				this.addWriteForCurrentScope(node.argument.name, 
					(info.isUnsure || !info.actualCalling) ? null : newConstant, 
					curStat);
			}
		} else if (node.operator === '&') {
			if (node.argument.type === 'Identifier') {
				const scopePath = this.currentScope.lookupScopeOf(node.argument.name).getPath();
				const localConstants = this.localConstants.get(scopePath);
				if (curStat) {
					if (localConstants.has(node.argument.name)) {
						localConstants.delete(node.argument.name);
					}
					this.addWriteForCurrentScope(node.argument.name, null, curStat);
				}
				if (node.symbol && node.symbol.isArray()) {
					this.arrayState.downgrade(node.symbol.getAssemblySymbol());
				}
			}
		} else {
			// Using '*' toward a simple array will be considered fetching its first element, so this won't
			// result in downgrading.
			this.analyzeNode(node.argument, info);
		}
	}
	
	
	// (End)
	// ! Manually added scope entrance !
	// The condition for inlining has been adjusted.
    analyzeFunction(funcNode, info = null) {
        const funcName = funcNode.name;
		if (!(info && !info.actualCalling)) {
			this.visitedFunction.add(funcName);
		}
        //this.currentFunction = funcName;
		this.enterFunction(funcName);
        // 记录函数信息
		/**
		 * @type {functionInfoEntry}
		 */
		let myFunc;
        if (!this.functionInfo.has(funcName)) {
			myFunc = {
                calls: 0,
                hasSideEffects: false,
				isAddressed: false,
                isDefinition: funcNode.body !== null,
                isInline: funcNode.isInline || false,
				canInline: true,
                node: funcNode,
                scope: funcNode.scope,
				writtenVarsUnknown: true,
				writtenVars: []
            };
            this.functionInfo.set(funcName, myFunc);
        } else {
			myFunc = this.functionInfo.get(funcName);
			myFunc.isDefinition = true;
			myFunc.node = funcNode;
			myFunc.scope = funcNode.scope;
			myFunc.writtenVarsUnknown = true;
			myFunc.writtenVars = [];
		}
        
		let enteredScope = false;
		if (funcNode.scope && funcNode.scope.name !== this.currentScope.name) {
			enteredScope = true;
			this.enterScope(funcNode.scope);
		}
		
		const funcInfo = {
			inFunction: true,
			returnCount: new ReturnRegistry(),
			atFunction: funcNode.body.scope ?? funcNode.scope,
			parentInfo: info
		};
		
        // 分析函数体
		const analyzedBody = Boolean(funcNode.body && (!info.fromCallNodeAnalysis 
			|| !this.findRecursiveFunctions(this.functionCallGraph).has(funcNode.name)));
        if (analyzedBody) {
            this.analyzeNode(funcNode.body, funcInfo);
        }
		if (enteredScope) {
			this.exitScope();
		}
		
		// Consider whether this can be inlined
		
		if (analyzedBody) {
			if (!this.functionInfo.get(funcName).isAddressed) {
				/*
				Counting through `info.returnCount` MISSES the returns that sit
				inside a nested block (`if`, `while`): those are analysed with a
				child `info`, and the child's registry never merges back. A
				function whose only *counted* return is the one at the end then
				looks inlinable -- and inlining it drops the earlier exits,
				which is exactly what cannot be reproduced without a goto.

				So count them by looking at the body itself.
				*/
				const returns = [];
				const seenReturns = new Set();
				const collectReturns = node => {
					if (!node || typeof node !== 'object' || seenReturns.has(node)) return;
					seenReturns.add(node);
					if (node.type === 'ReturnStatement') { returns.push(node); return; }
					for (const key of Object.keys(node)) {
						if (key === 'parent') continue;	// back edge
						const child = node[key];
						if (Array.isArray(child)) child.forEach(collectReturns);
						else if (child && typeof child === 'object' && child.type) collectReturns(child);
					}
				};
				collectReturns(funcNode.body);
				if (returns.length > 1) {
					this.functionInfo.get(funcName).canInline = false;
				} else if (returns.length === 1) {
					const last = funcNode.body.statements[funcNode.body.statements.length - 1];
					this.functionInfo.get(funcName).canInline = returns[0] === last;
				} else {
					this.functionInfo.get(funcName).canInline = true;
				}
			}
			myFunc.writtenVarsUnknown = false;
		}
		
		this.exitFunction();
    }

	/**
	 * 
	 * @param {SymbolEntry} symbol 
	 */
	loadArrayForSymbol(symbol) {
		let arrPrototype = this.arrayState.defaultState;
		
		const extracted = symbol.extractType();
		// This might happen as those arrays are "no longer pointerImpl() but still have pointerTo" -- so we have to be ready.
		if (!extracted.pointerTo) {
			return;
		}
		if (extracted.isVolatile()) {
			return;
		}
		if ((!['basic', 'device', 'null'].includes(extracted.pointerTo.kind)) || extracted.pointerTo.isPointerImpl()) {
			this.arrayState.load(symbol.getAssemblySymbol(), {
				type: 'traditional',
				mutable: !symbol.isConst,
				implement: null
			});
			return;
		}

		if (this.optimizeConfig["doArrayOptimize"]) {
			if (symbol.isConst) {
				arrPrototype = this.optimizeConfig["constArrayOptimize"] ?? this.optimizeConfig["arrayOptimize"] ?? {
					type: 'encoded',
					mutable: false,
					implement: null
				};
			} else {
				arrPrototype = this.optimizeConfig["arrayOptimize"] ?? {
					type: 'embedded',
					mutable: true,
					implement: null
				};
			}
		} 
		this.arrayState.load(symbol.getAssemblySymbol(), arrPrototype);
	}

	/**
	 * 
	 * @param {VariableDeclarationNode} node 
	 * @param {*} info 
	 */
    analyzeVariableDeclaration(node, info = null) {
        // Must be in current scope, so it's correct
		if (this.shouldRunEvaluation(node, info) && !(info && info.optimizingLoop)) {
			// Copy to prevent deletion
			[...node.declarators].forEach(declarator => {
				this.replaceConstantAtPresent(declarator);
			});
			// This will delete useless declarators
		}
        node.declarators.forEach(declarator => {
            if (!declarator.name) return;
			/**
			 * @type {SymbolEntry}
			 */
			const symbol = this.currentScope.lookup(declarator.name);
			const scopePath = symbol ? symbol.scope.getPath() : this.getCurrentScopePath();
			const localConstants = this.localConstants.get(scopePath);
			const variableUses = this.variableUses.get(scopePath);
			
			const canOptimizeThis = !(symbol.isVolatile || symbol.isStatic || symbol.isExtern
				|| symbol.isRemoteTarget);
            symbol.variableReferrer.rejecting = !canOptimizeThis;

            // 初始化使用统计
            variableUses.set(declarator.name, {
                reads: 0,
                writes: 0, // 初始化算作一次写入
				canOptimize: canOptimizeThis,	// False: already symbolized as no-optimize
                location: declarator.location,
                node: declarator
            });
            
            // 检查常量初始化
            if (declarator.initializer) {
                this.analyzeNode(declarator.initializer, info);
				/*
				An array initialised with constants: remember every element, so
				that a constant subscript can be folded later.
				*/
				this.discardArrayGivenToPointer(declarator, declarator.initializer);
				if (canOptimizeThis && this.isFoldableArray(symbol)
					&& declarator.initializer.type === 'InitializerList') {
					const elements = (declarator.initializer.children ?? []).map(
						item => this.evaluateConstantExpression(item));
					this.recordConstantArray(symbol, elements);
				}
                if (canOptimizeThis && this.shouldRunEvaluation(node, info, symbol)) {
					const constValue = this.evaluateConstantExpression(declarator.initializer);
					if (constValue !== undefined) {
						localConstants.set(declarator.name, constValue);
						const shouldEval = this.shouldRunEvaluation(node, info);	// For compatibility
						this.addWrittenSymbol(symbol, constValue, shouldEval);
						// 标记符号为常量
						// symbol.markAsConstant(constValue);

						// Same reasoning: a simulation knowing the value is no
						// reason to delete the declaration.
						if (shouldEval && !(info && info.optimizingLoop)) {
							declarator.setAttribute('candidateRemoval', 1);
						}
						
					} else if (declarator.initializer.type === 'Identifier' && !declarator.initializer.name.startsWith('@')) {
						// Can't be builtin constant, or already optimized in the past component
						const relevantSymbol = this.currentScope.lookup(declarator.initializer.name);
						if (symbol.variableReferrer && relevantSymbol) {
							symbol.variableReferrer.linkTo(relevantSymbol.variableReferrer);
							symbol.variableReferrer.registerCandidate(declarator);
						}
					}
				}
                // Regardless of whether the assignment really occurs:
				/**
				 * @type {string?}
				 */
				let arrayRooted = null, arrayMutable = true;
				if (declarator.initializer.type === 'Identifier' || declarator.initializer.type === 'InitializerList') {
					if (declarator.initializer.symbol && declarator.initializer.symbol.isArray()) {
						arrayRooted = declarator.initializer.symbol.getAssemblySymbol();
					}
				}
				/**
				 * @type {TypeInfo?}
				 */
				const initType = declarator.initializer.dataType;
				if (symbol.isArray()) {
					/**
					 * @type {arrayStyle}
					 */
					let arrPrototype = {
						type: 'traditional',
						mutable: !symbol.isConst,
						implement: null
					};
					if (arrayRooted) {
						this.arrayState.assign(symbol.getAssemblySymbol(), arrayRooted);
					} else if ((initType && initType.annotations.getAttribute('hasArrayState'))) {
						arrPrototype = initType.annotations.getAttribute('arrayState');
						arrPrototype.mutable = arrPrototype.mutable || (!symbol.isConst);
					}
					
					if (!arrayRooted)
						this.arrayState.load(symbol.getAssemblySymbol(), arrPrototype);
					
				} else if (arrayRooted) {
					this.arrayState.downgrade(arrayRooted);
				}
            } else if (symbol.isArray()) {
				this.loadArrayForSymbol(symbol);
			}
        });
    }

	// Something might be to do here:
	/**
	 * 
	 * @param {ASTNode} node 
	 * @param {Object} info
	 * @remark Notice: this also handles function pointers. 
	 */
    analyzeIdentifier(node, info = null) {
		if (node.name.length && node.name[0] === '@') {
			return;
		}
        /*
        A name the scope chain cannot resolve is not ours to complain about:
        semantic analysis reports undeclared identifiers, and by this point a
        block may already have been rewritten (an empty inlined function, a
        folded-away `if`), which can leave a reference whose scope is gone.
        Crashing here only produced an internal error with no message at all.
        */
        const owningScope = this.currentScope.lookupScopeOf(node.name);
        if (!owningScope) {
            return;
        }
        const scopePath = owningScope.getPath();
        const variableUses = this.variableUses.get(scopePath);
        
        // 记录变量读取
        const usage = variableUses.get(node.name);
        if (usage) {
            usage.reads++;
        }
        
        // 检查是否为函数调用
		// A function pointer
        const symbol = this.currentScope.lookup(node.name);
        if (symbol && symbol.kind === 'function') {
            // 函数调用统计在analyzeFunctionCall中处理
			// Debug:
			//console.log('identifier being called on function: node:');
			//console.log(node);
			symbol.isAddressed = true;	// Function doesn't need 'access-through-pointer'
			const funcInfo = this.functionInfo.get(node.name);
			this.recordFunctionCall(this.currentFunction.name ?? 'anonymous', node.name);
			if (funcInfo) {
				funcInfo.isAddressed = true;
				funcInfo.canInline = false;
			}
        }
    }

	/**
	 * `p = a` (or `int *p = a`): the array's address escapes into a pointer, so
	 * it can be written through `p[i]` with any subscript. Forget its contents.
	 *
	 * @param {ASTNode} left
	 * @param {ASTNode} right
	 * @returns {void}
	 */
	discardArrayGivenToPointer(left, right) {
		// Only when the destination is (or holds) a pointer
		const leftSymbol = this.baseSymbolOf(left);
		const leftType = leftSymbol?.myType?.() ?? left?.dataType ?? null;
		const leftIsPointer = !!leftType && (leftType.kind === 'pointer'
			|| (left.dataType && left.dataType.kind === 'pointer'));
		if (!leftIsPointer) return;
		const source = this.baseSymbolOf(right);
		if (this.isFoldableArray(source)) this.invalidateConstantArray(source);
	}

	/**
	 * Apply the effect of an assignment on a constant array: update one element
	 * when both subscript and value are known, forget everything otherwise.
	 *
	 * @param {ASTNode} node An AssignmentExpression
	 * @param {any} info
	 * @returns {void}
	 */
	applyArrayWrite(node, info) {
		const left = node.left;
		if (!left || left.type !== 'MemberExpression') return;
		const owner = left.children?.[0] ?? left.object ?? null;
		const subscript = left.children?.[1] ?? left.property ?? null;
		if (!owner || owner.type !== 'Identifier' || !subscript) return;
		const symbol = this.currentScope.lookup(owner.name);
		if (!symbol || !this.isFoldableArray(symbol)) return;
		if (!this.getConstantArray(symbol)) return;

		// `+=` and friends: evaluate the whole expression; `=`: just the value
		const plain = !node.operator || node.operator === '=';
		const value = this.evaluateConstantExpression(plain ? node.right : node);
		const index = this.evaluateConstantExpression(subscript);
		const definite = this.isDefinite(this.evaluationState(node, info, symbol));
		if (definite && value !== undefined && typeof index === 'number' && Number.isInteger(index)) {
			const elements = this.getConstantArray(symbol);
			if (elements && index >= 0 && index < elements.length) {
				this.writeConstantArrayElement(symbol, index, value);
				/*
				The element is known now, but a real `write` instruction is still
				emitted for it. Remember that, so the array is not later turned
				into an immutable constant -- that would leave the write with
				nowhere to go.
				*/
				symbol.arrayHasRuntimeWrite = true;
				return;
			}
		}
		// Unknown subscript, unknown value, or an unsure write
		this.invalidateConstantArray(symbol);
	}

	// !! This has manual changes !!
    analyzeAssignmentExpression(node, info = null) {
		/*
		An array handed to a pointer escapes our tracking: from now on the
		array can be written through `p[anything]`, so its contents are unknown.
		*/
		this.discardArrayGivenToPointer(node.left, node.right);

		/*
		Writing into an array. When BOTH the subscript and the value are known
		at compile time the element is simply updated -- that is what makes
		`a[2] = 7; print(a[2]);` fold. Anything else (an unknown subscript, an
		unknown value, or a write we are not sure even happens) throws the known
		contents away.
		*/
		this.applyArrayWrite(node, info);

        // 分析右值
        this.analyzeNode(node.right, info);
		
		// 分析左值
		// If there's no side effect, still consider optimizing.
		// Logic manually adjusted !
        if (node.left.type === 'Identifier') {
			if (this.hasSideEffects(node.right)) {
				this.addWriteForCurrentScope(node.left.name);
			} else {
				const symbol = this.currentScope.lookup(node.left.name);
				const scopePath = this.currentScope.lookupScopeOf(node.left.name).getPath();
				const localConstants = this.localConstants.get(scopePath);
				const variableUses = this.variableUses.get(scopePath);
				const usage = variableUses.get(node.left.name);
				/*
				Fold the assignment when the variable may be tracked -- or when
				we are SIMULATING a loop. There the constants were planted by
				`simulateLoop` on purpose, and these writes are exactly what
				computes the final values the loop is then replaced by.

				`canOptimize` is false for anything written inside a loop body
				(`evaluationState` returns TRACKED there, so that `i = i + 1` is
				not folded into `i = 1`). That is right for ordinary analysis but
				must not stop the simulation, which runs in a `optimizingLoop`
				context where every value really is known.
				*/
				if (usage && (usage.canOptimize || (info && info.optimizingLoop))) {
					// ! Manually modified (now constant expression also consider '+=') !
					let constValue = undefined;
					if (node.operator && node.operator !== '=') {
						constValue = this.evaluateConstantExpression(node);	// left might be unavailable!
					} else {
						constValue = this.evaluateConstantExpression(node.right);
					}
					if (constValue !== undefined) {
						//localConstants.set(node.left.name, constValue);
						const symbol = this.currentScope.lookup(node.left.name);
						if (symbol && this.shouldRunEvaluation(node, info, symbol)) {	// Not considering symbol currently!
							// symbol.markAsConstant(constValue);
							this.addWrittenSymbol(symbol, constValue, true);
							/*
							The write folded, so the assignment looks redundant
							-- but NOT while we are SIMULATING a loop. There the
							constants describe one imagined iteration, while the
							statement has to survive for the real loop (which is
							only replaced when it can be fully optimized).
							Dropping `i = i + 1` here is what left a loop that
							never advanced.
							*/
							if (this.shouldRunEvaluation(node, info)
								&& !(info && info.optimizingLoop))
								node.setAttribute('candidateRemoval', 1);
						}
						
					} else if (node.operator === '=' &&
						this.shouldRunEvaluation(node, info, symbol) && node.right.type === 'Identifier' && !node.right.name.startsWith('@')) {
						const relevantSymbol = this.currentScope.lookup(node.right.name);
						if (relevantSymbol) {
							symbol.variableReferrer.linkTo(relevantSymbol.variableReferrer);
							symbol.variableReferrer.registerCandidate(node);
						}
					}
					// Manually added
					this.addWriteForCurrentScope(node.left.name, 
						(info.isUnsure || !info.actualCalling) ? null : constValue, 
						this.shouldRunEvaluation(node, info));
				}
				/*
				When the write could NOT be folded at all (inside a loop, or in a
				branch we cannot decide) nothing above recorded it -- so an alias
				established earlier, `int t = b;`, would survive a later write to
				`b` and make `a = t` compile into `a = b`. Record the write
				anyway; its VALUE stays unknown, only the access is known.
				*/
				if (!(usage && usage.canOptimize)) {
					const written = this.currentScope.lookup(node.left.name);
					if (written) {
						this.addWrittenSymbol(written, null,
							this.evaluationState(node, info, written));
					}
				}
				
				if (symbol.isArray()) {
					if (node.right.symbol && node.right.symbol.isArray()) {
						this.arrayState.assign(symbol.getAssemblySymbol(), node.right.symbol.getAssemblySymbol());
					} else {
						this.arrayState.downgrade(symbol.getAssemblySymbol());
					}
				}
			}
		} else {
			this.analyzeNode(node.left, info);
			// If the right side is array
			if (node.right.symbol && node.right.symbol.isArray()) {
				this.arrayState.downgrade(node.right.symbol.getAssemblySymbol());
			}
		}
		if (node.right.type === 'Identifier' && node.right.symbol && node.right.symbol.kind === 'function') {
			this.recordFunctionCall(this.currentFunction, node.right.name);
		}
    }

	/**
	 * Functions that must be kept even when no call to them is visible: the
	 * stack helpers reach them through `#pragma stackcheck(fn)` /
	 * `#pragma stackunderflow(fn)`, which is not a call the optimizer can see.
	 *
	 * @returns {Set<string>}
	 */
	protectedHandlerNames() {
		const config = this.optimizeConfig ?? {};
		const names = new Set();
		['stackCheck', 'stackUnderflow'].forEach(key => {
			const value = config[key];
			if (typeof value === 'string' && value.length > 0) names.add(value);
		});
		return names;
	}

	recordFunctionCall(callerName, funcName) {
		if (!this.functionCallGraph.has(callerName)) {
			this.functionCallGraph.set(callerName, new Set());
		}
		this.functionCallGraph.get(callerName).add(funcName);
	}

    analyzeFunctionCall(node, info = null) {
		const shouldRun = this.shouldRunEvaluation(node, info);
		const valid = shouldRun || (info && (info.firstLoopRun || info.isUnsure || info.inFunction));

		/**
		 * @type {string?[]}
		*/
		let arrCallMapper = [];
		let arrCallFail = false;

		/*
		An array handed to a function may be written there. So may a GLOBAL one:
		the callee can name it directly without it appearing in the argument
		list. Since we do not analyse what a call does to globals, every global
		array becomes unknown at every call site; locals are only affected when
		they are actually passed.
		*/
		(node.arguments ?? []).forEach(argument => {
			const argumentSymbol = this.baseSymbolOf(argument);
			if (this.isFoldableArray(argumentSymbol)) this.invalidateConstantArray(argumentSymbol);
		});
		if (this.constantArrays && this.constantArrays.size > 0) {
			const visit = scope => {
				if (!scope) return;
				(scope.getAllSymbols?.() ?? []).forEach(symbol => {
					if (symbol.isGlobal && this.isFoldableArray(symbol)) this.invalidateConstantArray(symbol);
				});
				scope.children?.forEach(visit);
			};
			visit(this.globalScope);
		}

        if (node.callee.type === 'Identifier') {
            const funcName = node.callee.name;
            
            // 更新函数调用计数
			/**
			 * @type {functionInfoEntry?}
			 */
			let funinfo;
			if (this.functionInfo.has(funcName)) {
				funinfo = this.functionInfo.get(funcName);
				if (funinfo) {
					if (valid) {
						funinfo.calls++;	// The number of call sites
					}
					//if (shouldRun) {
					
					//}
				}
			} else {
				// When function call is being analyzed, it is possible that the function body hasn't undergone
				// optimizer's analysis!
				let foundNode = null;
				this.ast.functions.forEach(func => {
					if (func.name === funcName) {
						foundNode = func;
					}
				});
				/*
				`foundNode` is null when the name is not one of this program's
				functions -- a builtin that the parser left as a plain call, or
				a function that has already been taken out of the AST.
				Registering `scope: foundNode.scope` then threw much later, in a
				completely unrelated place. Keep the entry (so the call is
				counted) but make it plainly unusable.
				*/
				this.functionInfo.set(funcName, {
					calls: valid ? 1 : 0,
					hasSideEffects: false,
					isAddressed: false,
					isDefinition: false,
					canInline: false,
					writtenVarsUnknown: true,
					writtenVars: [],
					node: foundNode,
					scope: foundNode ? foundNode.scope : null
				});	// Other information given as undefined
				funinfo = this.functionInfo.get(funcName);
			}
			// 记录调用关系
			if (this.currentScope && this.currentScope.astNode) {
				const callerName = this.currentFunction || 'anonymous';
				this.recordFunctionCall(callerName, funcName);
			}

			// [TODO: Consider recursive functions ...?]
			if (funinfo.node) {
				this.analyzeNode(funinfo.node, {
					actualCalling: Boolean(info.actualCalling && (shouldRun || info.optimizingLoop)),
					fromCallNodeAnalysis: true,
					parentInfo: info
				});
			}

			const funcSymbol = this.currentScope.lookup(funcName);
			const parameters = funcSymbol?.type?.parameters ?? null;
			(parameters ?? []).forEach(param => {
				const resolvedType = this.semantic.getTypeInfo(param.type);
				const resolvedSymbol = funinfo.scope?.lookupCurrent(param.name);
				if (resolvedType.kind === 'array') {
					arrCallMapper.push(resolvedSymbol.getAssemblySymbol());
				} else {
					arrCallMapper.push(null);
				}
			});
			
        } else {
			arrCallFail = true;
		}

		for (let i = 0; i < node.arguments.length; i++) {
			this.analyzeNode(node.arguments[i], info);
			if (node.arguments[i].symbol) {
				const assemblyName = node.arguments[i].symbol.getAssemblySymbol();
				if (arrCallFail) {
					this.arrayState.downgrade(assemblyName);
				} else if (arrCallMapper.length > i && arrCallMapper[i]) {
					this.arrayState.assign(assemblyName, arrCallMapper[i]);
				}
			}
		}
    }
	// ! Manually updated !
	analyzeIfStatement(node, info = null) {
		// 分析条件表达式
		let conditionValue;
		if (node.test) {
			this.analyzeNode(node.test, info);
			
			// 检查条件是否为常量
			conditionValue = this.evaluateConstantExpression(node.test);
			
			// 记录条件信息用于优化
			if (this.shouldRunEvaluation(node, info)) {
				if (conditionValue !== undefined) {
					node.setAttribute('constantCondition', conditionValue);
					if (!(info && info.optimizingLoop)) {
						if (conditionValue) {
							// 条件总是为真，可以标记then分支总是执行
							this.addWarning(`Condition always true in if statement`, node.test.location);
						} else {
							// 条件总是为假，可以标记else分支总是执行（如果有）
							this.addWarning(`Condition always false in if statement`, node.test.location);
						}
					}			
					
				}
			}
			
			
			// 检查条件是否有副作用
			if (this.hasSideEffects(node.test) || this.controlExpressionHasCall(node.test)) {
				node.setAttribute('conditionHasSideEffects', true);
			}
		}
		if (conditionValue != undefined && conditionValue != null && this.shouldRunEvaluation(node, info)) {
			if (!conditionValue) {
				if (node.alternate) this.analyzeNode(node.alternate, info);
			} else {
				if (node.consequent) this.analyzeNode(node.consequent, info);
			}
		} else {
			// 分析then分支
			const hasNoCondit = conditionValue == null || conditionValue == undefined;
			const ifInfo = {
				isUnsure: (hasNoCondit) && !(info && info.firstLoopRun),
				hasNoCondit: hasNoCondit,
				parentInfo: info
			};
			if (node.consequent) {
				this.analyzeNode(node.consequent, {
					...ifInfo,
					atUnsure: node.consequent.scope ?? node.scope
				});
				
				// 检查then分支是否有副作用
				if (this.hasSideEffects(node.consequent)) {
					node.setAttribute('consequentHasSideEffects', true);
				}
			}
			
			// 分析else分支
			if (node.alternate) {
				this.analyzeNode(node.alternate, {
					...ifInfo,
					atUnsure: node.alternate.scope ?? node.scope
				});
				
				// 检查else分支是否有副作用
				if (this.hasSideEffects(node.alternate)) {
					node.setAttribute('alternateHasSideEffects', true);
				}
			}
		}
	}
	
	analyzeWhileStatement(node, info = null) {
		const loopInfo = {
			isLoop: true,
			firstLoopRun: true,
			parentLoop: node,
			hasBreak: false,
			hasContinue: false,
			parentInfo: info
		};
		const testInfo = {
			isLoop: true,
			firstLoopRun: true,
			parentLoop: node,
			cannotCascade: true,
			parentInfo: info
		};
		node.setAttribute('hasSideEffects', false);
		// 分析条件表达式
		if (node.test) {
			this.analyzeNode(node.test, testInfo);
			
			// 检查条件是否为常量
			const conditionValue = this.evaluateConstantExpression(node.test);
			
			// 记录条件信息用于优化
			if (conditionValue !== undefined) {
				//node.setAttribute('constantCondition', conditionValue);
				
				if (conditionValue == 0) {
					// 条件总是为假，循环永远不会执行
					node.setAttribute('neverExecuted', true);
					this.addWarning(`Loop never executed (while(false))`, node.test.location);
				}
			}
			
			// 检查条件是否有副作用
			if (this.hasSideEffects(node.test)) {
				node.setAttribute('conditionHasSideEffects', true);
			}
		}

		// 尝试分析while循环是否可以优化
		if (!(info && info.optimizingLoop)) {
			// 分析循环体
			if (node.body) {
				// 进入循环体作用域
				node.setAttribute('bodyHasSideEffects', node.getAttribute('confirmedSideEffects') || this.hasSideEffects(node.body));
				
				// 如果循环永远不会执行，不需要分析循环体 (But what if the side effect is necessary?)
				if (node.getAttribute('neverExecuted')) {
					return;
				}

				const modifieds = (info ? info.modifiedVars : null) ?? this.collectModifiedVariablesInLoop(node);
				let weird = false;
				if (modifieds.has("unrecognized")) {
					weird = true;
					modifieds.delete("unrecognized");
				}
				// 分析循环体
				this.analyzeNode(node.body, {
					...loopInfo,
					atLoop: weird ? null : (node.body.scope ?? node.scope),
					strangeLoop: node.getAttribute('hasUnknownSideEffects') || weird,
					modifiedVars: modifieds
				});
				// 检查循环体是否有副作用
				
				
				// 记录是否有break/continue
				if (loopInfo.hasBreak) {
					node.setAttribute('hasBreak', true);
					node.setAttribute('infiniteLoop', false);
				}
				if (loopInfo.hasContinue) {
					node.setAttribute('hasContinue', true);
				}
			}

			let optimized = false;
			if ((!node.getAttribute('bodyHasSideEffects')) && (!node.getAttribute('conditionHasSideEffects'))
				&& (!node.getAttribute('infiniteLoop'))) {
				if (!info || !info.isLoop) {
					/*
					An "endless" loop is worth a try too: `while (true) { ...;
					if (done) break; }` does end, it just does not say where. The
					simulation finds the `break`. Nothing observable happens here
					(the body was checked to be free of side effects), so a loop
					that never reaches its `break` may simply be dropped.
					*/
					const tried = this.analyzeWhileLoopForOptimization(node, info)
						|| this.analyzeEndlessLoopForOptimization(node, info);
					if (tried) {
						if ((!info || !info.optimizingLoop) && this.optimizeDeterministicWhileLoop(node)) {
							this.modified = true;
							optimized = true;
						}
					}
				}
			}
			if (!optimized && !(info && info.isLoop)) {	// If not fully optimized, then there's something that we don't know
				this.markLoopAsPotentiallyWithSideEffects(node);
			}
			
			if (node.getAttribute('infiniteLoop')) {
				this.addWarning(`Infinite loop detected (while(true))`, node.test.location);
			}
		} else {
			if (!node.getAttribute('iteration')) {
				this.analyzeWhileLoopForOptimization(node, {
					...info,
					noRunning: true
				});
			}
			const prefetch = node.getAttribute('iteration');
			if (prefetch) {
				this.simulateLoop(node, prefetch.varName, prefetch.varInit, prefetch.condition, prefetch.update, Infinity, info);
			} else {
				this.markLoopAsPotentiallyWithSideEffects(node);
			}
		}
	}
	

	/**
	 * 分析while循环是否可以优化
	 * @param {ASTNode} whileNode - while语句节点
	 */
	analyzeWhileLoopForOptimization(whileNode, info) {
		// while循环的优化比for循环更复杂
		// 这里我们只处理简单情况：条件中的变量在循环体中被简单修改
		
		// 检查条件是否为简单比较
		if (whileNode.test.type === 'BinaryExpression') {
			// 尝试识别循环变量
			let loopVarName = null;
			let comparisonOp = null;
			let constValue = null;
			
			/*
			Both sides being identifiers used to be given up on at once. It
			should not be: the other side may still be a variable whose value is
			known here (`while (i < sum)` with `sum` folded to 5). Only the VALUE
			matters, and `evaluateConstantExpression` below answers that.
			*/
			if (whileNode.test.left.type === 'Identifier') {
				loopVarName = whileNode.test.left.name;
				comparisonOp = whileNode.test.operator;
				constValue = this.evaluateConstantExpression(whileNode.test.right);
			} else if (whileNode.test.right.type === 'Identifier') {
				loopVarName = whileNode.test.right.name;
				comparisonOp = this.reverseComparisonOperator(whileNode.test.operator);
				constValue = this.evaluateConstantExpression(whileNode.test.left);
			}
			
			if (loopVarName) {
				// 检查循环体中是否对循环变量有简单修改
				const updateInfo = this.findLoopVariableUpdateInBody(whileNode.body, loopVarName);
				
				if (updateInfo) {
					// 获取循环变量的初始值
					const scopePath = this.currentScope.lookupScopeOf(loopVarName).getPath();
					const localConstants = this.localConstants.get(scopePath);
					const initValue = localConstants.get(loopVarName);
					
					if (initValue !== undefined) {
						// Successful to make registrations
						whileNode.setAttribute('loopVarName', loopVarName);
						const conditionInfo = { comparisonOp, constValue };
						/*
						The bound need not be a compile-time constant. When it
						is not, the loop is still simulated -- the simulation
						stops by itself when the condition becomes false.
						*/
						const iterationCount = (constValue === undefined) ? null
							: this.calculateLoopIterations(initValue, conditionInfo, updateInfo);
						const MAX_OPTIMIZE_ITERATIONS = 250; // 最大优化循环次数

						if (iterationCount !== null && iterationCount >= 0) {
							whileNode.setAttribute('iterationCount', iterationCount);
							if (iterationCount === 0) {
								whileNode.setAttribute('neverExecuted', true);
								return true;
							} else if (iterationCount <= MAX_OPTIMIZE_ITERATIONS) {
								return this.runWhileSimulation(whileNode, loopVarName, initValue,
									conditionInfo, updateInfo, iterationCount, info);
							}
						} else if (constValue === undefined) {
							/*
							Unknown bound (`while (i < f())`, or a bound that is
							only known while simulating). Give the simulation a
							budget and let the condition decide when it ends.
							*/
							const nowFalse = this.evaluateConstantExpression(whileNode.test);
							if (nowFalse === 0) {
								whileNode.setAttribute('neverExecuted', true);
								return true;
							}
							return this.runWhileSimulation(whileNode, loopVarName, initValue,
								conditionInfo, updateInfo, MAX_OPTIMIZE_ITERATIONS, info);
						}
					}
				}
			}
		}
		return false;
	}

	/**
	 * 在循环体中查找循环变量的更新
	 * @param {ASTNode} body - 循环体
	 * @param {string} loopVarName - 循环变量名
	 * @returns {Object|null} - 更新信息
	 */
	findLoopVariableUpdateInBody(body, loopVarName) {
		if (!body) return null;
		
		let updateInfo = null;
		
		const searchForUpdate = (node) => {
			if (!node || updateInfo) return;
			
			if (node.type === 'AssignmentExpression' &&
				node.left.type === 'Identifier' &&
				node.left.name === loopVarName) {
				
				if (node.operator === '+=' || node.operator === '-=') {
					const stepValue = this.evaluateConstantExpression(node.right);
					if (stepValue !== undefined) {
						updateInfo = {
							type: 'assignment',
							operator: node.operator,
							step: node.operator === '+=' ? stepValue : -stepValue
						};
					}
				} else if (node.operator === '=' && node.right
					&& node.right.type === 'BinaryExpression'
					&& node.right.left && node.right.left.type === 'Identifier'
					&& node.right.left.name === loopVarName
					&& (node.right.operator === '+' || node.right.operator === '-')) {
					/*
					`i = i + 1` (and `i = i - 1`): the same thing as `i += 1`,
					just written out. `analyzeLoopUpdate` (used by `for`) already
					knows this shape; a `while` body needs it too, or
					`while (i < n) { ... i = i + 1; }` is never optimized.
					*/
					const stepValue = this.evaluateConstantExpression(node.right.right);
					if (stepValue !== undefined) {
						updateInfo = {
							type: 'binary',
							operator: node.right.operator,
							step: node.right.operator === '+' ? stepValue : -stepValue
						};
					}
				}
			} else if (node.type === 'UnaryExpression' &&
					   (node.operator === '++' || node.operator === '--') &&
					   node.argument.type === 'Identifier' &&
					   node.argument.name === loopVarName) {
				
				updateInfo = {
					type: 'unary',
					operator: node.operator,
					step: node.operator === '++' ? 1 : -1
				};
			}
			
			// 递归搜索
			/*
			if (node.children && !updateInfo) {
				for (const child of node.children) {
					searchForUpdate(child);
					if (updateInfo) break;
				}
			}
			
			// 搜索特定字段
			if (!updateInfo) {
				const fields = [
					'functions', 'globalDeclarations', 'typeDefinitions',
					'statements', 'expression', 'test', 'consequent', 'alternate',
					'body', 'init', 'update', 'argument', 'left', 'right',
					'declarators', 'arguments', 'callee', 'initializer'
				];
				
				for (const field of fields) {
					if (node[field] && !updateInfo) {
						if (Array.isArray(node[field])) {
							for (const item of node[field]) {
								searchForUpdate(item);
								if (updateInfo) break;
							}
						} else if (typeof node[field] === 'object') {
							searchForUpdate(node[field]);
						}
					}
					if (updateInfo) break;
				}
			}
				*/
			this.further(node, searchForUpdate, () => updateInfo);
		};
		
		searchForUpdate(body);
		return updateInfo;
	}
	
	// ! Manually modified !
	analyzeForStatement(node, info = null) {
		// 进入循环体作用域
		node.setAttribute('hasSideEffects', false);
		const loopInfo = {
			isLoop: true,
			firstLoopRun: true,
			parentLoop: node,
			hasBreak: false,
			hasContinue: false,
			parentInfo: info
		};
		const testInfo = {
			isLoop: true,
			firstLoopRun: true,
			cannotCascade: true,
			parentLoop: node,
			parentInfo: info
		};
		// 分析初始化部分
		if (node.init) {
			let initInfo = {
				...(info ?? {}),
				cannotCascade: true,
				isLoopInit: true
			};
			initInfo.isLoop = false;				// Ensure initializer
			this.analyzeNode(node.init, initInfo);
			node.init.setAttribute('candidateRemoval', 0);	// Suppress potential removal
			
			// 检查初始化是否有副作用
			node.setAttribute('initHasSideEffects', this.hasSideEffects(node.init));
		}
		
		// 分析条件部分
		if (node.test) {
			// ! Always make changes for initialization !
			this.analyzeNode(node.test, testInfo);
			
			// 检查条件是否为常量
			const conditionValue = this.evaluateConstantExpression(node.test);
			
			// 记录条件信息用于优化
			if (conditionValue !== undefined) {
				//node.setAttribute('constantCondition', conditionValue);
				
				if (conditionValue) {
					// 条件总是为真
					// !! The relevant changes, if necessary, will be done in 'break' check !!
					if (!node.update) {
						// 没有更新表达式，可能是无限循环
						node.setAttribute('infiniteLoop', true);
					}
				} else {
					// 条件总是为假，循环永远不会执行
					node.setAttribute('neverExecuted', true);
					this.addWarning(`Loop never executed (for with constant false condition)`, node.test.location);
				}
			}
			
			// 检查条件是否有副作用
			/*
			A call in the condition counts as a side effect even when the call
			looks harmless: if it were inlined, the loop simulator would count
			iterations without ever running what it does.
			*/
			node.setAttribute('conditionHasSideEffects',
				Boolean(this.hasSideEffects(node.test) || this.controlExpressionHasCall(node.test)));
		} else {
			// 没有条件表达式，是无限循环 (for(;;))
			// ! Notice that there might be 'break' !
			node.setAttribute('infiniteLoop', true);
		}
		
		// 分析更新部分
		if (node.update) {
			this.analyzeNode(node.update, testInfo);
			
			// 检查更新是否有副作用
			node.setAttribute('updateHasSideEffects',
				Boolean(this.hasSideEffects(node.update) || this.controlExpressionHasCall(node.update)));
		}
		
		// 分析循环体
		if (!(info && info.optimizingLoop)) {
			if (node.body) {
				// 检查循环体是否有副作用
				node.setAttribute('bodyHasSideEffects', node.getAttribute('confirmedSideEffects') || this.hasSideEffects(node.body));
				
				// 分析循环体
				const modifieds = (info ? info.modifiedVars : null) ?? this.collectModifiedVariablesInLoop(node);
				let weird = false;
				if (modifieds.has("unrecognized")) {
					weird = true;
					modifieds.delete("unrecognized");
				}
				// 分析循环体
				this.analyzeNode(node.body, {
					...loopInfo,
					atLoop: weird ? null : (node.body.scope ?? node.scope),
					strangeLoop: node.getAttribute('hasUnknownSideEffects') || weird,
					modifiedVars: modifieds
				});
				
				
				// 记录是否有break/continue
				if (loopInfo.hasBreak) {
					node.setAttribute('hasBreak', true);
					node.setAttribute('infiniteLoop', false);
				}
				if (loopInfo.hasContinue) {
					node.setAttribute('hasContinue', true);
				}
			}

			let optimized = false;
			// (Manually added) Do optimize afterwards
			if ((!node.getAttribute('initHasSideEffects')) && (!node.getAttribute('bodyHasSideEffects'))
				&& (!node.getAttribute('updateHasSideEffects'))
				&& (!node.getAttribute('conditionHasSideEffects'))
				&& (!node.getAttribute('infiniteLoop'))) {
				if (!info || !info.isLoop) {
					if (this.analyzeAndOptimizeForLoop(node, info)) {
						if ((!info || !info.optimizingLoop) && this.optimizeDeterministicForLoop(node)) {
							this.modified = true;
							optimized = true;
						}
					}
				}
			}
			if (!optimized && !(info && info.isLoop)) {
				this.markLoopAsPotentiallyWithSideEffects(node);
			}
			// Warning for infinite for loop
			if (node.getAttribute('infiniteLoop')) {
				this.addWarning(`Infinite loop detected (for with constant true condition)`, node.location);
			}
		} else {
			if (!node.getAttribute('iteration')) {
				this.analyzeAndOptimizeForLoop(node, {
					...info,
					noRunning: true
				});
			}
 			const prefetch = node.getAttribute('iteration');
			if (prefetch) {
				this.simulateLoop(node, prefetch.varName, prefetch.varInit, prefetch.condition, prefetch.update, Infinity, info);
			} else {
				this.markLoopAsPotentiallyWithSideEffects(node);
			}
		}
		
	}
	
	// Refer to for/while:
	
	/**
	 * 分析并尝试优化for循环
	 * @param {ASTNode} forNode - for语句节点
	 * @returns {boolean} - 返回是否进行了优化
	 * @bug Seems that this function is not called now...
	 */
	 // ! Manually modified !
	analyzeAndOptimizeForLoop(forNode, info = null) {
		if (!forNode || forNode.type !== 'ForStatement') return false;
		
		// 获取当前作用域路径
		//const scopePath = this.getCurrentScopePath();
		//const localConstants = this.localConstants.get(scopePath);
		
		// 分析初始化部分
		let initValue = null;
		let initVarName = null;
		
		if (forNode.init) {
			if (forNode.init.type === 'VariableDeclaration' && 
				forNode.init.declarators && 
				forNode.init.declarators.length > 0) {
				// 简单的变量声明初始化
				const declarator = forNode.init.declarators[0];
				initVarName = declarator.name;
				
				if (declarator.initializer) {
					initValue = this.evaluateConstantExpression(declarator.initializer);
				}
			} else if (forNode.init.type === 'AssignmentExpression' &&
					   forNode.init.left.type === 'Identifier') {
				// 赋值表达式初始化
				initVarName = forNode.init.left.name;
				initValue = this.evaluateConstantExpression(forNode.init.right);
			} else if (forNode.init.type === 'ExpressionStatement' && forNode.init.children.length >= 1 &&
					   forNode.init.children[0].type === 'AssignmentExpression' &&
					   forNode.init.children[0].left.type === 'Identifier') {
						   // ! Manually modified !
						   // This is seemingly the only place where children nodes are used.
						   // TODO: Only consider the first variable
				// 表达式语句中的赋值
				initVarName = forNode.init.children[0].left.name;
				initValue = this.evaluateConstantExpression(forNode.init.children[0].right);
			}
		}
		
		// 如果没有找到初始化变量，无法优化
		if (!initVarName) {
			// 标记循环可能有副作用
			this.markLoopAsPotentiallyWithSideEffects(forNode);
			return false;
		}
		
		// 分析条件表达式
		let conditionInfo = this.analyzeLoopCondition(forNode.test, initVarName);
		if (!conditionInfo) {
			this.markLoopAsPotentiallyWithSideEffects(forNode);
			return false;
		}
		
		// 分析更新表达式
		let updateInfo = this.analyzeLoopUpdate(forNode.update, initVarName);
		if (!updateInfo) {
			this.markLoopAsPotentiallyWithSideEffects(forNode);
			return false;
		}
		
		// 尝试计算循环次数
		const iterationCount = this.calculateLoopIterations(
			initValue, 
			conditionInfo, 
			updateInfo
		);
		
		if (iterationCount !== null && iterationCount >= 0) {
			// 循环次数可以确定
			forNode.setAttribute('iterationCount', iterationCount);
			forNode.setAttribute('iterationCountKnown', true);
			// 检查循环次数是否超过优化限制
			const MAX_OPTIMIZE_ITERATIONS = 250; // 最大优化循环次数
			if (iterationCount <= MAX_OPTIMIZE_ITERATIONS) {
				// 可以优化这个循环
				forNode.setAttribute('canOptimize', true);
				
				// 模拟循环，更新变量的常量值
				if (iterationCount > 0) {
					forNode.setAttribute('iteration', {
						node: forNode,
						type: 'for',
						varName: initVarName,
						varInit: initValue,
						condition: conditionInfo,
						update: updateInfo,
						count: iterationCount,
						info: info
					});
					if (info && info.noRunning) return true;
					if (!this.simulateLoop(forNode, initVarName, initValue, 
									 conditionInfo, updateInfo, iterationCount, info)) {
						// Failures in it...
						forNode.setAttribute('iterationOptimized', false);
						return true;
					}
				} else {
					// 循环次数为0，永远不会执行
					forNode.setAttribute('neverExecuted', true);
				}
				forNode.setAttribute('iterationOptimized', !forNode.getAttribute('maybeHasSideEffects'));
				return true;
			} else {
				// 循环次数太多，不进行优化，但标记为可确定循环次数
				forNode.setAttribute('iterationOptimized', false);
				return true;
			}
		}
		
		// 如果不能优化，标记可能有副作用
		this.markLoopAsPotentiallyWithSideEffects(forNode);
		return false;
	}

	/**
	 * 分析循环条件
	 * @param {ASTNode} condition - 条件表达式
	 * @param {string} loopVarName - 循环变量名
	 * @returns {Object|null} - 条件信息，或null如果无法分析
	 */
	analyzeLoopCondition(condition, loopVarName) {
		if (!condition) return null;
		
		// 简单条件：loopVar < constant 或 loopVar <= constant 等
		if (condition.type === 'BinaryExpression') {
			// 检查左边是否是循环变量，右边是否是常量
			let varSide = null;
			let constSide = null;
			let comparisonOp = condition.operator;
			
			if (condition.left.type === 'Identifier' && 
				condition.left.name === loopVarName) {
				varSide = 'left';
				constSide = 'right';
			} else if (condition.right.type === 'Identifier' && 
					   condition.right.name === loopVarName) {
				varSide = 'right';
				constSide = 'left';
				// 调整比较操作符
				comparisonOp = this.reverseComparisonOperator(comparisonOp);
			} else {
				return null;
			}
			
			// 获取常数值
			const constValue = this.evaluateConstantExpression(
				varSide === 'left' ? condition.right : condition.left
			);
			
			if (constValue !== undefined) {
				return {
					comparisonOp: comparisonOp,
					constValue: constValue,
					varSide: varSide
				};
			}
		}
		
		return null;
	}

	/**
	 * 分析循环更新表达式
	 * @param {ASTNode} update - 更新表达式
	 * @param {string} loopVarName - 循环变量名
	 * @returns {Object|null} - 更新信息，或null如果无法分析
	 * @remark Notice that multiple/division updates are not analyzed currently.
	 */
	analyzeLoopUpdate(update, loopVarName) {
		if (!update) return null;
		
		// 检查更新表达式类型
		if (update.type === 'UnaryExpression' && 
			(update.operator === '++' || update.operator === '--') &&
			update.argument.type === 'Identifier' &&
			update.argument.name === loopVarName) {
			// i++ 或 i--
			return {
				type: 'unary',
				operator: update.operator,
				step: update.operator === '++' ? 1 : -1
			};
		} else if (update.type === 'AssignmentExpression') {
			// i += constant, i -= constant 等
			if (update.left.type === 'Identifier' && 
				update.left.name === loopVarName) {
				
				const operator = update.operator;
				if (operator === '+=' || operator === '-=' || 
					operator === '*=' || operator === '/=' || operator === '%=') {
					
					const stepValue = this.evaluateConstantExpression(update.right);
					if (stepValue !== undefined) {
						let step = stepValue;
						if (operator === '-=') {
							step = -step;
						} else if (operator === '/=' || operator === '%=') {
							// 除法和取模操作可能导致非整数步长，我们暂时不处理
							return null;
						}
						
						return {
							type: 'assignment',
							operator: operator,
							step: step
						};
					}
				} else if (operator === '=') {
					// i = i + constant 等模式
					if (update.right.type === 'BinaryExpression' &&
						update.right.left.type === 'Identifier' &&
						update.right.left.name === loopVarName) {
						
						const binOp = update.right.operator;
						const stepValue = this.evaluateConstantExpression(update.right.right);
						
						if (stepValue !== undefined && 
							(binOp === '+' || binOp === '-' || binOp === '*' || binOp === '/')) {
							
							let step = stepValue;
							if (binOp === '-') {
								step = -step;
							} else if (binOp === '/' || binOp === '*') {
								// TODO: (I guess they are in far future...)
								// 乘除可能导致非整数步长，暂时不处理
								return null;
							}
							
							return {
								type: 'binary',
								operator: binOp,
								step: step
							};
						}
					}
				}
			}
		}
		
		return null;
	}

	/**
	 * 计算循环迭代次数
	 * @param {number} initValue - 初始值
	 * @param {Object} conditionInfo - 条件信息
	 * @param {Object} updateInfo - 更新信息
	 * @returns {number|null} - 迭代次数，或null如果无法计算
	 */
	calculateLoopIterations(initValue, conditionInfo, updateInfo) {
		if (initValue === undefined || !conditionInfo || !updateInfo) {
			return null;
		}
		
		const { comparisonOp, constValue } = conditionInfo;
		const step = updateInfo.step;
		
		// 检查步长是否为0
		if (step === 0) {
			// 步长为0可能导致无限循环或一次也不执行
			return null;
		}
		
		// 根据比较操作符计算迭代次数
		let iterations = 0;
		let currentValue = initValue;
		
		if (step > 0) {
			// 递增循环
			switch (comparisonOp) {
				case '<':
					while (currentValue < constValue) {
						iterations++;
						currentValue += step;
					}
					break;
				case '<=':
					while (currentValue <= constValue) {
						iterations++;
						currentValue += step;
					}
					break;
				case '>':
					while (currentValue > constValue) {
						iterations++;
						currentValue += step;
					}
					break;
				case '>=':
					while (currentValue >= constValue) {
						iterations++;
						currentValue += step;
					}
					break;
				case '!=':
					while (currentValue != constValue) {
						iterations++;
						currentValue += step;
					}
					break;
				default:
					return null;
			}
		} else {
			// 递减循环（step < 0）
			switch (comparisonOp) {
				case '<':
					while (currentValue < constValue) {
						iterations++;
						currentValue += step; // step为负
					}
					break;
				case '<=':
					while (currentValue <= constValue) {
						iterations++;
						currentValue += step;
					}
					break;
				case '>':
					while (currentValue > constValue) {
						iterations++;
						currentValue += step;
					}
					break;
				case '>=':
					while (currentValue >= constValue) {
						iterations++;
						currentValue += step;
					}
					break;
				case '!=':
					while (currentValue != constValue) {
						iterations++;
						currentValue += step;
					}
					break;
				default:
					return null;
			}
		}
		
		return iterations;
	}

	/**
	 * 模拟循环执行，更新变量常量值
	 * @param {ASTNode} loopNode - 循环节点
	 * @param {string} loopVarName - 循环变量名
	 * @param {number} initValue - 初始值
	 * @param {Object} conditionInfo - 条件信息
	 * @param {Object} updateInfo - 更新信息
	 * @param {number} iterationCount - 迭代次数
	 * @return {boolean} True for successful or false for unsuccessful (too many loops)
	 */
	simulateLoop(loopNode, loopVarName, initValue, conditionInfo, updateInfo, iterationCount, envInfo = null) {
		
		// User configuration
		const configuredMaxSimulation = 300;
		const configuredMaxGrossSimulation = 50000;
		// End
		/*
		`configuredMaxGrossSimulation` bounds how many rounds are simulated in
		total over one optimization round. Without it, a program with several
		(or nested) loops of 300 rounds each spends a long time simulating. When
		the budget is gone the simulation reports that it did not finish and the
		loop is left alone.
		*/
		if (this.grossSimulationBudget === undefined || this.grossSimulationBudget === null) {
			this.grossSimulationBudget = configuredMaxGrossSimulation;
		}
		
		const loopVarSymbol = this.currentScope.lookup(loopVarName);
		const scopePath = this.currentScope.lookupScopeOf(loopVarName).getPath();
		const localConstants = this.localConstants.get(scopePath);
		const variableUses = this.variableUses.get(scopePath);
		
		// 保存循环前的常量状态
		const savedConstants = new Map(localConstants);
		/*
		If the simulation does NOT end in the loop being replaced (the body
		reads memory, say, so no final value is known), these imagined constants
		must not survive: a later pass would then treat `i` as a compile-time
		constant and delete the very `i = i + 1` that advances the real loop.
		*/
		loopNode.setAttribute('savedConstants', savedConstants);
		loopNode.setAttribute('savedConstantsScopePath', scopePath);
		/*
		Ordinary analysis marks everything written inside a loop as not
		foldable (`canOptimize` false, see `evaluationState`). That is what stops
		`i = i + 1` from being folded into `i = 1`. Here, though, it would also
		stop the simulation after zero rounds. Lift it for the loop variable
		while we simulate; if the loop is not replaced afterwards,
		`markLoopAsPotentiallyWithSideEffects` puts the mark back.
		*/
		const loopUsage = variableUses.get(loopVarName);
		if (loopUsage) loopUsage.canOptimize = true;
		
		// ! Currently, new information might override old ones in combined loop !
		const info = {
			conditionInfo: conditionInfo,
			updateInfo: updateInfo,
			iterationCount: iterationCount,
			optimizingLoop: true,	/* Specially consider this in evaluations !!! */
			tryingAssignment: true,
			currentRound: 0,
			currentLoopVar: null,
			currentLoopVarName: loopVarName,
			grossIterationCount: iterationCount,
			parentInfo: envInfo
			// No atloop information
		};
		
		// 初始化循环变量
		loopVarSymbol.variableReferrer.modify();
		localConstants.set(loopVarName, initValue);
		loopNode.setAttribute('loopVarName', loopVarName);
		
		// 如果有循环体，尝试分析它
		if (loopNode.body && iterationCount > 0) {
			// 对于简单循环，我们可以模拟少量迭代来传播常量
			let maxSimulatedIterations = Number.isFinite(iterationCount)
				? Math.min(iterationCount, configuredMaxSimulation)
				: configuredMaxSimulation;
			maxSimulatedIterations = Math.max(0,
				Math.min(maxSimulatedIterations, this.grossSimulationBudget ?? 0));
			let i = 0;
			let broken = false, continuing = false;
			for (i = 0; i < maxSimulatedIterations; i++) {
				// 分析循环体
				// This will result in an execution
				let currentValue = localConstants.get(loopVarName) || initValue;
				
				info.currentRound = i;
				info.currentLoopVar = currentValue;
				
				try {
					this.analyzeNode(loopNode.body, info);
				} catch (error) {
					switch (error.type) {
						case 'BreakException':
							broken = true;
							break;
						case 'ContinueException':
							continuing = true;	// No special handler
							break;
						default:
							throw error;
							break;
					}
				}
				
				// 更新循环变量
				if (loopNode.type !== 'WhileStatement') {
					currentValue = localConstants.get(loopVarName) || initValue;
					const newValue = currentValue + updateInfo.step;
					localConstants.set(loopVarName, newValue);	// Should be done inside otherwise!
					this.addWrittenSymbol(loopVarSymbol, newValue, true);
				}
				
				
				// Manually added: quit also when the condition is no longer satisfied:
				if (loopNode.test) {
					const testEval = this.evaluateConstantExpression(loopNode.test);
					if (testEval == 0) {
						break;
					}
				}
				if (broken) {
					break;
				}
				
				// 如果循环变量不再可优化，停止模拟
				const usage = variableUses.get(loopVarName);
				if (usage && !usage.canOptimize) {
					loopNode.setAttribute('maybeHasSideEffects', true);
					break;
				}
			}
			
			/*
			Did the simulation actually reach the end of the loop? Only then may
			its results be used. `broken` (a `break` we ran into) and a condition
			that has become false both count as reaching the end; running out of
			iterations does not.
			*/
			let completed = broken;
			if ((!broken) && loopNode.test) {
				const testEval = this.evaluateConstantExpression(loopNode.test);
				if (testEval != 0) {
					// Still can be evaluated!!!
					loopNode.setAttribute('maybeHasSideEffects', true);
				} else {
					completed = true;
				}
			} else if (!broken && !loopNode.test) {
				// `while (true)`: never ends by itself
				completed = false;
			}
			this.grossSimulationBudget = (this.grossSimulationBudget ?? 0) - i;
			loopNode.setAttribute('simulationComplete', completed);
			
			// 设置循环后的变量值
			// const finalValue = initValue + (iterationCount * updateInfo.step);
			// localConstants.set(loopVarName, finalValue);
			
			// 标记循环已被优化
			// !! WARNING: Some rounds of loops might REMAIN UNOPTIMIZED !!
			loopNode.setAttribute('optimized', true);
			
			// !! In further generation, let it begin from i instead of its preset value !!
			loopNode.setAttribute('simulatedIterations', i);
			loopNode.setAttribute('simulatedTermination', info.currentLoopVar);
		}
		return true;
	}

	/**
	 * 反转比较操作符（当变量在右边时）
	 * @param {string} op - 原始操作符
	 * @returns {string} - 反转后的操作符
	 */
	reverseComparisonOperator(op) {
		switch (op) {
			case '<': return '>';
			case '<=': return '>=';
			case '>': return '<';
			case '>=': return '<=';
			case '==': return '==';
			case '!=': return '!=';
			default: return op;
		}
	}

	/**
	 * 
	 * @param {ASTNode} loopNode 
	 */
	markNodeAsWithSideEffects(loopNode) {
		// 收集循环中可能被修改的变量
		const modifiedVars = this.collectModifiedVariablesInLoop(loopNode);
		
		// 标记这些变量为不可优化
		modifiedVars.forEach(varInfo => {
			if (varInfo === 'unrecognized') return;
			const varName = varInfo.varName;
			const varSymbol = varInfo.scope.lookup(varName);
			const scopePath = ((
				varInfo ? varInfo.scope.lookupScopeOf(varName) : null
			) ?? this.currentScope.lookupScopeOf(varName)).getPath();			// This is because variable collector will not seriously consider scope information
			const variableUses = this.variableUses.get(scopePath);
			const constantList = this.localConstants.get(scopePath);
			const usage = variableUses.get(varName);
			if (usage) {
				usage.canOptimize = false;
			}
			if (constantList.has(varName)) {
				constantList.delete(varName);
			}
			if (varSymbol) {
				varSymbol.isConstant = false;
				varSymbol.constantValue = null;
			}
		});
		
		if (loopNode.getAttribute('hasUnknownSideEffects')) {
			console.log("Has undetermined side effects, clearing all variables!:", loopNode);
			// In particular, clearing out anything that could have been written in all functions
			[...(new Set([...this.functionInfo].flatMap(([name, data]) => data.writtenVars)))]
				.forEach(symb => {
					//const scopePath = symb.scope.getPath();
					//const localConstants = this.localConstants.get(scopePath);
					//const variableUses = this.variableUses.get(scopePath);
					this.addWrittenSymbol(symb);
			});
		}
		
		// 标记循环有副作用
		loopNode.setAttribute('hasSideEffects', true);
	}

	/**
	 * 标记循环可能有副作用
	 * @param {WhileStatementNode | ForStatementNode} loopNode - 循环节点
	 */
	markLoopAsPotentiallyWithSideEffects(loopNode) {
		this.markNodeAsWithSideEffects(loopNode);		
	}

	/**
	 * 收集循环中可能被修改的变量
	 * 
	 * @param {ASTNode} loopNode - 循环节点
	 * @returns {Set<variableModification | "unrecognized">} - 被修改的变量集合
	 * @remarks - Manually modified for side effect checkers
	 * @remarks - Can't be directly bound to symbols somehow
	 */
	collectModifiedVariablesInLoop(loopNode) {
		const modifiedVars = new Set();
		let callee;

		// 递归收集赋值表达式中的变量
		// There's no need to make this closure :(
		const collectFromNode = (node) => {
			if (!node) return;
			
			switch (node.type) {
				case 'Program':
					return;
				case 'AssignmentExpression':
					if (node.left.type === 'Identifier' && !node.left.name.startsWith('@')) {
						modifiedVars.add({
							varName: node.left.name,
							scope: (node.left.symbol ? node.left.symbol.scope : null) ?? node.scope,
							symbol: node.left.symbol
						});
					} else {
						// Unrecognized...
						console.log('Internal Error: Unrecognized variable in loop optimizer', node.left);
						modifiedVars.add("unrecognized");
					}
					break;
				
				case 'BinaryExpression':
					if (node.operator.includes('=') && node.operator !== '==' && node.operator !== '!='
					&& node.operator !== '>=' && node.operator !== '<=') {
						if (node.left.type === 'Identifier' && !node.left.name.startsWith('@')) {
							modifiedVars.add({
								varName: node.left.name,
								scope: (node.left.symbol ? node.left.symbol.scope : null) ?? node.scope,
								symbol: node.left.symbol
							});
						} else {
							console.log('Internal Error: Unrecognized variable in loop optimizer',node.left);
							modifiedVars.add("unrecognized");
						}
					}
					break;
				case 'UnaryExpression':
					if ((node.operator === '++' || node.operator === '--' || node.operator == '*' || node.operator == '&')) {
						if (node.argument.type === 'Identifier' && !node.argument.name.startsWith('@')) {
							modifiedVars.add({
								varName: node.argument.name,
								scope: (node.argument.symbol ? node.argument.symbol.scope : null) ?? node.scope,
								symbol: node.argument.symbol
							});
						} else {
							//console.log('Internal Error: Unrecognized variable in loop optimizer',node.left);
							collectFromNode(node.argument);
						}
					}
					break;
				case 'CastExpression':
					if (node.dataType && (node.dataType.isVolatile() || node.dataType.isPointerImpl())) {
						if (node.expression.type === 'Identifier' && !node.expression.name.startsWith('@')) {
							modifiedVars.add({
								varName: node.expression.name,
								scope: (node.expression.symbol ? node.expression.symbol.scope : null) ?? node.scope,
								symbol: node.expression.symbol
							});
						} else {
							collectFromNode(node.expression);
						}
					}
					break;
				case 'FunctionCall':
					if (node.callee && node.callee.type === 'Identifier') {
						callee = node.callee.name;
					}
					// Deliberately pass down
				case 'BuiltinCall':
					if (node.functionName) {
						callee = node.functionName;
					}
					// 函数调用可能修改全局变量
					// Replaced manually !!
					const calleeInfo = this.functionInfo.get(callee);
					if (this.hasSideEffects(node)) {
						// 标记循环有副作用
						if (calleeInfo && !calleeInfo.writtenVarsUnknown) {
							[...new Set(calleeInfo.writtenVars ?? [])]
								.forEach(symb => {
									this.addWrittenSymbol(symb);			// I don't know whether this is correct
									modifiedVars.add({
										varName: symb.name,
										scope: symb.scope,
										symbol: symb
									});
								});
						} else if (calleeInfo) {
							collectFromNode(calleeInfo.node);
						} else {
							loopNode.setAttribute('hasUnknownSideEffects', true);
						}
						loopNode.setAttribute('hasFunctionCall', true);
					}
					break;
				
			}
			
			// 递归处理子节点
			if (node.children) {
				node.children.forEach(child => collectFromNode(child));
			}

			const fieldsToCheck = [
				'functions', 'globalDeclarations', 'typeDefinitions',
				'statements', 'expression', 'test', 'consequent', 'alternate',
				'body', 'init', 'update', 'argument', 'left', 'right',
				'declarators', 'arguments', 'callee', 'initializer'
			];

			for (const field of fieldsToCheck) {
				if (Array.isArray(node[field])) {
					node[field].forEach(elem => collectFromNode(elem));
				} else if (node[field] && typeof node[field] === 'object') {
					collectFromNode(node[field]);
				}
			}
		};
		
		collectFromNode(loopNode);
		return modifiedVars;
	}
	
		/**
	 * 优化可确定的循环，将其替换为直接赋值
	 * @param {ASTNode} node - 要处理的AST节点
	 * @returns {boolean} - 是否进行了优化
	 */
	optimizeDeterministicLoops(node) {
		if (!node) return false;
		
		let modified = false;
		
		switch (node.type) {
			case 'ForStatement':
				modified = this.optimizeDeterministicForLoop(node);
				break;
				
			case 'WhileStatement':
				modified = this.optimizeDeterministicWhileLoop(node);
				break;
				
			case 'CompoundStatement':
				if (node.statements) {
					// 处理复合语句中的所有语句
					for (let i = 0; i < node.statements.length; i++) {
						const stmt = node.statements[i];
						if (this.optimizeDeterministicLoops(stmt)) {
							modified = true;
							// 重新检查当前索引，因为stmt可能已被替换
							i--;
						}
					}
				}
				break;
				
			case 'IfStatement':
				modified = this.optimizeDeterministicLoops(node.consequent) ||
						  (node.alternate ? this.optimizeDeterministicLoops(node.alternate) : false);
				break;
				
			default:
				// 递归处理子节点
				if (node.children) {
					node.children.forEach(child => {
						if (this.optimizeDeterministicLoops(child)) {
							modified = true;
						}
					});
				}
				break;
		}
		
		return modified;
	}

	/**
	 * Undo what `simulateLoop` planted, when the loop turned out not to be
	 * replaceable after all.
	 *
	 * @param {ASTNode} loopNode
	 */
	restoreLoopConstants(loopNode) {
		const saved = loopNode?.getAttribute?.('savedConstants');
		const scopePath = loopNode?.getAttribute?.('savedConstantsScopePath');
		if (!saved || !scopePath) return;
		const live = this.localConstants.get(scopePath);
		if (!live) return;
		live.clear();
		for (const [key, value] of saved) live.set(key, value);
		loopNode.setAttribute('savedConstants', null);	// only once
	}

	/**
	 * 优化可确定的for循环
	 * @param {ASTNode} forNode - for语句节点
	 * @returns {boolean} - 是否进行了优化
	 * @remark Has manual changes !
	 */
	optimizeDeterministicForLoop(forNode) {
		if (!forNode || forNode.type !== 'ForStatement') return false;
		
		// 检查循环是否可优化
		const canOptimize = forNode.getAttribute('canOptimize');
		const iterationCount = forNode.getAttribute('iterationCount');
		// ! Manually adds !
		const simulatedIterations = forNode.getAttribute('simulatedIterations');
		const simulatedTerminations = forNode.getAttribute('simulatedTerminations');
		const loopVarName = forNode.getAttribute('loopVarName');
		
		const hasSideEffects = forNode.getAttribute('hasSideEffects');
		const neverExecuted = forNode.getAttribute('neverExecuted');
		
		if (!canOptimize && !neverExecuted) return false;
		
		// 如果循环永远不会执行
		if (neverExecuted) {
			return this.replaceLoopWithInitialization(forNode);
		}
		
		// 如果循环有副作用，不能完全删除
		if (hasSideEffects || forNode.getAttribute('maybeHasSideEffects')) {
			// 检查是否可以部分优化（减少循环次数）
			const done = this.partiallyOptimizeLoop(forNode, loopVarName, iterationCount);
			if (!done) this.restoreLoopConstants(forNode);
			return done;
		}
		
		// 完全可优化的循环
		const done = this.fullyOptimizeDeterministicLoop(forNode, iterationCount);
		if (!done) this.restoreLoopConstants(forNode);
		return done;
	}

	/**
	 * 用初始化语句替换循环（用于永远不会执行的循环）
	 * @param {ASTNode} forNode - for语句节点
	 * @returns {boolean} - 是否进行了替换
	 */
	replaceLoopWithInitialization(forNode) {
		// 创建替换语句
		const replacementStatements = [];
		
		// 保留初始化部分（如果有）
		if (forNode.init) {
			if (forNode.init.type === 'VariableDeclaration') {
				replacementStatements.push(forNode.init);
			} else if (forNode.init.type === 'ExpressionStatement') {
				replacementStatements.push(forNode.init);
			}
		}
		
		// 保留条件的副作用（如果有）
		const conditionHasSideEffects = forNode.getAttribute('conditionHasSideEffects');
		if (conditionHasSideEffects && forNode.test) {
			const exprStmt = new ASTNode('ExpressionStatement');
			exprStmt.addChild(forNode.test);
			replacementStatements.push(exprStmt);
		}
		
		// 创建替换节点
		let replacementNode;
		if (replacementStatements.length === 0) {
			replacementNode = null;
		} else if (replacementStatements.length === 1) {
			replacementNode = replacementStatements[0];
		} else {
			const compoundStmt = ASTBuilder.compoundStatement();
			compoundStmt.statements = replacementStatements;
			replacementNode = compoundStmt;
		}
		
		// 替换循环
		this.replaceNode(forNode, replacementNode);
		this.logOptimization(`Replaced never-executed loop with initialization only`);
		return true;
	}

	
	/**
	 * 优化可确定的while循环
	 * @param {ASTNode} whileNode - while语句节点
	 * @returns {boolean} - 是否进行了优化
	 * @todo -- Use the similar skill (partial execution) in FOR loops !
	 * @todo
		!!! THE CALL PARAMETER INSIDE THIS FUNCTION !!!
		(Must fetch the variable index)
	 */
	optimizeDeterministicWhileLoop(whileNode) {
		if (!whileNode || whileNode.type !== 'WhileStatement') return false;
		
		// 检查循环是否可优化
		const canOptimize = whileNode.getAttribute('canOptimize');
		const iterationCount = whileNode.getAttribute('iterationCount');
		const hasSideEffects = whileNode.getAttribute('hasSideEffects');
		const neverExecuted = whileNode.getAttribute('neverExecuted');
		const loopVarName = whileNode.getAttribute('loopVarName');
		
		if (!canOptimize && !neverExecuted) return false;
		
		// 如果循环永远不会执行
		if (neverExecuted) {
			// while循环没有初始化部分，直接删除
			// 但需要保留条件的副作用（如果有）
			const conditionHasSideEffects = whileNode.getAttribute('conditionHasSideEffects');
			if (conditionHasSideEffects) {
				// 用条件表达式替换整个while循环
				const exprStmt = new ASTNode('ExpressionStatement');
				exprStmt.addChild(whileNode.test);
				this.replaceNode(whileNode, exprStmt);
			} else {
				// 完全删除
				this.replaceNode(whileNode, null);
			}
			return true;
		}
		
		// 如果循环有副作用，不能完全删除
		if (hasSideEffects || whileNode.getAttribute('maybeHasSideEffects')) {
			// 检查是否可以部分优化（减少循环次数）
			const done = this.partiallyOptimizeLoop(whileNode, loopVarName, iterationCount);
			if (!done) this.restoreLoopConstants(whileNode);
			return done;
		}
		
		// 完全可优化的循环
		const done = this.fullyOptimizeDeterministicLoop(whileNode, iterationCount);
		if (!done) this.restoreLoopConstants(whileNode);
		return done;
	}

	/**
	 * 完全优化确定性循环（替换为直接赋值）
	 * @param {ASTNode} loopNode - 循环节点
	 * @param {number} iterationCount - 迭代次数
	 * @returns {boolean} - 是否进行了优化
	 * @remark ! Has manual changes !
	 */
	fullyOptimizeDeterministicLoop(loopNode, iterationCount) {
		if (iterationCount === undefined || iterationCount < 0) return false;
		
		// 收集循环中修改的所有变量
		const modifiedVars = this.collectModifiedVariablesInLoop(loopNode);
		
		// 创建替换语句
		const replacementStatements = [];
		
		// 对于for循环，保留初始化部分（如果有）
		if (loopNode.type === 'ForStatement' && loopNode.init) {
			// 检查初始化部分是否是变量声明
			if (loopNode.init.type === 'VariableDeclaration') {
				replacementStatements.push(loopNode.init);
			} else if (loopNode.init.type === 'ExpressionStatement') {
				replacementStatements.push(loopNode.init);
			} else if (loopNode.init.type === 'AssignmentExpression') {
				const exprStmt = new ASTNode('ExpressionStatement');
				exprStmt.addChild(loopNode.init);
				replacementStatements.push(exprStmt);
			}
		}
		
		// After init!
		// 检查所有修改的变量在循环结束后是否有确定的常量值
		const varFinalValues = new Map();
		for (const varInfo of modifiedVars) {
			if (varInfo === 'unrecognized') {
				throw "Unexpected 'unrecognized'!";
			}
			const varName = varInfo.varName;

			const varSymbol = varInfo.symbol ?? this.currentScope.lookup(varName);
			/*
			A variable the outside world can also reach must not be pinned to a
			value: it may be changed by another processor (`extern`), it has to
			be stored in memory (`volatile`), it keeps its value between calls
			(`static`), or somebody took its address.
			*/
			if (varSymbol && (varSymbol.isVolatile || varSymbol.isExtern
				|| varSymbol.isStatic || varSymbol.isAddressed)) {
				return false;
			}
			const scopePath = (varInfo.scope ?? this.currentScope.lookupScopeOf(varName)).getPath();
			const localConstants = this.localConstants.get(scopePath);
			const variableUses = this.variableUses.get(scopePath);
			const finalValue = localConstants.get(varName);
			if (finalValue === undefined) {
				// 变量没有确定的最终值，不能完全优化
				return false;
			}
			/*
			Emit the assignment for EVERY variable the loop writes, not only for
			those declared in the loop's own scope. Skipping the others used to
			drop writes to outer/global variables silently: `while (i < 10) {
			sid++; i++; }` was replaced by nothing at all, leaving `sid` at 0.
			*/
			varFinalValues.set(varName, finalValue);
			
			// 检查变量的使用情况
			const usage = variableUses.get(varName);
			if (usage && !usage.canOptimize) {
				// 变量标记为不可优化
				// Side effect processor will be done in further operations
				// (Given that it has been side effects, how the values are adjusted
				// no longer matters)
				return false;
			}
			// Add information for these writes
			this.addWrittenSymbol(varSymbol, finalValue, true);
		}
		
		// 为每个修改的变量创建赋值语句
		for (const [varName, finalValue] of varFinalValues) {
			// 创建数值字面量
			const numericLiteral = ASTBuilder.numericLiteral(finalValue);
			
			// 创建标识符
			const identifier = ASTBuilder.identifier(varName);
			
			// 创建赋值表达式
			const assignmentExpr = ASTBuilder.assignmentExpression('=', identifier, numericLiteral);
			
			// 创建表达式语句
			const exprStmt = new ASTNode('ExpressionStatement');
			exprStmt.addChild(assignmentExpr);
			
			replacementStatements.push(exprStmt);
			
		}
		
		// 如果有多个语句，用复合语句包装
		let replacementNode;
		if (replacementStatements.length === 0) {
			// 没有语句要生成，完全删除循环
			replacementNode = null;
		} else if (replacementStatements.length === 1) {
			// 只有一个语句，直接使用
			replacementNode = replacementStatements[0];
		} else {
			// 多个语句，用复合语句包装
			const compoundStmt = ASTBuilder.compoundStatement();
			compoundStmt.statements = replacementStatements;
			replacementNode = compoundStmt;
			replacementStatements.forEach(stmt => stmt.parent = compoundStmt);
		}
		
		// 替换循环节点
		this.replaceNode(loopNode, replacementNode);
		
		this.logOptimization(`Optimized deterministic loop with ${iterationCount} iterations to direct assignments`);
		return true;
	}

	/**
	 * 部分优化循环（减少循环次数）
	 * @param {ASTNode} loopNode - 循环节点
	 * @param {string} loopVariable
	 * @param {number} totalIterations - 总迭代次数
	 * @returns {boolean} - 是否进行了优化
	 * @remark This is manually adjusted !
	 * @remark The function collectModifiedVariables() should be actually collectModifiedVariablesInLoop()
	 * @remark This function is currently not widely used
	 * @todo This might not be useful for WHILE loops !
	 * @todo The variables to collect might be more than loop variables themselves
	 */
	partiallyOptimizeLoop(loopNode, loopVariable, totalIterations) {
		if (!totalIterations || totalIterations <= 1) return false;
		
		// 获取当前作用域路径
		const scopePath = this.getCurrentScopePath();
		const localConstants = this.localConstants.get(scopePath);
		
		// 收集循环中修改的变量
		const modifiedVars = this.collectModifiedVariablesInLoop(loopNode);
		
		// 找到第一个有副作用的迭代
		const firstSideEffectIteration = loopNode.getAttribute('simulatedIterations');
		
		if (firstSideEffectIteration === 0) {
			// 第一轮就有副作用，不能优化
			return false;
		}
		
		// TODO: In fact, it might be because of UNEXPECTED CONDITION CHANGE INSIDE THE LOOP!
		/*
		if (firstSideEffectIteration >= totalIterations) {
			// 所有迭代都没有副作用，可以完全优化
			// Debug:
			console.log('unexpected side effect call');
			return this.fullyOptimizeDeterministicLoop(loopNode, totalIterations);
		}
		*/
		// 部分优化：展开没有副作用的部分，保留有副作用的部分
		const newLoopState = this.partiallyUnrollLoop(loopNode, loopVariable, totalIterations, firstSideEffectIteration, modifiedVars);
		this.markLoopAsPotentiallyWithSideEffects(loopNode);
		return newLoopState;
	}


	/**
	 * 部分展开循环
	 * @param {ASTNode} loopNode - 循环节点
	 * @param {string} loopVariable - The loop index variable
	 * @param {number} totalIterations - 总迭代次数
	 * @param {number} firstSideEffect - 第一个有副作用的迭代索引
	 * @param {Set<{varName: string, scope: Scope}>} modifiedVars - 修改的变量集合
	 * @returns {boolean} - 是否进行了优化
	 * @remark There are adjustements done manually to adapt to existing code
	 */
	partiallyUnrollLoop(loopNode, loopVariable, totalIterations, firstSideEffect, modifiedVars) {
		const indexAtSideEffect = loopNode.getAttribute('simulatedTermination');	// This is a value
		
		// 创建替换语句
		const replacementStatements = [];
		
		// 对于for循环，保留初始化部分
		if (loopNode.type === 'ForStatement' && loopNode.init) {
			if (loopNode.init.type === 'VariableDeclaration') {
				replacementStatements.push(loopNode.init);
			} else if (loopNode.init.type === 'ExpressionStatement') {
				replacementStatements.push(loopNode.init);
			}
		}
		
		// 展开没有副作用的部分（通过直接赋值）
		if (firstSideEffect > 0) {
			
			modifiedVars.forEach(varInfo => {
				if (varInfo === 'unrecognized') {
					throw "Unexpected 'unrecognized' when partially unrolling loop!";
				}
				const varName = varInfo.varName;
				const scopePath = (varInfo.scope ?? this.currentScope).lookupScopeOf(varName).getPath();
				const localConstants = this.localConstants.get(scopePath);
				const value = localConstants.get(varName);
				const numericLiteral = ASTBuilder.numericLiteral(value);
				const identifier = ASTBuilder.identifier(varName);
				const assignmentExpr = ASTBuilder.assignmentExpression('=', identifier, numericLiteral);
				const exprStmt = new ASTNode('ExpressionStatement');
				exprStmt.addChild(assignmentExpr);
				replacementStatements.push(exprStmt);
			});
			
			/*
			// 计算无副作用迭代后的变量值
			const optimizedValues = this.calculateValuesAfterIterations(loopNode, firstSideEffect, modifiedVars);
			
			// 为每个变量创建赋值语句
			for (const [varName, value] of optimizedValues) {
				const numericLiteral = ASTBuilder.numericLiteral(value);
				const identifier = ASTBuilder.identifier(varName);
				const assignmentExpr = ASTBuilder.assignmentExpression('=', identifier, numericLiteral);
				const exprStmt = new ASTNode('ExpressionStatement');
				exprStmt.expression = assignmentExpr;
				replacementStatements.push(exprStmt);
			}
			*/
			this.logOptimization(`Partially unrolled loop: ${firstSideEffect} side-effect-free iterations converted to direct assignments`);
		}
		
		// 修改原循环，减少迭代次数
		// 需要创建一个新的循环，执行剩余的有副作用迭代
		let modifiedLoop;
		if (loopNode.type === 'ForStatement') {
			modifiedLoop = this.createModifiedForLoop(loopNode, loopVariable, indexAtSideEffect);
		} else {
			modifiedLoop = this.createModifiedWhileLoop(loopNode, loopVariable, indexAtSideEffect);
		}
		
		if (modifiedLoop) {
			replacementStatements.push(modifiedLoop);
		}
		
		
		// 创建替换节点
		let replacementNode;
		if (replacementStatements.length === 0) {
			replacementNode = null;
		} else if (replacementStatements.length === 1) {
			replacementNode = replacementStatements[0];
		} else {
			const compoundStmt = ASTBuilder.compoundStatement();
			compoundStmt.statements = replacementStatements;
			replacementStatements.forEach(stmt => {
				stmt.parent = compoundStmt;
			});
			replacementNode = compoundStmt;
		}
		replacementStatements.forEach(stmt => {
			stmt.scope = loopNode.scope;
		});
		
		// 替换原循环
		this.replaceNode(loopNode, replacementNode);
		return true;
	}

	/**
	 * 创建修改后的for循环（减少迭代次数）
	 * @param {ASTNode} originalLoop - 原始循环
	 * @param {number} completedIterations - 已完成的迭代次数
	 * @param {number} remainingIterations - 剩余迭代次数
	 * @returns {ASTNode} - 修改后的循环
	 * @remark <NOTICE> WE MUST DEAL WITH SPECIAL INITIALIZATIONS !!!
	 * @remark Warning: data types there are not considered, which may result in problems.
	 */
	createModifiedForLoop(originalLoop, loopVariable, newInitValue) {
		// 复制原始循环
		const typeReference = this.currentScope.lookup(loopVariable).type.type;
		const newLoop = new ASTNode('ForStatement');
		newLoop.init = originalLoop.init;
		newLoop.test = originalLoop.test;
		newLoop.update = originalLoop.update;
		newLoop.body = originalLoop.body;
		newLoop.scope = originalLoop.scope;
		// Shallow copy only!!!
		
		// 修改初始化部分：将循环变量设置为已完成的迭代后的值
		if (loopVariable) {
			const initValue = newInitValue;
			if (initValue !== undefined) {
				const numericLiteral = ASTBuilder.numericLiteral(newInitValue);
				const identifier = ASTBuilder.identifier(loopVariable);
				numericLiteral.dataType = typeReference;
				identifier.dataType = typeReference;
				const assignmentExpr = ASTBuilder.assignmentExpression('=', identifier, numericLiteral);
				numericLiteral.parent = assignmentExpr;
				identifier.parent = assignmentExpr;

				if (originalLoop.init.type === 'VariableDeclaration') {
					// 修改变量声明的初始值
					const newDeclarator = ASTBuilder.variableDeclarator(loopVariable);
					newDeclarator.initializer = numericLiteral;
					numericLiteral.parent = newDeclarator;
					newLoop.init.declarators = [newDeclarator];
					newDeclarator.parent = newLoop;
				} else if (originalLoop.init.type === 'ExpressionStatement') {
					// To be verified
					originalLoop.init.addChild(assignmentExpr);
					assignmentExpr.parent = originalLoop;
				} else {
					// 创建赋值表达式语句
					const exprStmt = new ASTNode('ExpressionStatement');
					exprStmt.addChild(assignmentExpr);
					newLoop.init = exprStmt;
					exprStmt.parent = newLoop;
				}
			}
		}
		
		newLoop.setAttribute('confirmedSideEffects', true);
		return newLoop;
	}

	/**
	 * 
	 * @param {WhileStatementNode} originalLoop 
	 * @param {string} loopVariable 
	 * @param {number} newInitValue 
	 * @remark Scheisse! Deepseek had an illusion and didn't implement this
	 */
	createModifiedWhileLoop(originalLoop, loopVariable, newInitValue) {
		const typeReference = this.currentScope.lookup(loopVariable).type.type;
		const clonedLoop = this.cloneAST(originalLoop);
		const packer = ASTBuilder.compoundStatement();
		const ident = ASTBuilder.identifier(loopVariable);
		const numeric = ASTBuilder.numericLiteral(newInitValue);
		const assigner = ASTBuilder.assignmentExpression('=', ident, numeric);
		ident.parent = assigner;
		numeric.parent = assigner;
		ident.dataType = typeReference;
		numeric.dataType = typeReference;
		packer.statements.push(assigner);
		packer.statements.push(originalLoop);
		assigner.parent = packer;
		packer.scope = ident.scope = numeric.scope = assigner.scope = originalLoop.scope;
		originalLoop.parent = packer;
		originalLoop.setAttribute('confirmedSideEffects', true);
		return packer;
	}

	
	analyzeReturnStatement(node, info = null) {
		// 分析返回值表达式
		if (node.argument) {
			this.analyzeNode(node.argument, info);
		}
		
		// 标记当前函数有return语句
		if (this.currentScope && this.currentScope.astNode) {
			const funcName = this.currentScope.astNode.name;
			const funcInfo = this.functionInfo.get(funcName);
			if (funcInfo) {
				funcInfo.hasReturn = true;
			}
		}
		if (info && info.inFunction) {
			info.returnCount.record();
		}
	}

	analyzeBreakStatement(node, info = null) {
		// 标记在循环或switch中有break
		if (info && info.isLoop) {
			info.hasBreak = true;
		}
		if (info && info.optimizingLoop) {
			throw new BreakException(node);
		}
		// 也可以记录在其他结构中，如switch
	}

	analyzeContinueStatement(node, info = null) {
		// 标记在循环中有continue
		if (info && info.isLoop) {
			info.hasContinue = true;
		}
		if (info && info.optimizingLoop) {
			throw new ContinueException(node);
		}
	}

	// ! Manually adjusted !
	/**
	 * 
	 * @param {SymbolEntry} varSymbol 
	 * @param {*} value
	 * @param {boolean} doUpdate null: Record write count only; true: Try no side effect value; false: Label as with side effect
	 */
	/**
	 * Record that `varSymbol` is written, and how much we know about it.
	 *
	 * The third argument is one of the three evaluation states
	 * (`Optimizer.EVAL_*`), or the booleans that used to stand for them:
	 *
	 *   DEFINITE (true)  -- the value is known; it goes into the constant table.
	 *   TRACKED  (null)  -- only the WRITE is known: count it, but do not touch
	 *                       the constant table in either direction (leaving a
	 *                       stale constant would be wrong, deleting a good one
	 *                       would be pessimistic).
	 *   UNKNOWN  (false) -- nothing is reliable; drop whatever we thought we knew.
	 *
	 * A `value` of `null`/`undefined` means "the value is not known", which
	 * forces the pessimistic path even when the state is DEFINITE.
	 *
	 * @param {SymbolEntry} varSymbol
	 * @param {any} value The value written, or null/undefined if unknown
	 * @param {string|boolean?} [state] EVAL_DEFINITE / EVAL_TRACKED / EVAL_UNKNOWN
	 * @returns {void}
	 */
	addWrittenSymbol(varSymbol, value, state = Optimizer.EVAL_DEFINITE) {
		if (!varSymbol) return;
		/*
		A write to an array destroys whatever we knew about its elements: the
		subscript may be anything, and even a known one invalidates the rest.
		*/
		if (this.isFoldableArray(varSymbol)) this.invalidateConstantArray(varSymbol);
		// Accept the old `doUpdate` booleans: true -> DEFINITE, false -> UNKNOWN
		const resolved = this.resolveWriteState(state);
		const state_ = Optimizer;
		const valueKnown = (value != null && value != undefined);

		const varName = varSymbol.name;
		const scopePath = varSymbol.scope.getPath();
		const variableUses = this.variableUses.get(scopePath);
		const localConstants = this.localConstants.get(scopePath);

		if (!variableUses || !localConstants) return;

		/* ---- 1. The write itself is known, whatever the state ---- */
		const usage = variableUses.get(varName);
		if (usage) {
			usage.writes++;
			if (resolved !== state_.EVAL_UNKNOWN) {
				// A known value may be folded; an unknown one forbids it
				usage.canOptimize = valueKnown;
			}
		}

		// Record variable access in it
		if (this.currentFunction && this.functionInfo.has(this.currentFunction)) {
			const target = this.functionInfo.get(this.currentFunction);
			target.writtenVars.push(varSymbol);
		}

		/* ---- 2. What does this do to the constant table? ---- */
		if (resolved === state_.EVAL_TRACKED) {
			// Only the access is known: leave the constant table alone
			varSymbol.variableReferrer?.modify();
			return;
		}
		if (resolved === state_.EVAL_UNKNOWN || !valueKnown) {
			// The value is gone (or was never known): forget any constant
			localConstants.delete(varName);
			varSymbol.isConstant = false;
			varSymbol.constantValue = null;
			varSymbol.variableReferrer?.modify();
			return;
		}

		/* ---- 3. DEFINITE with a known value: record it ---- */
		let recorded = value;
		const varType = varSymbol.myType();
		if (this.semantic.isTypeCompatibleForAny(varType, ['int', 'long', 'unsigned', 'signed'], true)) {
			if (typeof recorded === 'number') recorded = Math.floor(recorded);
		}
		if (this.semantic.isSameType(varType, this.semantic.getTypeInfo('bool'))) {
			recorded = Boolean(recorded);
		}
		localConstants.set(varName, recorded);
		varSymbol.markAsConstant(recorded);
		varSymbol.variableReferrer?.modify();
	}

	/* ------------------------------------------------------------------ *
	 * Constant arrays
	 *
	 * An array whose every element is a compile-time constant, and that is
	 * never written again, can be folded at compile time just like a plain
	 * constant: `a[2]` becomes a number. The elements are kept per scope, and
	 * any write to the array (or taking its address) throws them away.
	 * ------------------------------------------------------------------ */

	/**
	 * The variable an lvalue ultimately refers to: `a` for `a`, `a` for `a[i]`
	 * and `a.b.c`, `p` for `*p`.
	 *
	 * @param {any} expr
	 * @returns {SymbolEntry?}
	 */
	baseSymbolOf(expr) {
		let current = expr;
		while (current) {
			if (current.type === 'Identifier') {
				return this.currentScope.lookup(current.name) ?? null;
			}
			if (current.type === 'MemberExpression') {
				current = current.children?.[0] ?? current.object ?? null;
				continue;
			}
			if (current.type === 'UnaryExpression' && current.operator === '*') {
				current = current.argument ?? null;
				continue;
			}
			return null;
		}
		return null;
	}

	/**
	 * @param {SymbolEntry} symbol
	 * @returns {string}
	 */
	constantArrayKey(symbol) {
		return `${symbol.scope.getPath()}|${symbol.name}`;
	}

	/**
	 * @param {SymbolEntry} symbol
	 * @returns {number[]?} The elements, or null when they are not known
	 */
	getConstantArray(symbol) {
		if (!symbol) return null;
		this.constantArrays = this.constantArrays ?? new Map();
		return this.constantArrays.get(this.constantArrayKey(symbol)) ?? null;
	}

	/**
	 * Remember the elements of an array.
	 * @param {SymbolEntry} symbol
	 * @param {any[]} values
	 * @returns {void}
	 */
	recordConstantArray(symbol, values) {
		if (!symbol || !Array.isArray(values) || values.length === 0) return;
		this.constantArrays = this.constantArrays ?? new Map();
		this.constantArrays.set(this.constantArrayKey(symbol), values.slice());
	}

	/**
	 * Update ONE element of an array whose value we know.
	 *
	 * This is what makes `a[2] = 7;` followed by `print(a[2])` fold, instead of
	 * throwing the whole array away.
	 *
	 * @param {SymbolEntry} symbol
	 * @param {number} index
	 * @param {any} value
	 * @returns {void}
	 */
	writeConstantArrayElement(symbol, index, value) {
		const elements = this.getConstantArray(symbol);
		if (!elements) return;
		if (!Number.isInteger(index) || index < 0 || index >= elements.length) {
			// Out of bounds or not a constant: nothing can be assumed any more
			this.invalidateConstantArray(symbol);
			return;
		}
		if (value === undefined || value === null) {
			elements[index] = undefined;
			return;
		}
		elements[index] = value;
	}

	/**
	 * Whether every element of the array is known.
	 * @param {SymbolEntry} symbol
	 * @returns {boolean}
	 */
	constantArrayComplete(symbol) {
		const elements = this.getConstantArray(symbol);
		if (!elements) return false;
		const length = this.arrayLengthOf(symbol);
		if (length > 0 && elements.length !== length) return false;
		return elements.every(item => item !== undefined && item !== null);
	}

	/**
	 * How many elements the array symbol has.
	 * @param {SymbolEntry} symbol
	 * @returns {number}
	 */
	arrayLengthOf(symbol) {
		const type = symbol.myType?.() ?? null;
		const sizes = (type && type.arraySize) ? type.arraySize : null;
		const asNumber = item => {
			if (item === null || item === undefined) return null;
			if (typeof item === 'number') return item;
			if (typeof item.value === 'number') return item.value;
			return null;
		};
		const list = Array.isArray(sizes) ? sizes : [sizes];
		const numbers = list.map(asNumber).filter(n => n !== null && n > 0);
		if (numbers.length > 0) return numbers.reduce((a, b) => a * b, 1);
		return type && type.size ? type.size : 0;
	}

	/**
	 * Promote arrays whose every element turned out to be a compile-time constant.
	 *
	 * After all the analysis rounds, an array may be left with a complete set of
	 * known elements -- because it was initialised with constants, or because
	 * every write to it could be folded (including writes that appeared inside an
	 * unrolled loop). Such an array IS a constant array, and is treated as one:
	 *
	 *  - its values are handed to the code generator, so a constant subscript
	 *    needs no memory access at all;
	 *  - when nothing writes to it at run time any more, it is stored the way a
	 *    `const` array is (a single encoded string instead of memory cells).
	 *
	 * An array that still has real `write` instructions is left in its current
	 * storage: it is the values that are known, not the array that is immutable.
	 *
	 * @returns {void}
	 */
	/**
	 * Turn one tracked constant into a number.
	 *
	 * @param {any} value
	 * @returns {number?} null when it is not a compile time number
	 */
	numberOfTrackedValue(value) {
		if (typeof value === 'number') return Number.isFinite(value) ? value : null;
		if (value && typeof value === 'object' && value.isBuiltinConstant && typeof value.name === 'string') {
			const table = this.contentTable;
			if (!table) return null;
			for (const category of ['item', 'liquid', 'unit', 'block']) {
				const id = table.idOf(category, value.name);
				if (id !== undefined) return (id === null) ? null : id;
			}
			return null;
		}
		if (typeof value === 'boolean') return value ? 1 : 0;
		return null;
	}

	finalizeConstantArrays() {
		if (!this.constantArrays || this.constantArrays.size === 0) return;
		const visit = scope => {
			if (!scope) return;
			(scope.getAllSymbols?.() ?? []).forEach(symbol => {
				if (!this.isFoldableArray(symbol)) return;
				if (!this.constantArrayComplete(symbol)) return;
				const elements = this.getConstantArray(symbol);
				if (!elements) return;
				symbol.isConstantArray = true;
				/*
				The tracked values are optimizer objects: a content constant such
				as `@lead` is `{isBuiltinConstant: ...}`, not a number. Turning an
				array into an encoded table needs real numbers, so translate (and
				when that is impossible, keep whatever the semantic pass recorded
				rather than overwriting it with garbage).
				*/
				const numbers = elements.map(v => this.numberOfTrackedValue(v));
				if (numbers.every(v => v !== null && Number.isFinite(v))) {
					symbol.arrayInitialValues = numbers;
				}
				if (!symbol.arrayHasRuntimeWrite) {
					/*
					Nothing writes to it any more, so it can live in the cheapest
					constant representation instead of in memory.
					*/
					if (!symbol.requestedArrayImplementation) {
						symbol.requestedArrayImplementation = 'encoded';
					}
					symbol.isConst = true;
				}
			});
			scope.children?.forEach(visit);
		};
		visit(this.globalScope);
	}

	/**
	 * Forget the elements of an array: it has been written (or its address has
	 * been taken), so nothing about its contents is reliable any more.
	 * @param {SymbolEntry} symbol
	 * @returns {void}
	 */
	invalidateConstantArray(symbol) {
		if (!symbol) return;
		this.constantArrays = this.constantArrays ?? new Map();
		this.constantArrays.delete(this.constantArrayKey(symbol));
	}

	/**
	 * Whether an array symbol may be folded at compile time at all.
	 *
	 * Excluded are the arrays whose contents somebody else may change behind our
	 * back:
	 *  - `volatile` and `extern` are read and written by OTHER PROCESSORS, so
	 *    not even a single read can be folded -- `extern` says so explicitly;
	 *  - an array whose address is taken can be reached through a pointer we do
	 *    not track.
	 *
	 * A global array is allowed: it is only visible to this program. What makes
	 * it delicate is that any call may write it, so every call site throws the
	 * known contents away (see `analyzeFunctionCall`).
	 *
	 * @param {SymbolEntry} symbol
	 * @returns {boolean}
	 */
	isFoldableArray(symbol) {
		if (!symbol) return false;
		const type = symbol.myType?.() ?? null;
		if (!type || type.kind !== 'array') return false;
		/*
		A remote target is written by the OTHER processor, which this program
		cannot see -- so its elements are never known constants.
		*/
		if (symbol.isVolatile || symbol.isExtern || symbol.isRemoteTarget) return false;
		if (symbol.isAddressed) return false;
		/*
		Passed to a function: the callee may write into it at any time, so its
		contents are not known constants any more.
		*/
		if (symbol.arrayEscapes === true) return false;
		return true;
	}

	/**
	 * Normalise the third argument of `addWrittenSymbol` into an evaluation
	 * state, accepting the legacy booleans.
	 *
	 * @param {string|boolean?} state
	 * @returns {string}
	 */
	resolveWriteState(state) {
		const state_ = Optimizer;
		if (state === true) return state_.EVAL_DEFINITE;
		if (state === false) return state_.EVAL_UNKNOWN;
		if (state === null || state === undefined) return state_.EVAL_TRACKED;
		return state;
	}

	addWriteForCurrentScope(varName, noSideEffectValue, doUpdate = true) {
		const varSymbol = this.currentScope.lookup(varName);
		this.addWrittenSymbol(varSymbol, noSideEffectValue, doUpdate);
	}

    // =============== 作用域优化 ===============

	// New feature added in 2. conv
	/**
	 * @param {Scope} scope 
	 * @bug Too many loops!
	 */
    optimizeScope(scope) {
        const scopePath = scope.getPath();
        const variableUses = this.variableUses.get(scopePath) || new Map();
        const localConstants = this.localConstants.get(scopePath) || new Map();

        // 1. 删除无用变量
        //this.removeUnusedVariablesInScope(scope, variableUses);
        
        // 2. 常量传播 -- Done in normal analyses now!
        this.constantPropagationInScope(scope, localConstants);
        
        // 3. 控制流优化（使用分析阶段收集的信息）-- Already done!
		//this.optimizeControlFlowInScope(scope);	// Not implemented
		
		// 4. 死代码消除 (Experimental)
		if (this.extraConfig.getAttribute('deleteDeadCode')) {
			this.deadCodeEliminationInScope(scope);
		}

		scope.children.forEach(child => this.optimizeScope(child));
    }

	/**
	 * 
	 * @param {string | SymbolEntry} varName 
	 */
	variableCanBeRemoved(varName) {
		let symbol = varName;
		if (typeof varName === 'string') {
			symbol = this.currentScope.lookup(varName);
		} else {
			varName = symbol.name;
		}
		if ((!symbol) || (!symbol.scope) || (symbol.myType().kind === 'function')) {
			return false;
		}
		const varState = this.variableUses.get(symbol.scope.getPath()).get(varName);
		const varOk = varState && (varState.reads == 0 && varState.canOptimize);
		/*
		A remote target is only mentioned by `remote(...)`, which is consumed in
		the semantic pass -- so it looks unused here, while the other processor
		does read it.
		*/
		/*
		A name-based struct pointer looks unused: the members are reached as
		`p.m` registers, so nothing ever reads `p` itself. Removing the
		declaration would drop the instructions that STORE those names, leaving
		every member pointing at an undefined register.
		*/
		if (symbol.addressedByName === true) return false;
		/*
		A `remote` variable is an import of another processor's state: writing it
		IS the side effect (the other processor sees it), even when this program
		never reads it back. Deleting the write would delete the message.
		*/
		if (symbol.isRemote) return false;
		return  (!symbol.isVolatile) && (!symbol.isExtern) && (!symbol.isStatic)
			&& (!symbol.isAddressed) && (!symbol.isRemoteTarget) && varOk;
	}

	/**
	 * 
	 * @param {Scope} scope 
	 * @param {Object} variableUses 
	 * @bug Maybe unused
	 */
    removeUnusedVariablesInScope(scope, variableUses) {
        const symbols = scope.getAllSymbols();
        
        symbols.forEach(symbol => {
            if (symbol.kind !== 'variable') return;
			if (symbol.isPointer()) return;
			if (!this.variableCanBeRemoved(symbol)) return;
            
            const usage = variableUses.get(symbol.name);
            if (!usage) return;
            
            // 检查变量是否被使用
            const isUsed = usage.reads > 0 || usage.writes > 1;	// Initialization is not considered!!!
            const hasSideEffects = this.hasSideEffects(usage.node);
            
            if (!isUsed && !hasSideEffects) {
                // 标记为可删除
                symbol.canBeEliminated = true;
                symbol.isDead = true;
                
                // 从AST中删除变量声明
                const astNode = usage.node;
                if (astNode && astNode.parent) {
                    this.removeNodeFromParent(astNode);
                    this.modified = true;
                    this.logOptimization(`Removed unused variable '${symbol.name}' in scope ${scope.getPath()}`);
                }
            }
        });
    }

	/**
	 * 
	 * @param {Scope} scope 
	 * @param {Map<string, any>} localConstants 
	 */
    constantPropagationInScope(scope, localConstants) {
        if (!scope.astNode) return;
        this.currentScope = scope;
        // 遍历作用域内的所有节点，传播常量
        this.traverseAndReplaceConstants(scope.astNode, localConstants);
    }

	/**
	 * 
	 * @param {number | boolean | string | extBuiltinConstant} value 
	 * @returns 
	 */
	getLiteral(value) {
		if (typeof value === 'number' || typeof value === 'boolean') {
			const result = ASTBuilder.numericLiteral(value);
			result.dataType = this.typeTable.get('int');
			return result;
		} else if (typeof value === 'string') {
			const result = ASTBuilder.stringLiteral(value);
			result.dataType = this.typeTable.get('char');
			return result;
		} else if (value == null) {
			// Rarely used
			console.log("Note: getting null literal");
			const result = ASTBuilder.nullLiteral();
			result.dataType = this.typeTable.get('null_t');
			return result;
		} else if (typeof value === 'object') {
			if (value.isBuiltinConstant) {
				const result = ASTBuilder.identifier(value.name);
				result.dataType = this.typeTable.get('content_t');
				return result;
			} else {
				return null;
			}
		} else {
			return null;
		}
	}

	/**
	 * Determine whether the node is literal.
	 * @param {ASTNode} node
	 * @returns {boolean} 
	 */
	isLiteralNode(node) {
		const literalTypes = ['NumericLiteral', 'StringLiteral', 'NullLiteral'];
		if (literalTypes.includes(node.type)) return true;
		if (node.type === 'Identifier' && node.name[0] === '@' && !this.semantic.specialTypes[node.name]) return true;
		return false;
	}

	/**
	 * @param {IdentifierNode} node
	 * @returns {boolean}
	 */
	isLeftValueIdentifier(node) {
		if (node.parent) {
			if (node.parent.type === 'AssignmentExpression' && node.parent.left === node) return true;
			if (node.parent.type === 'MemberExpression' && node.parent.children[0] === node) return true;
			if (node.parent.type === 'VariableDeclarator' || node.parent.type === 'Declarator') {
				if (node !== node.parent.initializer) return true;
			}
			if (node.parent.type === 'UnaryExpression' && node.parent.operator === '&') return true;
		}
		return false;
	}

	/**
	 * 
	 * @param {ASTNode} node 
	 */
	replaceAliasAt(node) {
		switch (node.type) {
			case 'Identifier':
				if (this.isLeftValueIdentifier(node)) break;
				const symbol = this.currentScope.lookup(node.name);
				if (symbol && symbol.variableReferrer.parent) {
					const target = symbol.variableReferrer.getName();
					console.log(`Replaced identifier for shrunk expression as:`,target,`at:`,node);
					node.setAttribute('originalName', node.name);
					node.setAttribute('originalSymbol', symbol);
					// node.setAttribute('performedReplacement', target);
					node.name = target;
					node.symbol = symbol.variableReferrer.parent.entry ?? node.symbol;
					// node.setAttribute('candidateRemoval', 2);
					// symbol.variableReferrer.registerCandidate(node);
					this.modified = true;				// This creates only candidate removal
					return true;
				}
		}
		return false;
	}

	/**
	 * 
	 * @param {ASTNode} node 
	 * @param {Map<string, any>} constants 
	 * @param {Set<SymbolEntry> | "strict"} restrictions
	 * Currently not tracking whether the node self is deleted.
	 */
	replaceConstantsAt(node, constants, restrictions = null) {
		if (!node) return false;
        // Maybe not numeric?
		let rightConstValue, modified = false;
        switch (node.type) {
            case 'Identifier':
				if (this.isLeftValueIdentifier(node)) break;
				/**
				 * @type {SymbolEntry?}
				 */
				const symbol = node.symbol;
				if (symbol && (!(symbol.isAddressed || symbol.isExtern || symbol.isVolatile))
						&& ((!( symbol.isGlobal || (restrictions && (restrictions === 'strict' || restrictions.has(symbol))) ))
						 || (symbol.isConst && symbol.type !== 'parameter'))) {
					const constValue = constants.get(node.name);
					if (constValue !== undefined) {
						// 替换为常量值
						console.log(`Prepare to replaced identifier as:`,constValue,`at:`,node);
						const replacement = this.getLiteral(constValue);
						replacement.location = node.location;
						replacement.dataType = node.dataType;
						this.replaceNode(node, replacement);
						modified = true;
						break;
					}
				}
                // Still possible to match aliases
				modified = modified || this.replaceAliasAt(node);
                break;
                
			case 'MemberExpression': {
				/*
				`a[3]` where every element of `a` is known becomes a literal --
				the array version of constant folding. Never on the left of an
				assignment: there the subscript is a destination, not a value.
				*/
				if (!node.getAttribute || !node.getAttribute('computed')) break;
				const owner = node.parent ?? null;
				const isTarget = owner && (
					(owner.type === 'AssignmentExpression' && owner.left === node) ||
					(owner.type === 'BinaryExpression' && owner.operator.includes('=')
						&& !['==', '!=', '>=', '<=', '>', '<'].includes(owner.operator) && owner.left === node));
				if (!isTarget) {
					const elementValue = this.evaluateConstantExpression(node);
					if (elementValue !== undefined) {
						const replacement = this.getLiteral(elementValue);
						replacement.location = node.location;
						replacement.dataType = node.dataType;
						this.replaceNode(node, replacement);
						modified = true;
						break;
					}
				}
				// The subscript itself may still contain constants
				const subscriptNode = node.children ? node.children[1] : null;
				if (subscriptNode) modified = this.replaceConstantsAt(subscriptNode, constants, restrictions) || modified;
				break;
			}
            case 'BinaryExpression':
                // 常量折叠
                const leftValue = this.evaluateConstantExpression(node.left);
                const rightValue = this.evaluateConstantExpression(node.right);
				const isAssignment = node.operator.includes('=') && node.operator !== '==' && node.operator !== '!=' && node.operator !== '>=' && node.operator !== '<=';
                /*
				if (isAssignment) {
					if (node.left.type === 'Identifier' && this.variableCanBeRemoved(node.left.name) && leftValue !== undefined) {
						console.log(`Removing assignment instruction:`,node);
						this.replaceNode(node, null);
						break;
					}
				}
				*/
                if ((isAssignment || leftValue !== undefined) && rightValue !== undefined) {
                    const result = this.evaluateBinaryOperation(node.operator, leftValue, rightValue, node.dataType);
                    if (result !== undefined) {
                        const replacement = this.getLiteral(result);
                        replacement.location = node.location;
						replacement.dataType = node.dataType;
                        this.replaceNode(node, replacement);
                        modified = true;
                    }
                }

				if (!this.isLiteralNode(node.left) && leftValue != undefined) {
					console.log(`Replacing binary expression left`,node.left,`with value`,leftValue);
					this.replaceNode(node.left, this.getLiteral(leftValue));
				}
				if (!this.isLiteralNode(node.right) && rightValue != undefined) {
					console.log(`Replacing binary expression right`,node.right,`with value`,rightValue);
					this.replaceNode(node.right, this.getLiteral(rightValue));
				}
                
                // 代数简化
                this.simplifyBinaryExpression(node, leftValue, rightValue);
                
                break;
                
            case 'UnaryExpression':
                const argValue = this.evaluateConstantExpression(node.argument);
                if (!this.isLiteralNode(node.argument) && argValue !== undefined) {
                    const result = this.evaluateUnaryOperation(node.operator, argValue);
                    if (result !== undefined) {
						console.log(`Replacing unary expression`,node,`with value`,result);
                        const replacement = this.getLiteral(result);
                        replacement.location = node.location;
						replacement.dataType = node.dataType;
                        this.replaceNode(node, replacement);
                        modified = true;
                    }
                }
                break;
                
            case 'IfStatement':
                const testValue = this.evaluateConstantExpression(node.test);
                if (testValue !== undefined) {
                    if (testValue) {
                        // 条件为真，只保留then分支
                        this.replaceNode(node, node.consequent);
						console.log(`Optimizing always-true if statement`);
                    } else {
                        // 条件为假，只保留else分支（如果有）
                        this.replaceNode(node, node.alternate || null);
						console.log(`Optimizing always-false if statement`);
                    }
                    modified = true;
                }
                break;
                
            case 'WhileStatement':
                const whileTestValue = this.evaluateConstantExpression(node.test);
                if (whileTestValue !== undefined && !whileTestValue) {
                    // 条件为假的while循环，直接删除
                    this.replaceNode(node, null);
                    modified = true;
					break;
                }
                break;
			
			case 'AssignmentExpression':
				// It is possible that assign value is given a constant
				rightConstValue = this.evaluateConstantExpression(node.right);
				if (!this.isLiteralNode(node.right) && rightConstValue != null) {
					// This is a constant, add it into constant set
					console.log(`Optimzing assignment expression right`,node.right);
					this.replaceNode(node.right, this.getLiteral(rightConstValue));
				} else {
					// Consider removal
					// this.replaceConstantsAt(node.right, constants);
					return false;
				}
				break;
			
			case 'VariableDeclarator':
			case 'Declarator':
				if (node.initializer) {
					rightConstValue = this.evaluateConstantExpression(node.initializer);
					if (!this.isLiteralNode(node.initializer) && rightConstValue != null
					&& !(typeof rightConstValue === 'object' && rightConstValue.isBuiltinConstant && rightConstValue.category === 'unknown')) {
						// This is a constant, add it into constant set
						console.log(`Optimzing assignment expression right`,node.initializer);
						this.replaceNode(node.initializer, this.getLiteral(rightConstValue));
					} else {
						// Consider removal
						// this.traverseAndReplaceConstants(node.initializer, constants);
						return false;
					}
				} else {
					return false;
				}
				
				break;

			case 'CompoundStatement':
				// Too stupid:
				break;
				// node.statements.forEach(stmt => this.traverseAndReplaceConstants(stmt, constants));
			default:
				break;
		}
		if (modified) {
			this.modified = true;
			return true;
		}
		return false;
	}

	/**
	 * 
	 * @param {ASTNode} node 
	 * @param {boolean} [currentOnly=false] 
	 * @param {Set<variableModification>?} [restriction] 
	 */
	replaceConstantAtPresent(node, currentOnly = false, restriction = new Set()) {
		let constants = new Map();
		let scope = this.currentScope;
		while (scope) {
			const scopePath = scope.getPath();
			scope.getAllSymbols().forEach(symb => {
				if (symb.isConstant && !constants.has(symb.name)) {
					constants.set(symb.name, symb.constantValue);
				}
			});
			// if (currentOnly) break;
			scope = scope.parent;
		}
		/**
		 * @type {Set<SymbolEntry?>}
		 */
		const restrictive = new Set([...restriction].map(val => val.symbol));
		/**
		 * @type {Set<SymbolEntry?> | 'strict' | null}
		 */
		this.restrictiveVariables = currentOnly ? 'strict' : restrictive;
		return this.replaceConstantsAt(node, constants, this.restrictiveVariables);
	}

	/**
	 * 
	 * @param {ASTNode} node 
	 * @param {Map<string, any>} constants 
	 * No bug because only inside current scope (but time complexity might be a problem)
	 */
    traverseAndReplaceConstants(node, constants) {
        
		if (!node) return;
		let replaced = false;

		/**
		 * 
		 * @param {ASTNode} nodeTarget 
		 * @param {ASTNode} sideTarget 
		 */
		const tryReplacing = (nodeTarget, sideTarget) => {
			if (sideTarget && this.hasSideEffects(sideTarget)) {
				const expr = new ASTNode('ExpressionStatement', sideTarget.location);
				expr.addChild(sideTarget);
				sideTarget.parent = expr;
				expr.parent = nodeTarget.parent;
				this.replaceNode(nodeTarget, expr);
				console.log(`[ScopeOp Stage] reserving side effect expression`,expr);
			} else {
				this.replaceNode(nodeTarget, null);
			}
		};

		switch (node.type) {
			case 'VariableDeclarator':
			case 'Declarator':
				if (this.variableCanBeRemoved(node.name)) {
					console.log(`[ScopeOp Stage] Remove unused variable declarator:`,node);
					tryReplacing(node, node.initializer);
					replaced = true;
				}
				break;
			case 'AssignmentExpression':
				if (node.left.type === 'Identifier' && this.variableCanBeRemoved(node.left.name)) {
					console.log(`[ScopeOp Stage] Remove unused assignment expression:`,node);
					tryReplacing(node, node.right);
					replaced = true;
				}
				break;
			case 'UnaryExpression':
				if (node.argument.type === 'Identifier' && constants.has(node.argument.name) && this.variableCanBeRemoved(node.argument.name)) {
					console.log(`[ScopeOp Stage] Remove unused assignment expression:`,node);
					this.replaceNode(node, null);
					replaced = true;
				}
				break;
		}
		if (replaced) return;
		// this.replaceConstantsAt(node, constants);
		this.further(node, ast => this.traverseAndReplaceConstants(ast, constants));
    }

    deadCodeEliminationInScope(scope) {
		if (!scope.astNode) return;
		
		// 移除死代码
		if (this.removeDeadCode(scope.astNode)) {
			this.modified = true;
		}
		
		// 清理空的复合语句
		this.cleanupEmptyStatements(scope.astNode);
	}

    // =============== 全局优化 ===============

    collectGlobalInfo() {
        this.functionCallGraph.clear();
        this.functionInfo.clear();
        
        // 从全局作用域开始收集函数信息
        this.collectFunctionInfoFromScope(this.globalScope);
    }

    collectFunctionInfoFromScope(scope) {
        // 收集当前作用域的函数
        const symbols = scope.getAllSymbols();
        symbols.forEach(symbol => {
            if (symbol.kind === 'function' && symbol.scope.astNode) {
                const funcNode = symbol.owningScope ? symbol.owningScope.astNode : symbol.scope.astNode;
                this.functionInfo.set(symbol.name, {
                    calls: 0,
                    hasSideEffects: false,
                    isDefinition: funcNode.body !== null,
                    isInline: funcNode.isInline || false,
					isAddressed: symbol.isAddressed,
					canInline: funcNode.body !== null,
                    node: funcNode,
                    scope: symbol.scope,
					writtenVarsUnknown: true,
					writtenVars: [],
					...this.estimateFunctionSize(funcNode)
                });	
            }
        });
        
        // 递归收集子作用域
        scope.children.forEach(childScope => {
            this.collectFunctionInfoFromScope(childScope);
        });
    }

	// ! Has manual changes !
    removeUnusedFunctions() {
        const usedFunctions = new Set(['main']);
        /*
        Nothing recorded a single call: the bodies have not been looked at yet.
        Going on would delete every function but main and leave the call sites
        dangling, so wait for a round that knows something.
        */
        if (this.functionCallGraph.size === 0) return;
        
        // 从main函数开始标记使用的函数
        const markUsed = (funcName) => {
            if (usedFunctions.has(funcName)) return;
            
            usedFunctions.add(funcName);
            const calledFunctions = this.functionCallGraph.get(funcName) || new Set();
            for (const called of calledFunctions) {
                markUsed(called);
            }
        };
        
        // 标记所有被调用的函数
        this.functionCallGraph.forEach((called, caller) => {
            if (usedFunctions.has(caller)) {
                called.forEach(callee => markUsed(callee));
            }
        });

		this.functionInfo.forEach((fn, name) => {
			if (fn.isAddressed) markUsed(name);
		})
        
        // 删除未使用的函数定义
		// Manually use owningScope instead of scope
        const globalSymbols = this.globalScope.getAllSymbols();
		// Any change in this loop won't affect globalSymbols
        globalSymbols.forEach(symbol => {
            if (symbol.kind === 'function' && symbol.scope.astNode) {
                const funcName = symbol.name;
				if (symbol.owningScope) {
					const funcNode = symbol.owningScope.astNode;
                
					// 保留main函数、内联函数和被使用的函数
					if (funcName !== 'main' && 
						!funcNode.isInline && 
						!usedFunctions.has(funcName) &&
						funcNode.body) {
						// For functions we use 'body (=== null)' instead of 'isDefinition'
						
						// Never drop a stack handler: the walker calls it
						if (this.protectedHandlerNames().has(funcName)) {
							return;
						}
						// Manually added: also remove symbols
						this.globalScope.removeSymbol(funcName);
						
						// 从AST中删除函数定义
						this.removeNodeFromParent(funcNode);
						this.modified = true;
						this.logOptimization(`Removed unused function '${funcName}'`);
						
					}
				}
                
            }
        });
    }

	/**
	 * Whether this expression (a loop condition or a for-update) contains a
	 * function call.
	 *
	 * @param {ASTNode?} node
	 * @returns {boolean}
	 */
	controlExpressionHasCall(node) {
		if (!node) return false;
		let found = false;
		const walk = n => {
			if (!n || typeof n !== 'object' || found) return;
			if (Array.isArray(n)) { n.forEach(walk); return; }
			if (n.type === 'FunctionCall') { found = true; return; }
			for (const key of ['children', 'test', 'update', 'argument', 'left', 'right',
				'expression', 'arguments', 'callee', 'consequent', 'alternate']) {
				const c = n[key];
				if (Array.isArray(c)) c.forEach(walk);
				else if (c && typeof c === 'object') walk(c);
			}
		};
		walk(node);
		return found;
	}

	/**
	 * Call nodes that sit in a loop's controlling position: the `test` of a
	 * while/for, or the `update` of a for.
	 *
	 * These decide how often the body runs. The loop simulator (`simulateLoop`)
	 * evaluates the condition to count iterations, but it does NOT re-run
	 * whatever the condition does -- so if a call with side effects were
	 * inlined there, the compiled loop would run it once per iteration while
	 * the simulation counted as if it never ran. Inlining at these positions is
	 * therefore simply not done.
	 *
	 * @returns {Set<ASTNode>}
	 */
	loopControlCallSites() {
		if (this._loopControlCallSites) return this._loopControlCallSites;
		/** @type {Set<ASTNode>} */
		const found = new Set();
		const isLoop = node => ['WhileStatement', 'DoWhileStatement', 'ForStatement']
			.includes(node?.type);
		const walk = (node, root) => {
			if (!node || typeof node !== 'object') return;
			if (Array.isArray(node)) { node.forEach(n => walk(n, root)); return; }
			if (isLoop(node)) {
				for (const field of ['test', 'update']) walk(node[field], node);
				walk(node.body, null);
				walk(node.init, null);
				return;
			}
			if (root && node.type === 'FunctionCall') {
				found.add(node);
				root.setAttribute('controlHasCall', true);
			}
			for (const key of ['children', 'statements', 'body', 'consequent', 'alternate',
				'test', 'init', 'update', 'expression', 'argument', 'left', 'right',
				'initializer', 'declarators', 'arguments', 'callee']) {
				const c = node[key];
				if (Array.isArray(c)) c.forEach(x => walk(x, root));
				else if (c && typeof c === 'object') walk(c, root);
			}
		};
		walk(this.ast, null);
		this._loopControlCallSites = found;
		return found;
	}

	/**
	 * Whether this call sits in a loop's controlling position, where inlining
	 * would break the loop computation.
	 *
	 * @param {ASTNode} callNode
	 * @returns {boolean}
	 */
	isLoopControlCall(callNode) {
		return this.loopControlCallSites().has(callNode);
	}

	/**
	 * `while (true) { ... }` -- a loop without a condition that names a
	 * variable. There is nothing to compute an iteration count from, so the
	 * body is simply simulated until it either hits a `break` or runs out of
	 * budget.
	 *
	 * Only attempted when the body is free of side effects, which the caller
	 * has already checked.
	 *
	 * @param {ASTNode} whileNode
	 * @param {any} info
	 * @returns {boolean}
	 */
	analyzeEndlessLoopForOptimization(whileNode, info) {
		const test = whileNode.test;
		// Only for a condition that is plainly true (or absent)
		if (test) {
			/*
			`true` may arrive as a keyword-like identifier, as `1`, or as
			something the constant folder already knows.
			*/
			const plainlyTrue = (test.type === 'Identifier' && test.name === 'true')
				|| (test.type === 'NumericLiteral' && test.value !== 0 && test.value !== null);
			const value = this.evaluateConstantExpression(test);
			const foldedTrue = (value !== undefined && value !== null && value !== 0 && value !== false);
			if (!plainlyTrue && !foldedTrue) return false;
		}
		const modifiedVars = this.collectModifiedVariablesInLoop(whileNode);
		// Pick any variable the body writes and whose value is known: it stands
		// in for the (missing) loop variable.
		let loopVarName = null, initValue = undefined;
		for (const varInfo of modifiedVars) {
			if (varInfo === 'unrecognized') return false;
			const scopePath = (varInfo.scope ?? this.currentScope.lookupScopeOf(varInfo.varName)).getPath();
			const value = this.localConstants.get(scopePath)?.get(varInfo.varName);
			if (value !== undefined) {
				loopVarName = varInfo.varName;
				initValue = value;
				break;
			}
		}
		if (!loopVarName) return false;

		const MAX_OPTIMIZE_ITERATIONS = 250;
		return this.runWhileSimulation(whileNode, loopVarName, initValue,
			{ comparisonOp: null, constValue: undefined },
			{ type: 'none', step: 0 }, MAX_OPTIMIZE_ITERATIONS, info);
	}

	/**
	 * Simulate a `while` loop and decide whether the result may be used.
	 *
	 * A `while` loop is flexible enough that a simulation which did NOT reach
	 * the end tells us nothing: the remaining rounds are exactly the ones we
	 * could not run. So when the simulation runs out of budget without the
	 * condition turning false, nothing is kept -- the loop is left as it was
	 * and the constants planted by the simulation are rolled back.
	 *
	 * @returns {boolean}
	 */
	runWhileSimulation(whileNode, loopVarName, initValue, conditionInfo, updateInfo,
		iterationCount, info) {
		whileNode.setAttribute('iterationCount', iterationCount);
		whileNode.setAttribute('iteration', {
			node: whileNode,
			type: 'while',
			varName: loopVarName,
			varInit: initValue,
			condition: conditionInfo,
			update: updateInfo,
			count: iterationCount,
			info: info
		});
		whileNode.setAttribute('canOptimize', true);
		if (info && info.noRunning) return true;
		if (!this.simulateLoop(whileNode, loopVarName, initValue, conditionInfo, updateInfo,
			iterationCount, info)) {
			whileNode.setAttribute('optimized', false);
			return false;
		}
		if (whileNode.getAttribute('simulationComplete') === false) {
			// Ran out of budget: keep the loop, forget the imagined values
			whileNode.setAttribute('optimized', false);
			whileNode.setAttribute('canOptimize', false);
			this.restoreLoopConstants(whileNode);
			return false;
		}
		whileNode.setAttribute('optimized', true);
		return true;
	}

	/**
	 * Rewrite every `for` loop into the `while` loop it means:
	 *
	 *     for (init; test; update) body
	 *   -->
	 *     init; while (test) { body; update; }
	 *
	 * Done once, before any analysis, so that the whole optimizer, the loop
	 * simulator and the generator only ever see one kind of loop. It also gets
	 * rid of the separate `for` code paths (whose `update` sits outside the
	 * body and is therefore easy to forget when the loop is replaced).
	 *
	 * A body containing `continue` is left alone: in a `for`, `continue` jumps
	 * to `update`, in a `while` it jumps to the test -- and there is no `goto`
	 * to repair that with.
	 */
	convertForLoopsToWhile() {
		if (this._forLoopsConverted) return;
		this._forLoopsConverted = true;
		let converted = 0;

		const containsContinue = (node, stopAt) => {
			let found = false;
			const walk = n => {
				if (!n || typeof n !== 'object' || found) return;
				if (Array.isArray(n)) { n.forEach(walk); return; }
				if (n.type === 'ContinueStatement') { found = true; return; }
				// A nested loop keeps its own `continue`
				if (n !== node && ['WhileStatement', 'ForStatement', 'DoWhileStatement'].includes(n.type)) return;
				for (const key of ['children', 'statements', 'body', 'consequent', 'alternate',
					'test', 'update', 'init', 'expression', 'argument', 'left', 'right']) {
					const c = n[key];
					if (Array.isArray(c)) c.forEach(walk);
					else if (c && typeof c === 'object') walk(c);
				}
			};
			walk(node);
			return found;
		};

		const scopeOf = node => node?.scope ?? node?.parent?.scope ?? this.globalScope;

		const walkStatements = node => {
			if (!node || typeof node !== 'object') return;
			if (Array.isArray(node)) { node.forEach(walkStatements); return; }

			// Depth first, so nested `for` loops are converted from the inside out
			for (const key of ['statements', 'body', 'consequent', 'alternate', 'functions']) {
				const c = node[key];
				if (Array.isArray(c)) c.forEach(walkStatements);
				else if (c && typeof c === 'object') walkStatements(c);
			}

			if (!Array.isArray(node.statements)) return;
			for (let i = 0; i < node.statements.length; i++) {
				const stmt = node.statements[i];
				if (!stmt || stmt.type !== 'ForStatement') continue;
				if (containsContinue(stmt.body ?? stmt)) {
					this.logOptimization(`Left a 'for' loop alone because its body uses 'continue'`);
					continue;
				}
				this.replaceForWithWhile(node, i, stmt, scopeOf);
				converted++;
			}
		};

		walkStatements(this.ast);
		// New scopes were introduced: make sure they get their constant tables
		if (converted > 0) this.modified = true;
	}

	/**
	 * Replace `statements[index]` (a ForStatement) with the equivalent
	 * `while` form.
	 *
	 * @param {ASTNode} holder the compound statement holding the loop
	 * @param {number} index
	 * @param {ASTNode} forNode
	 * @param {(node: ASTNode) => Scope} scopeOf
	 */
	replaceForWithWhile(holder, index, forNode, scopeOf) {
		const outerScope = scopeOf(forNode);
		const update = forNode.update;
		const body = forNode.body;

		// `body; update;` -- keep the body's own compound when there is one, so
		// that its scope (and any symbols in it) survive untouched.
		let newBody = body;
		if (body && body.type === 'CompoundStatement') {
			if (update) {
				const updStmt = new ASTNode('ExpressionStatement');
				updStmt.addChild(update);
				update.parent = updStmt;
				updStmt.parent = body;
				body.statements.push(updStmt);
			}
		} else {
			const compound = ASTBuilder.compoundStatement();
			if (body) { compound.statements.push(body); body.parent = compound; }
			if (update) {
				const updStmt = new ASTNode('ExpressionStatement');
				updStmt.addChild(update);
				update.parent = updStmt;
				updStmt.parent = compound;
				compound.statements.push(updStmt);
			}
			const bodyScope = new Scope(outerScope, 'l', compound, 'forbody');
			compound.scope = bodyScope;
			outerScope.children.push(bodyScope);
			newBody = compound;
		}

		/*
		`for (;;)` has no test at all, which is an INFINITE loop. The `while`
		it becomes must say so explicitly: a missing test is not the same as
		`true` to the code generator, which asks for the condition's type and
		crashes on a null node.
		*/
		const whileTest = forNode.test ?? ASTBuilder.numericLiteral(true);
		const whileNode = new WhileStatementNode(whileTest, newBody);
		whileNode.parent = holder;
		// A statement's scope is the one holding it (the `for` had the same)
		whileNode.scope = outerScope;
		if (newBody) newBody.parent = whileNode;
		whileTest.parent = whileNode;

		let replacement = whileNode;
		if (forNode.init) {
			const compound = ASTBuilder.compoundStatement();
			const initStmt = forNode.init.type === 'ExpressionStatement'
				? forNode.init
				: (() => {
					const st = new ASTNode('ExpressionStatement');
					st.addChild(forNode.init);
					forNode.init.parent = st;
					return st;
				})();
			initStmt.parent = compound;
			compound.statements.push(initStmt);
			whileNode.parent = compound;
			compound.statements.push(whileNode);
			const scope = new Scope(outerScope, 'l', compound, 'forinit');
			compound.scope = scope;
			outerScope.children.push(scope);
			compound.parent = holder;
			replacement = compound;
		}

		holder.statements[index] = replacement;
		this.modified = true;
	}

	/**
	 * Register that a function must not be inlined automatically.
	 *
	 * @param {string?} name null means "every function"
	 * @param {boolean} [soft=true] true when the request comes from
	 *        `#pragma noinline`, which the `inline` keyword outranks
	 */
	forbidAutoInline(name = null, soft = true) {
		const state = this.autoInlineForbidden;
		if (name === null) {
			if (soft) state.all = true;
			else state.allHard = true;
			return;
		}
		(soft ? state.soft : state.hard).add(name);
	}

	/**
	 * Whether automatic inlining of this function is allowed AT ALL.
	 *
	 * This is the "has anybody forbidden it" question. It says nothing about
	 * whether inlining is possible or worthwhile.
	 *
	 * @param {string} name
	 * @param {boolean} [declaredInline=false] whether `inline` was written
	 * @returns {boolean}
	 */
	autoInlineAllowed(name, declaredInline = false) {
		const state = this.autoInlineForbidden;
		if (state.allHard || state.hard.has(name)) return false;
		// An explicit `inline` outranks the pragma, but never a hard veto
		if (declaredInline) return true;
		if (state.all || state.soft.has(name)) return false;
		return true;
	}

	/**
	 * Apply the `#pragma noinline` requests of this program.
	 */
	applyNoInlinePragmas() {
		const config = this.optimizeConfig ?? {};
		if (config.noInlineAll === true) this.forbidAutoInline(null, true);
		for (const name of config.noInlineList ?? []) this.forbidAutoInline(name, true);
		/*
		The stack walkers jump to these with `set @counter`, so they must stay
		real functions: inlining them away would leave the jump with no target.
		*/
		for (const key of ['stackCheck', 'stackUnderflow']) {
			const handler = config[key];
			if (typeof handler === 'string' && handler) this.forbidAutoInline(handler, false);
		}
	}

	/**
	 * Whether inlining this function is POSSIBLE. Structure only: nothing here
	 * depends on whether it would be a good idea, and nothing depends on side
	 * effects -- moving a write from the function body to the call site is
	 * exactly what inlining means.
	 *
	 * @param {string} funcName
	 * @returns {boolean}
	 */
	canInlineFunction(funcName) {
		const info = this.functionInfo.get(funcName);
		if (!info) return false;
		if (funcName === 'main') return false;
		if (!info.node || !info.node.body) return false;
		// Reachable through a function pointer: the call sites are not all known
		if (info.isAddressed) return false;
		if (this.recuriveInfo?.has(funcName)) return false;
		// The body shape: no `return`, or a single one at the very end.
		// (No goto, so a `return` in the middle cannot be reproduced.)
		if (!info.canInline) return false;
		return true;
	}

	/**
	 * Whether the function is STRICTLY free of side effects, meaning a call to
	 * it may be deleted when the result is unused.
	 *
	 * Deliberately a separate question from `canInlineFunction`: a function that
	 * writes to a variable can be inlined perfectly well, it just cannot be
	 * thrown away.
	 *
	 * @param {string} funcName
	 * @returns {boolean}
	 */
	isStrictlyPureFunction(funcName) {
		const info = this.functionInfo.get(funcName);
		if (!info || !info.node || !info.node.body) return false;
		if (info.strictlyPure !== undefined) return info.strictlyPure;
		info.strictlyPure = !this.hasSideEffects(info.node.body, {
			inFunction: true,
			atFunction: info.node
		});
		return info.strictlyPure;
	}

	/**
	 * Whether inlining is worth the code it duplicates.
	 * @param {string} funcName
	 * @returns {boolean}
	 */
	worthInlining(funcName) {
		const info = this.functionInfo.get(funcName);
		if (!info || !info.node) return false;
		const paraSize = info.node.parameters ? (info.node.parameters.length ?? 0) : 0;
		/*
		One call site, or an explicit `inline`: do it. Otherwise compare the size
		of N copies of the body against keeping the function plus N calls.
		*/
		const inlineEstimate = (info.calls + paraSize) * info.size;
		const noInlineEstimate = (info.size + paraSize + 4 + (4 + paraSize) * info.calls);
		if (!(info.calls === 1 || info.isInline || inlineEstimate <= noInlineEstimate)) {
			return false;
		}
		const maxSize = this.compiler?.config?.getAttribute('maxInlineSize') ?? 25;
		const maxDepth = this.compiler?.config?.getAttribute('maxInlineDepth') ?? 5;
		if (info.size > maxSize && !info.isInline) return false;
		if (info.maxDepth > maxDepth) return false;
		return true;
	}

	// Has manual changes on 'isAddressed'
    inlineFunctions() {
		this.recuriveInfo = this.findRecursiveFunctions(this.functionCallGraph);
		this.applyNoInlinePragmas();
		// 收集适合内联的函数
		const inlineCandidates = [];

		this.functionInfo.forEach((info, funcName) => {
			if (!this.canInlineFunction(funcName)) return;
			if (!this.autoInlineAllowed(funcName, info.isInline === true)) return;
			if (!this.worthInlining(funcName)) return;
			inlineCandidates.push(funcName);
		});

		// 尝试内联候选函数
		inlineCandidates.forEach(funcName => {
			if (this.shouldInlineFunction(funcName)) {
				if (this.performFunctionInlining(funcName)) {
					// The AST moved: recompute which calls sit where
					this._loopControlCallSites = null;
					/*
					The body now sits in another function, under a different
					scope path, and next to different constants. Everything
					known about it was computed where it used to be, so ask for
					another round: `optimize()` re-runs the whole analysis while
					`modified` is set.
					*/
					this.modified = true;
				}
			}
		});
	}

	globalDeadCodeElimination() {
		// 全局级别的死代码消除
		// 例如：删除空的全局声明、无用类型定义等

		if (this.ast.type === 'Program') {
			// 清理全局声明
			this.cleanupGlobalDeclarations();
		}
	}

	// =============== 辅助方法 ===============

	/**
	 * @typedef {{
	 * 	isBuiltinConstant: boolean,
	 *  category: string,
	 *  name: string
	 * }} extBuiltinConstant
	 *
	 */
	/**
	 * The `#pragma id` / `#pragma var` definitions of this program.
	 * @returns {import('./mindcContent.js').ContentIdTable?}
	 */
	get contentTable() {
		return this.compiler?.extraConfig?.getAttribute('contentTable') ?? null;
	}


	/**
	 * The value of `@itemCount` / `@liquidCount` / `@unitCount` / `@blockCount`,
	 * when the compiler is allowed to know it.
	 *
	 * @param {string} name
	 * @returns {number?} null when it is not a count constant, or when ids were
	 *          declared by hand and the real counts are therefore unknown
	 */
	contentCountOf(name) {
		const category = {
			'@itemCount': 'item', '@liquidCount': 'liquid',
			'@unitCount': 'unit', '@blockCount': 'block'
		}[name];
		if (!category) return null;
		const table = this.contentTable;
		if (table && table.hasOverrides) return undefined;	// Deliberately unknown
		return table ? table.namesOf(category).length : null;
	}

    evaluateConstantExpression(node, allowConstants = true) {
        if (!node) return undefined;
        
        switch (node.type) {
            case 'NumericLiteral': case 'StringLiteral':
                return node.value;
            case 'Identifier':
				// Try evaluating in current environment
				if (node.name[0] === '@' && allowConstants) {
					/*
					`@itemCount` and friends are fixed by the content tables, so
					they fold like any other number -- unless the program declared
					ids of its own, in which case how many contents exist is no
					longer something the compiler may assume.
					*/
					const count = this.contentCountOf(node.name);
					if (count !== null) return count;
					if (this.contentTable && this.contentTable.isOpaque(node.name)) {
						// An unknown content: its id is not knowable here
						return undefined;
					}
					// Regard this as a constant value...
					return {
						isBuiltinConstant: true,
						category: ConstantManager.categorize(node.name),
						name: node.name
					};
				}
				const scope = this.currentScope.lookupScopeOf(node.name);
				if (scope != null) {
					const symbol = this.currentScope.lookup(node.name);
					if (!symbol.isConst && 
						(symbol.isGlobal || symbol.isAddressed || symbol.isExtern || symbol.isVolatile ||
						(this.restrictiveVariables && ((this.restrictiveVariables === 'strict') 
						|| (this.restrictiveVariables !== 'strict' && this.restrictiveVariables.has(symbol)))))) {
						return undefined;
					}
					const scopePath = scope.getPath();
					const localConstantRef = this.localConstants.get(scopePath).get(node.name);
					return localConstantRef ?? this.constants.get(node.name);
				} else if (this.functionInfo.has(node.name)) {
					// Consider this a function call
					const funcInfo = this.functionInfo.get(node.name);
					funcInfo.calls++;
					funcInfo.isAddressed = true;
					funcInfo.canInline = false;
					const callerName = this.currentFunction || 'anonymous';
                    this.recordFunctionCall(callerName, funcInfo.name);
				} else {
					return undefined;
				}
				break;
            case 'BinaryExpression':
			case 'AssignmentExpression':	// Manually added
                const left = this.evaluateConstantExpression(node.left);
                const right = this.evaluateConstantExpression(node.right);
                if (left !== undefined && right !== undefined) {
                    return this.evaluateBinaryOperation(node.operator, left, right, node.dataType);
                }
                return undefined;
            case 'UnaryExpression':
                const arg = this.evaluateConstantExpression(node.argument);
                if (arg !== undefined) {
                    return this.evaluateUnaryOperation(node.operator, arg);
                }
                return undefined;
			case 'MemberExpression': {
				if (!node.getAttribute('computed')) return undefined;
				const object = node.children?.[0] ?? node.object ?? null;
				const subscript = node.children?.[1] ?? node.property ?? null;
				if (!object || object.type !== 'Identifier' || !subscript) return undefined;
				const symbol = this.currentScope.lookup(object.name);
				if (!symbol || !this.isFoldableArray(symbol)) return undefined;
				const index = this.evaluateConstantExpression(subscript);
				if (typeof index !== 'number' || !Number.isInteger(index)) return undefined;
				const elements = this.getConstantArray(symbol);
				if (!elements || index < 0 || index >= elements.length) return undefined;
				return elements[index];
			}
			case 'ConditionalExpression':
				const test = this.evaluateConstantExpression(node.test);
				if (test !== undefined) {
					if (test) {
						return this.evaluateConstantExpression(node.consequent);
					} else {
						return this.evaluateConstantExpression(node.alternate);
					}
				}
            default:
                return undefined;
        }
    }

	// ! Manually modified !
	// Special compatibility for '+=' and '='
	/**
	 * 
	 * @param {string} operator 
	 * @param {constantInfo} left 
	 * @param {constantInfo} right 
	 * @param {TypeInfo?} [typeDelim=null]
	 * @returns 
	 */
    evaluateBinaryOperation(operator, left, right, typeDelim = null) {

		const isBuiltinConstant = x => typeof x === 'object' && x.isBuiltinConstant;

		let isEqual = (l, r) => {
			if (isBuiltinConstant(l) && isBuiltinConstant(r)) {
				return l.name === r.name;
			}
			return l === r;
		};

		const isInt = typeDelim && (this.semantic.isTypeCompatibleForAny(typeDelim, ['int', 'long', 'signed', 'unsigned'], true));
		const isBool = typeDelim && (this.semantic.isTypeCompatibleForAny(typeDelim, ['bool'], true));

		if (!(['==', '!='].includes(operator)) && (isBuiltinConstant(left) || isBuiltinConstant(right))) {
			return undefined;
		}

        switch (operator) {
			case '=': return right;
            case '+': case '+=': return left + right;
            case '-': case '-=': return left - right;
            case '*': case '*=': return left * right;
            case '/': case '/=':
				if (right === 0) {
					this.addError('Explicit division by zero');
					return undefined;
				}
				if (isInt) return Math.floor(left / right);
				if (isBool) return Boolean(left / right);
				return left / right;
            case '%': case '%=':
				if (right === 0) {
					this.addError('Explicit modulo by zero');
					return undefined;
				}
				return left % right;
            case '&': case '&=': return left & right;
            case '|': case '|=': return left | right;
            case '^': case '^=': return left ^ right;
			case '||': return left || right;
			case '&&': return left && right;
            case '<<': case '<<=': return left << right;
            case '>>': case '>>=': return left >> right;
            case '==': return isEqual(left, right);
            case '!=': return !isEqual(left, right);
            case '<': return left < right;
            case '>': return left > right;
            case '<=': return left <= right;
            case '>=': return left >= right;
            default: return undefined;
        }
    }
	
	// Maybe TODO: ! Add ++/-- !
    evaluateUnaryOperation(operator, arg) {
        switch (operator) {
            case '+': return +arg;
            case '-': return -arg;
            case '~': return ~arg;
            case '!': return !arg ? 1 : 0;
            default: return undefined;
        }
    }

	// ! This function has TO-DOs !
	// This function is manually merged from AI output.
	/**
	 * 
	 * @param {ASTNode} node 
	 * @param {*?} special 
	 * @param {Scope?} selectedScope 
	 * @returns {boolean}
	 */
    hasSideEffects(node, special = null, selectedScope = null) {
        if (!node) return false;
		selectedScope = node.scope ?? (selectedScope ?? this.currentScope);
		/**
		 * @type {boolean}
		 */
		const inFunction = special && special.inFunction;

		const functionBuiltin = ['set', 'op', 'jump', 'asm'];

        // 检查节点是否有副作用
        switch (node.type) {
			case 'VariableDeclarator':
			case 'Declarator':
				if (node.dataType && (node.dataType.isPointerImpl() ||
					node.dataType.qualifiers.includes('volatile') 
					|| node.dataType.qualifiers.includes('static')
					|| node.dataType.qualifiers.includes('extern'))) {	// 'extern' means that the variable might be modified from the outside
					return true;
				}
				if (node.initializer) {
					return this.hasSideEffects(node.initializer, special, selectedScope);
				}
				break;
            case 'AssignmentExpression':
				if (node.left.dataType && (node.left.dataType.isPointerImpl() ||
					node.left.dataType.qualifiers.includes('volatile') 
					|| node.left.dataType.qualifiers.includes('static')
					|| node.left.dataType.qualifiers.includes('extern'))) {
					return true;
				}
			case 'BinaryExpression':
				return (this.hasSideEffects(node.left, special, selectedScope)) || (this.hasSideEffects(node.right, special, selectedScope));
				// TODO: Double-check logic here
				break;
			//case 'InitializerList':	// Depending on its children
			case 'AsmStatement':
				// There won't be cross-function optimizing
				return true;
				break;
			case 'FunctionCall':		// Consider all function calls to be with side effects...
				if (!inFunction) return true;
				return node.arguments.some(param => this.hasSideEffects(param, special, selectedScope));
				break;
			case 'BuiltinCall':
				// Exclusion of some
				if (!inFunction) return true;
				return functionBuiltin.includes(node.functionName);
				break;
            case 'UnaryExpression':
				// ++ and -- itself actually have no side effect.
				// side effect of '&' depends
				if (node.operator === '&' || node.operator === '*') {
					//return this.hasSideEffects(node.argument);
					return true;	// No pointer analyses!
				}
				break;
			
				// 复合赋值运算符有副作用
				/*
				if (node.operator.includes('=') && node.operator !== '==' && node.operator !== '!=' && node.operator !== '>=' && node.operator !== '<=') {
					return true;
				}
				break;
				*/
			case 'CastExpression':
				// Any 'volatile' cast should cause side effects
				if (node.dataType && node.dataType.isVolatile()) return true;
				// Deliberately pass down.		
			case 'ReturnStatement':
				// return可能有副作用（返回值表达式）
				// Abortion of control is dealt with elsewhere
				return node.argument ? this.hasSideEffects(node.argument, special, selectedScope) : false;
			case 'IfStatement':
			case 'WhileStatement':
			case 'ForStatement':
				// 控制语句可能有副作用
				return (node.test && this.hasSideEffects(node.test, special, selectedScope)) ||
					   (node.consequent && this.hasSideEffects(node.consequent, special, selectedScope)) ||
					   (node.alternate && this.hasSideEffects(node.alternate, special, selectedScope)) ||
					   (node.body && this.hasSideEffects(node.body, special, selectedScope)) ||
					   (node.init && this.hasSideEffects(node.init, special, selectedScope)) ||
					   (node.update && this.hasSideEffects(node.update, special, selectedScope));
			case 'CompoundStatement':
				// 如果其任何子语句有副作用，复合语句有副作用
				if (node.statements) {
					for (const stmt of node.statements) {
						if (this.hasSideEffects(stmt, special, selectedScope)) {
							return true;
						}
					}
				}
				break;
			case 'MemberExpression':
			case 'PointerTypeNode':
				// Not considering struct members
				return true;
			case 'Identifier':
				if (node.name.length > 0 && node.name[0] === '@') return ConstantManager.categorize(node.name) === 'unknown';
				const scopeInfo = selectedScope.lookupScopeOf(node.name);
				if (!scopeInfo) {
					return true;	// Cannot get information
				}
				const scopePath = scopeInfo.getPath();
				const variableUses = this.variableUses.get(scopePath);
				const variableState = variableUses.get(node.name);
				if (variableState && (!variableState.canOptimize)) {
					if (!inFunction) return true;
				}
				break;
        }
        
        // 递归检查子节点
		if (node.declarators) {
			let hasSideEffects = false;
			node.declarators.forEach(declarator => {
				hasSideEffects = hasSideEffects || this.hasSideEffects(declarator, special, selectedScope);
			});
			return hasSideEffects;
		}
        if (node.children) {
            for (const child of node.children) {
                if (this.hasSideEffects(child, special, selectedScope)) {
                    return true;
                }
            }
        }
        
        return false;
    }

    simplifyBinaryExpression(node, leftValue, rightValue) {
        // 代数恒等式简化
        // 例如：x * 1 → x, x + 0 → x
        const leftConst = this.evaluateConstantExpression(node.left);
        const rightConst = this.evaluateConstantExpression(node.right);
        
        switch (node.operator) {
            case '+':
                if (leftConst === 0) {
                    this.replaceNode(node, node.right);
                    return true;
                }
                if (rightConst === 0) {
                    this.replaceNode(node, node.left);
                    return true;
                }
                break;
            case '-':
                if (rightConst === 0) {
                    this.replaceNode(node, node.left);
                    return true;
                }
                break;
            case '*':
                if (leftConst === 1) {
                    this.replaceNode(node, node.right);
                    return true;
                }
                if (rightConst === 1) {
                    this.replaceNode(node, node.left);
                    return true;
                }
                if (leftConst === 0 || rightConst === 0) {
                    this.replaceNode(node, ASTBuilder.numericLiteral(0));
                    return true;
                }
                break;
            case '/':
                if (rightConst === 1) {
                    this.replaceNode(node, node.left);
                    return true;
                }
                break;
        }
        
        return false;
    }

	/**
	 * 替换AST节点
	 * @param {ASTNode} oldNode - 要替换的旧节点
	 * @param {ASTNode|null} newNode - 新节点，为null时表示删除
	 */
	replaceNode(oldNode, newNode) {
		if (!oldNode || oldNode === newNode) return;
		this.modified = true;
		
		const parent = oldNode.parent;
		if (!parent) {
			// 根节点替换（通常不会发生）
			if (oldNode === this.ast) {
				this.ast = newNode;
			}
			return;
		}
		
		// 根据父节点类型处理替换
		switch (parent.type) {
			case 'Program':
				if (parent.functions) {
					const index = parent.functions.indexOf(oldNode);
					if (index !== -1) {
						if (newNode) {
							newNode.parent = parent;
							parent.functions[index] = newNode;
						} else {
							parent.functions.splice(index, 1);
						}
						return;
					}
				}
				if (parent.globalDeclarations) {
					const index = parent.globalDeclarations.indexOf(oldNode);
					if (index !== -1) {
						if (newNode) {
							newNode.parent = parent;
							parent.globalDeclarations[index] = newNode;
						} else {
							parent.globalDeclarations.splice(index, 1);
						}
						return;
					}
				}
				if (parent.typeDefinitions) {
					const index = parent.typeDefinitions.indexOf(oldNode);
					if (index !== -1) {
						if (newNode) {
							newNode.parent = parent;
							parent.typeDefinitions[index] = newNode;
						} else {
							parent.typeDefinitions.splice(index, 1);
						}
						return;
					}
				}
				break;
				
			case 'CompoundStatement':
				const stmtIndex = parent.statements.indexOf(oldNode);
				if (stmtIndex !== -1) {
					if (newNode) {
						newNode.parent = parent;
						parent.statements[stmtIndex] = newNode;
					} else {
						parent.statements.splice(stmtIndex, 1);
					}
					return;
				}
				break;
				
			case 'ConditionalExpression':
				if (newNode) {
					newNode.parent = parent;
				}
				if (oldNode === parent.test) {
					if (parent) {
						newNode.parent = parent;
						parent.test = newNode;
					} else {
						parent.test = null;
					}
				}
				if (oldNode === parent.consequent) {
					if (parent) {
						newNode.parent = parent;
						parent.consequent = newNode;
					} else {
						parent.consequent = null;
					}
				}
				if (oldNode === parent.alternate) {
					if (parent) {
						newNode.parent = parent;
						parent.alternate = newNode;
					} else {
						parent.alternate = null;
					}
				}
				break;

			case 'ExpressionStatement':
				if (oldNode === parent.expression) {
					if (newNode) {
						newNode.parent = parent;
						parent.expression = newNode;
					} else {
						// 删除整个表达式语句
						parent.expression = null;
						//this.removeNodeFromParent(parent);
					}
					return;
				}
				break;
				
			case 'VariableDeclaration':
				// 替换声明符
				const declIndex = parent.declarators.indexOf(oldNode);
				if (declIndex !== -1) {
					if (newNode) {
						newNode.parent = parent;
						parent.declarators[declIndex] = newNode;
					} else {
						parent.declarators.splice(declIndex, 1);
					}
					return;
				}
				break;
				
			case 'IfStatement':
				if (oldNode === parent.test) {
					if (newNode) {
						newNode.parent = parent;
						parent.test = newNode;
					}
					return;
				}
				if (oldNode === parent.consequent) {
					if (newNode) {
						newNode.parent = parent;
						parent.consequent = newNode;
					}
					return;
				}
				if (oldNode === parent.alternate) {
					if (newNode) {
						newNode.parent = parent;
						parent.alternate = newNode;
					}
					return;
				}
				break;
				
			case 'WhileStatement':
				if (oldNode === parent.test) {
					if (newNode) {
						newNode.parent = parent;
						parent.test = newNode;
					}
					return;
				}
				if (oldNode === parent.body) {
					if (newNode) {
						newNode.parent = parent;
						parent.body = newNode;
					}
					return;
				}
				break;
				
			case 'ForStatement':
				if (oldNode === parent.init) {
					if (newNode) {
						newNode.parent = parent;
						parent.init = newNode;
					}
					return;
				}
				if (oldNode === parent.test) {
					if (newNode) {
						newNode.parent = parent;
						parent.test = newNode;
					}
					return;
				}
				if (oldNode === parent.update) {
					if (newNode) {
						newNode.parent = parent;
						parent.update = newNode;
					}
					return;
				}
				if (oldNode === parent.body) {
					if (newNode) {
						newNode.parent = parent;
						parent.body = newNode;
					}
					return;
				}
				break;
				
			case 'ReturnStatement':
				if (oldNode === parent.argument) {
					if (newNode) {
						newNode.parent = parent;
						parent.argument = newNode;
					}
					return;
				}
				break;
				
			case 'BinaryExpression':
				if (oldNode === parent.left) {
					if (newNode) {
						newNode.parent = parent;
						parent.left = newNode;
					}
					return;
				}
				if (oldNode === parent.right) {
					if (newNode) {
						newNode.parent = parent;
						parent.right = newNode;
					}
					return;
				}
				break;
				
			case 'UnaryExpression':
				if (oldNode === parent.argument) {
					if (newNode) {
						newNode.parent = parent;
						parent.argument = newNode;
					}
					return;
				}
				break;
				
			case 'AssignmentExpression':
				if (oldNode === parent.left) {
					if (newNode) {
						newNode.parent = parent;
						parent.left = newNode;
					}
					return;
				}
				if (oldNode === parent.right) {
					if (newNode) {
						newNode.parent = parent;
						parent.right = newNode;
					}
					return;
				}
				break;
				
			case 'FunctionCall':
			case 'BuiltinCall':
				if (oldNode === parent.callee) {
					if (newNode) {
						newNode.parent = parent;
						parent.callee = newNode;
					}
					return;
				}
				const argIndex = parent.arguments.indexOf(oldNode);
				if (argIndex !== -1) {
					if (newNode) {
						newNode.parent = parent;
						parent.arguments[argIndex] = newNode;
					} else {
						parent.arguments.splice(argIndex, 1);
					}
					return;
				}
				break;
			
			case 'VariableDeclarator':
			case 'Declarator':
				if (oldNode === parent.initializer) {
					parent.initializer = newNode;
					newNode.parent = parent;
				}
			
			case 'MemberExpression':
				if (oldNode === parent.getChild(0)) {
					if (newNode) {
						parent.children[0] = newNode;
						newNode.parent = parent;
					}
					return;
				}
				if (oldNode === parent.getChild(1)) {
					if (newNode) {
						parent.children[1] = newNode;
						newNode.parent = parent;
					}
					return;
				}
				break;
				
			default:
				// 处理通用子节点
				
				break;
		}

		const foundDeletionIn = seq => {
			const childIndex = seq.indexOf(oldNode);
			if (childIndex !== -1) {
				if (newNode) {
					newNode.parent = parent;
					seq[childIndex] = newNode;
				} else {
					seq.splice(childIndex, 1);
				}
				return true;
			}
			return false;
		};
		if (parent.children) {
			foundDeletionIn(parent.children);
		}
		if (parent.declarators) {
			foundDeletionIn(parent.declarators);
		}
		
		// 如果找不到父节点引用，可能需要遍历查找
		this.findAndReplaceNode(this.ast, oldNode, newNode);
	}
	
	/**
	* 递归查找并替换节点
	*/
	findAndReplaceNode(currentNode, oldNode, newNode) {
	if (!currentNode) return false;

	// 检查各种节点类型的特定字段
	const fieldsToCheck = [];

	switch (currentNode.type) {
		case 'Program':
			fieldsToCheck.push('functions', 'globalDeclarations', 'typeDefinitions');
			break;
		case 'CompoundStatement':
			fieldsToCheck.push('statements');
			break;
		case 'ExpressionStatement':
			fieldsToCheck.push('expression');
			break;
		case 'VariableDeclaration':
			fieldsToCheck.push('declarators');
			break;
		case 'IfStatement':
			fieldsToCheck.push('test', 'consequent', 'alternate');
			break;
		case 'WhileStatement':
			fieldsToCheck.push('test', 'body');
			break;
		case 'ForStatement':
			fieldsToCheck.push('init', 'test', 'update', 'body');
			break;
		case 'ReturnStatement':
			fieldsToCheck.push('argument');
			break;
		case 'BinaryExpression':
		case 'AssignmentExpression':
			fieldsToCheck.push('left', 'right');
			break;
		case 'UnaryExpression':
			fieldsToCheck.push('argument');
			break;
		case 'FunctionCall':
		case 'BuiltinCall':
			fieldsToCheck.push('callee', 'arguments');
			break;
		case 'MemberExpression':
			fieldsToCheck.push('object', 'property');
			break;
	}

	// 检查特定字段
	for (const field of fieldsToCheck) {
		if (Array.isArray(currentNode[field])) {
			const index = currentNode[field].indexOf(oldNode);
			if (index !== -1) {
				if (newNode) {
					newNode.parent = currentNode;
					currentNode[field][index] = newNode;
				} else {
					currentNode[field].splice(index, 1);
				}
				return true;
			}
		} else if (currentNode[field] === oldNode) {
			if (newNode) {
				newNode.parent = currentNode;
				currentNode[field] = newNode;
			} else {
				currentNode[field] = null;
			}
			return true;
		}
	}

	// 递归检查子节点
	if (currentNode.children) {
		const index = currentNode.children.indexOf(oldNode);
		if (index !== -1) {
			if (newNode) {
				newNode.parent = currentNode;
				currentNode.children[index] = newNode;
			} else {
				currentNode.children.splice(index, 1);
			}
			return true;
		}
		
		for (const child of currentNode.children) {
			if (this.findAndReplaceNode(child, oldNode, newNode)) {
				return true;
			}
		}
	}

	return false;
	}

	/**
	 * 从父节点中删除节点
	 */
	removeNodeFromParent(node) {
		if (!node || !node.parent) return;
		
		this.replaceNode(node, null);
	}


    estimateFunctionSize(funcNode) {
        if (!funcNode.body) return 0;
        
        let size = 0, maxDepth = 0;
        const countStatements = (node, depth = 0) => {
            if (!node) return;
			if (depth > maxDepth) maxDepth = depth;
            
            switch (node.type) {
                case 'ExpressionStatement':
                case 'VariableDeclaration':
                case 'ReturnStatement':
                case 'IfStatement':
                case 'WhileStatement':
                case 'ForStatement':
                    size++;
                    break;
				case 'FunctionCall':
					size += 3 + node.arguments.length;
					break;
            }
            
            if (node.children) {
                node.children.forEach(child => countStatements(child, depth + 1));
            }

			const fieldsToCheck = [
				'functions', 'globalDeclarations', 'typeDefinitions',
				'statements', 'expression', 'test', 'consequent', 'alternate',
				'body', 'init', 'update', 'argument', 'left', 'right',
				'declarators', 'arguments', 'callee', 'initializer'
			];

			for (const field of fieldsToCheck) {
				if (Array.isArray(node[field])) {
					node[field].forEach(item => countStatements(item), depth + 1);
				} else if (node[field] && typeof node[field] === 'object') {
					countStatements(node[field], depth + 1);
				}
			}

        };
        
        countStatements(funcNode.body);
        return { size, maxDepth };
    }

    shouldInlineFunction(funcName) {
		/*
		Three separate questions, deliberately kept apart:
		  - can it be inlined at all (structure)?
		  - has anybody forbidden automatic inlining of it?
		  - (worthwhileness is decided by the caller, see `worthInlining`)
		Side effects play no part: they only decide whether an unused CALL may
		be deleted, see `isStrictlyPureFunction`.
		*/
		if (!this.canInlineFunction(funcName)) return false;
		const info = this.functionInfo.get(funcName);
		return this.autoInlineAllowed(funcName, info?.isInline === true);
    }

	/**
	 * 执行函数内联
	 */
	performFunctionInlining(funcName) {
		const funcInfo = this.functionInfo.get(funcName);
		if (!funcInfo || !funcInfo.node || !funcInfo.node.body) {
			return false;
		}
		
		// 找到所有调用点
		const callSites = [];
		this.collectFunctionCallSites(this.ast, funcName, callSites);
		
		if (callSites.length === 0) return false;

		let inlined = false;
		
		// 对每个调用点进行内联
		for (const {callNode, parentNode, context} of callSites) {
			// A call that decides how often a loop runs is never inlined
			if (this.isLoopControlCall(callNode)) continue;
			if (this.inlineFunctionAtSite(funcInfo.node, callNode, parentNode, context)) {
				inlined = true;
			}
			
			/**
			 * 
			 * @param {Scope} s 
			 */
			 /*
			const scopeUpdater = s => {
				if (s.astNode) s.astNode.scope = s;
				s.children.forEach(s => scopeUpdater(s));
			};
			scopeUpdater(this.globalScope);	// Band-Aid solution, but I just can't do anything better
			// Let it runnable first
			*/
		}
		
		// 如果函数现在没有被调用，可以删除它
		if (inlined && funcInfo.calls === 0) {
			this.globalScope.removeSymbol(funcName);	// Manually added
			this.removeNodeFromParent(funcInfo.node);
			this.logOptimization(`Removed unused function '${funcName}' after inlining`);
		}
		return inlined;
	}

	/**
	 * 收集函数调用点
	 */
	collectFunctionCallSites(node, funcName, callSites, context = {}) {
		if (!node) return;
		
		// 更新上下文
		const newContext = {...context};
		
		switch (node.type) {
			case 'FunctionCall':
				if (node.callee.type === 'Identifier' && node.callee.name === funcName) {
					callSites.push({
						callNode: node,
						parentNode: node.parent,
						context: {...newContext}
					});
				}
				break;
				
			case 'FunctionDeclaration':
				// 进入函数作用域，记录函数名
				newContext.currentFunction = node.name;
				break;
				
			case 'CompoundStatement':
				// 记录作用域深度
				newContext.scopeDepth = (newContext.scopeDepth || 0) + 1;
				break;
		}
		
		// 递归检查子节点
		if (node.children) {
			for (const child of node.children) {
				this.collectFunctionCallSites(child, funcName, callSites, newContext);
			}
		}
		
		// 检查特定字段
		const fieldsToCheck = [
			'functions', 'globalDeclarations', 'typeDefinitions',
			'statements', 'expression', 'test', 'consequent', 'alternate',
			'body', 'init', 'update', 'argument', 'left', 'right',
			'declarators', 'arguments', 'callee', 'initializer'
		];
		
		for (const field of fieldsToCheck) {
			if (Array.isArray(node[field])) {
				node[field].forEach(item => {
					if (item && typeof item === 'object') {
						this.collectFunctionCallSites(item, funcName, callSites, newContext);
					}
				});
			} else if (node[field] && typeof node[field] === 'object') {
				this.collectFunctionCallSites(node[field], funcName, callSites, newContext);
			}
		}
	}

	/**
	 * Re-rooting scope to target as its parent.
	 * Migrating all information (including VARIABLE USES and LOCAL CONSTANTS)
	 * Re-rooting scope does not modify symbols in it.
	 * @param {Scope} scope 
	 * @param {Scope} target 
	 * @param {boolean} [deleteOld=false] 
	 */
	reRootScope(scope, target, deleteOld = false) {
		/**
		 * @type {{scope: Scope, path: string}[]}
		 */
		let oldScopes = [];
		/**
		 * 
		 * @param {Scope} s 
		 */
		const scopeVisitor = s => {
			oldScopes.push({
				scope: s, path: s.getPath()
			});
			s.children.forEach(sch => scopeVisitor(sch));
		};
		scopeVisitor(scope);

		if (scope.parent) {
			const finder = scope.parent.children.indexOf(scope);
			if (finder != -1){
				scope.parent.children.splice(finder, 1);
			}
		}
		if (!target.children.includes(scope)) {
			target.children.push(scope);
		}
		scope.parent = target;

		oldScopes.forEach(o => {
			const originalPath = o.path;
			const newPath = o.scope.getPath();
			this.variableUses.set(newPath, this.variableUses.get(originalPath));
			if (deleteOld) this.variableUses.delete(originalPath);
			this.localConstants.set(newPath, this.localConstants.get(originalPath));
			if (deleteOld) this.localConstants.delete(originalPath);
		});
		
	}

	/**
	 * 
	 * @param {FunctionDeclarationNode} funcNode 
	 * @param {FunctionCallNode} callNode 
	 * @param {ASTNode} parentNode 
	 * @param {any} context 
	 * @returns {boolean} Whether the inlining is successful
	 */
	/**
	 * The array an argument handed to an inlined parameter really means.
	 *
	 * Inlining does not deliver arguments into parameter slots -- it rebinds
	 * the parameter to the argument's own storage. For an array that has no
	 * address, "rebinding" can only mean: USE THE ARGUMENT'S OWN REGISTERS.
	 * Anything else (making the parameter a fresh array of its own) copies it,
	 * and a write inside the function then lands in the copy and is lost.
	 *
	 * @param {any} argNode The argument at this call site
	 * @returns {import('./mindcSemantic.js').SymbolEntry?} null when the
	 *          argument is not a plain array name, so nothing can be aliased
	 */
	arrayArgumentSymbolOf(argNode) {
		if (!argNode || argNode.type !== 'Identifier') return null;
		const symbol = argNode.symbol ?? null;
		if (symbol && symbol.myType?.()?.kind === 'array') return symbol;
		const scope = argNode.scope ?? this.currentScope ?? null;
		const found = scope?.lookup?.(argNode.name) ?? null;
		if (found && found.myType?.()?.kind === 'array') return found;
		return null;
	}

	/**
	 * Whether this call may be inlined given the arrays it hands over.
	 *
	 * @param {any} funcNode
	 * @param {any} callNode
	 * @returns {boolean}
	 */
	canInlineArrayArguments(funcNode, callNode) {
		const params = funcNode.parameters ?? [];
		for (let i = 0; i < params.length; i++) {
			const arg = (callNode.arguments ?? [])[i];
			if (!arg) continue;
			const type = arg.dataType ?? null;
			if (!type || type.kind !== 'array') continue;
			if (this.arrayArgumentSymbolOf(arg)) continue;
			/*
			Not a plain array name -- there is nothing to alias to, so the
			parameter would have to become an array of its own. Keep the real
			call instead, which passes a handle and stays correct.
			*/
			return false;
		}
		return true;
	}

	inlineFunctionAtSite(funcNode, callNode, parentNode, context) {
		// Cloned body must be a compound statement
		// 检查是否适合内联
		if (!this.isSuitableForInlining(funcNode, callNode, context)) {
			return false;
		}
		/*
		An array argument has to be aliasable, or inlining would hand the
		function a COPY of it (see `arrayArgumentSymbolOf`).
		*/
		if (!this.canInlineArrayArguments(funcNode, callNode)) return false;
		/*
		const funcSymbol = funcNode.scope.lookup(funcNode.name);
		if (!funcSymbol) {
			return false;	// Can't inline
		}
		*/
		// 复制函数体
		if (funcNode.body.type !== 'CompoundStatement') {
			return false;
		}

		const clonedBody = this.reparentCloned(this.cloneAST(funcNode.body), null);
		if (!clonedBody) return false;
		
		// 处理返回语句
		// (This function is no longer used)
		//const returnValue = this.extractAndReplaceReturns(clonedBody, parentNode, callNode);
		
		/**
		 * 
		 * @param {Scope} ziel 
		 * @param {CompoundStatementNode} target
		 * @returns 
		 */
		const changeParameter = (ziel, target) => {
			// Insert parameters after determining where the parent is!
			// 创建参数映射
			/**
			 * @type {Map<string, IdentifierNode>}
			 */
			const paramMap = new Map();
			/**
			 * @type {AssignmentExpressionNode[]}
			 */
			let unshifts = [];
			funcNode.parameters.forEach((param, index) => {
				if (index < callNode.arguments.length && param.name) {
					//const paramSymbol = funcNode.scope.lookup(param.name).duplicate();
					/**
					 * @type {TypeInfo}
					 */
					const currentType = callNode.arguments[index].dataType;
					const symbolName = `${funcNode.name}:${param.name}:${this.inlinedScopeId}`;//funcNode.name + ':' + param.name;
					const ident = ASTBuilder.identifier(symbolName);
					const value = this.cloneAST(callNode.arguments[index]);
					// const assigner = ASTBuilder.assignmentExpression('=', ident, value);
					const decler = ASTBuilder.variableDeclarator(symbolName);
					decler.setAttribute('inliningAlias', param);
					decler.initializer = value;
					// decler.type.setAttribute('qualifiers', currentType.qualifiers);
					value.parent = decler;
					// Warning: not a type specifier now!!!
					const decl = ASTBuilder.variableDeclaration(new TypeSpecifierNode(callNode.arguments[index].dataType), [decler]);
					decler.parent = decl;
					ident.dataType = currentType;
					//ident.parent = assigner;
					decl.setAttribute('inliningAlias', param);
					//clonedBody.statements.unshift(assigner);
					unshifts.push(decl);
					decl.parent = clonedBody;
					decl.dataType = ident.dataType;
					ident.scope = value.scope = decl.scope = decler.scope = ziel;// parentNode.scope ?? callNode.scope;
					//paramMap.set(param.name, callNode.arguments[index]);
					paramMap.set(param.name, ident);

					if (ziel) {
						// TODO: Add new symbol into caller's symbol table!
						this.duplicateVariable(funcNode.scope, param.name, ziel, symbolName);
						const symbolTarget = ziel.lookupCurrent(symbolName);
						symbolTarget.kind = 'variable';	// Don't be 'parameter'!
						/*
						An array parameter is an ALIAS, not an array of its own:
						point it at the caller's array so that a write inside the
						inlined body really lands there. Without this the
						parameter gets a fresh layout and the write is lost.
						*/
						const aliased = this.arrayArgumentSymbolOf(callNode.arguments[index]);
						if (aliased) {
							symbolTarget.arrayAliasOf = aliased;
							// It is no longer a slot that gets a handle delivered
							symbolTarget.arrayParamRep = null;
						}
					}
				}
			});
			
			// 替换参数
			this.replaceParametersInAST(clonedBody, paramMap, funcNode, ziel);
			//clonedBody.statements.unshift(...unshifts);
			target.statements.unshift(...unshifts);
			return paramMap;
		};

		// TODO: ! See whether the return value is got somewhere !
		const doInsertInto = (targetNode, lvalOfReturn, doDeletion) => {
			let retIndex = -1;
			const index = targetNode.statements.indexOf(parentNode);
			if (lvalOfReturn && lvalOfReturn.arguments) {
				retIndex = lvalOfReturn.arguments.indexOf(callNode);
				if (retIndex == -1) return false;
				
			}
			if (index !== -1) {
				let newCompound = ASTBuilder.compoundStatement();
				const tmpScope = new Scope(targetNode.scope, 'l', newCompound, `${this.inlinedScopeId}`);
				newCompound.scope = tmpScope;
				const params = changeParameter(tmpScope, newCompound);
				const addedVariables = [...params].map(([key, val]) => val.name);
				let returnerName, returnerDecl, returnerWrapper, returnerIdent;
				// 删除原表达式语句，插入函数体内容
				if (lvalOfReturn) {
					// To prepare for returning, the corresponding content must be put elsewhere
					targetNode.statements.splice(index, 1);
					// For returning, create a special symbol
					returnerName = `${funcNode.name}:@${this.inlineCounter++}:${this.inlinedScopeId}`;
					const returnerSymbol = new SymbolEntry(returnerName, callNode.dataType, tmpScope, 'variable', null, callNode.dataType.size ?? 1);
					tmpScope.parent.addSymbol(returnerSymbol);	// So that the code block can have access
					/**
					 * @todo Create an identifier for returning...
					 */
					returnerDecl = ASTBuilder.variableDeclarator(returnerName);
					returnerWrapper = ASTBuilder.variableDeclaration(new TypeSpecifierNode(callNode.dataType));
					returnerWrapper.declarators.push(returnerDecl);
					returnerDecl.parent = returnerWrapper;
					returnerIdent = ASTBuilder.identifier(returnerName);
					returnerIdent.symbol = returnerSymbol;
					returnerIdent.dataType = returnerSymbol.myType();
					returnerIdent.parent = lvalOfReturn; // To be added somewhere!
					newCompound.statements.push(returnerWrapper);
					returnerWrapper.parent = newCompound;
					addedVariables.push(returnerName);
				}

				/**
				 * Duplicating and rebinding scope for cloned body.
				 * @param {ASTNode} stmt 
				 * @returns {Scope[]} Scope that should become a descend of current available parent
				 * (given that the root is one single node, this return value is not necessary to be proceed)
				 */
				const scopeRebinder = stmt => {
					/**
					 * @type {Scope}
					 */
					let relatedNewScope;
					if (stmt.type === 'CompoundStatement') {
						relatedNewScope = stmt.scope.duplicate();
						relatedNewScope.astNode = stmt;
						relatedNewScope.children = [];
						stmt.scope = relatedNewScope;
					}
					const rets = this.further(stmt, scopeRebinder, null, (prev, curr) => {
						if (!prev) return curr;
						if (!curr) return prev;
						return [...prev, ...curr];
					});
					/**
					 * @type {Scope[]}
					 */
					const expectedChildren = rets ?? [];
					// Post-visit
					if (relatedNewScope) {
						/*this.further(stmt, sub => {
							sub.parent = stmt;
							sub.scope.parent = relatedNewScope;
							relatedNewScope.children.push(sub.scope);
						});
						*/
						expectedChildren.forEach(kinder => {
							relatedNewScope.children.push(kinder);
							kinder.parent = relatedNewScope;
						});
						return [relatedNewScope];
					}
					return expectedChildren;
				};
				const deliveringScope = scopeRebinder(clonedBody)[0];	// Given that the cloneFor() works for only references!
				
				const scopeRelinker = stmt => {
					if (stmt.type !== 'CompoundStatement') {
						stmt.scope = stmt.parent.scope;
					}
					this.further(stmt, scopeRelinker);
				};
				scopeRelinker(clonedBody);

				/*
				The clone still points at the symbols of the function it came
				from. Every pass that works on symbols -- `collectModifiedVariablesInLoop`
				first among them -- would then be looking at the OLD symbol while
				the scope holds the NEW one, and comparing them (as the constant
				propagation does) simply fails. Re-point every identifier at what
				its (new) scope resolves the name to.
				*/
				const symbolRelinker = stmt => {
					if (stmt.type === 'Identifier' && typeof stmt.name === 'string'
						&& stmt.name && stmt.name[0] !== '@') {
						const found = stmt.scope?.lookup?.(stmt.name) ?? null;
						if (found) stmt.symbol = found;
						return;
					}
					this.further(stmt, symbolRelinker);
				};
				symbolRelinker(clonedBody);

				let originals;// = ASTBuilder.compoundStatement();
				//newCompound.scope = funcNode.scope;					// Apply similar structure...

				/**
				 * Guaranteed that the return statement is at the top!
				 * @param {ASTNode} stm
				 */
				const cloneFor = stm => {
					let original = ASTBuilder.compoundStatement();
					/**
					 * Guaranteed that the return statement is at the top!
					 * @param {ASTNode} stmt
					 */
					const cloneFunction = stmt => {
						let actualStmt = stmt;
						if (stmt.type === 'ReturnStatement') {
							// Initially insert an auxiliary computer
							const compStmt = stmt.argument;

							actualStmt = null;
							if (lvalOfReturn) {
								// The right value is the function call
								//lvalOfReturn.right = stmt.argument;
								if (lvalOfReturn.left && lvalOfReturn.left === callNode) {
									//lvalOfReturn.left = compStmt;
									this.replaceNode(lvalOfReturn.left, returnerIdent);
								}
								if (lvalOfReturn.right && lvalOfReturn.right === callNode) {
									this.replaceNode(lvalOfReturn.right, returnerIdent);
								}
								if (lvalOfReturn.argument) {
									if (lvalOfReturn.argument === callNode) {
										this.replaceNode(lvalOfReturn.argument, returnerIdent);
									}
								}
								if (lvalOfReturn.arguments) {
									this.replaceNode(lvalOfReturn.arguments[retIndex], returnerIdent);
								}
								if (lvalOfReturn.initializer && lvalOfReturn.initializer === callNode) {
									this.replaceNode(lvalOfReturn.initializer, returnerIdent);
								}
								//actualStmt = lvalOfReturn;	// There's no need to insert

								// Add an assignment expression!
								const returnAssignerIdent = ASTBuilder.identifier(returnerName);
								const returnAssigner = ASTBuilder.assignmentExpression('=', returnAssignerIdent, stmt.argument);
								stmt.argument.parent = returnAssigner;
								returnAssignerIdent.parent = returnAssigner;
								returnAssignerIdent.symbol = returnerIdent.symbol;
								returnAssignerIdent.dataType = returnerIdent.symbol.extractType();
								returnAssigner.symbol = returnerIdent.symbol;
								returnAssigner.dataType = returnerIdent.symbol.extractType();
								actualStmt = returnAssigner;
								
								// Not that easy. In fact, you have to migrate the whole 'parentNode'...
								//actualStmt = lvalOfReturn;	// Regard it as a part of new function
								// (or the returning will be unable to use anything in the original scope)
							} else if (!actualStmt) {
								actualStmt = stmt.argument;
								// Might have side effects
							}
						} else if (stmt.type === 'CompoundStatement') {
							// Ensure that the return statement is at the top.
							actualStmt = cloneFor(stmt);
						}
						/**
						 * @todo MORE WORK IS NEEDED HERE
						*/
						if (actualStmt) {
							/** This doesn't really work.
							 */
							original.statements.push(actualStmt); // newCompound.statements.push(actualStmt);
							actualStmt.parent = original;// actualStmt.parent = newCompound;
							actualStmt.scope = stmt.scope;
						}
					};
					stm.statements.forEach(stmt => void (cloneFunction(stmt)));
					original.scope = stm.scope;
					return original;
				};
				
				originals = cloneFor(clonedBody);
				/**
				 * @todo !! adjust scope for clonedBody !!
				*/
				
				// Relocate the parentNode to new compound statement
				
				newCompound.statements.push(originals);
				if (lvalOfReturn) {	// Or there will be errors in further inlining!
					newCompound.statements.push(parentNode);
					parentNode.parent = newCompound;
				}
				originals.parent = newCompound;
				targetNode.statements.splice(index, 0, newCompound);
				newCompound.parent = targetNode;

				/**
				 * @todo Consider: use "(function):@" for returner, and then the
				 * returner is assigned this value -- so that to prevent scope issue.
				 */
				// deliveringScope is now bound much earlier
				if (tmpScope && deliveringScope) {
					// const newScope = deliveringScope.duplicateAll();
					this.reRootScope(deliveringScope, tmpScope, false);
					const tmpPath = tmpScope.getPath();
					let varUseRef = this.variableUses.get(tmpPath);
					if (!varUseRef) {
						varUseRef = new Map();
						this.variableUses.set(tmpPath, varUseRef);
					}
					addedVariables.forEach(
						/**
						 * 
						 * @param {string} addedVarName 
						 */
						addedVarName => {
							varUseRef.set(addedVarName, {
								reads: 1,			// Use 'infinity' to prevent variable deletion
								writes: 1,					// Actual situation
								canOptimize: true			// Allow continuous operation
							});
					});
					/*
					// Seems useless
					deliveringScope.symbols = new Map(
						[...deliveringScope.symbols].filter(([key, symb]) => symb.kind !== 'parameter')
					);	// Remove all parameters!
					// Correspondingly:
					if (lvalOfReturn) {
						// Migrate aliases (I think this is correct anyway)
						params.forEach((value, key) => this.duplicateVariable(parentNode.scope, value, tmpScope, value));
						parentNode.scope = tmpScope;
					}
					*/
				} else {
					throw `No scope for function to inline!`;
					return false;
				}
				newCompound.setAttribute('throughInline', true);
				newCompound.setAttribute('functionName', funcNode.name);

				if (lvalOfReturn) {
					// If its descendant has sth...
					// Then that staff of parentNode must have its scope relocated
					const controller = sub => {
						if (sub.type === 'CompoundStatement') {
							throw `Internal Error: unexpected compound statement!`;
						}
						sub.scope = tmpScope;
					};
					this.further(parentNode, cont => {
						// Only compound statements may own its scope, as functions can't be embedded
						
						if (cont.type === 'CompoundStatement' && cont.scope) {
							this.reRootScope(cont.scope, tmpScope, false);
						} else {
							this.further(cont, controller);
						}
					});
					lvalOfReturn.scope = tmpScope; // Used for side effect analyses
				}
				
				/**
				 * The incorrectness is, in a word, caused by incorrect order of Scopes. re-ordering doesn't help. 15 Feb 22Z
				 * @todo WHAT YOU REALLY NEED TO DO IS: ADD AN INDEPENDENT COMPOUND STATEMENT FOR THE ORIGINAL STATEMENTS IN FUNCTIONS!
				 */
				/*
				const tidyScope = scope => {
					scope.children = [...new Set(scope.children)];
					scope.children.forEach(tidyScope);
				};
				tidyScope(newCompound.scope);
				*/
				/**
				 * @param {ASTNode} ast
				 */
				/*
				
				rebinding(newCompound);
				*/

				this.inlinedScopeId++;
				return true;
			} else {
				return false;
			}
		};
		
		let parentOfParent = parentNode.parent, result = true;
		const directParent = parentNode;	// Not modified ver
		const noInlining = ['ForStatement', 'WhileStatement'];
		// Get first compound statement
		if (noInlining.includes(parentNode.type)) return false;
		while (parentOfParent && parentOfParent.type !== 'CompoundStatement') {
			parentNode = parentOfParent;
			parentOfParent = parentOfParent.parent;
			if (noInlining.includes(parentNode.type)) return false;
		}

		// There should be only one call in the 'parent node'
		/**
		 * @type {number}
		 */
		let totalCalls = 0;
		const countTotalCalls = node => {
			if (node.type === 'FunctionCall' && node.name === callNode.name) totalCalls++;
			this.further(node, countTotalCalls, () => (totalCalls > 1));
		};
		countTotalCalls(parentNode);
		if (totalCalls > 1) {
			console.log(`Internal Error: Unable to inline function ${funcNode.name} because of too many calls`);
			return false;
		}

		if (directParent.type === 'ExpressionStatement') {
			// 没有返回值的函数调用，用函数体替换整个表达式语句
			// ExpressionStatement means that there's no assignment
			if (clonedBody.type === 'CompoundStatement') {
				// 将复合语句的内容插入到父节点中
				if (parentOfParent && parentOfParent.statements) {
					result = doInsertInto(parentOfParent, null, true);	// Why not delete it? It is simply an expression statement...
				} else {
					console.log(`Internal Error: Unable to inline function '${funcNode.name} (E3)'`);
					return false;
				}
			}
			// Then delete the function call in the expression statement
			const index = directParent.children.indexOf(callNode);
			if (index !== -1) directParent.children.splice(index, 1);
		} else if (directParent.type === 'AssignmentExpression' || directParent.type === 'BinaryExpression'
			 || directParent.type === 'UnaryExpression' || directParent.type === 'VariableDeclarator'
			|| directParent.type === 'Declarator' || directParent.type === 'FunctionCall' || directParent.type === 'BuiltinCall') {
			// Also need this:
			if (clonedBody.type === 'CompoundStatement') {
				// 将复合语句的内容插入到父节点中
				if (parentOfParent && parentOfParent.statements) {
					// Should be already replaced inside the loop
					result = doInsertInto(parentOfParent, directParent, false);	// The statement can't be deleted!
				} else {
					console.log(`Internal Error: Unable to inline function '${funcNode.name} (E1)'`);
					return false;
				}
			}
			// Replacement already done
		} else if (directParent.type === 'CompoundStatement') {
			// Directly insert it? I should go for the call node!
			result = doInsertInto(callNode, null, true);
			// Delete the original statement inside
		} else {
			console.log(`Internal Error: Unable to inline function '${funcNode.name} (E4)'`);
			return false;
		}
		
		if (!result) {
			console.log(`Internal Error: Unable to inline function '${funcNode.name}' (E2)`);
			return false;
		}
		
		// 更新调用计数
		const funcInfo = this.functionInfo.get(funcNode.name);
		if (funcInfo) {
			funcInfo.calls--;
		}
		
		this.modified = true;
		this.logOptimization(`Inlined function '${funcNode.name}'`);
		return true;
	}

	/**
	 * 检查函数是否适合内联
	 * @remark This function is probably not needed
	 * @param {FunctionDeclarationNode} funcNode 
	 * @param {FunctionCallNode} callNode 
	 */
	isSuitableForInlining(funcNode, callNode, context) {
		// 检查参数数量
		if (funcNode.parameters.length !== callNode.arguments.length) {
			return false;
		}
		
		// 检查递归调用
		if (context.currentFunction === funcNode.name) {
			return false; // 递归调用不内联
		}
		// Size is checked elsewhere
		
		return true;
	}

	/**
	 * 
	 * @param {ASTNode} node 
	 * @param {Map<string, ASTNode>} paramMap 
	 * @param {FunctionDeclarationNode} funcNode 
	 * @param {Scope} symbolMapper
	 * @returns 
	 */
	replaceParametersInAST(node, paramMap, funcNode, symbolMapper) {
		if (!node || !paramMap) return;
		
		const doReplacement = node => {
			switch (node.type) {
				case 'Identifier':
					if (node.symbol && node.symbol.kind === 'parameter') {
						const replacement = paramMap.get(node.name);
						if (replacement) {
							const clonedReplacement = this.cloneAST(replacement);
							clonedReplacement.location = node.location;
							clonedReplacement.symbol = symbolMapper.lookup(replacement.name);
							/*
							Renamed in place rather than swapped into its parent:
							a clone keeps the ORIGINAL's `parent`, so `replaceNode`
							edited the function's own body -- and inlining the
							function a second time then renamed the original's
							parameters, leaving the newer copy referring to names
							that belong to the earlier inlining.
							*/
							this.replaceNode(node, clonedReplacement);
						}
					}
					break;
			}
		};

		doReplacement(node);

		const tsrc = node.getAttribute('totalSource');
		if (tsrc && tsrc.type === 'Identifier') {
			/*
			Attributes are shared with the node this was cloned from, so `tsrc`
			may well be the ORIGINAL function's identifier. Replacing it would
			rename the function's own body -- and inlining it a second time then
			produced a copy referring to the parameters of the FIRST inlining.
			Only this node's attribute is updated.
			*/
			const tsrcReplacement = paramMap.get(tsrc.name);
			if (tsrcReplacement) {
				const clonedReplacement = this.cloneAST(tsrcReplacement);
				clonedReplacement.location = tsrc.location;
				clonedReplacement.symbol = symbolMapper.lookup(tsrcReplacement.name);
				node.setAttribute('totalSource', clonedReplacement);
			}
		}
		
		// 递归处理子节点
		if (node.children) {
			node.children.forEach(child => this.replaceParametersInAST(child, paramMap, funcNode, symbolMapper));
		}
		
		// 处理特定字段
		const fieldsToCheck = [
			'functions', 'globalDeclarations', 'typeDefinitions',
			'statements', 'expression', 'test', 'consequent', 'alternate',
			'body', 'init', 'update', 'argument', 'left', 'right',
			'declarators', 'arguments', 'callee', 'initializer'
		];
		
		for (const field of fieldsToCheck) {
			if (Array.isArray(node[field])) {
				node[field].forEach(item => {
					if (item && typeof item === 'object') {
						this.replaceParametersInAST(item, paramMap, funcNode, symbolMapper);
					}
				});
			} else if (node[field] && typeof node[field] === 'object') {
				this.replaceParametersInAST(node[field], paramMap, funcNode, symbolMapper);
			}
		}
	}

	/**
	 * 提取并替换返回语句
	 * @returns {ASTNode|undefined} 返回值，如果没有返回语句则返回undefined
	 */
	extractAndReplaceReturns(node, parentNode, callNode) {
		let returnValue = undefined;
		let hasReturn = false;
		
		const processNode = (currentNode, currentParent) => {
			if (!currentNode) return;
			
			if (currentNode.type === 'ReturnStatement') {
				hasReturn = true;
				if (currentNode.argument) {
					returnValue = this.cloneAST(currentNode.argument);
				}
				
				// 删除返回语句
				if (currentParent && currentParent.statements) {
					const index = currentParent.statements.indexOf(currentNode);
					if (index !== -1) {
						currentParent.statements.splice(index, 1);
					}
				}
				return true; // 停止处理
			}
			
			// 递归处理子节点
			if (currentNode.children) {
				for (let i = 0; i < currentNode.children.length; i++) {
					const child = currentNode.children[i];
					if (processNode(child, currentNode)) {
						return true;
					}
				}
			}
			
			return false;
		};
		
		processNode(node, null);
		
		// 如果没有返回语句，检查是否需要创建void返回值
		if (!hasReturn && callNode.parent.type === 'ExpressionStatement') {
			returnValue = null; // 表示没有返回值
		}
		
		return returnValue;
	}

	/**
	 * 检查函数是否有副作用
	 */
	functionHasSideEffects(funcNode) {
		/*
		let hasSideEffects = false;
		
		const checkNode = (node) => {
			if (!node) return;
			
			switch (node.type) {
				case 'FunctionCall':
				case 'BuiltinCall':
					return true;
//				case 'AssignmentExpression':
				case 'UnaryExpression':
					if (node.operator === '++' || node.operator === '--') {
						hasSideEffects = true;
					}
					break;
			}
			
			if (node.children) {
				node.children.forEach(child => checkNode(child));
			}
		};
		
		checkNode(funcNode.body);
		return hasSideEffects;
		*/
		if (funcNode.body) {
			return this.hasSideEffects(funcNode.body);
		} else {
			return false;
		}
	}

	/**
	 * 清理全局声明 (Probably unused)
	 */
	cleanupGlobalDeclarations() {
		if (!this.ast || this.ast.type !== 'Program') return;
		
		// 清理空的变量声明
		if (this.ast.globalDeclarations) {
			const originalLength = this.ast.globalDeclarations.length;
			this.ast.globalDeclarations = this.ast.globalDeclarations.filter(decl => {
				if (decl.type !== 'VariableDeclaration') return true;
				
				// 检查声明符
				if (!decl.declarators || decl.declarators.length === 0) {
					this.modified = true;
					this.logOptimization('Removed empty global variable declaration');
					return false;
				}
				
				// 检查是否有未使用的声明符
				const usefulDeclarators = decl.declarators.filter(declarator => {
					if (!declarator.name) return false;
					
					// 检查是否被使用
					const symbol = this.globalScope.lookup(declarator.name);
					if (!symbol) return true;
					
					if (symbol.readCount === 0 && symbol.writeCount === 0) {
						// 未使用的变量
						if (declarator.initializer && this.hasSideEffects(declarator.initializer)) {
							// 有副作用的初始化，需要保留
							return true;
						}
						return false;
					}
					
					return true;
				});
				
				if (usefulDeclarators.length === 0) {
					this.modified = true;
					this.logOptimization(`Removed global variable declaration with no used variables`);
					return false;
				}
				
				if (usefulDeclarators.length !== decl.declarators.length) {
					decl.declarators = usefulDeclarators;
					this.modified = true;
				}
				
				return true;
			});
			
			if (this.ast.globalDeclarations.length !== originalLength) {
				this.modified = true;
			}
		}
		
		// 清理未使用的类型定义
		if (this.ast.typeDefinitions) {
			const originalLength = this.ast.typeDefinitions.length;
			this.ast.typeDefinitions = this.ast.typeDefinitions.filter(typeDef => {
				if (!typeDef.name) return true; // 匿名类型定义，保留
				
				// 检查类型是否被使用
				const typeName = typeDef.name;
				const isTypeUsed = this.isTypeUsed(typeName);
				
				if (!isTypeUsed && typeDef.type !== 'TypedefDeclaration') {
					this.modified = true;
					// this.addWarning(`Removed unused type definition '${typeName}'`);
					return false;
				}
				
				return true;
			});
			
			if (this.ast.typeDefinitions.length !== originalLength) {
				this.modified = true;
			}
		}
		
		// 清理空函数声明（没有函数体）
		if (this.ast.functions) {
			const originalLength = this.ast.functions.length;
			this.ast.functions = this.ast.functions.filter(func => {
				if (!func.body && func.name !== 'main') {
					// 函数声明（不是定义），可以删除
					this.modified = true;
					this.logOptimization(`Removed unused function declaration '${func.name}'`);
					return false;
				}
				
				return true;
			});
			
			if (this.ast.functions.length !== originalLength) {
				this.modified = true;
			}
		}
	}

	/**
	 * 检查类型是否被使用
	 */
	isTypeUsed(typeName) {
		let isUsed = false;
		
		const checkNode = (node) => {
			if (!node || isUsed) return;
			
			// 检查类型说明符
			if (node.type === 'TypeSpecifier' && node.typeName === typeName) {
				isUsed = true;
				return;
			}
			
			// 检查标识符（可能是typedef定义的类型）
			if (node.type === 'Identifier' && node.name === typeName) {
				// 需要检查上下文，确认是类型使用而不是变量使用
				// 简化处理：假设所有匹配都是类型使用
				isUsed = true;
				return;
			}
			
			// 递归检查子节点
			if (node.children) {
				for (const child of node.children) {
					if (isUsed) break;
					checkNode(child);
				}
			}
			
			// 检查特定字段
			const fieldsToCheck = [
				'functions', 'globalDeclarations', 'typeDefinitions',
				'statements', 'expression', 'test', 'consequent', 'alternate',
				'body', 'init', 'update', 'argument', 'left', 'right',
				'declarators', 'arguments', 'callee', 'initializer'
			];
			
			for (const field of fieldsToCheck) {
				if (isUsed) break;
				
				if (Array.isArray(node[field])) {
					for (const item of node[field]) {
						if (isUsed) break;
						if (item && typeof item === 'object') {
							checkNode(item);
						}
					}
				} else if (node[field] && typeof node[field] === 'object') {
					checkNode(node[field]);
				}
			}
		};
		
		checkNode(this.ast);
		return isUsed;
	}

	/**
	 * 克隆AST节点
	 */
	/**
	 * Point every node of a CLONED subtree at its parent WITHIN that subtree.
	 *
	 * `cloneAST` copies `parent` from the original, so a clone's root still
	 * points into the original tree. That leaks: `replaceNode` edits
	 * `oldNode.parent`, so inlining a function a SECOND time renamed the
	 * identifiers of the ORIGINAL body -- and the second copy then referred to
	 * the parameters of the first inlining (names belonging to a scope that is
	 * not an ancestor of it), which failed as "Unknown variable".
	 *
	 * @param {any} root
	 * @param {any?} [parent]
	 * @returns {any} root
	 */
	reparentCloned(root, parent = null) {
		const seen = new Set();
		const skip = new Set(['parent', 'scope', 'symbol', '_attributes', 'dataType', 'location']);
		const fix = (node, from) => {
			if (!node || typeof node !== 'object' || seen.has(node)) return;
			seen.add(node);
			node.parent = from;
			const visit = child => {
				if (!child || typeof child !== 'object') return;
				if (Array.isArray(child)) { child.forEach(visit); return; }
				if (typeof child.type !== 'string') return;
				fix(child, node);
			};
			for (const key of Object.keys(node)) {
				if (skip.has(key)) continue;
				visit(node[key]);
			}
		};
		fix(root, parent);
		return root;
	}

	cloneAST(node) {
		if (!node) return null;
		// console.log("Now cloning AST",node);	// debug ...
		
		// 创建新节点
		const clone = new ASTNode(node.type, node.location);
		clone.parent = node.parent;
		clone.scope = node.scope;
		clone.symbol = node.symbol;

		// 复制属性
		for (const [key, value] of node._attributes) {
			clone.setAttribute(key, value);
		}
		
		// 复制特定字段
		switch (node.type) {
			case 'Program':
				break;
				
			case 'FunctionDeclaration':
				clone.name = node.name;
				clone.isInline = node.isInline || false;
				break;
				
			case 'VariableDeclaration':
				clone.type = node.type ? this.cloneAST(node.type) : null;
				clone.storageClass = node.storageClass;
				break;
				
			case 'VariableDeclarator':
			case 'Declarator':
				clone.name = node.name;
				break;
				
			case 'Identifier':
				clone.name = node.name;
				break;
				
			case 'NumericLiteral':
				clone.value = node.value;
				clone.raw = node.raw;
				break;
				
			case 'StringLiteral':
				clone.value = node.value;
				clone.raw = node.raw;
				break;
				
			case 'AssignmentExpression':
			case 'BinaryExpression':
				clone.operator = node.operator;
				break;
				
			case 'UnaryExpression':
				clone.operator = node.operator;
				clone.prefix = node.prefix !== undefined ? node.prefix : true;
				break;
				
			case 'IfStatement':
				break;
				
			case 'WhileStatement':
				break;
				
			case 'ForStatement':
				break;
				
			case 'ReturnStatement':
				break;
				
			case 'CompoundStatement':
				break;
				
			case 'ExpressionStatement':
				break;
				
			case 'FunctionCall':
				break;
				
			case 'BuiltinCall':
				clone.functionName = node.functionName;
				break;
				
			case 'MemberExpression':
				clone.setAttribute('computed', node.getAttribute('computed') || false);
				break;
				
			default:
				// 默认处理：复制子节点
				break;
		}

		if (node.children) {
			node.children.forEach(child => clone.addChild(this.cloneAST(child)));
		}
		
		const fieldsToCheck = [
				'functions', 'globalDeclarations', 'typeDefinitions', 'returnType',
				'statements', 'expression', 'test', 'consequent', 'alternate', 'parameters',
				'body', 'init', 'update', 'argument', 'left', 'right',
				'declarators', 'arguments', 'callee', 'initializer'
			];
			
		for (const field of fieldsToCheck) {
			if (Array.isArray(node[field])) {
				clone[field] = node[field].map(elem => {
					const result = this.cloneAST(elem);
					result.parent = clone;
					return result;
				});
			} else if (node[field] && typeof node[field] === 'object') {
				clone[field] = this.cloneAST(node[field]);
				clone[field].parent = clone;
			}
		}

		clone.dataType = node.dataType;
		
		return clone;
	}

	/**
	 * 移除死代码
	 * @param {ASTNode} node - 要处理的AST节点
	 * @returns {boolean} - 返回是否进行了修改
	 */
	removeDeadCode(node) {
		if (!node) return false;
		
		let modified = false;
		
		switch (node.type) {
			case 'CompoundStatement':
				modified = this.removeDeadCodeInCompoundStatement(node);
				break;
				
			case 'IfStatement':
				modified = this.removeDeadCodeInIfStatement(node);
				break;
				
			case 'WhileStatement':
			case 'DoWhileStatement':
				modified = this.removeDeadCodeInLoopStatement(node);
				break;
				
			case 'ForStatement':
				modified = this.removeDeadCodeInForStatement(node);
				break;
				
			case 'FunctionDeclaration':
				if (node.body) {
					modified = this.removeDeadCode(node.body);
				}
				break;
				
			default:
				// 递归处理子节点
				modified = this.recursivelyRemoveDeadCode(node);
				break;
		}
		
		// 如果修改了当前节点，可能需要进一步处理
		if (modified) {
			this.modified = true;
		}
		
		return modified;
	}

	/**
	 * 递归移除死代码
	 * @param {ASTNode} node - 要处理的AST节点
	 * @returns {boolean} - 返回是否进行了修改
	 */
	recursivelyRemoveDeadCode(node) {
		if (!node) return false;
		
		let modified = false;
		
		// 根据节点类型处理不同的子节点
		switch (node.type) {
			case 'Program':
				// 处理函数和全局声明
				if (node.functions) {
					node.functions.forEach(func => {
						if (this.removeDeadCode(func)) {
							modified = true;
						}
					});
				}
				if (node.globalDeclarations) {
					node.globalDeclarations.forEach(decl => {
						if (this.removeDeadCode(decl)) {
							modified = true;
						}
					});
				}
				break;
				
			case 'ExpressionStatement':
				// ! Manually modified for experssion statement !
				node.children.forEach(expression => {
					modified = modified || this.recursivelyRemoveDeadCode(expression);
				});
				break;
				
			case 'VariableDeclaration':
				// 变量声明没有需要删除的死代码，但可能需要检查初始化器
				if (node.declarators) {
					node.declarators.forEach(declarator => {
						if (declarator.initializer) {
							if (this.recursivelyRemoveDeadCode(declarator.initializer)) {
								modified = true;
							}
						}
					});
				}
				break;
				
			case 'BinaryExpression':
				modified = this.recursivelyRemoveDeadCode(node.left) || 
						   this.recursivelyRemoveDeadCode(node.right);
				break;
				
			case 'UnaryExpression':
				modified = this.recursivelyRemoveDeadCode(node.argument);
				break;
				
			case 'AssignmentExpression':
				modified = this.recursivelyRemoveDeadCode(node.left) || 
						   this.recursivelyRemoveDeadCode(node.right);
				break;
				
			case 'FunctionCall':
			case 'BuiltinCall':
				if (node.callee) {
					modified = this.recursivelyRemoveDeadCode(node.callee);
				}
				if (node.arguments) {
					node.arguments.forEach(arg => {
						if (this.recursivelyRemoveDeadCode(arg)) {
							modified = true;
						}
					});
				}
				break;
				
			case 'MemberExpression':
				// 处理对象和属性
				if (node.object) {
					modified = this.recursivelyRemoveDeadCode(node.object);
				}
				if (node.property) {
					modified = modified || this.recursivelyRemoveDeadCode(node.property);
				}
				break;
				
			default:
				// 对于其他节点，递归处理所有子节点
				if (node.children) {
					node.children.forEach(child => {
						if (this.recursivelyRemoveDeadCode(child)) {
							modified = true;
						}
					});
				}
				break;
		}
		
		return modified;
	}

	/**
	 * 移除复合语句中的死代码
	 * @param {ASTNode} compoundStmt - 复合语句节点
	 * @returns {boolean} - 返回是否进行了修改
	 */
	removeDeadCodeInCompoundStatement(compoundStmt) {
		if (!compoundStmt || !compoundStmt.statements) return false;
		
		let modified = false;
		let newStatements = [];
		let hasUnconditionalControlFlow = false;
		
		for (let i = 0; i < compoundStmt.statements.length; i++) {
			const stmt = compoundStmt.statements[i];
			
			// 如果已经有无条件控制流（如return、break、continue、goto），
			// 则后面的语句都是死代码
			if (hasUnconditionalControlFlow) {
				// 标记为已修改
				modified = true;
				this.logOptimization(`Removed dead code after unconditional control flow in compound statement`);
				continue;
			}
			
			// 检查当前语句是否是无条件控制流
			if (this.isUnconditionalControlFlow(stmt)) {
				hasUnconditionalControlFlow = true;
			}
			
			// 递归处理当前语句中的死代码
			if (this.removeDeadCode(stmt)) {
				modified = true;
			}
			
			// 如果语句仍然有效（没有被完全删除），则保留它
			if (!this.isNullStatement(stmt)) {
				newStatements.push(stmt);
			} else {
				modified = true;
			}
		}
		
		// 更新语句列表
		if (modified) {
			compoundStmt.statements = newStatements;
		}
		
		return modified;
	}

	/**
	 * 移除if语句中的死代码
	 * @param {ASTNode} ifStmt - if语句节点
	 * @returns {boolean} - 返回是否进行了修改
	 */
	removeDeadCodeInIfStatement(ifStmt) {
		if (!ifStmt) return false;
		
		let modified = false;
		
		// 处理条件表达式
		if (ifStmt.test && this.recursivelyRemoveDeadCode(ifStmt.test)) {
			modified = true;
		}
		
		// 处理then分支
		if (ifStmt.consequent && this.removeDeadCode(ifStmt.consequent)) {
			modified = true;
		}
		
		// 处理else分支
		if (ifStmt.alternate && this.removeDeadCode(ifStmt.alternate)) {
			modified = true;
		}
		
		// 如果条件总是为真或为假，可以优化整个if语句
		/*
		const testValue = this.evaluateConstantExpression(ifStmt.test);
		if (testValue !== undefined) {
			if (testValue) {
				// 条件为真，只保留then分支
				if (ifStmt.alternate) {
					// 删除else分支
					ifStmt.alternate = null;
					modified = true;
					this.logOptimization(`Removed else branch from always-true if statement`);
				}
			} else {
				// 条件为假，只保留else分支（如果有）
				if (ifStmt.alternate) {
					// 用else分支替换整个if语句
					// 注意：这里我们只标记修改，实际替换在调用者中处理
					this.logOptimization(`If statement with always-false condition can be replaced with else branch`);
				} else {
					// 没有else分支，整个if语句都可以删除
					// 注意：这里我们只标记修改，实际删除在调用者中处理
					this.logOptimization(`If statement with always-false condition and no else branch can be removed`);
				}
			}
		}
		
		return modified;
		*/
		// A new, integrated version
		// The difference is that it uses cached value and it has more explanations, probably
		// 使用分析阶段收集的信息进行优化
		const conditionValue = ifStmt.getAttribute('constantCondition');
		
		if (conditionValue !== undefined) {
			if (conditionValue) {
				// 条件总是为真
				if (ifStmt.alternate) {
					// 有else分支，可以删除else分支
					this.logOptimization(`Removing else branch from always-true if statement`);
					
					// 检查else分支是否有副作用
					const elseHasSideEffects = ifStmt.getAttribute('alternateHasSideEffects');
					if (elseHasSideEffects) {
						// else分支有副作用，不能完全删除，需要保留副作用
						this.preserveSideEffects(ifStmt.alternate);
					}
					
					// 删除else分支
					ifStmt.alternate = null;
					modified = true;
				}
				// 如果then分支没有副作用，整个if语句可能都可以删除
				// 但这需要谨慎，因为条件表达式可能有副作用
			} else {
				// 条件总是为假
				if (ifStmt.consequent) {
					// 有then分支，可以删除then分支
					this.logOptimization(`Removing then branch from always-false if statement`);
					
					// 检查then分支是否有副作用
					const thenHasSideEffects = ifStmt.getAttribute('consequentHasSideEffects');
					if (thenHasSideEffects) {
						// then分支有副作用，不能完全删除，需要保留副作用
						this.preserveSideEffects(ifStmt.consequent);
					}
					
					// 如果有else分支，将整个if语句替换为else分支
					if (ifStmt.alternate) {
						// 将if语句替换为else分支
						this.replaceNode(ifStmt, ifStmt.alternate);
						return true;
					} else {
						// 没有else分支，删除整个if语句
						// 但需要保留条件表达式的副作用（如果有）
						const conditionHasSideEffects = ifStmt.getAttribute('conditionHasSideEffects');
						if (conditionHasSideEffects) {
							// 条件有副作用，将if语句替换为条件表达式
							this.replaceNode(ifStmt, ifStmt.test);
							return true;
						} else {
							// 条件没有副作用，删除整个if语句
							this.replaceNode(ifStmt, null);
							return true;
						}
					}
				}
			}
		}
		
		return modified;
	}
	
		/**
	 * 保留语句的副作用
	 * @param {ASTNode} stmt - 要处理的语句
	 * @returns {ASTNode} - 保留副作用后的新节点
	 */
	preserveSideEffects(stmt) {
		if (!stmt) return null;
		
		// 检查语句是否有副作用 (Not necessary and result in errors)
		/*if (!this.hasSideEffects(stmt)) {
			return null;
		}*/
		
		switch (stmt.type) {
				
			case 'CompoundStatement':
				// 对于复合语句，提取所有有副作用的子语句
				const sideEffectStatements = [];
				if (stmt.statements) {
					stmt.statements.forEach(child => {
						const preserved = this.preserveSideEffects(child);
						if (preserved) {
							// 如果保留的是语句，直接添加
							if (preserved.type === 'CompoundStatement' && preserved.statements) {
								sideEffectStatements.push(...preserved.statements);
							} else {
								// 否则创建一个表达式语句
								const exprStmt = new ASTNode('ExpressionStatement');
								exprStmt.addChild(preserved);
								sideEffectStatements.push(exprStmt);
							}
						}
					});
				}
				
				if (sideEffectStatements.length === 0) {
					return null;
				} else if (sideEffectStatements.length === 1) {
					return sideEffectStatements[0].children[0] || sideEffectStatements[0];
				} else {
					const newCompound = ASTBuilder.compoundStatement();
					newCompound.statements = sideEffectStatements;
					return newCompound;
				}
				
			case 'IfStatement':
				// 对于if语句，需要保留条件和分支中的副作用
				const preservedTest = stmt.test ? stmt.test : null;
				const preservedConsequent = stmt.consequent ? this.preserveSideEffects(stmt.consequent) : null;
				const preservedAlternate = stmt.alternate ? this.preserveSideEffects(stmt.alternate) : null;
				
				// 创建一个新的if语句只包含副作用
				if (preservedTest && (preservedConsequent || preservedAlternate)) {
					const newIf = new ASTNode('IfStatement');
					newIf.test = preservedTest;
					newIf.consequent = preservedConsequent;
					newIf.alternate = preservedAlternate;
					return newIf;
				} else if (preservedTest) {
					// 只有条件有副作用
					return preservedTest;
				} else {
					return null;
				}
				
			default:
				// 对于其他类型的语句，保留整个语句
				return stmt;
		}
	}

	/**
	 * 移除循环语句中的死代码
	 * @param {ASTNode} loopStmt - 循环语句节点
	 * @returns {boolean} - 返回是否进行了修改
	 * @remark (See code)
	 */
		removeDeadCodeInLoopStatement(loopStmt) {
		if (!loopStmt) return false;
		
		let modified = false;
		
		// 处理条件表达式
		if (loopStmt.test && this.recursivelyRemoveDeadCode(loopStmt.test)) {
			modified = true;
		}
		
		// 处理循环体
		if (loopStmt.body && this.removeDeadCode(loopStmt.body)) {
			modified = true;
		}
		
		// 使用分析阶段收集的信息进行优化
		const conditionValue = loopStmt.getAttribute('constantCondition');
		const neverExecuted = loopStmt.getAttribute('neverExecuted');
		const infiniteLoop = loopStmt.getAttribute('infiniteLoop');
		
		if (neverExecuted) {
			// 循环永远不会执行
			this.logOptimization(`Removing never-executed loop`);
			
			// 检查初始化部分是否有副作用
			// 对于while循环，没有独立的初始化部分，但条件可能有副作用
			const conditionHasSideEffects = loopStmt.getAttribute('conditionHasSideEffects');
			if (conditionHasSideEffects) {
				// 条件有副作用，将循环替换为条件表达式
				this.replaceNode(loopStmt, loopStmt.test);
				return true;
			} else {
				// 条件没有副作用，删除整个循环
				this.replaceNode(loopStmt, null);
				return true;
			}
		}
		
		// Notice: constant condition doesn't mean infinity loop. According to current implementation, it means the circumstance of the initial scan!
		
		// 注意：对于无限循环，我们只是标记，不删除，因为无限循环可能是故意的
		// 但在代码生成阶段可以减少指令条数
		
		return modified;
	}

	removeDeadCodeInForStatement(forStmt) {
		if (!forStmt) return false;
		
		let modified = false;
		
		// 处理初始化部分
		if (forStmt.init && this.recursivelyRemoveDeadCode(forStmt.init)) {
			modified = true;
		}
		
		// 处理条件部分
		if (forStmt.test && this.recursivelyRemoveDeadCode(forStmt.test)) {
			modified = true;
		}
		
		// 处理更新部分
		if (forStmt.update && this.recursivelyRemoveDeadCode(forStmt.update)) {
			modified = true;
		}
		
		// 处理循环体
		if (forStmt.body && this.removeDeadCode(forStmt.body)) {
			modified = true;
		}
		
		// 使用分析阶段收集的信息进行优化
		const conditionValue = forStmt.getAttribute('constantCondition');
		const neverExecuted = forStmt.getAttribute('neverExecuted');
		const infiniteLoop = forStmt.getAttribute('infiniteLoop');
		const wasOptimized = forStmt.getAttribute('wasOptimized');
		const iterationCount = forStmt.getAttribute('iterationCount');
		
		if (neverExecuted) {
			// 循环永远不会执行
			this.logOptimization(`Removing never-executed for loop`);
			
			// 检查初始化和条件是否有副作用
			const initHasSideEffects = forStmt.getAttribute('initHasSideEffects');
			const conditionHasSideEffects = forStmt.getAttribute('conditionHasSideEffects');
			
			// 如果有副作用，需要保留它们
			if (initHasSideEffects || conditionHasSideEffects) {
				// 创建一个包含副作用的复合语句
				const compoundStmt = ASTBuilder.compoundStatement();
				
				// 添加初始化（如果有）
				if (forStmt.init) {
					compoundStmt.statements.push(forStmt.init);
				}
				
				// 添加条件（如果有副作用）
				if (conditionHasSideEffects && forStmt.test) {
					// 将条件转换为表达式语句
					const exprStmt = new ASTNode('ExpressionStatement');
					exprStmt.addChild(forStmt.test);
					compoundStmt.statements.push(exprStmt);
				}
				
				// 用复合语句替换循环
				this.replaceNode(forStmt, compoundStmt);
				return true;
			} else {
				// 没有副作用，删除整个循环
				this.replaceNode(forStmt, null);
				return true;
			}
		}
		
		// 如果循环已被优化，可以进一步简化
		if (wasOptimized && iterationCount !== undefined) {
			if (iterationCount === 1) {
				// 只有一次迭代，可以展开
				this.logOptimization(`Unrolling single-iteration for loop`);
				
				// 创建复合语句代替循环
				const compoundStmt = ASTBuilder.compoundStatement();
				
				// 添加初始化
				if (forStmt.init) {
					compoundStmt.statements.push(forStmt.init);
				}
				
				// 添加循环体（一次）
				if (forStmt.body) {
					if (forStmt.body.type === 'CompoundStatement') {
						// 直接添加循环体内的语句
						compoundStmt.statements.push(...forStmt.body.statements);
					} else {
						compoundStmt.statements.push(forStmt.body);
					}
				}
				
				// 添加更新（但只执行一次）
				if (forStmt.update) {
					const updateStmt = new ASTNode('ExpressionStatement');
					updateStmt.addChild(forStmt.update);
					compoundStmt.statements.push(updateStmt);
				}
				
				this.replaceNode(forStmt, compoundStmt);
				return true;
			}
		}
		
		// 对于无限循环，我们只是标记，不删除
		
		return modified;
	}
	
	// I guess this function has become useless:
	/**
	 * 检查语句是否是无条件控制流
	 * @param {ASTNode} stmt - 语句节点
	 * @returns {boolean} - 如果是无条件控制流则返回true
	 */
	isUnconditionalControlFlow(stmt) {
		if (!stmt) return false;
		
		switch (stmt.type) {
			case 'ReturnStatement':
				// return语句总是无条件控制流
				return true;
				
			case 'BreakStatement':
			case 'ContinueStatement':
				// break和continue在循环或switch中是控制流
				// 这里我们简化处理，认为它们总是控制流
				return true;
				
			case 'IfStatement':
				// if语句只有在两个分支都有控制流时才是无条件控制流
				if (stmt.alternate) {
					return this.hasUnconditionalControlFlow(stmt.consequent) && 
						   this.hasUnconditionalControlFlow(stmt.alternate);
				}
				return false;
				
			case 'CompoundStatement':
				// 复合语句只有在包含无条件控制流时才可能
				return this.hasUnconditionalControlFlowInCompound(stmt);
				
			default:
				return false;
		}
	}

	/**
	 * 检查节点是否包含无条件控制流
	 * @param {ASTNode} node - 节点
	 * @returns {boolean} - 如果包含无条件控制流则返回true
	 */
	hasUnconditionalControlFlow(node) {
		if (!node) return false;
		
		// 如果是无条件控制流语句
		if (this.isUnconditionalControlFlow(node)) {
			return true;
		}
		
		// 递归检查子节点
		switch (node.type) {
			case 'CompoundStatement':
				return this.hasUnconditionalControlFlowInCompound(node);
				
			case 'IfStatement':
				if (node.alternate) {
					return this.hasUnconditionalControlFlow(node.consequent) && 
						   this.hasUnconditionalControlFlow(node.alternate);
				}
				return false;
				
			default:
				// 检查所有子节点
				if (node.children) {
					for (const child of node.children) {
						if (this.hasUnconditionalControlFlow(child)) {
							return true;
						}
					}
				}
				return false;
		}
	}

	/**
	 * 检查复合语句中是否包含无条件控制流
	 * @param {ASTNode} compoundStmt - 复合语句节点
	 * @returns {boolean} - 如果包含无条件控制流则返回true
	 */
	hasUnconditionalControlFlowInCompound(compoundStmt) {
		if (!compoundStmt || !compoundStmt.statements) return false;
		
		for (const stmt of compoundStmt.statements) {
			if (this.hasUnconditionalControlFlow(stmt)) {
				return true;
			}
		}
		
		return false;
	}

	/**
	 * 检查是否为空语句
	 * @param {ASTNode} stmt - 语句节点
	 * @returns {boolean} - 如果是空语句则返回true
	 * @remark Modified about expression statement.
	 */
	isNullStatement(stmt) {
		if (!stmt) return true;
		
		// 空表达式语句
		if (stmt.type === 'ExpressionStatement' && !stmt.children) {
			return true;
		}
		
		// 空的复合语句
		if (stmt.type === 'CompoundStatement' && 
			(!stmt.statements || stmt.statements.length === 0)) {
			return true;
		}
		
		// 已经被标记为删除的节点
		if (stmt.type === 'DeletedStatement') {
			return true;
		}
		
		return false;
	}
	
		/**
	 * 清理空语句
	 * @param {ASTNode} node - 要处理的AST节点
	 * @returns {boolean} - 返回是否进行了修改
	 */
	cleanupEmptyStatements(node) {
		if (!node) return false;
		
		let modified = false;
		
		switch (node.type) {
			case 'CompoundStatement':
				if (node.statements) {
					const originalLength = node.statements.length;
					
					// 过滤掉空语句
					node.statements = node.statements.filter(stmt => !this.isNullStatement(stmt));
					
					if (node.statements.length !== originalLength) {
						modified = true;
						this.logOptimization(`Removed empty statements from compound statement`);
					}
					
					// 递归清理每个语句
					node.statements.forEach(stmt => {
						if (this.cleanupEmptyStatements(stmt)) {
							modified = true;
						}
					});
				}
				break;
				
			case 'IfStatement':
				modified = this.cleanupEmptyStatements(node.consequent) || 
						   (node.alternate ? this.cleanupEmptyStatements(node.alternate) : false);
				break;
				
			case 'WhileStatement':
			case 'DoWhileStatement':
				modified = this.cleanupEmptyStatements(node.body);
				break;
				
			case 'ForStatement':
				modified = this.cleanupEmptyStatements(node.body);
				break;
				
			default:
				// 递归处理子节点
				if (node.children) {
					node.children.forEach(child => {
						if (this.cleanupEmptyStatements(child)) {
							modified = true;
						}
					});
				}
				break;
		}
		
		return modified;
	}

	/**
	 * 
	 * @param {constantInfo} object 
	 */
	canHaveConstantEval(object) {
		return ['number', 'string'].includes(typeof object);
	}

}

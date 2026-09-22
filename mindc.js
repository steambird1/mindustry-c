/*
MinCCompiler on JS
By Seabird Starch Gunnhildr and Deepseek, 2025 - 2026.

WARNING and DISCLAIMER:

This code is only tested by some very small and simple samples
so far.
*/

import { ASTNodeType, AttributeClass, ASTNode, CompilationPhase,
	ProgramNode, FunctionDeclarationNode, FunctionCallNode, BuiltinCallNode,
	VariableDeclarationNode, VariableDeclaratorNode, InitializerListNode,
	BinaryExpressionNode, UnaryExpressionNode, AssignmentExpressionNode, 
	CastExpression, LogicalExpressionNode, ConditionalExpressionNode, 
	IfStatementNode, WhileStatementNode, ForStatementNode, ReturnStatementNode,
	CompoundStatementNode, IdentifierNode, NumericLiteralNode, StringLiteralNode,
	CharacterLiteralNode, NullLiteralNode, TypeSpecifierNode, TypedefDeclarationNode,
	StructDefinitionNode, UnionDefinitionNode, StructMemberNode,
	TypeQualifierNode, AsmStatementNode, PointerTypeNode, DeclaratorNode,
	DeletedStatement, ASTBuilder, ASTVisitor, __convert, objectList,
	liquidList, unitList, buildingList
 } from "./mindcBase.js";


import { TokenType, Token, KEYWORDS, SPECIAL_INSTRUCTIONS, OPERATORS,
	PUNCTUATORS, Lexer
 } from "./mindcLexer.js";

import { Parser } from "./mindcParser.js";

import { SymbolEntry, Scope, TypeInfo, MemberInfo, SemanticAnalyzer } from "./mindcSemantic.js";

import { Optimizer } from "./mindcOptimizer.js";

import { Instruction, SingleInstruction, InstructionBuilder, MemoryBlock, MemoryBlockInfo } from "./mindcGeneratorBase.js";
import { ContentIdTable } from "./mindcContent.js";

import { CodeGenerator } from "./mindcGenerator.js";

// 符号表条目
/**
 * @deprecated
 */
class SymbolTableEntry {
    constructor(name, type, scope, isConstant = false, value = null) {
        this.name = name;
        this.type = type;
        this.scope = scope;
        this.isConstant = isConstant;
        this.value = value;
        this.memoryLocation = null; // 在内存块中的位置
        this.isGlobal = false;
    }
}

// This function is in progress, and won't be done until I figure out how
// dependencies work in browsers on eart
export class CompilerExtensionBase {
	/**
	 * @callback returnTypeCallback
	 * @param {BuiltinCallNode} currentNode
	 * @returns {TypeInfo}
	 */
	/**
	 * 
	 * @typedef {{
	 * 		name: string,
	 * 		returnType: string,
	 * 		returnTypeEval: returnTypeCallback?,
	 * 		parameters: string[],
	 * 		hasVarArgs: boolean | undefined,
	 * 		special: string | null | undefined
	 * }} addinFunctionInfo
	 */
	/**
	 * @callback codeGenerationCallback
	 * @param {CodeGenerator} codeGenerationInst
	 * @param {BuiltinCallNode} callNode
	 * @returns {Instruction}
	 */
	/** 
	 * 
	 * @param {Map<string, TypeInfo>} types 
	 * @param {Map<string, addinFunctionInfo>} functions 
	 * @param {Map<string, codeGenerationCallback>} handlers
	 */
	constructor(types, functions, handlers) {
		this.types = types;
		this.functions = functions;
		this.handlers = handlers;
	}
}

// Example extension
// (This is just an example and won't be included in the compiler...)
/**
 * @deprecated
 */
const exampleExtension = new CompilerExtensionBase(
	new Map(	// Define types like this
		[['test_t', new TypeInfo('test_t', 'basic', 1)]]
		// Note: if you type is larger, it is recommended to label them as struct to enable automatic copying
	),
	new Map(	// Declare function signatures like this (they will be regarded as builtin calls!)
		[['hello', {
			name: 'hello',
			returnType: 'int',
			parameters: ['int', 'int']
		}]]
	),
	new Map(	// Declare builtin call processor like this
		[['hello', 
			/**
			 * 
			 * @param {CodeGenerator} cg
			 * @param {BuiltinCallNode} ast 
			 */
			(cg, ast) => {
				return cg.operates(
					cg.operatesReads(
						ast.arguments.map(arg => cg.visitAndRead(arg)),
						InstructionBuilder.op('add', '{op}', '{op_r0}', '{op_r1}')
					),
					cg.getTempSymbol(cg.semantic.getTypeInfo('int'))
				);
			}
		]]
	)
);

// Here are default extensions
// THIS IS STILL IN PROGRESS!
/**
 * Pseudo list type (so that you can store device/content_t!)
 * The pseudo list uses binary search.
 */
export const pseudoList = new CompilerExtensionBase(
	new Map(
		/**
		 * pdlist_t: The handler type of pseudo list.
		 * It is integers (currently) like a function pointer, referring to
		 * the actual operator.
		 * 
		 * btw It is a little foolish to use a struct to contain such structure
		 * (especially when this has become an alternative when no memory block is available.)
		 * So we use A * 16384 + B to store the function reference (A reading, B writing)
		 * 
		 * fyi: for a 60-element array the operations by theory takes approx 0.75 s
		 */
		[['pdlist_t', new TypeInfo('pdlist_t', 'basic', 1)]]
	),
	new Map(
		[['pdcreate', {
			name: 'pdcreate',
			returnType: 'pdlist_t',
			parameters: ['int']					// pdlist_t pdcreate(int size)
		}],
		['pdread', {
			name: 'pdread',
			returnType: 'null_t',
			parameters: ['pdlist_t', 'int']		// null_t pdread(pdlist_t list, int pos)
		}],
		['pdwrite', {
			name: 'pdwrite',
			returnType: 'void',
			parameters: ['pdlist_t', 'int', 'null_t']	// void pdwrite(pdlist_t list, int pos, null_t data)
		}]]
	),
	new Map(
		[['pdcreate',
			/**
			 * 
			 * @param {CodeGenerator} cg 
			 * @param {BuiltinCallNode} ast 
			 */
			(cg, ast) => {
				if (ast.arguments.length != 1 || ast.arguments[0].type !== 'NumericLiteral') {
					throw new InternalGenerationFailure(`Parameter 1 of pdcreate() must be an integer literal`, ast.location);
				}
				/**
				 * @type {number}
				 */
				const arraySize = ast.arguments[0].value;
				if (arraySize <= 0) {
					throw new InternalGenerationFailure(`Inappropriate array size`, ast.location)
				}
				
				const randomIdentifier = Math.floor(10000 * Math.random()) + 32768;
				/**
				 * 
				 * @param {number} l 
				 * @param {number} r 
				 * @param {Instruction} action Action to be done if reaching that value, with relevant number labelled as "{sval}"
				 * @returns {Instruction} Note: this returns with {exit}.
				 * @remarks This has {input} and {exit}.
				 */
				const generateOperations = (l, r, action) => {
					
					const actionSize = action.size() + 1;
					let result = new Instruction([
						InstructionBuilder.op('mul', '__tmpref', '{input}', actionSize),
						InstructionBuilder.op('add', '@counter', '@counter', '__tmpref')
					]);
					for (let i = l; i <= r; i++) {
						result.concat(action.duplicate().raw_replace('sval', `${i}`));
						result.concat(new SingleInstruction({
							content: '{exit}',
							referrer: []
						}));
					}
					return result;
					// Implemented as binary search (deleted)
					/*
					if (l > r) {
						return new SingleInstruction({
							content: '{exit}',
							referrer: []
						});
					}
					else if (l == r) {
						return action.duplicate().raw_replace('sval', l).concat(new SingleInstruction({
							content: '{exit}',
							referrer: []
						}));
					} else {
						let result = new Instruction();
						let mid = Math.floor((l+r)/2);
						const tgtName = `tgt_${l}_${r}_at${randomIdentifier}`;
						const jumper = InstructionBuilder.jump(`{${tgtName}}`, 'lessThanEq', '{input}', mid);
						result.concat(jumper);
						// Condition of 'greater'
						result.concat(generateOperations(mid+1, r, action));
						result.concat(new InstructionReferrer(jumper, tgtName));
						result.concat(generateOperations(l, mid, action));
						return result;
					}
						*/
				};
				const funcManager = cg.functionManagement;

				const readerName = `__ps_read_${randomIdentifier}`;
				const writerName = `__ps_write_${randomIdentifier}`;
				const tmpReturner = cg.getTempSymbol(cg.semantic.getTypeInfo('pdlist_t'));
				const returnedObject = new Instruction([
					InstructionBuilder.op('mul', '__putmp', `_${readerName}`, 16384),
					InstructionBuilder.op('add', '__putmp', '__putmp', `_${writerName}`),
					cg.generateSymbolWrite(tmpReturner, '__putmp')
				]).set_returns(tmpReturner);

				const reader = generateOperations(0, arraySize - 1, InstructionBuilder.set('__return', `_pu_${randomIdentifier}_{sval}`))
						.raw_replace('input', '__pos').raw_replace('exit', 
							InstructionBuilder.set('@counter', '__stackpos').content
						);
				funcManager.addFunction(readerName, reader, null, [], true);
				const writer = generateOperations(0, arraySize - 1, InstructionBuilder.set(`_pu_${randomIdentifier}_{sval}`, '__value'))
						.raw_replace('input', '__pos').raw_replace('exit',
							InstructionBuilder.set('@counter', '__stackpos').content
						);
				funcManager.addFunction(writerName, writer, null, [], true);
				// Create a local variable (but the problem is: how to get function info?):
				
				return returnedObject;
			}
		],
		['pdread',
			/**
			 * 
			 * @param {CodeGenerator} cg 
			 * @param {BuiltinCallNode} ast 
			 */
			(cg, ast) => {
				const tmpSymb = cg.getTempSymbol(cg.semantic.getTypeInfo('int'));
				return cg.operatesReads(
					[cg.visit(ast.arguments[0]), cg.visit(ast.arguments[1])],
					new Instruction([
						// Evaluate reader part
						InstructionBuilder.set('__pos', '{op_r1}'),
						InstructionBuilder.op('shr', '__funcpos', '{op_r0}', 14),
						InstructionBuilder.set('__internal_stackpos', '__stackpos'),
						InstructionBuilder.op('add', '__stackpos', '@counter', 1),
						InstructionBuilder.set('@counter', '__funcpos'),
						InstructionBuilder.set('__stackpos', '__internal_stackpos'),
						cg.generateSymbolWrite(tmpSymb, '__return')
					]).set_returns(tmpSymb)
				);
			}
		],
		['pdwrite',
			/**
			 * 
			 * @param {CodeGenerator} cg 
			 * @param {BuiltinCallNode} ast 
			 */
			(cg, ast) => {
				return cg.operatesReads(
					[cg.visit(ast.arguments[0]), cg.visit(ast.arguments[1]), cg.visit(ast.arguments[2])],
					new Instruction([
						InstructionBuilder.set('__pos', '{op_r1}'),
						InstructionBuilder.set('__value', '{op_r2}'),
						InstructionBuilder.op('and', '__funcpos', '{op_r0}', 16383),
						InstructionBuilder.set('__internal_stackpos', '__stackpos'),
						InstructionBuilder.op('add', '__stackpos', '@counter', 1),
						InstructionBuilder.set('@counter', '__funcpos'),
						InstructionBuilder.set('__stackpos', '__internal_stackpos')
					], 'null')
				);
			}
		]]
	)
);
// End.

/**
 * @type {CompilerExtensionBase[]}
 */
export const defaultExtensions = [pseudoList];

// 主编译器接口
export class Compiler {
	/**
	 * 
	 * @param {List<Object>} memoryInfo 
	 * @param {AttributeClass} config 
	 * @param {CompilerExtensionBase[]} extensions (Currently not working!!!)
	 */
    constructor(sourceCode, memoryInfo, config, extensions = defaultExtensions) {

		this.embeddedExtensions = [...extensions];
		this.config = config ?? new AttributeClass();
		if (!this.config.hasAttribute('targetVersion')) {
			this.config.setAttribute('targetVersion', 154.3);
		}

		this.memoryInfo = memoryInfo;
		
        this.lexer = new Lexer(sourceCode);
		let parserConfig = new AttributeClass();
		parserConfig.setAttribute('extraTypes', []);
        this.parser = new Parser(this.lexer, parserConfig);
        this.semanticAnalyzer = new SemanticAnalyzer(this);
        this.optimizer = new Optimizer(this);
        this.codeGenerator = new CodeGenerator(this);
		this.extensions = extensions;

    }
    
    async compile() {
        try {
            // 编译管道
            const tokens = await this.lexer.tokenize();

			// Handle extensions here now
			const extensions = [...this.embeddedExtensions, ...tokens.extraExtensions];
			const exTypeNames = extensions.flatMap(ext => [...ext.types].map(([name, type]) => name));
			const exTypes = extensions.flatMap(ext => [...ext.types].map(([name, type]) => type));
			const exFuncNames = extensions.flatMap(ext => [...ext.functions].map(([name, func]) => name));
			const exFuncs = extensions.flatMap(ext => [...ext.functions].map(([name, func]) => func));

			if (exTypeNames.length != new Set(exTypeNames).size) {
				throw new InternalGenerationFailure('Ambiguous type name in imported extensions');
			}

			if (exFuncNames.length != new Set(exFuncNames).size) {
				throw new InternalGenerationFailure('Ambiguous function name in imported extensions');
			}
			this.extensions = extensions;
			let parserConfig = new AttributeClass();
			parserConfig.setAttribute('extraTypes', exTypes);
			parserConfig.setAttribute('extraFunctions', exFuncs);
			parserConfig.setAttribute('extraHandler', new Map(extensions.flatMap(ext => [...ext.handlers])));
			const optimizerConfig = {};
			/** @type {ContentIdTable} `#pragma id` / `#pragma var` definitions */
			const contentTable = new ContentIdTable();
			tokens.pragmas.forEach(
				/**
				 * 
				 * @param {string} info 
				 */
				infos => {
					const info = infos.substring(infos.indexOf('#pragma') + '#pragma'.length).trim();
					if (info.startsWith("comment(")) {
						// #pragma comment(attribute,value)
						const comment = info.substring(info.indexOf('(') + 1, info.lastIndexOf(')'));
						const config = comment.split(',').map(s => s.trim());
						if (config.length < 2) return;
						parserConfig.setAttribute(config[0], JSON.parse(config[1]));
					} else if (info.startsWith("optimize(")) {
						/*
						#pragma optimize(array[, mutable_optimize[, const_optimize]])
						#pragma optimize(pointer)          -- name pointers, off by default
						*/
						const comment = info.substring(info.indexOf('(') + 1, info.lastIndexOf(')'));
						const config = comment.split(',').map(s => s.trim());
						switch (config[0]) {
							case "array":
								optimizerConfig.doArrayOptimize = true;
								if (config.length >= 2) {
									optimizerConfig.arrayOptimize = JSON.parse(config[1]);
								}
								if (config.length >= 3) {
									optimizerConfig.constArrayOptimize = JSON.parse(config[2]);
								}
								break;
							case "pointer":
								// Pointers may store the NAME of what they point at.
								// Off unless asked for: it needs v150 and forbids
								// pointer arithmetic on the affected pointers.
								optimizerConfig.doPointerOptimize = true;
								break;
							case "struct_pointer":
								/*
								A pointer to a struct may carry the NAME of every
								member instead of an address.
								  #pragma optimize(struct_pointer)      -- any size
								  #pragma optimize(struct_pointer, 4)   -- up to 4
									 registers; larger structs stay in memory
								With no number the threshold is 4 when memory is
								available and unbounded when it is not (then there
								is nowhere else to put them anyway).
								*/
								optimizerConfig.doStructPointerOptimize = true;
								if (config.length >= 2) {
									const parsed = Number(String(config[1]).trim());
									if (Number.isFinite(parsed)) optimizerConfig.structPointerThreshold = parsed;
								}
								break;
						}
					} else if (info.startsWith("overload")) {
						/*
						#pragma overload(auto|default|none)
						Reserved for function overloading: how the compiler may
						react to several functions sharing a name, and to an
						`auto` parameter (one version of the function per
						representation of the argument). None of it is
						implemented yet -- recording the mode is what lets the
						semantic analyzer reject it with a clear message
						instead of silently ignoring it.
						*/
						const comment = info.substring(info.indexOf('(') + 1, info.lastIndexOf(')'));
						const mode = String(comment).trim();
						if (!['auto', 'default', 'none'].includes(mode)) {
							throw new InternalGenerationFailure(
								`#pragma overload expects auto, default or none, but got '${mode}'`);
						}
						this.overloadMode = mode;
					} else if (info.startsWith("noinline")) {
						/*
						#pragma noinline        -- never inline anything automatically
						#pragma noinline(fn)    -- never inline `fn` automatically
						The `inline` keyword outranks both. The names are collected
						in a LIST because other things may forbid automatic
						inlining too, without going through a pragma.
						*/
						const rest = info.substring("noinline".length).trim();
						optimizerConfig.noInlineList ??= [];
						if (rest === '') {
							optimizerConfig.noInlineAll = true;
						} else if (rest.startsWith('(')) {
							const body = rest.substring(1, rest.lastIndexOf(')'));
							body.split(',').map(t => t.trim()).filter(Boolean)
								.forEach(name => optimizerConfig.noInlineList.push(name));
						}
					} else if (info.startsWith("stackunderflow(")) {
						/*
						#pragma stackunderflow(fn) -- call `fn` when the stack is
						                              walked below the first block
						#pragma stackunderflow()   -- no check (this is the default)
						*/
						const handler = info.substring(info.indexOf('(') + 1, info.lastIndexOf(')')).trim();
						optimizerConfig.stackUnderflow = handler === '' ? false : handler;
					} else if (info.startsWith("stackcheck(")) {
						/*
						#pragma stackcheck(fn) -- call `fn` when the stack frame cannot grow
						#pragma stackcheck()   -- do not check the capacity at all
						Without the pragma: write a fixed message to __error and stop.
						*/
						const handler = info.substring(info.indexOf('(') + 1, info.lastIndexOf(')')).trim();
						optimizerConfig.stackCheck = handler === '' ? false : handler;
					} else if (info.startsWith("id(")) {
						/*
						#pragma id(item, @myitem:300)   -- give `@myitem` the id 300
						#pragma id(unit, @oldname:null) -- the name has no id
						These win over the built-in table.
						*/
						const body = info.substring(info.indexOf('(') + 1, info.lastIndexOf(')'));
						const parts = body.split(',').map(t => t.trim());
						if (parts.length >= 2) {
							const spec = parts.slice(1).join(',');
							const at = spec.indexOf('@');
							const colon = spec.lastIndexOf(':');
							if (at >= 0 && colon > at) {
								const name = spec.substring(at, colon).trim();
								const raw = spec.substring(colon + 1).trim();
								const id = (raw === 'null' || raw === '') ? null : Number(raw);
								contentTable.define(parts[0], name,
									(id === null || Number.isFinite(id)) ? id : null);
							}
						}
					} else if (info.startsWith("var(")) {
						/*
						#pragma var(int, @myVar) -- a processor variable, like @this
						*/
						const body = info.substring(info.indexOf('(') + 1, info.lastIndexOf(')'));
						const parts = body.split(',').map(t => t.trim());
						if (parts.length >= 2) {
							const name = parts[1];
							if (name.startsWith('@')) contentTable.defineVariable(name, parts[0]);
						}
					}
			});
			parserConfig.setAttribute('optimizeOption', optimizerConfig);
			parserConfig.setAttribute('contentTable', contentTable);
			/*
			Declaring ids by hand means the built-in `lookup` tables no longer
			necessarily match the processor the program runs on, so arrays must not
			use them.
			*/
			if (contentTable.hasOverrides) optimizerConfig.lookupUnavailable = true;

			this.parser = new Parser(this.lexer, parserConfig);
			this.extraConfig = parserConfig;
			// Renew everything
			this.semanticAnalyzer = new SemanticAnalyzer(this);
			this.optimizer = new Optimizer(this);
			this.codeGenerator = new CodeGenerator(this);

            const ast = this.parser.parse();
			if (ast.errors.length) {
				return {
					success: false,
					code: "",
					errors: this.getAllErrors(),
                	warnings: this.getAllWarnings()
				}
			}
            const analyzed = this.semanticAnalyzer.analyze(ast.ast);
			if (analyzed.errors.length) {
				return {
					success: false,
					code: "",
					errors: this.getAllErrors(),
                	warnings: this.getAllWarnings()
				}
			}
            const optimizedAst = this.optimizer.optimize(this.semanticAnalyzer, analyzed.ast);
			if (optimizedAst.errors.length) {
				return {
					success: false,
					code: "",
					errors: this.getAllErrors(),
                	warnings: this.getAllWarnings()
				}
			}
            const targetCode = this.codeGenerator.generate(optimizedAst.ast);
            return {
                success: targetCode.errors.length == 0,
                code: targetCode.result.output().join('\n'),
                errors: this.getAllErrors(),
                warnings: this.getAllWarnings()
            };
        } catch (error) {
            return {
                success: false,
				code: "",
                fatalError: error.message + "\n" + error.stack,
                errors: this.getAllErrors(),
                warnings: this.getAllWarnings()
            };
        }
    }
    
    getAllErrors() {
        // 收集所有阶段的错误
		return [ ...this.lexer.errors, ...this.parser.errors, ...this.semanticAnalyzer.errors,
			...this.optimizer.errors, ...this.codeGenerator.errors
		]
    }
    
    getAllWarnings() {
        // 收集所有阶段的警告
		return [ ...this.semanticAnalyzer.warnings,
			...this.optimizer.warnings, ...this.codeGenerator.warnings
		]
    }
}

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
	 * 
	 * @typedef {{
	 * 		name: string,
	 * 		returnType: string,
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
				
				const randomIdentifier = Math.floor(10000 * Math.random());
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

		let parserConfig = new AttributeClass();
		parserConfig.setAttribute('extraTypes', exTypes);
		parserConfig.setAttribute('extraFunctions', exFuncs);
		parserConfig.setAttribute('extraHandler', new Map(extensions.flatMap(ext => [...ext.handlers])));

		this.config = config ?? new AttributeClass();
		if (!this.config.hasAttribute('targetVersion')) {
			this.config.setAttribute('targetVersion', 154.3);
		}

		this.extraConfig = parserConfig;

		this.memoryInfo = memoryInfo;
		
        this.lexer = new Lexer(sourceCode);
        this.parser = new Parser(this.lexer, parserConfig);
        this.semanticAnalyzer = new SemanticAnalyzer(this);
        this.optimizer = new Optimizer(this);
        this.codeGenerator = new CodeGenerator(this);
		this.extensions = extensions;

		
    }
    
    compile() {
        try {
            // 编译管道
            const tokens = this.lexer.tokenize();
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

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

import { SymbolEntry, Scope, TypeInfo, MemberInfo, SemanticAnalyzer } from "./mindcSemantic.js";

import { Optimizer } from "./mindcOptimizer.js";

import { Instruction, InstructionBuilder, InstructionReferrer, MemoryBlock, MemoryBlockInfo,
    MemoryManager, FunctionRegisterer, BuildingLinker, SingleInstruction, EmptyInstruction
 } from "./mindcGeneratorBase.js";

export class InternalGenerationFailure {
	constructor(message, node = null) {
		this.message = message;
		this.node = node;
	}

	toString() {
		return this.message;
	}
}

// TODO: Internal instruction optimizer to solve the 'invaded assigment'
// or through visit-params

// 代码生成器
export class CodeGenerator extends ASTVisitor {
	/**
	 * 
	 * @param {Compiler} compiler 
	 */
    constructor(compiler) {
        super(compiler);
		/**
		 * @type {SemanticAnalyzer}
		 */
		this.semantic = compiler.semanticAnalyzer;
		this.functionCallGraph = compiler.optimizer.functionCallGraph;
		this.registryId = 0;
		this.registrySymbolId = 0;
		this.registryPointerSymbolId = 0;
		this.temporarySpaceId = 0;
        this.outputCode = new Instruction();
		this.memory = new MemoryManager(compiler.memoryInfo);
		this.functionManagement = new FunctionRegisterer(this.memory);
		this.currentFunction = "";
		this.currentReturns = [];
		this.currentBreaks = [];
		this.currentContinues = [];
		this.breakStack = [];
		this.continueStack = [];
		/**
		 * @type {Scope}
		 */
		this.currentScope = null;
		this.errors = [];
		this.warnings = [];
		
		this.targetVersion = this.compiler ? this.compiler.config.getAttribute('targetVersion') : 0;
		this.guaranteeTemporarySymbolReg = true;
	}
    
	/**
	 * 
	 * @param {ASTNode} ast Root program node of the program
	 */
    generate(ast) {
        // 将AST转换为目标代码
        let result = new Instruction(), success = true;
		try {
			this.recuriveInfo = this.findRecursiveFunctions(this.functionCallGraph);
			this.currentScope = this.semantic.globalScope;
			if (ast.scope) this.processHeapMemory(ast.scope);
			else this.addWarning('Program has no global scope');
			/*
			this.semantic.typeTable.forEach(elem => {
				elem.initializeReferenceTable();
			});
			*/
			this.RValueAssigner = this.memory.currentState().duplicate();
			this.RValueMax = 0;
			const buildings = new BuildingLinker(this.memory.memoryBlocks.map(block => block.name), 
				this.functionManagement);
			result.concat(buildings.outputLinkInitializer());
			result.concat(this.memory.outputLinkInitializer());
			if (!this.compiler.config.getAttribute('noPointerFunction')) {
				result.concat(this.memory.outputPointerForwardFunction(this.functionManagement));
				result.concat(this.memory.outputPointerBackwardFunction(this.functionManagement));
				result.concat(this.memory.outputMemcpyFunction(this.functionManagement));
			}
			const programBody = this.visit(ast);
			const mainSymbol = programBody.getAttribute('mainSymbol');
			result.concat(programBody);
			this.memory.forwarding(this.RValueMax);
			result.concat(this.functionManagement.getSystemInitializer(this.memory));
			if (mainSymbol) {
				result.concat(this.functionManagement.getFunctionCall('main', new Map(), this.memory, true));
			} else {
				this.addWarning('No main function in program');
			}
		} catch (error) {
			success = false;
			this.addError(`Internal parser error: ${error.message}\n${error.stack}`);
		}
		return {
			success: success,
			result: result,
			errors: this.errors,
			warnings: this.warnings,
			memory: this.memory
		}
    }

	/**
	 * 
	 * @param {Scope} scope 
	 * @param {boolean} [staticAllocOnly=false]
	 * @param {boolean} [mustAlloc=false] 
	 * @param {string} [insideFunctionName=""] Unused
	 * @remark Adding all heap memories (static variable and global variables)
	 * Note:
	 * accessThroughPointer - This value is stored in heap memory area (e.g. volatile int)
	 * implementAsPointer - This value is a pointer
	 * (These can be used simultaneously)
	 */
	processHeapMemory(scope, staticAllocOnly = false, mustAlloc = false, insideFunctionName = "") {
		
		if (scope.astNode && scope.astNode.type === 'FunctionDeclaration') {
			if (this.recuriveInfo.has(scope.astNode.name)) {
				mustAlloc = true;
			}
		}
			
		scope.getAllSymbols().forEach(
			/**
			 * 
			 * @param {SymbolEntry} symbol 
			 */
			symbol => {
			// Update symbol size information
			if ((mustAlloc || symbol.isAddressed || symbol.isVolatile) && symbol.kind !== 'function') {
				/*
				if (symbol.isBasic()) {
					symbol.accessThroughPointer = true;
				} else {
					symbol.implementAsPointer = true;
				}
					*/
				if (symbol.isStructOrUnion()) {
					symbol.implementAsPointer = true;	// Use recursive processor otherwise
				}
				symbol.needMemoryAllocation = true;
			}
			switch (symbol.kind) {
				case 'variable':
				//case 'struct':	// Not allocating memory space for structures for now
				case 'parameter':
				//case 'union':
					const symbolType = symbol.type.type;
					symbol.size = symbolType.size;
					switch (symbolType.kind) {
						case 'struct':
						case 'union':
							break;
						case 'device':
							if (symbol.isAutoDevice) {
								symbol.implementAsPointer = false;
								//symbol.accessThroughPointer = false;
								//symbol.needMemoryAllocation = false;
							} else if (symbol.accessThroughPointer || symbol.implementAsPointer || symbol.needMemoryAllocation) {
								const diagnosis = `symbol ${symbol.name} with device-like type ('device' or 'content_t') is stored in memory. This will result in data loss.`;
								if (this.compiler.config.getAttribute('noDevicePointerError')) {
									this.addWarning(diagnosis);
								} else {
									this.addError(diagnosis);
								}
								
							}
							break;
						case 'basic':
						case 'null_t':
							break;
						case 'pointer':
							symbol.size = 2;
							symbol.implementAsPointer = true;	// There's no need to give them memory space
							break;
						case 'array':
							symbol.implementAsPointer = true;
							symbol.needMemoryAllocation = true;
							break;
						case 'function':
							break;
						default:
							// struct/union, etc.
							symbol.implementAsPointer = true;
							symbol.needMemoryAllocation = true;
							//symbol.accessThroughPointer = false;	// They aren't actually pointers
							// BE ADVISED: This implementation means that struct and union ARE CONSIDERED
							// 'access through pointer' but not a real pointer.
					}
					break;
				default:
					break;
			}
			// Assign heap memory
			if (symbol.accessThroughPointer || symbol.needMemoryAllocation) {
				symbol.needMemoryAllocation = true;
				if (!staticAllocOnly || symbol.isStatic || symbol.isVirtualSymbol) {
					if (symbol.isNearPointer) {
						// Already specified as near, try to assign continuous space
						let attempt = this.memory.currentState().duplicate(), skippedPage = 0;
						while (!attempt.full && attempt.currentRemain() < symbol.size) {
							attempt.skipToNextPage();
							skippedPage++;
						}
						if (attempt.full) {
							this.addWarning(`Cannot assign a complete block for ${symbol.name}`);
							symbol.isNearPointer = false;
						} else {
							symbol.memoryLocation = attempt;
							this.memory.pushStack();
							//let curState = this.memory.currentState();
							this.memory.setState(attempt.forwarding(symbol.size));
							if (skippedPage > 0) {
								this.addWarning(`Skipping ${skippedPage} page(s) for near pointer assignment. This may cause memory insufficiency`);
							}
						}
					}
					symbol.memoryLocation = this.memory.assign(symbol.getAssemblySymbol(), symbol.size);
					if ((symbol.kind !== 'pointer')
						&& symbol.memoryLocation.duplicate().forwarding(symbol.size).block === symbol.memoryLocation.block) {
							symbol.isNearPointer = true;
					}
				}
			}
		});
		scope.children.forEach(child => {
			this.processHeapMemory(child, true, mustAlloc);
		});
	}

	requireRValueMemory(size) {
		this.RValueMax = Math.max(this.RValueMax, size);
		return this.RValueAssigner.duplicate();
	}


	enterLoop() {
		this.breakStack.push(this.currentBreaks);
		this.continueStack.push(this.currentContinues);
		this.currentBreaks = [];
		this.currentContinues = [];
	}

	exitLoop() {
		this.currentBreaks = this.breakStack.pop();
		this.currentContinues = this.continueStack.pop();
	}

	/**
	 * 
	 * @param {SymbolEntry} symbol 
	 * @param {boolean} [duplicate=false]
	 * @return {Instruction} Instructions to execute BEFORE reading.
	 * @remarks Reading symbol through its original name.
	 * @remarks BE SURE NOT TO USE THIS TO READ STRUCT/UNION (or you will get the first member)
	 */
	generateSymbolRead(symbol, duplicate = false, givenName = null) {
		const raw = () => {
			if (givenName) {
				return new Instruction([
					InstructionBuilder.set(givenName, symbol.getAssemblySymbol())
				], givenName);
			} else {
				return new Instruction([], null, new Map([['disallowReplacement', true]])).set_returns(symbol);
			}	
		};

		let result = new Instruction();
		if (symbol.implementAsPointer) {
			const temporary = givenName ?? this.getTempVariable();
			if (symbol.accessThroughPointer) {
				if (symbol.memoryLocation) {
					result = this.memory.outputFetchOf(symbol.memoryLocation, `${temporary}_block`);
					result.concat(this.memory.outputFetchOf(symbol.memoryLocation.duplicate().forwarding(1), `${temporary}_pos`));
				} else {
					result = new Instruction([
						InstructionBuilder.reads(`${temporary}_block`, `${symbol.getAssemblySymbol()}.__pointer_block`, `${symbol.getAssemblySymbol()}.__pointer_pos`),
						this.memory.outputSymbolForwardCall(1, symbol, this.functionManagement,
							false, false),
						InstructionBuilder.reads(`${temporary}_pos`, '__ptrblock', '__ptrpos')
					]);
				}
				result.instructionReturn = temporary;
			} else {
				// Perhaps you don't really need to duplicate...
				if (!duplicate || (!symbol.isVolatile && symbol.isConst)) {
					return raw();
				}
				result = new Instruction([
					InstructionBuilder.set(`${temporary}_block`, `${symbol.getAssemblySymbol()}_block`),
					InstructionBuilder.set(`${temporary}_pos`, `${symbol.getAssemblySymbol()}_pos`)
				], temporary);
			}
			result.setAttribute('isPointer', true);
		} else if (symbol.accessThroughPointer) {
			let temporary = givenName ?? this.getTempVariable();
			if (symbol.memoryLocation) {
				result = this.memory.outputFetchOf(symbol.memoryLocation, temporary);
			} else {
				// Heap variables
				result = InstructionBuilder.reads(temporary, 
					`${symbol.getAssemblySymbol()}.__pointer_block`, `${symbol.getAssemblySymbol()}.__pointer_pos`
				);
			}
			result.instructionReturn = temporary;
			result.setAttribute('isPointer', false);	// No longer a pointer in fact, nor is it accessed through pointer!
		} else if (!duplicate || (!symbol.isVolatile && symbol.isConst)) {
			// Never considering whether it's indirect
			return raw();
		} else {
			let temporary = givenName ?? this.getTempVariable();
			result = InstructionBuilder.set(temporary, symbol.getAssemblySymbol());
			result.instructionReturn = temporary;
			result.setAttribute('isPointer', false);
		}
		return result;
	}

	/**
	 * 
	 * @param {SymbolEntry} symbol 
	 * @param {any} data 
	 * @return {Instruction}
	 * @warning This has **no** operation on structures. Be careful!
	 */
	generateSymbolWrite(symbol, data) {
		let hasTemplate = null;
		if (typeof data === 'object' && data.template) {
			hasTemplate = data.template;
		}
		if (symbol.implementAsPointer) {
			//throw new InternalGenerationFailure(`Pointer can't be directly written: ${symbol.name}`);
			if (symbol.accessThroughPointer) {
				if (symbol.memoryLocation) {
					return new Instruction([
						this.memory.outputStorageOf(symbol.memoryLocation, hasTemplate ? `{${hasTemplate}_block_entry}` : `${data}_block`),
						this.memory.outputStorageOf(symbol.memoryLocation.duplicate().forwarding(1), hasTemplate ? `{${hasTemplate}_pos_entry}` :`${data}_pos`)
					]);
				} else {
					return new Instruction([
						this.memory.outputPointerStorageOf(`${symbol.getAssemblySymbol()}.__pointer`, `${data}_block`),
						this.memory.outputSymbolForwardCall(1, symbol, this.functionManagement,
							false, false),
						this.memory.outputPointerStorageOf('__ptr', `${data}_pos`)
					]);
				}
			} else {
				return new Instruction([
					InstructionBuilder.set(`${symbol.getAssemblySymbol()}_block`, hasTemplate ? `{${hasTemplate}_block_entry}` : `${data}_block`),
					InstructionBuilder.set(`${symbol.getAssemblySymbol()}_pos`, hasTemplate ? `{${hasTemplate}_pos_entry}` : `${data}_pos`)
				]);
			}
		}
		if (symbol.accessThroughPointer) {
			if (symbol.memoryLocation) {
				return this.memory.outputStorageOf(symbol.memoryLocation, hasTemplate ?? data);
			} else {
				// Heap variables
				return InstructionBuilder.writes(hasTemplate ?? data, 
					`${symbol.getAssemblySymbol()}.__pointer_block`, `${symbol.getAssemblySymbol()}.__pointer_pos`
				);
			}
			
		} else {
			return InstructionBuilder.set(symbol.getAssemblySymbol(), hasTemplate ?? data);
		}
	}

	/**
	 * This stresses on writing.
	 * Write instruction data (with return labelled as {op}) to the symbol.
	 * @param {Instruction} instruction (The instruction will be modified!)
	 * @param {SymbolEntry} symbol 
	 * @returns Instruction
	 * @deprecated Please specify writing (i.e. operatesWrite).
	 */
	operates(instruction, symbol) {
		let result;
		if ((!symbol.accessThroughPointer) && (!symbol.implementAsPointer)) {
			result = instruction.raw_replace_all('op', symbol.getAssemblySymbol());
		} else {
			const tmpVar = this.getTempVariable();
			result = instruction.raw_replace_all('op', tmpVar);
			result.concat(this.generateSymbolWrite(symbol, tmpVar));
		}
		result.set_returns(symbol);
		return result;
	}

	/**
	 * This stresses on reading.
	 * @param {Instruction} instruction The instruction you want to opeate (with number labelled as {opw}).
	 * @param {Instruction} returner The instruction returned.
	 * @returns {Instruction}
	 * @remarks The 'returner' must have a previous concat.
	 * @deprecated Please specify reading (i.e. operatesRead).
	 */
	operatesWith(instruction, returner) {
		
		if (!returner.getAttribute('isPointerAccess')) {
			return instruction.raw_replace_all('opw', returner.instructionReturn);
		} else {
			const tmpVar = this.getTempVariable();
			return new Instruction([
				InstructionBuilder.reads(tmpVar, `${returner.instructionReturn}.__pointer_block`, `${returner.instructionReturn}.__pointer_pos`),
			]).concat_returns(instruction.raw_replace_all('opw', tmpVar));
		}
	}

	/**
	 * 
	 * @param {Instruction} instruction (The instruction will be modified!)
	 * @param {SymbolEntry} symbol 
	 * @returns {Instruction}
	 */
	operatesWrite(instruction, symbol) {
		return this.operates(instruction.raw_replace_all('op_write', '{op}'), symbol);
	}

	/**
	 * 
	 * @param {Instruction} instruction (The instruction will be modified!)
	 * @param {Instruction} returner 
	 * @returns {Instruction}
	 */
	operatesRead(instruction, returner) {
		return this.operatesWith(instruction.raw_replace_all('op_read', '{opw}'), returner);
	}

	/**
	 * Do **NOT** use this with `visitAndRead()` or it will return unexpected results.
	 * This function perform re-reading of return results to ensure correctness for operations
	 * using multiple values. If your operation uses only single value, then this is not necessary
	 * (just use `visitAndRead` then, although usage of this function will not significantly affect
	 * efficiency).
	 * @param {Instruction[]} instructions
	 * @param {Instruction} operation
	 * @returns {Instruction} 
	 * @example operateReads(node.arguments.map(arg => this.visit(arg)), new Instruction())
	 */
	operatesReads(instructions, operation) {
		let result = new Instruction([...instructions]);
		for (let i = 0; i < instructions.length; i++) {
			const inst = instructions[i];
			if (inst.getAttribute('isPointerAccess')) {
				const tmpVar = this.getTempVariable();
				result.concat(
					InstructionBuilder.reads(tmpVar, `${inst.instructionReturn}.__pointer_block`, `${inst.instructionReturn}.__pointer_pos`)
				);
				operation.raw_replace_all(`op_r${i}`, tmpVar);
			} else {
				operation.raw_replace_all(`op_r${i}`, inst.instructionReturn);
			}
		}
		result.concat_returns(operation);
		return result;
	}

	/**
	 * 
	 * @param {Instruction} inst 
	 * @param {boolean} [isPointerType=false] Whether the instruction returns a pointer type.
	 * @returns {Instruction}
	 * @deprecated
	 */
	convertInstructionForReading(inst, isPointerType = false) {
		if (inst.getAttribute('isPointerAccess')) {
			// Afterwards, it won't be PointerAccess.
			const temp = this.getTempVariable();
			return new Instruction([inst])
					.concat_returns(
						isPointerType ? (
							new Instruction([
								// Scheisse, I need to refer to not only the current but only the next.
								InstructionBuilder.reads(`${temp}_block`, `${inst.instructionReturn}.__pointer_block`, `${inst.instructionReturn}.__pointer_pos`),
								// ...
								this.memory.outputPointerForwardCall(1, `${inst.instructionReturn}.__pointer`, this.functionManagement, false, false),
								InstructionBuilder.reads(`${temp}_ptr`, `__ptrblock`, `__ptrpos`)
							], temp, new Map([['isPointer', true]]))
						) : (
							new Instruction([
								InstructionBuilder.reads(temp, `${inst.instructionReturn}.__pointer_block`, `${inst.instructionReturn}.__pointer_pos`)
							], temp)
						)
					);
		} else {
			return inst;
		}
	}

	/**
	 * 
	 * @param {ASTNode} node
	 * @returns {Instruction} 
	 * @warning Please consider operate_read when you need to use both vaild values at the same time.
	 * @remarks Ensure that the expected value is NOT a pointer before calling this.
	 * @remarks This should be used for most of the scenes (hardly is it necessary to write as well)
	 */
	visitAndRead(node, param = null) {
		return this.convertInstructionForReading(this.visit(node, param), node.dataType && node.dataType.isPointerImpl());
		// Note: then it reads a ORIGINAL VALUE
		// And besides: This won't dereference a pointer if it should appear to be one to the user (i.e. implementAsPointer)
		// but will dereference those accessed through pointer!
	}

	/**
	 * 
	 * @param {ProgramNode} node 
	 * @returns {Instruction}
	 */
	visitProgram(node) {
		let result = new Instruction();
		let mainSymbol = null;
		node.functions.forEach(func => {
			//result.concat();
			// Check the scope that this function owns
			const funcSymbol = this.currentScope.lookup(func.name);
			if (func.name === 'main') {
				mainSymbol = funcSymbol;
			}
			let stackSymbols = [];
			funcSymbol.owningScope.recursivelyGetAllSymbols().forEach(symbol => {
				// Symbol values
				if ((symbol.accessThroughPointer || symbol.needMemoryAllocation) && !(symbol.isStatic)) {
					stackSymbols.push(symbol);
				}
			});
			this.functionManagement.addFunction(func.name, new Instruction(), funcSymbol.owningScope, stackSymbols, func.name === 'main', this.recuriveInfo.has(func.name));
			const funcBody = this.visit(func);	// Note: visit comes first
			this.functionManagement.functionCollection.get(func.name).body = funcBody;
			
		});
		/**
		 * @type {SymbolEntry[]}
		 */
		const allSymbols = this.semantic.globalScope.recursivelyGetAllSymbols();
		let staticProcessor = new Instruction();
		allSymbols.forEach(symbol => {
			if (symbol.isStatic) {
				const endTag = `${symbol.getAssemblySymbol()}:_static_tag`;
				staticProcessor.concat(InstructionBuilder.set(endTag, 'false'));
			}
		});
		result.concat(staticProcessor);
		node.globalDeclarations.forEach(decl => result.concat(this.visit(decl)));
		result.concat(this.functionManagement.getAllFunctionDecl(this.memory));
		result.setAttribute('mainSymbol', mainSymbol);
		// Main function is now called in generate() after some preprocessing
		return result;
	}

	/**
	 * 
	 * @param {AsmStatementNode} node 
	 */
	visitAsmStatement(node) {
		if (node.code.length && node.code[0] === ':') {
			const instructionSplit = node.code.split(' ');
			switch (instructionSplit[0]) {
				case ":varStorage":
				case ":varStorageCp":
					if (instructionSplit.length < 3) {
						this.addError(`Invalid asm statement: ${node.code}`, node.location);
					}
					const symbol = this.currentScope.lookup(instructionSplit[2]);
					if (!symbol) {
						this.addError(`Unknown symbol: ${instructionSplit[2]}`, node.location);
					}
					return this.generateSymbolRead(symbol, instructionSplit[0] === ":varStorageCp", instructionSplit[1]);
					break;
				case ":varRead":
					return new Instruction([], instructionSplit.length >= 2 ? instructionSplit[1] : "null",
						new Map([['disallowReplacement', true]])
					);
					break;
			}
		} 
		return new SingleInstruction({
			content: node.code,
			referrer: []
		});
		
	}

	/**
	 * 
	 * @param {string} target 
	 * @param {string} source 
	 * @param {TypeInfo} actualType Type of **target**
	 * @param {TypeInfo | null} [sourceType=null] Type of **source** or as actualType if null
	 * @param {boolean} [shallow=true] 
	 * @returns {Instruction}
	 */
	copyObject(target, source, actualType, sourceType = null, shallow = true) {
		/**
		 * 
		 * @param {string} typeName 
		 * @returns {string}
		 */
		const referrer = typeName => typeName.slice(0, typeName.length - 2);
		const special = ['item_t', 'liquid_t', 'unit_t', 'block_t'];

		let result = new Instruction();
		const finalSourceType = sourceType ? sourceType : actualType;
		if (actualType.kind === 'pointer' || actualType.kind === 'array' || actualType.kind === 'struct'
				|| actualType.kind === 'union'
		) {
			if (actualType.kind === 'struct' || actualType.kind === 'union') {
				return this.copyStruct(target, source, actualType, finalSourceType);
			} else if (actualType.isPointerImpl()) {
				if (shallow || actualType.kind === 'pointer') {
					result.concat(new Instruction([
						InstructionBuilder.set(`${target}_block`, `${source}_block`),
						InstructionBuilder.set(`${target}_pos`, `${source}_pos`)
					]));
				} else {
					result.concat(this.memory.outputMemcpyCall(target, source, actualType.size, this.functionManagement));
				}
			} else {
				throw new InternalGenerationFailure(`Cannot generate copy from ${source} to ${target}`);
			}
		} else if (finalSourceType.name === 'content_t' && special.includes(actualType.name)) {
			const implicitConv = this.implicitToContentRaw(
				new Instruction([], source, new Map([['disallowReplacement', true]])), referrer(actualType.name));
			result.concat(implicitConv);
			result.concat(InstructionBuilder.set(target, implicitConv.instructionReturn));
		} else if (special.includes(finalSourceType.name) && !this.semantic.isSameType(finalSourceType, actualType)) {
			const implicitConv = this.implicitContentToNumericRaw(new Instruction([], source, new Map([['disallowReplacement', true]])));
			result.concat(implicitConv);
			result.concat(InstructionBuilder.set(target, implicitConv.instructionReturn));
		} else {
			result.concat(InstructionBuilder.set(target, source));
		}
		return result;
	}

	/**
	 * 
	 * @param {string} target 
	 * @param {string} source 
	 * @param {TypeInfo} type 
	 * @param {TypeInfo | null} [sourceType=null] 
	 * @returns {Instruction}
	 */
	copyStruct(target, source, type, sourceType = null) {
		const finalSourceType = sourceType ? sourceType : type;
		if (type.isPointerImpl()) {
			if (finalSourceType.isPointerImpl()) {
				return this.memory.outputMemcpyCall(target, source, type.size, this.functionManagement);
			} else {
				let latest = 0;
				return new Instruction(
					[
						InstructionBuilder.set('__ptrblock', `${target}_block`),
						InstructionBuilder.set('__ptrpos', `${target}_pos`),
						...(type.members.map(
							/**
							 * 
							 * @param {MemberInfo} member
							 */
							member => {
								const result = new Instruction(
									type.isNear() ? [
										InstructionBuilder.op('add', '__ptrpos', '__ptrpos', member.offset - latest)
									] : [
										InstructionBuilder.set('__step', member.offset - latest),
										this.functionManagement.getFunctionCall('__pointerForward', new Map(), this.memory, false)
									]
								).concat(InstructionBuilder.writes(`${source}.${member.name}`, '__ptrblock', '__ptrpos'));
								latest = member.offset;
								return result;
							}
						))
					]
				);
			}
		} else if (finalSourceType.isPointerImpl()) {
			// !type.isPointerImpl(), so must read everything for the target
			let latest = 0;
			return new Instruction(
				[
					InstructionBuilder.set('__ptrblock', `${source}_block`),
					InstructionBuilder.set('__ptrpos', `${source}_pos`),
					...(type.members.map(
						/**
						 * 
						 * @param {MemberInfo} member
						 */
						member => {
							const result = new Instruction(
								finalSourceType.isNear() ? [
									InstructionBuilder.op('add', '__ptrpos', '__ptrpos', member.offset - latest)
								] : [
									InstructionBuilder.set('__step', member.offset - latest),
									this.functionManagement.getFunctionCall('__pointerForward', new Map(), this.memory, false)
								]
							).concat(InstructionBuilder.reads(`${target}.${member.name}`, '__ptrblock', '__ptrpos'));
							latest = member.offset;
							return result;
						}
					))
				]
			);
		} else {
			return new Instruction(
				type.members.map(
					/**
					 * 
					 * @param {MemberInfo} member 
					 */
					member => this.copyObject(`${target}.${member.name}`, `${source}.${member.name}`, member.type, null, false)
				)
			);
		}
		
	}
    
	/**
	 * 
	 * @param {FunctionDeclarationNode} node
	 * @returns {Instruction} 
	 * @remark This function doesn't do registration itself. It simply calls for body information.
	 */
	visitFunctionDeclaration(node) {
		const recursive = this.recuriveInfo.has(node.name);
		let pushedScope = null;
		if (node.scope) {
			pushedScope = this.currentScope;
			this.currentScope = node.scope;
			this.currentFunctionScope = node.scope;
		}
		this.currentFunction = node.name;
		this.currentReturns = [];
		let result = new Instruction(), paramId = 0;
		const functionSymbol = this.currentScope.lookup(node.name);
		if (!(functionSymbol && !functionSymbol.isAddressed)) {
			result.concat(new Instruction(node.scope.getAllSymbols().flatMap(sym => {
				if (sym.kind !== 'parameter') return [];
				const paramName = `__param_${paramId++}`;
				if ((sym.type.type.kind === 'struct' || sym.type.type.kind === 'union')) {
					// Must be copied (NOTE: They are NOT .__pointer NOw!)
					return [this.copyStruct(sym.getAssemblySymbol(), paramName, sym.type.type)];
				}
				return [this.generateSymbolWrite(sym, paramName)];
			})));	// Copy the parameters
			// If there's no function address calling, this is not necessary
		}
		let body = this.visit(node.body);
		//body.replace('func_ret', body.size() + 1);
		this.currentReturns.forEach(rets => {
			body.concat(new InstructionReferrer(rets, 'func_ret', 0));
		});
		this.releaseTempVariable();
		/*
		if (recursive) {
			// Before execution...
			this.currentScope.recursivelyGetAllSymbols().forEach(symbol => {
				result.concat(this.generateSymbolRead(symbol, true, `${symbol.getAssemblySymbol()}.__restore`));
			});
		}
			*/
		result.concat(body);
		/*
		if (recursive) {
			this.currentScope.recursivelyGetAllSymbols().forEach(symbol => {
				result.concat(this.generateSymbolWrite(symbol, `${symbol.getAssemblySymbol()}.__restore`));
			});
		}
			*/
		if (pushedScope) {
			this.currentScope = pushedScope;
			this.currentFunctionScope = null;
		}
		this.currentFunction = "";
		this.currentReturns = [];
		return result;
	}

	/**
	 * 
	 * @param {ReturnStatementNode} node 
	 * @return {Instruction}
	 * @remark '__return' is the result of function return !!
	 */
	visitReturnStatement(node) {
		const result = node.argument ? this.visitAndRead(node.argument) : new Instruction();
		let stmt = new Instruction();
		stmt.concat(result);
		if (node.argument) {
			// Copying should be done in callers, not callees
			if (node.dataType && node.dataType.isPointerImpl()) {
				stmt.setAttribute('isPointer', true);
				stmt.concat([
					InstructionBuilder.set(`__return_block`, `${result.instructionReturn}_block`),
					InstructionBuilder.set(`__return_pos`, `${result.instructionReturn}_pos`)
				])
			} else if (result.getAttribute('isPointer')) {
				stmt.concat(this.memory.outputPointerFetchOf(result.instructionReturn, "__return"));
			} else {
				stmt.concat(this.copyObject('__return', result.instructionReturn, node.dataType, node.argument.dataType, true));
				//stmt.concat(InstructionBuilder.set('__return', result.instructionReturn));
			}
		}
		this.releaseTempVariable();
		let jumper = InstructionBuilder.jump(`{func_ret}`, 'always');
		this.currentReturns.push(jumper);
		stmt.concat(jumper);
		return stmt;
	}

	visitCompoundStatement(node) {
		let instruction = new Instruction();
		let pushedScope = null;
		if (node.scope) {
			pushedScope = this.currentScope;	// After optimizer, the scope relationship will be damanged!
			this.currentScope = node.scope;
		}
		node.statements.forEach(stmt => {
			instruction.concat(this.visit(stmt));
			this.releaseTempVariable();
		});
		if (pushedScope) {
			this.currentScope = pushedScope;
		}
		return instruction;
	}
	/**
	 * 
	 * @param {ASTNode} node 
	 */
	visitExpressionStatement(node, param) {
		let instruction = new Instruction();
		node.children.forEach(stmt => {
			//this.releaseTempVariable();
			// As we apply variable replacement, we can't release temporary variable here
			const result = this.visitAndRead(stmt, param);
			instruction.concat_returns(result);
		});
		return instruction;
	}

	/**
	 * 
	 * @param {ConditionalExpressionNode} node 
	 * @warning Some different behavior for >=152 version: whatever happens, both the consequent and the alternate will be evaulated!!
	 */
	visitConditionalExpression(node, param) {
		let result = new Instruction();
		let returner = this.getTempSymbol(node.dataType);//this.getTempVariable();
		const returnerTarget = (param && param.assignmentTarget) ? param.assignmentTarget : returner.getAssemblySymbol();

		// TODO if it is a single operation, then directly use 'select':
		// currently we use sth else
		if (this.targetVersion >= 152) {
			const tester = this.visitToCondition(node.test, false);
			return this.operatesReads(
				[this.visit(node.consequent), this.visit(node.alternate)],
				this.operatesWrite(
					new Instruction([
						tester,
						InstructionBuilder.selectx('{op_write}', tester.instructionReturn, '{op_r0}', '{op_r1}')
					]), returner
				)
			);
			
		}

		const test = this.visitAndRead(node.test);
		result.concat(test);
		const jumper = this.operatesWith(
			InstructionBuilder.jump('{alt}', 'notEqual', '{opw}', 'true'),
			test
		);
		result.concat(jumper);
		const consequent = this.visitAndRead(node.consequent);
		const alternate = this.visitAndRead(node.alternate);
		const isPointer = node.dataType && node.dataType.isPointerImpl();
		result.concat(consequent);
		if (isPointer) {
			returner.implementAsPointer = true;
			result.setAttribute('isPointer', true);
		} else if (consequent.getAttribute('isPointer')) {
			returner.accessThroughPointer = true;
		}
		result.concat(this.generateSymbolWrite(returner, consequent.instructionReturn));
		const finalize = InstructionBuilder.jump('{end}', 'always');
		result.concat(finalize);
		result.concat(new InstructionReferrer(jumper, 'alt', 0));
		result.concat(alternate);
		if (alternate.getAttribute('isPointer')) {
			returner.accessThroughPointer = true;
			result.setAttribute('isPointer', true);
		} else {
		}
		result.concat(this.generateSymbolWrite(returner, alternate.instructionReturn));
		result.concat(new InstructionReferrer(finalize, 'end', 0));
		result.set_returns(returner);
		return result;
	}

	/**
	 * 
	 * @param {IfStatementNode} node 
	 * @remark The problem is that the comparers are all in the jumpers.
	 * To handle this, we still make them values.
	 * @todo I think we can simplify 'test' generation (judge whether it's a binary expression)
	 * -- and if it is, directly use it!
	 * @bug
	 */
	visitIfStatement(node) {
		let result = new Instruction();
		//let test = this.implicitToBoolean(node.test);
		//result.concat(test);
		const tester = this.visitToCondition(node.test, true);
		this.releaseTempVariable();
		let consequentProcessor = new Instruction();
		let consequent = this.visit(node.consequent), alternateProcessor = new Instruction();
		const noConsequent = new Instruction([
				tester,
				InstructionBuilder.jumpx('{alt_begin}', tester.instructionReturn)
			]);
		if (node.alternate) {
			let alternate = this.visit(node.alternate);
			//alternateProcessor.concat(InstructionBuilder.jump('{0}', 'always', null, null, [alternate.size() + 1]))// Skip alternate block if executing consequent
			const jumper = InstructionBuilder.jump('{end_of_alt}', 'always');
			alternateProcessor.concat(jumper);
			alternateProcessor.concat(new InstructionReferrer(noConsequent, 'alt_begin'));
			alternateProcessor.concat(alternate);
			alternateProcessor.concat(new InstructionReferrer(jumper, 'end_of_alt'));
		} else {
			alternateProcessor.concat(new InstructionReferrer(noConsequent, 'alt_begin'));
		}
		consequentProcessor.concat(noConsequent);
		consequentProcessor.concat(consequent);
		result.concat(consequentProcessor);
		result.concat(alternateProcessor);
		return result;
	}

	/**
	 * 
	 * @param {WhileStatementNode} node 
	 */
	visitWhileStatement(node) {
		//const test = this.implicitToBoolean(node.test);
		const tester = this.visitToCondition(node.test, true);
		let connector = new Instruction();
		let bodyProcessor = new Instruction();
		this.enterLoop();
		const body = node.body ? this.visit(node.body) : new Instruction();
		const conditBreak = InstructionBuilder.jumpx('{loop_break}', tester.instructionReturn);	// Already connected
		bodyProcessor.concat(conditBreak);
		this.currentBreaks.push(conditBreak);
		this.releaseTempVariable();
		bodyProcessor.concat(body);
		const testJumper = InstructionBuilder.jump('{goto_test}', 'always', null, null);
		connector.concat(new InstructionReferrer(testJumper, 'goto_test'));
		connector.concat(tester);
		connector.concat(bodyProcessor);
		connector.concat(testJumper);
		// Replace 'continue' and 'break':
		//connector.replace('loop_continue', 0);
		//connector.replace('loop_break', connector.size() + 1);
		let finalize = new Instruction();
		this.currentContinues.forEach(conts => {
			finalize.concat(new InstructionReferrer(conts, 'loop_continue'));
		})
		finalize.concat(connector);
		this.currentBreaks.forEach(breaks => {
			finalize.concat(new InstructionReferrer(breaks, 'loop_break'));
		});
		this.exitLoop();
		return finalize;
	}

	/**
	 * 
	 * @param {ForStatementNode} node 
	 */
	visitForStatement(node) {
		let loopSystem = new Instruction();
		this.enterLoop();
		const body = node.body ? this.visit(node.body) : new Instruction();
		const update = node.update ? this.visit(node.update) : new Instruction();
		let initSystem = new Instruction();
		this.releaseTempVariable();
		if (node.init) {
			initSystem = this.visit(node.init);
			this.releaseTempVariable();
		}
		let bodyLoop = new Instruction(), continueIns = new Instruction();
		bodyLoop.concat(body);
		bodyLoop.concat(continueIns);
		bodyLoop.concat(update);
		const testJumper = InstructionBuilder.jump('{goto_test}', 'always', null, null);
		bodyLoop.concat(testJumper);
		loopSystem.concat(new InstructionReferrer(testJumper, 'goto_test'));
		if (node.test) {
			const test = this.visitToCondition(node.test, true);
			loopSystem.concat(test);
			const conditBreak = InstructionBuilder.jumpx('{loop_break}', test.instructionReturn);
			this.currentBreaks.push(conditBreak);
			loopSystem.concat(conditBreak);
		}	// Otherwise this directly proceeds
		this.releaseTempVariable();
		loopSystem.concat(bodyLoop);
		let finalize = new Instruction();
		if (node.init) {
			finalize.concat(initSystem);
		}
		this.currentContinues.forEach(conts => {
			continueIns.concat(new InstructionReferrer(conts, 'loop_continue'));
		})
		finalize.concat(loopSystem);
		this.currentBreaks.forEach(breaks => {
			finalize.concat(new InstructionReferrer(breaks, 'loop_break'));
		});
		this.exitLoop();
		return finalize;
	}

	/**
	 * 
	 * @param {ASTNode} node 
	 */
	visitBreakStatement(node) {
		const jumper = InstructionBuilder.jump('{loop_break}', 'always');
		this.currentBreaks.push(jumper);
		return jumper;
	}

	/**
	 * 
	 * @param {ASTNode} node 
	 */
	visitContinueStatement(node) {
		const jumper = InstructionBuilder.jump('{loop_continue}', 'always');
		this.currentContinues.push(jumper);
		return jumper;
	}

	/**
	 * 
	 * @param {VariableDeclarationNode} node 
	 */
	visitVariableDeclaration(node) {
		let instruction = new Instruction();
		node.declarators.forEach(stmt => {
			instruction.concat(this.visit(stmt));
			this.releaseTempVariable();
		});
		return instruction;
	}

	/**
	 * 
	 * @param {VariableDeclaratorNode} node 
	 * @remark This has to do with many operations...
	 */
	visitVariableDeclarator(node) {
		// Simple symbols can have direct lookup in scope
		const special = ['item_t', 'liquid_t', 'unit_t', 'block_t'];
		const referrer = typeName => typeName.slice(0, typeName.length - 2);

		const symbol = this.currentScope.lookup(node.name);
		if (!symbol) {
			throw new InternalGenerationFailure(`Could not get symbol ${node.name}`, node);
		}
		if (symbol.isAutoDevice) {
			return new Instruction();
		}
		let finalize = new Instruction();
		if (symbol.memoryLocation) {
			finalize.concat(symbol.memoryLocation.getAssignmentInstruction(symbol.getAssemblySymbol()));
		}
		if (!symbol.memoryLocation || node.initializer) {
			let endJumper = null;
			const endTag = `${symbol.getAssemblySymbol()}:_static_tag`;
			if (symbol.isStatic) {
				endJumper = InstructionBuilder.jump('{skip_init}', 'equal', endTag, 'true');
				finalize.concat(endJumper);
			}
			// This is reserved
			let evaluation = node.initializer ? this.visitAndRead(node.initializer) : new Instruction();
			
			if (node.initializer) {
				// Necessary implicit converts that is not supported by the Mindustry VM
				if (node.initializer.dataType) {
					if (symbol.type.type.name === 'content_t' && special.includes(node.initializer.dataType.name)) {
						evaluation = this.implicitToContentRaw(evaluation, referrer(node.initializer.dataType.name));
					} else if (special.includes(symbol.type.name) && !this.semantic.isSameType(symbol.type.type, node.initializer.dataType)) {
						evaluation = this.implicitContentToNumericRaw(evaluation);
					}
				}
				
				// Oh no, this was wrong. YOU CAN'T ASSUME THAT THE RIGHT SIDE IS SIMILAR THING AS WELL!

				if (node.initializer.dataType && (
					node.initializer.dataType.kind === 'struct' || node.initializer.dataType.kind === 'union'
				)) {
					if (evaluation.getAttribute('disallowReplacement')) {
						finalize.concat(evaluation);
						finalize.concat(this.copyStruct(symbol.getAssemblySymbol(), evaluation.instructionReturn ?? 'null', symbol.myType(), node.initializer.dataType));
					} else {
						finalize.concat(evaluation.replace_variable(symbol.getAssemblySymbol(), evaluation.instructionReturn));
					}
				} else {
					finalize.concat(evaluation);
					finalize.concat(this.generateSymbolWrite(symbol, evaluation.instructionReturn ?? 'null'));
				}
				
			}
			
			if (symbol.isStatic) {
				finalize.concat(InstructionBuilder.set(endTag, 'true'));
				finalize.concat(new InstructionReferrer(endJumper, 'skip_init', 0));
			}
			this.releaseTempVariable();
		}
		return finalize;
	}

	/**
	 * 
	 * @param {DeclaratorNode} node 
	 * @returns 
	 */
	visitDeclarator(node) {
		return this.visitVariableDeclarator(node);
	}

	/**
	 * 
	 * @param {string} pointer 
	 * @returns {Instruction}
	 */
	castPointerToValue(pointer) {
		const tempVar = this.getTempVariable();
		this.operates(new Instruction([
			InstructionBuilder.reads(tempVar, `${pointer}_block`, 4),
			InstructionBuilder.op('add', tempVar, `${pointer}_pos`)
		]), tempVar);
	}
	

	/**
	 * 
	 * @param {InitializerListNode} node 
	 * @remark This function fetchs parent information...
	 * - If its parent is a declarator (or variable declarator), it directly sets values
	 * - Otherwise, it creates a new memory space and copies it
	 */
	visitInitializerList(node) {
		const declaratorTypes = ['VariableDeclarator', 'Declarator'];
		let assignedSpace = node.symbol ? node.symbol.memoryLocation : null, result = new Instruction(), tmpVar = this.getTempSymbol(node.dataType);//this.getTempVariable();
		let knownType = null;
		if (node.parent) {
			if (declaratorTypes.includes(node.parent.type)) {
				const manualSpace = this.currentScope.lookup(node.parent.name).memoryLocation;
				assignedSpace = manualSpace ?? assignedSpace;
				if (manualSpace) {
					// Actually, the preprocessing has been done in the variable declarator visitor.
					result.setAttribute('noCopy', true);
				}
			}
		}
		assignedSpace = assignedSpace ?? this.memory.assign(this.getTempSpace(), node.dataType.size);
		result.concat(assignedSpace.getAssignmentInstruction(tmpVar.getAssemblySymbol()));
		result.instructionReturn = tmpVar.getAssemblySymbol();
		result.setAttribute('isPointer', true);
		//let currentPointer = 0;
		/**
		 * 
		 * @param {ASTNode} obj 
		 * @param {TypeInfo} typeLayer
		 */
		const initializerProcessor = (obj, typeLayer) => {

			const warnedTypes = ['content_t', 'device', 'null_t'];

			if (obj.type === 'InitializerList') {
				obj.children.forEach(child => initializerProcessor(child, typeLayer ? typeLayer.pointerTo : null));
			} else {
				if (obj.dataType && warnedTypes.includes(obj.dataType.name)) {
					this.addWarning(`Initializing object of type ${obj.dataType.toString()} in initializer list is an undefined behavior`, node.location);
				}
				const valueFetch = this.visitAndRead(obj);
				result.concat(valueFetch);
				// Process assignment-like
				if (valueFetch.getAttribute('isPointer') || obj.dataType.isPointerImpl()) {
					// They are implemented as pointer...
					result.concat(new Instruction([
						this.memory.outputStorageOf(assignedSpace, `${valueFetch.instructionReturn}_block`),
						this.memory.outputStorageOf(assignedSpace.duplicate().forwarding(1), `${valueFetch.instructionReturn}_ptr`)
					]));
				} else {
					result.concat(this.memory.outputStorageOf(assignedSpace, valueFetch.instructionReturn));
				}
				assignedSpace.forwarding(
					(typeLayer && (typeLayer.size != null)) ? typeLayer.size 
					: (obj.dataType.size ?? 0));
			}
		};
		initializerProcessor(node, knownType);
		
		return result;
	}

	implicitContentToNumericRaw(inst) {
		const temporary = this.getTempSymbol(this.semantic.getTypeInfo('int'));
		let result = new Instruction([inst]);
		result.concat_returns(this.operates(
			this.operatesWith(InstructionBuilder.sensor('{op}', '{opw}', '@id'), inst), temporary));
		return result;
	}

	/**
	 * 
	 * @param {ASTNode} node 
	 * @returns {Instruction} Return something with implicit conversion to int
	 */
	implicitToNumeric(node) {
		let result, temporary;
		if (node.dataType) {
			switch (node.dataType.name) {
				case 'bool':
					result = this.visitAndRead(node);
					temporary = this.getTempSymbol(this.semantic.getTypeInfo('int'));//this.getTempVariable();
					result.concat(this.generateSymbolWrite(temporary, 0));
					let jumper = this.operatesWith(
						InstructionBuilder.jump('{notequal_end}', 'notEqual', 'true', '{opw}'),
						result
					);
					result.concat(jumper);
					result.concat(this.generateSymbolWrite(temporary, 1));
					result.concat(new InstructionReferrer(jumper, 'notequal_end', 0));
					result.set_returns(temporary);
					return result;
					break;
				case 'float': case 'double':
					result = this.visitAndRead(node);
					temporary = this.getTempSymbol(this.semantic.getTypeInfo('int'));
					result.concat_returns(this.operates(this.operatesWith(
						InstructionBuilder.op('floor', '{op}', '{opw}'),
						result
					), temporary));
					return result;
					break;
				case 'content_t':
					return this.implicitContentToNumericRaw(this.visitAndRead(node));
					break;
			}
		}
		return this.visitAndRead(node);
	}

	/**
	 * 
	 * @param {Instruction} inst 
	 * @param {string} contentType 
	 * @returns SYMBOLIC CONTENT
	 */
	implicitToContentRaw(inst, contentType) {
		let result = inst;
		const temporary = this.getTempSymbol(this.semantic.getTypeInfo('content_t'));//this.getTempVariable();
		result.concat_returns(this.operates(
			this.operatesWith(InstructionBuilder.lookup(contentType, '{op}', '{opw}'), result), temporary
		));
		return result;
	}

	/**
	 * 
	 * @param {ASTNode} node 
	 * @param {string | null} [contentTypeGiven=null]
	 * @return {Instruction}
	 */
	implicitToContent(node, contentTypeGiven = null) {
		let result, temporary;
		let contentType = contentTypeGiven;
		const special = ['item_t', 'liquid_t', 'unit_t', 'block_t'];
		const referrer = typeName => typeName.slice(0, typeName.length - 2);
		if (node.type === 'Identifier' && node.name[0] === '@') {
			return new Instruction([], node.name, new Map([['disallowReplacement', true]]));
		}
		if (node.dataType) {
			if (!contentType && special.includes(node.dataType.name)) {
				contentType = referrer(node.dataType.name);
			}
			switch (node.dataType.name) {
				// These types are actually stored in integral form
				case 'int': case 'item_t': case 'liquid_t': case 'unit_t': case 'block_t':
					return this.implicitToContentRaw(this.visitAndRead(node), contentType);
					break;
			}
		}
		return this.visitAndRead(node);
	}

	implicitToBoolean(node) {
		if (!node.dataType || node.dataType.name !== 'bool') {
			let result = this.visitAndRead(node);
			let temporary = this.getTempSymbol(this.semantic.getTypeInfo('bool'));
			if (this.targetVersion >= 152) {
				result.concat(this.operatesWrite(
					InstructionBuilder.select('{op_write}', 'equal', result.instructionReturn, '0', 'false', 'true'),
					temporary
				));
			} else {
				result.concat(this.generateSymbolWrite(temporary, 'false'));
				let jumper = this.operatesWith(
					InstructionBuilder.jump('{equal_end}', 'equal', '0', '{opw}'),
					result
				);
				result.concat(jumper);
				result.concat(this.generateSymbolWrite(temporary, 'true'));
				result.concat(new InstructionReferrer(jumper, 'equal_end', 0));
			}
			result.set_returns(temporary);
			return result;
		} else {
			return this.visitAndRead(node);
		}
	}

	/**
	 * 
	 * @param {ASTNode} node 
	 * @param {boolean} requireOpposite Whether the result is 'opposed'
	 * @returns {Instruction}
	 * @remarks This returns a **segment** (like `equal a b`).
	 */
	visitToCondition(node, requireOpposite) {
		const comparerTranslation = new Map([['==', 'equal'], ['!=', 'notEqual'], ['<', 'lessThan'], ['>', 'greaterThan'], ['<=', 'lessThanEq'], ['>=', 'greaterThanEq']]);
		const oppositeTranslation = new Map([['==', 'notEqual'], ['!=', 'equal'], ['<', 'greaterThanEq'], ['>', 'lessThanEq'], ['<=', 'greaterThan'], ['>=', 'lessThan']]);
		if (node.type === 'BinaryExpression' && comparerTranslation.has(node.operator)) {
			const evaluatingKinds = ['pointer', 'array'];
			const leftIsPointer = node.left.dataType && evaluatingKinds.includes(node.left.dataType.kind);
			const rightIsPointer = node.right.dataType && evaluatingKinds.includes(node.right.dataType.kind);
			if (!(leftIsPointer || rightIsPointer)) {
				return this.operatesReads(
					[this.visit(node.left), this.visit(node.right)],
					new Instruction([], `${requireOpposite ? oppositeTranslation.get(node.operator) : comparerTranslation.get(node.operator)} {op_r0} {op_r1}`,
					new Map([['disallowReplacement', true]]))
				);
			}
		}
		let result = this.implicitToBoolean(node);
		result.instructionReturn = `${requireOpposite ? 'notEqual' : 'equal'} ${result.instructionReturn} true`;
		return result;
	}

	/**
	 * 
	 * @param {BinaryExpressionNode} node 
	 * @remark MUST CONSIDER: Clear up temporary staff in general expression processor
	 * @bug Notice: pointer access for recursive functions might result in errors
	 */
	visitBinaryExpression(node) {
		if (node.getAttribute('disposeReturn')) {
			return new Instruction([
				this.visit(node.left),
				this.visit(node.right)
			]);
		}

		const operatorTranslation = new Map([['+', 'add'], ['-', 'sub'], ['*', 'mul'], ['/', 'div'], ['%', 'mod'], ['|', 'or'], ['||', 'or'], ['&', 'and'], ['&&', 'land'], ['^', 'xor'], ['<<', 'shl'], ['>>', 'shr'], ['==', 'equal'], ['!=', 'notEqual'], ['<', 'lessThan'], ['>', 'greaterThan'], ['<=', 'lessThanEq'], ['>=', 'greaterThanEq']]);
		const comparerTranslation = new Map([]);	// unused now
		let result = new Instruction();
		if (operatorTranslation.has(node.operator)) {
			// Boolean can't directly participate in this!
			const evaluatingKinds = ['pointer', 'array'];
			const leftIsPointer = node.left.dataType && evaluatingKinds.includes(node.left.dataType.kind);
			const rightIsPointer = node.right.dataType && evaluatingKinds.includes(node.right.dataType.kind);
			if (leftIsPointer || rightIsPointer) {
				const right = this.visitAndRead(node.right);
				const left = this.visitAndRead(node.left);
				//const duplicate = this.getTempVariable();
				const duplicateResult = this.getTempSymbol(node.dataType);
				const pointerSide = leftIsPointer ? left : right;
				const nonPointerSide = rightIsPointer ? left : right;
				result = this.operatesReads(
					[pointerSide, nonPointerSide],
					new Instruction([
						//InstructionBuilder.set(`${duplicate}_block`, `{op_r0}_block`),
						//InstructionBuilder.set(`${duplicate}_pos`, `{op_r0}_pos`),
						(node.operator === '+')
							? (this.memory.outputPointerForwardCall('{op_r1}', '{op_r0}', this.functionManagement, true, false))
							: (this.memory.outputPointerBackwardCall('{op_r1}', '{op_r0}', this.functionManagement, false)),
						InstructionBuilder.set(`${duplicateResult.getAssemblySymbol()}_block`, `__ptrblock`),
						InstructionBuilder.set(`${duplicateResult.getAssemblySymbol()}_pos`, `__ptrpos`)
					])).set_returns(duplicateResult);
				/*
				const pointerReferrer = leftIsPointer ? left.instructionReturn : right.instructionReturn;
				result.instructionReturn = duplicate;
				result.concat(left);
				result.concat(right);
				// These options are atomic!
				result.concat(InstructionBuilder.set(`${duplicate}_block`, `${pointerReferrer}_block`));
				result.concat(InstructionBuilder.set(`${duplicate}_pos`, `${pointerReferrer}_pos`));
				switch (node.operator) {
					case '+':
						result.concat(this.memory.outputPointerForwardCall(leftIsPointer ? right.instructionReturn : left.instructionReturn, duplicate, this.functionManagement));
						break;
					case '-':
						result.concat(this.memory.outputPointerBackwardCall(leftIsPointer ? right.instructionReturn : left.instructionReturn, duplicate, this.functionManagement));
						break;
					default:
						throw new InternalGenerationFailure(`Unsupported operator for pointers: ${node.operator}`, node);
				}
				result.concat(this.generateSymbolWrite(duplicateResult, duplicate));
				result.instructionReturn = duplicateResult.getAssemblySymbol();
						*/
			} else {
				const left = this.visit(node.left);
				const right = this.visit(node.right);
				//const varName = this.getTempVariable();
				const varResult = this.getTempSymbol(node.dataType);
				/*
				result.concat_returns(this.operates(
					InstructionBuilder.op(operatorTranslation.get(node.operator), '{op}',
					 left.instructionReturn, right.instructionReturn),
					varResult
				));
				*/
				result.concat_returns(this.operates(
					this.operatesReads([left, right], new Instruction([
						InstructionBuilder.op(operatorTranslation.get(node.operator), '{op}', '{op_r0}', '{op_r1}')
					])),
					varResult
				));
			}
		} else if (comparerTranslation.has(node.operator)) { // Unused
			const left = this.implicitToBoolean(node.left);
			const right = this.implicitToBoolean(node.right);	// Has a copy, so won't result in errors
			const outlet = this.getTempVariable();
			let leftComparison = left.instructionReturn;
			let rightComparison = right.instructionReturn;
			result.concat(left);
			result.concat(right);
			if (left.dataType && left.dataType.kind === 'pointer') {
				// Right is also. Convert both
				// I guess literals can't do this
				const leftTransform = this.castPointerToValue(leftComparison);
				result.concat(leftTransform);
				leftComparison = leftTransform.instructionReturn;
				const rightTransform = this.castPointerToValue(rightComparison);
				result.concat(rightTransform);
				rightComparison = rightComparison.instructionReturn;
			}
			// It is now guaranteed THAT: left and right comparisons are IMMEDIATE VALUES
			const outletSymbol = this.getTempSymbol(node.dataType);
			result.concat_returns(this.operates(
				InstructionBuilder.op(comparerTranslation.get(node.operator), '{op}', leftComparison, rightComparison),
				outletSymbol
			));
		} else {
			this.addWarning('Going to assignment through binary expression:', node);
			result = this.visitAssignmentExpression(node);
		}
		return result;
	}
	
	/**
	 * 
	 * @param {CastExpression} node 
	 * @todo
	 */
	visitCastExpression(node) {
		if (node.getAttribute('disposeReturn')) {
			return this.visit(node);
		}
		let casting;

		const transmitToSymbol = () => {
			const copiedSymbol = this.getTempSymbol(node.dataType);
			casting.concat(this.generateSymbolWrite(copiedSymbol, casting.instructionReturn));
			casting.set_returns(copiedSymbol);	
		};

		if (node.dataType) {
			if (node.dataType.qualifiers.indexOf('volatile') != -1) {
				// This means EXPLICITLY reserve everything.
				if (node.expression.type === 'Identifier') {
					// Directly return it.
					if (node.expression.name.length > 0 && node.expression.name[0] == '@') {
						return new Instruction([], node.expression.name, new Map([['disallowReplacement', true]]));
					}
					const symbol = this.currentScope.lookup(node.expression.name);
					casting = new Instruction([], symbol.getAssemblySymbol(), new Map([['disallowReplacement', true]]));
				} else {
					casting = this.visit(node.expression);	// Don't convert it directly, if volatile is given
				}
				// Still, transmission is necessary
				//transmitToSymbol();
			} else if (node.expression && node.expression.dataType) {
				if (node.expression.dataType.kind === 'pointer' && node.dataType.name === 'int') {
					casting = this.visitAndRead(node.expression);
					const receiver = this.getTempVariable();
					const result = this.castPointerToValue(casting.instructionReturn);
					casting.concat(result);
					casting.concat(InstructionBuilder.set(receiver, '__builtin_return'));
					casting.instructionReturn = receiver;
					transmitToSymbol();
				} else if (node.expression.dataType.name === 'int' && node.dataType.kind === 'pointer') {
					casting = this.visitAndRead(node.expression);
					const receiver = this.getTempVariable();
					const result = this.memory.outputPointerForwardCall(casting.instructionReturn, receiver, this.functionManagement, true);
					casting.concat(result);
					casting.instructionReturn = receiver;
					transmitToSymbol();
				} else if (node.expression.dataType.name === 'bool' && node.dataType.name !== 'bool') {
					casting = this.implicitToNumeric(node.expression);
				} else if (node.dataType.name === 'bool' && node.expression.dataType.name !== 'bool') {
					casting = this.implicitToBoolean(node.expression);
				} else if (
					['item_t', 'liquid_t', 'unit_t', 'block_t', 'content_t'].includes(node.expression.dataType.name)
					 && ['item_t', 'liquid_t', 'unit_t', 'block_t', 'int'].includes(node.dataType.name)) {
					casting = this.implicitToNumeric(node.expression);
				} else if (['item_t', 'liquid_t', 'unit_t', 'block_t', 'int'].includes(node.expression.dataType.name) && node.dataType.name === 'content_t') {
					const referrer = typeName => typeName.slice(0, typeName.length - 2);
					casting = this.implicitToContent(node.expression, referrer(node.expression.dataType.name));
				}
			}
		} 
		if (!casting) {
			casting = this.visitAndRead(node.expression);
		}
		casting.setAttribute('fromCast', true);
		// Make a copy
		return casting;
	}
	
	/**
	 * 
	 * @param {AssignmentExpressionNode} node 
	 * @todo Make simple numeric operations simplified (direct operation through op, etc.)
	 * @todo Consider some "invaded operations." (e.g. Especially for arrays and structs:
	 * don't let the right side assign to a temporary symbol first.)
	 */
	visitAssignmentExpression(node) {
		let value = null;
		const special = ['item_t', 'liquid_t', 'unit_t', 'block_t'];
		const referrer = typeName => typeName.slice(0, typeName.length - 2);

		if (node.operator !== '=') {
			value = this.visitBinaryExpression(ASTBuilder.binaryExpression(
				node.operator.slice(null, -1),
				node.left, node.right
			));
		} else {
			// struct will be copied through memcpy.
			// Note that arrays or pointers are "byref".
			if (node.right.type === 'InitializerList') {
				// Pre-evaluated pointer with initializer list, perform something like memcpy
				let result = new Instruction();
				const leftPointer = this.processLValGetter(node.left, true, true);	// Because we want the array information
				node.setAttribute('generatedLVal', leftPointer);
				const listData = this.visitAndRead(node.right, {
					assignmentTarget: leftPointer.instructionReturn
				});
				result.concat(leftPointer);
				result.concat(listData);
				result.concat(this.memory.outputMemcpyCall(leftPointer.instructionReturn, listData.instructionReturn,
					node.right.dataType.size ?? 0, this.functionManagement
				));
				result.setAttribute('isPointer', true);	// Must be a pointer...
				result.instructionReturn = leftPointer.instructionReturn;
				return result;
			} else if (node.right.dataType && (node.right.dataType.kind === 'struct' || node.right.dataType.kind === 'union')) {
				const leftPointer = this.processLValGetter(node.left, true, true);
				const structData = this.visitAndRead(node.right, {
					assignmentTarget: leftPointer.instructionReturn
				});
				
				if (structData.getAttribute('disallowReplacement')) {	// Rarely used
					return leftPointer
						.concat(structData)
						.concat(this.copyStruct(leftPointer.instructionReturn, structData.instructionReturn, 
							node.right.dataType, node.left.dataType));
				} else {
					return leftPointer.concat(structData.replace_variable(leftPointer.instructionReturn, 
						structData.instructionReturn));
				}
				
			} else if (node.left.dataType && node.right.dataType) {
				if (special.includes(node.right.dataType.name) && node.left.dataType.name === 'content_t') {
					value = this.implicitToContent(node.right, referrer(node.right.dataType.name));
				} else if (special.includes(node.left.dataType.name) 
					&& !this.semantic.isSameType(node.left.dataType, node.right.dataType)) {
					value = this.implicitToNumeric(node.right);
				}
			}
			if (!value) value = this.visitAndRead(node.right);
		}
		
		return this.processAssignment(node.left, value);
	}

	/**
	 * 
	 * @param {UnaryExpressionNode} node 
	 * @returns {Instruction}
	 * @todo Similar to the simplifier of assignment expressions
	 */
	visitUnaryExpression(node) {
		let result, collector, temp;
		let asPointer = false;

		const normalUnary = new Map([['-', 'sub'], ['~', 'not']]);

		switch (node.operator) {
			case '+':
				return this.visitAndRead(node.argument);
				break;
			case '-': case '~':
				if (node.argument.type === 'NumericLiteral') {
					return new Instruction([], -node.argument.value, new Map([['disallowReplacement', true]]));
				}
				result = this.visitAndRead(node.argument);
				temp = this.getTempSymbol(node.dataType);
				result.concat(this.operates(
					InstructionBuilder.op(normalUnary.get(node.operator), '{op}', 0, result.instructionReturn),
					temp
				));
				result.instructionReturn = temp.getAssemblySymbol();
				return result;
				break;
			case '++': case '--':
				// Note: this does not have copy because it has actions on its own
				result = this.visitAndRead(node.argument);	// We have LVal getter there, so this is read-only.
				let duplicate = this.getTempSymbol(node.dataType);
				asPointer = node.argument.dataType && node.argument.dataType.isPointerImpl();
				const shouldDuplicate = (!node.prefix) && (!node.getAttribute('disposeReturn'));
				if (shouldDuplicate) {
					result.concat(this.generateSymbolWrite(duplicate, result.instructionReturn));
				}
				if (asPointer) {
					result.concat(node.operator === '++'
						? this.memory.outputPointerForwardCall(1, result.instructionReturn, this.functionManagement, false, true, result.getAttribute('isNearPointer'))
						: this.memory.outputPointerBackwardCall(1, result.instructionReturn, this.functionManagement, false, true));
					if (node.argument.type !== 'Identifier') {
						result.concat(this.processLValGetter(node.argument)
							.replace('pointer_block_entry', `${result.instructionReturn}_block`)
							.replace('pointer_ptr_entry', `${result.instructionReturn}_pos`));
					}	// Otherwise, already modified.
				} else {
					result.concat(InstructionBuilder.op(node.operator === '++' ? 'add' : 'sub', result.instructionReturn, result.instructionReturn, '1'));
					if (node.argument.type !== 'Identifier') {
						result.concat(this.processLValGetter(node.argument).replace('value_entry', result.instructionReturn));
					}
				}
				if (shouldDuplicate) {
					result.set_returns(duplicate);
				}
				return result;
				break;
			case '&':
				// Get address of given symbol (thus should be a LValue - and having 'pointer')
				// Thus demand some small changes of L-value getting.
				return this.processLValGetter(node.argument, true);	// -- Notice: this is not 'traditional' because we really want something to MODIFY IT
				break;
			case '*':
				// Get value from given address (variable address).
				// Assume that this is only about RValue (LValue is independently handled.)
				// ...
				collector = this.getTempSymbol(node.dataType);
				result = this.visitAndRead(node.argument);
				result.concat_returns(this.operates(
					InstructionBuilder.reads('{op}', `${result.instructionReturn}_block`, `${result.instructionReturn}_pos`),
					collector
				));
				return result;
				break;
				
			case '!':
				result = this.visitAndRead(node.argument);
				temp = this.getTempSymbol(node.dataType);
				result.concat_returns(this.operates(
					InstructionBuilder.op('equal', '{op}', result.instructionReturn, 'false'),
					temp
				));
				return result;
		}
	}

	/**
	 * 
	 * @param {ASTNode} node 
	 * @returns {Instruction}
	 * @todo Optimize for a common situation: Left side refers to a determined variable
	 * (which requires changes in LVal-getter as well)
	 * @todo I guess this won't result in any error. But who knows...
	 * @remarks This is an atomic operation for generation (at least so far). This feature is guaranteed by the LValGetter.
	 */
	resolveMemberExpression(node) {
		let finalResult = new Instruction(), index;
		if (node.getAttribute('stringAccess')) {
			// Special string access
			const finalSymbol = this.getTempSymbol(this.semantic.getTypeInfo('char'));
			return this.operatesReads(node.children.map(child => this.visit(child)),
				this.operatesWrite(
					InstructionBuilder.read('{op_write}', '{op_r0}', '{op_r1}'), finalSymbol)
			);
		}
		
		// TODO: WHAT IF IT IS A 'AccessThroughPointer'?
		const leftsideOrigin = this.processLValGetter(node.children[0], true, true); // Thus it returns a duplicated version of address
		const isNearPointerHere = leftsideOrigin.getAttribute('isNearPointer') ? true : false;
		const isRegStructHere = leftsideOrigin.getAttribute('isRegStruct') ? true : false;
		finalResult.concat(leftsideOrigin);
		if (node.getAttribute('computed')) {
			index = this.visitAndRead(node.children[1]);	// Simple calculation
			finalResult.concat(index);				// Make such visit first!!
		}
		if (isRegStructHere) {
			finalResult.instructionReturn = `${leftsideOrigin.instructionReturn}.${node.children[1].name}`;
			finalResult.setAttribute('isNearPointer', isNearPointerHere); // inherit the feature
			finalResult.setAttribute('isRegStruct', isRegStructHere);
			return finalResult;
		} 
		const finalSymbol = this.getTempSymbol(this.semantic.getTypeInfo('null_t*'));
		let leftsideDuplicate;	// Then perform setter
		if (this.guaranteeTemporarySymbolReg) {
			//throw new InternalGenerationFailure(`Member expression system requires temporary symbol registry guarantee`);
			leftsideDuplicate = finalSymbol.getAssemblySymbol();
		} else {
			leftsideDuplicate = this.getTempVariable();
		}
		finalResult.concat(InstructionBuilder.set(`${leftsideDuplicate}_block`, `${leftsideOrigin.instructionReturn}_block`));
		finalResult.concat(InstructionBuilder.set(`${leftsideDuplicate}_pos`, `${leftsideOrigin.instructionReturn}_pos`));
		if (node.getAttribute('computed')) {
			// Array, continue to get left-side
			// TODO: The index of the left side should be multiplied by right-side size
			// The left side must also return a pointer, but the problem is that: what it the right-side size?
			
			let ratio = 1, childSize = 0;
			if (node.dataType.size > 0) {
				ratio = node.dataType.size;
			}
			// Got correction ratio of array index
			//finalResult.concat(InstructionBuilder.op('mul', index.instructionReturn, index.instructionReturn, ratio));
			// Get multiplier
			let resultIndex;
			if (ratio > 1) {
				resultIndex = this.getTempVariable();
				finalResult.concat(InstructionBuilder.op('mul', resultIndex, index.instructionReturn, ratio));
			} else {
				resultIndex = index.instructionReturn;
			}
			finalResult.concat(this.memory.outputPointerForwardCall(resultIndex, leftsideDuplicate, this.functionManagement, false, true, isNearPointerHere));
			finalResult.instructionReturn = leftsideDuplicate;
		} else {
			// Struct/union: seek member inside it. Already calculated by SEM.
			// Also it is guaranteed that this is a member
			
			finalResult.concat(this.memory.outputPointerForwardCall(node.getAttribute('memberOffset'), leftsideDuplicate, this.functionManagement, false, true, isNearPointerHere));
			finalResult.instructionReturn = leftsideDuplicate;
			
		}
		if (!this.guaranteeTemporarySymbolReg) {
			finalResult.concat(this.generateSymbolWrite(finalSymbol, finalResult.instructionReturn));
		}
		// Because it is an address whatever happens
		finalResult.set_returns(finalSymbol);
		finalResult.setAttribute('isNearPointer', isNearPointerHere); // inherit the feature
		finalResult.setAttribute('isRegStruct', isRegStructHere);
		//finalResult.instructionReturn = finalSymbol.getAssemblySymbol();
		return finalResult;
	}

	/**
	 * This returns LVal. For a pointer, it reserves the name (and if offset exists, it configures)!
	 * Left value can't have some really-elegant process.
	 * 
	 * @param {ASTNode} left 
	 * @param {boolean} [returnOnlyPointer=false] Whether only providing pointer information (returning a pointer WHICH CAN MODIFY IT if traditional=false).
	 * @param {boolean} [traditional=false] Whether using "C tradition" (that is, regarding arrays, **structs and unions** as pointer -- even if structs / unions are not considered pointers usually now)
	 * The `traditional` currently means (when used with `returnOnlyPointer`): I just need a 'mindustry-accessible name'
	 * @returns {Instruction} Instructions to get node as LValue. This function returns an instruction sequence that can assign value (since you're looking for a LValue!) by replacing {value_entry}.
	 * @throws {InternalGenerationFailure} When attempting to get the pointer value of non-pointer value at all
	 * @remark To be tested
	 * @remark Notice if you are configuring a pointer!
	 * @remark If it's about some addressing, the value must be duplicated!
	 */
	processLValGetter(left, returnOnlyPointer = false, traditional = false) {
		let pointer = "";
		let finalResult = new Instruction();
		let isPointer = false;
		let precall = new Instruction();
		switch (left.type) {
			case 'Identifier':
				/**
				 * @type {SymbolEntry}
				 */
				const identity = this.currentScope.lookup(left.name);
				const assemblySymbol = identity.getAssemblySymbol();
				/**
				 * 
				 * @param {boolean} isAccess Whether it's "accessThroughPointer".
				 * @param {boolean} accessControl
				 * @returns {Instruction}
				 */
				const directAccess = (isAccess = true, accessControl = true) => new Instruction(
					[], `${assemblySymbol}.__pointer`, 
					new Map([['isPointer', true], ['isPointerAccess', isAccess && accessControl], 
						['pointerAccess', isAccess], ['relevantSymbolRead', identity], ['isNearPointer', identity.isNearPointer],
						['isRegStruct', identity.isRegistryStruct()], ['disallowReplacement', true]]));
				if (identity) {
					
					// This means that it IS a pointer. Pay attention to the circumstances
					// when it is a struct/union.
					if (identity.implementAsPointer) {
						if (identity.accessThroughPointer) {
							// Use the 'pointer' instead
							//let finalize = new Instruction();
							// ...
							if (returnOnlyPointer) {
								// The object itself is the pointer
								if (traditional) {
									// We don't hope for a pointer that modifies itself. Instead, we hope to get its actual pointer value
									return new Instruction([], null, 
										[['isPointer', true], ['isPointerAccess', true], ['pointerAccess', true],
									['relevantSymbolRead', identity], ['isNearPointer', identity.isNearPointer],
									['isRegStruct', identity.isRegistryStruct()], ['disallowReplacement', true]])
										.concat_returns(this.generateSymbolRead(identity));
								} else {
									return directAccess();
								}
								
							} else {
								return new Instruction([this.generateSymbolWrite(identity, {
									template: 'pointer'
								})], null, new Map([['isAssignment', true]]));
							}
						} else {
							// It is a pointer, but unfortunately, you can't modify it through a pointer.
							if (returnOnlyPointer) {
								if (traditional) {
									return new Instruction([], assemblySymbol, new Map([['isPointer', true],
									['relevantSymbol', identity], ['isNearPointer', identity.isNearPointer],
									['isRegStruct', identity.isRegistryStruct()], ['disallowReplacement', true]]));
								}
								throw new InternalGenerationFailure(`${left.name}: Attempt to get address from non-addressed variable (Hint: this variable has no address. Use 'volatile', 'static' or explicit '&' for its address.)`, left);
							}
							let finalize = new Instruction();
							finalize.setAttribute('isPointer', true);
							finalize.setAttribute('isAssignment', true);
							finalize.concat(InstructionBuilder.set(`${assemblySymbol}_block`, '{pointer_block_entry}'));
							finalize.concat(InstructionBuilder.set(`${assemblySymbol}_pos`, '{pointer_ptr_entry}'));
							return finalize;
						}
						
					} else if (identity.accessThroughPointer) {
						//pointer = `${identity.getAssemblySymbol()}.__pointer`;
						if (returnOnlyPointer) {
							if (traditional) {
								throw new InternalGenerationFailure(`${left.name}: Hoping to get its pointer value, but getting its pointer reference`);
							}
							return directAccess(true, false);	// Don't regard it 'access through pointer' (because you just want a pointer!)
						}
						return this.generateSymbolWrite(identity, '{value_entry}');
					} else {
						/*
						let result = new Instruction();
						result.instructionReturn = identity.getAssemblySymbol();
						return result;
						*/
						if (returnOnlyPointer) {
							if (traditional) {
								return new Instruction([], identity.getAssemblySymbol(), new Map(
									[['relevantSymbol', identity], ['isNearPointer', identity.isNearPointer],
						['isRegStruct', identity.isRegistryStruct()], ['disallowReplacement', true]]
								));
							}
							throw new InternalGenerationFailure(`${left.name}: Attempt to get address from non-addressed variable (Hint: this variable has no address. Use 'volatile', 'static' or explicit '&' for its address.)`, left);
						}
						return new Instruction(
						[InstructionBuilder.set(identity.getAssemblySymbol(), '{value_entry}')]);
					}
				} else {
					throw new InternalGenerationFailure(`Unknown variable ${left.name}`, left);
				}
				break;

			case 'MemberExpression':
				// TODO: Here has been modified as symbolic!!!!!!!
				// But you can't make it stored in registry right away
				if (left.getAttribute('stringAccess')) {
					// This should have been considered by semantic analyzer
					throw new InternalGenerationFailure(`Strings in Mindustry-C are immutable`, left);
				}
				// Given that registry structure can't be 'accessed through pointer', there's no need to
				// consider inheritance of the attribute.
				const resolution = this.convertInstructionForReading(this.resolveMemberExpression(left));
				if (left.dataType.isPointerImpl()) {
					isPointer = true;			// This determines whether it is about "accessThroughPointer"
				}
				//finalResult.setAttribute('isPointer', true);	// This determines whether it is a pointer
				// Silent this check by default.
				/* else if (returnOnlyPointer && (!traditional)) {
					throw new InternalGenerationFailure(`${left.name}: A struct/array/union member might not be a pointer`);
				}*/
				
				if (resolution.getAttribute('isRegStruct')) {
					if (returnOnlyPointer) {
						if (traditional) {
							return finalResult.concat_returns(resolution);
						}
						throw new InternalGenerationFailure(`${left.name}: Attempt to get address from a non-addressed variable`);
					} else {
						return finalResult.concat(resolution)
										.concat(
							InstructionBuilder.set(resolution.instructionReturn, '{value_entry}')
						);
					}
					
				} else {
					finalResult.concat(resolution);
				}
				
				pointer = resolution.instructionReturn;
				break;
			
			case 'UnaryExpression':
				if (left.getAttribute('isDereference')) {
					precall = this.visitAndRead(left.argument);	// Simple calculation
					// Not regarding as pointer!
					pointer = precall.instructionReturn;
					if (returnOnlyPointer) {
						return precall;
						//throw new InternalGenerationFailure(`${left.name}: Attempt to get address from non-addressed variable (Hint: this variable already has memory address. Use '&' to avoid this problem.)`, left);
					}
					break;
				} else {
					// This doesn't really make sense.
					this.addWarning(`Deprecated dereference`, left.location);
					return this.processLValGetter(left.argument, returnOnlyPointer, traditional);
				}
				// Otherwise, DELIBERATELY PASS DOWN
			default:
				// TODO: Might be expressions... must get its l-value pointer to process
				// Unary expression should also be evaluated here.
				// TODO: Yet I should consider how to deal with unary *, which returns a LValue
				precall = this.visitAndRead(left);
				pointer = precall.instructionReturn;
				if (traditional && returnOnlyPointer) {
					return precall;
				}
				if (this.compiler.config.getAttribute('regardValuesAsAddresses')) {
					this.addWarning(`Maybe not a LValue -- regarding the value as an address`, left.location);
				} else {
					return new Instruction([], "null", new Map([['disallowReplacement', true]]));
				}
				break;
		}
		finalResult.concat(precall);
		if (returnOnlyPointer) {
			finalResult.instructionReturn = pointer;
			return finalResult;
		}
		// Like 'implementAsPointer'
		if (isPointer) {
			// The pointer POINTS TO a pointer
			finalResult.setAttribute('isPointer', true);
			finalResult.setAttribute('isPointerSetter', true);	// However, it is a setter... (Reserved for compatibility)
			finalResult.setAttribute('isPointerAccess', true);
			finalResult.concat(this.memory.outputPointerStorageOf(pointer, '{pointer_block_entry}'));
			// Jump the address!
			finalResult.concat(this.memory.outputPointerForwardCall(1, pointer, this.functionManagement, false, false));
			finalResult.concat(this.memory.outputPointerStorageOf('__ptr', '{pointer_ptr_entry}'));
		} else {
			// The pointer POINTS TO a value
			finalResult.concat(this.memory.outputPointerStorageOf(pointer, '{value_entry}'));
		}
		finalResult.setAttribute('isAssignment', true);
		return finalResult;
	}

	/**
	 * 
	 * @param {ASTNode} left The identifier shall be reserved.
	 * @param {Instruction} right The parameters must be pre-processed and READ.
	 * @return {Instruction}
	 * @remark We consider NORMAL ASSIGNMENT, STRUCTURAL and POINTER.
	 * @remark This process is ATOMIC.
	 */
	processAssignment(left, right) {
		let result = new Instruction();
		
		if (right.getAttribute('disallowReplacement') || !(left.symbol && !left.symbol.isPointer())) {
			let lval = this.processLValGetter(left);
			result.concat(right);
			if (lval.getAttribute('isPointer')) {
				result.concat_returns(lval.raw_replace('pointer_block_entry', `${right.instructionReturn}_block`)
				.raw_replace('pointer_ptr_entry', `${right.instructionReturn}_pos`));
			} else {
				let returns = right.instructionReturn;
				result.concat_returns(lval.raw_replace('value_entry', returns));
			}
			return result;
		} else {
			const lval = this.processLValGetter(left, true, true);
			result.concat(lval);
			result.concat(right.replace_variable(lval.instructionReturn, right.instructionReturn));
			return result;
		}
	}

	/**
	 * 
	 * @param {FunctionCallNode} node 
	 * @throws {InternalGenerationFailure}
	 * @returns {Instruction}
	 * @todo To make it suitable for function pointer (in order of parameter...)
	 */
	visitFunctionCall(node) {
		const referrer = typeName => typeName.slice(0, typeName.length - 2);
		const special = ['item_t', 'liquid_t', 'unit_t', 'block_t'];
		const recursive = this.recuriveInfo.has(this.currentFunction);
		let result = new Instruction();
		// Prepare for all parameters
		let relevantFunction = node.callee ? node.callee.symbol : null, isFunctionPointer = false;
		let detFunctionSymbol = null;
		if (relevantFunction.kind !== 'function') {
			relevantFunction = relevantFunction.type.type.functionTo;
			isFunctionPointer = true;
		} else {
			detFunctionSymbol = relevantFunction;
		}
		if (!relevantFunction) {
			throw new InternalGenerationFailure(`Unknown function ${node.callee ? node.callee.name : '<error-function>'}`);
		}
		if (recursive) {
			result.concat(this.functionManagement.getPreservationCall(this.currentFunction));
		}
		//let paramDelivery = new Map();

		/**
		 * 
		 * @param {TypeInfo} actualType
		 * @param {string} paramName
		 * @param {Instruction} param
		 * @param {TypeInfo | null} [paramType=null]
		 */
		const tackleWithParam = (actualType, paramName, param, paramType = null) => 
			new Instruction([
//				param,
				this.copyObject(paramName, param.instructionReturn, actualType, paramType)
			]);

		for (let i = 0; i < relevantFunction.type.parameters.length && i < node.arguments.length; i++) {
			const param = this.visitAndRead(node.arguments[i]);
			result.concat(param);
			/*
			result.concat(this.generateSymbolWrite(
				relevantFunction.owningScope.lookupCurrent(relevantFunction.type.parameters[i].name),
				param.instructionReturn
			));
			*/
			let actualType;
			if (isFunctionPointer) {
				actualType = this.compiler.semanticAnalyzer.getTypeInfo(relevantFunction.type.parameters[i].type);
			} else {
				actualType = relevantFunction.owningScope.lookupCurrent(relevantFunction.type.parameters[i].name).type.type;
			}
			// This is so-called "post-processing"
			
			if (isFunctionPointer) {
				result.concat(tackleWithParam(actualType, `__param_${i}`, param, node.arguments[i].dataType));
			} else {
				result.concat(
					tackleWithParam(actualType, 
					relevantFunction.owningScope.lookupCurrent(relevantFunction.type.parameters[i].name).getAssemblySymbol(),
				param, node.arguments[i].dataType)
				);
			}
			
		}

		// Pre-call ended.
		// Calling
		if (isFunctionPointer) {
			result.concat(this.functionManagement.getRawFunctionCall(node.callee.symbol.getAssemblySymbol(),
				new Map(), this.currentFunction === 'main'));
		} else {
			result.concat(this.functionManagement.getFunctionCall(
				node.callee.name, new Map(), this.memory, this.currentFunction === 'main'));
		}

		// If recursive:
		// (The position will be otherwise incorrect!!!)
		if (recursive) {
			// Restore all information (by rolling back and then forward)
			result.concat(this.functionManagement.getRollbackCall(this.currentFunction));
			// Then reread them
			/*
			result.concat(new Instruction(
				this.currentFunctionScope.recursivelyGetAllSymbols().map(
				symbol => {
					
			})
			));
			*/
		}
		
		// Get a copy of function return
		if (relevantFunction.type.returnType !== 'void' && !node.getAttribute('disposeReturn')) {
			//this.getTempVariable();
			const returnTypeContent = this.semantic.getTypeInfo(relevantFunction.type.returnType);
			if (!returnTypeContent) {
				this.addError("Return type unclear");
			}
			const returnContainer = this.getTempSymbol(returnTypeContent);
			result.set_returns(returnContainer);
			if (returnTypeContent.kind === 'struct' || returnTypeContent.kind === 'union') {
				// Do memcpy (this involves some strange memory allocation...)
				if (returnTypeContent.isPointerImpl()) {
					const temporaryMemory = this.requireRValueMemory(returnTypeContent.size);
					result.setAttribute('isRValueMem', true);
					result.concat(temporaryMemory.getAssignmentInstruction(result.instructionReturn));
					result.concat(this.memory.outputMemcpyCall(result.instructionReturn, '__return', returnTypeContent.size, this.functionManagement));
				} else {
					result.concat(this.copyStruct(returnContainer.getAssemblySymbol(), '__return', returnTypeContent));
				}
			} else {
				result.concat(this.generateSymbolWrite(returnContainer, '__return'));
			}
			/*
			if (returnTypeContent.isPointerImpl()) {
				result.setAttribute('isPointer', true);
				if (returnTypeContent.kind === 'struct' || returnTypeContent.kind === 'union') {
					// Do memcpy (this involves some strange memory allocation...)
					const temporaryMemory = this.requireRValueMemory(returnTypeContent.size);
					result.concat(temporaryMemory.getAssignmentInstruction(result.instructionReturn));
					result.concat(this.memory.outputMemcpyCall(result.instructionReturn, '__return', returnTypeContent.size, this.functionManagement));
				} else {
					result.concat(new Instruction([
						InstructionBuilder.set(`${result.instructionReturn}_ptr`, '__return_ptr'),
						InstructionBuilder.set(`${result.instructionReturn}_block`, '__return_block')
					]))
				}
			} else {
				result.concat(InstructionBuilder.set(result.instructionReturn, '__return'));
			}
			*/
		}

		return result;
	}

	/**
	 * 
	 * @param {IdentifierNode} node 
	 * @remark This function has a major change in Jan 31 version (no duplicate by default now).
	 * @remark Therefore, any duplicate function must have its own copy-processing!
	 */
	visitIdentifier(node) {
		const isBuiltinConstant = node.name.length && node.name[0] == '@';
		const isConstant = node.dataType && node.dataType.qualifiers.includes('const') 
			&& (!node.dataType.qualifiers.includes('volatile'));
		const special = ['item_t', 'liquid_t', 'unit_t', 'block_t'];
		const referrer = typeName => typeName.slice(0, typeName.length - 2);
		if (isBuiltinConstant) {
			// If the value is a builtin constant with type...
			if (node.dataType && special.includes(node.dataType.name)) {
				return this.implicitContentToNumericRaw(new Instruction([], node.name, new Map([['disallowReplacement', true]])));	// Make a conversion
			} else {
				return new Instruction([], node.name, new Map([['disallowReplacement', true]]));
			}
		}
		return this.generateSymbolRead(this.currentScope.lookup(node.name));	// Major change: now changed to false!!!
	}

	/**
	 * 
	 * @param {NumericLiteralNode} node 
	 */
	visitNumericLiteral(node) {
		return new Instruction([], `${node.value}`, new Map([['disallowReplacement', true]]));
	}

	/**
	 * 
	 * @param {StringLiteralNode} node 
	 */
	visitStringLiteral(node) {
		return new Instruction([], node.raw, new Map([['disallowReplacement', true]]));
	}

	visitCharacterLiteral(node) {
		return new Instruction([], `${node.raw.replace('"','\\"')}`, new Map([['disallowReplacement', true]]));
	}

	visitNullLiteral(node) {
		return new Instruction([], 'null', new Map([['disallowReplacement', true]]));
	}

	/**
	 * 
	 * @param {ASTNode} node Node for member expression
	 */
	visitMemberExpression(node) {
		// It must be a pointer-like, so we directly seek for a LValue
		//return this.resolveMemberExpression(node);
		const memberResolution = this.resolveMemberExpression(node);
		let result = new Instruction(), tmpVar;
		const tmpSymb = this.getTempSymbol(node.dataType);
		// Let still deliver them by reference
		// It is a pointer, keep it the same (until you make some explicit assignment works)
		if (memberResolution.getAttribute('isRegStruct') || node.getAttribute('stringAccess') || node.dataType.kind === 'pointer' || node.dataType.kind === 'array' || node.dataType.kind === 'struct' || node.dataType.kind === 'union') {	// [TODO:] Do a memory copy operation here (and point out that it has been a RValue!)
			return memberResolution;
			//tmpVar = memberResolution.instructionReturn;
		}
		result.concat(memberResolution);
		tmpVar = this.getTempVariable();
		result.concat(this.operatesWith(
			this.memory.outputPointerFetchOf('{opw}', tmpVar),
			memberResolution
		));
		result.concat(this.generateSymbolWrite(tmpSymb, tmpVar));
		result.set_returns(tmpSymb);
		return result;
	}

	/**
	 * 
	 * @param {BuiltinCallNode} node 
	 * @remarks In fact, only in this part does the program include interactions
	 */
	visitBuiltinCall(node) {

		/**
		 * 
		 * @param {string} prefix 
		 * @param {ASTNode[]} params 
		 * @param {TypeInfo | null} [hasReturnAs=null]
		 * @param {number} parameterSize 
		 * @param {boolean} [returnAtEnd=null]
		 * @returns 
		 */
		const packer = (prefix, params, hasReturnAs = null, parameterSize = -1, returnAtEnd = false) => {
			let varNames = [], vid = 0;
			let result = new Instruction(params.map(
				/**
				 * 
				 * @param {ASTNode} ast 
				 * @returns {Instruction}
				 * @remark The implicit convertor is actually hardly used!
				 */
				ast => {
					const converts = ['item_t', 'liquid_t', 'unit_t', 'block_t'];
					const referrer = typeName => typeName.slice(0, typeName.length - 2);
					let fetcher;
					// If providing device types, try it
					if (ast.dataType && converts.includes(ast.dataType.name)) {
						fetcher = this.convertInstructionForReading(this.implicitToContent(ast, referrer(ast.dataType.name)));
					} else {
						fetcher = this.visitAndRead(ast);
					}
					//varNames.push(fetcher.instructionReturn);
					varNames.push(`{op_r${vid++}}`);
					return fetcher;
				}
			));
			if (hasReturnAs) {
				//returner = this.getTempSymbol(hasReturnAs);//this.getTempVariable();
				//varNames.push(returner);
				if (returnAtEnd) varNames.push('{op}');
				else varNames.unshift("{op}");	// do returners!
			}
			for (let i = varNames.length; i < parameterSize; i++) {
				varNames.push('x');	// null filler
			}
			result = this.operatesReads(
				result.instructions,
				new SingleInstruction({
					content: prefix + ' ' + varNames.join(' '),
					referrer: []
				})
			);
			if (hasReturnAs && !node.getAttribute('disposeReturn')) {
				const returnSymbol = this.getTempSymbol(hasReturnAs);
				result = this.operates(result, returnSymbol);
				result.set_returns(returnSymbol);
			}
			return result;
		};

		const opProcesser = (name, params, parameterSize = 2) => {
			let varNames = [], vid = 0;
			let result = new Instruction(params.map(ast => {
				const fetcher = this.visit(ast);
				//varNames.push(fetcher.instructionReturn);
				varNames.push(`{op_r${vid++}}`);
				return fetcher;
			}));
			let returner = this.getTempSymbol();//this.getTempVariable();
			for (let i = varNames.length; i < parameterSize; i++) {
				varNames.push('x');	// null filler
			}
			result = (this.operates(
				this.operatesReads(
					result.instructions,
					new SingleInstruction({
						content: `op ${name} {op} ${varNames.join(' ')}`,
						referrer: []
					})
				), returner
			));
			result.set_returns(returner);
			return result;
		};

		const handlers = {
			end: ast => {
				return InstructionBuilder.end();
			},
			stop: ast => {
				return InstructionBuilder.stop();
			},
			print: ast => {
				let result = new Instruction();
				ast.arguments.forEach(arg => {
					const printing = this.visitAndRead(arg);
					result.concat(printing);
					result.concat(InstructionBuilder.print(printing.instructionReturn));
				});
				return result;
			},
			printchar: ast => {
				let result = new Instruction();
				ast.arguments.forEach(arg => {
					const printing = this.visitAndRead(arg);
					result.concat(printing);
					result.concat(InstructionBuilder.printchar(printing.instructionReturn));
				});
				return result;
			},
			format: ast => {
				let result = new Instruction();
				ast.arguments.forEach(arg => {
					const printing = this.visitAndRead(arg);
					result.concat(printing);
					result.concat(InstructionBuilder.format(printing.instructionReturn));
				});
				return result;
			},
			printflush: ast => {
				return packer(`printflush`, ast.arguments);
			},
			drawflush: ast => {
				return packer(`drawflush`, ast.arguments);
			},
			getlink: ast => {
				return packer(`getlink`, ast.arguments, this.semantic.getTypeInfo('device'));
			},
			wait: ast => {
				return packer(`wait`, ast.arguments);
			},
			ubind: ast => {
				return packer(`ubind`, ast.arguments);
			},
			ucontrol: ast => {
				if (ast.arguments[0].type === 'StringLiteral') {
					return packer(`ucontrol ${ast.arguments[0].value}`, ast.arguments.slice(1), null, 5);
				} else {
					throw `Parameter 1 for ucontrol() must be string literal`
				}
			},
			ulocate: ast => {
				let result = new Instruction(), tmpRet = this.getTempVariable();
				let postConcat = new Instruction();
				if (ast.arguments[0].type === 'StringLiteral') {
					let preConcat = "", subArg1, tailPosition = 1;
					switch (ast.arguments[0].value) {
						case 'ore':
							subArg1 = this.visitAndRead(ast.arguments[1]);
							tailPosition = 2;
							result.concat(subArg1);
							preConcat = `ore core true ${subArg1.instructionReturn}`;
							break;
						case 'building':
							if (ast.arguments[1].type !== 'StringLiteral') {
								throw `Parameter 2 for ulocate() must be string literal for building`;
								break;
							}
							tailPosition = 3;
							subArg1 = this.visitAndRead(ast.arguments[2]);
							result.concat(subArg1);
							preConcat = `building ${ast.arguments[1].value} ${subArg1.instructionReturn} @copper`;
							break;
						case 'spawn':
							preConcat = `spawn core true @copper`;
							break;
						case 'damaged':
							preConcat = `damaged core true @copper`;
							break;
					}
					for (let i = tailPosition; i < tailPosition + 4; i++) {
						const argContent = this.visitAndRead(ast.arguments[i]);
						if (argContent.getAttribute('fromCast') && ast.arguments[i].dataType.qualifiers
							&& ast.arguments[i].dataType.qualifiers.includes('volatile')) {
								preConcat += ` ${argContent.instructionReturn}`;
						} else {
							const tmpVar = this.getTempVariable();
							preConcat += ` ${tmpVar}`;
							postConcat.concat(InstructionBuilder.writes(tmpVar, `${argContent.instructionReturn}_block`, `${argContent.instructionReturn}_ptr`));
						}
					}
					result.concat(new SingleInstruction({
						content: `ulocate ${preConcat}`,
						referrer: []
					}));
				} else {
					throw `Parameter 1 for ulocate() must be string literal`;
				}
				result.concat(postConcat);
				return result;
			},
			uradar: ast => {
				// The first 4 parameters must all be string literal
				for (let i = 0; i < 4; i++) {
					if (ast.arguments[i].type !== 'StringLiteral') {
						throw `Parameter ${i} for radar() must be a string literal`;
					}
				}
				const processSequence = ast.arguments.slice(0, 4).map(arg => arg.value).join(" ");
				return packer(`uradar ${processSequence}`, ast.arguments.slice(4), this.semantic.getTypeInfo('device'), -1, true);
			},
			draw: ast => {

				const draw_lengths = new Map([
					['clear', 4], ['color', 4], ['stroke', 2], ['line', 5], ['rect', 5],
					['lineRect', 5], ['poly', 6], ['linePoly', 6], ['triangle', 7], ['image', 6]
				]);

				if (ast.arguments[0].type === 'StringLiteral') {
					const instruction = ast.arguments[0].value;
					if (draw_lengths.has(instruction) && draw_lengths.get(instruction) === ast.arguments.length) {
						return packer(`draw ${instruction}`, ast.arguments.slice(1), null, 6);
					} else {
						throw `Unsupported parameter 1 for draw()`;
					}
				} else {
					throw `Parameter 1 for draw() must be a string literal`;
				}

			},
			control: ast => {

				const draw_lengths = new Map([
					['enabled', 3], ['shoot', 5], ['shootp', 4], ['configure', 3],
					['color', 5]
				]);

				if (ast.arguments[0].type === 'StringLiteral') {
					const instruction = ast.arguments[0].value;
					if (draw_lengths.has(instruction) && draw_lengths.get(instruction) === ast.arguments.length) {
						return packer(`control ${instruction}`, ast.arguments.slice(1), null, 4);
					} else {
						throw `Unsupported parameter 1 for control()`;
					}
				} else {
					throw `Parameter 1 for control() must be a string literal`;
				}

			},
			radar: ast => {
				// The first 4 parameters must all be string literal
				for (let i = 0; i < 4; i++) {
					if (ast.arguments[i].type !== 'StringLiteral') {
						throw `Parameter ${i} for radar() must be a string literal`;
					}
				}
				const processSequence = ast.arguments.slice(0, 4).map(arg => arg.value).join(" ");
				return packer(`radar ${processSequence}`, ast.arguments.slice(4), this.semantic.getTypeInfo('device'), -1, true);
			},
			sensor: ast => {
				return packer(`sensor`, ast.arguments, this.semantic.getTypeInfo('null_t'));
			},
			lookup: ast => {
				if (ast.arguments[0].type === 'StringLiteral') {
					const instruction = ast.arguments[0].value;
					return packer(`lookup ${instruction}`, ast.arguments.slice(1), this.semantic.getTypeInfo('content_t'));
				} else {
					throw `Parameter 1 for lookup() must be a string literal`;
				}
				//return packer(`lookup`, ast.arguments, true);
			},
			sin: ast => {
				return opProcesser(`sin`, ast.arguments);
			},
			cos: ast => {
				return opProcesser(`cos`, ast.arguments);
			},
			tan: ast => {
				return opProcesser(`tan`, ast.arguments);
			},
			asin: ast => {
				return opProcesser(`asin`, ast.arguments);
			},
			acos: ast => {
				return opProcesser(`acos`, ast.arguments);
			},
			atan: ast => {
				return opProcesser(`atan`, ast.arguments);
			},
			adiff: ast => {
				return opProcesser(`angleDiff`, ast.arguments);
			},
			min: ast => {
				return opProcesser(`min`, ast.arguments);
			},
			max: ast => {
				return opProcesser(`max`, ast.arguments);
			},
			abs: ast => {
				return opProcesser(`abs`, ast.arguments);
			},
			ceil: ast => {
				return opProcesser(`ceil`, ast.arguments);
			},
			floor: ast => {
				return opProcesser(`floor`, ast.arguments);
			},
			sqrt: ast => {
				return opProcesser(`sqrt`, ast.arguments);
			},
			rand: ast => {
				return opProcesser(`rand`, ast.arguments);
			},
			asm: ast => {
				if (ast.arguments.length <= 0 || ast.arguments[0].type !== 'StringLiteral') {
					throw `Incorrect asm() call`;
				}
				return this.visitAsmStatement(ASTBuilder.asmStatement(ast.arguments[0].value));
			},
			memcpy: ast => {
				let result = new Instruction();
				const targetSpace = this.visitAndRead(ast.arguments[0]);
				const sourceSpace = this.visitAndRead(ast.arguments[1]);
				const sizeValue = this.visitAndRead(ast.arguments[2]);
				result.concat(targetSpace);
				result.concat(sourceSpace);
				result.concat(sizeValue);
				result.concat(this.memory.outputMemcpyCall(targetSpace.instructionReturn, 
					sourceSpace.instructionReturn, sizeValue.instructionReturn, this.functionManagement));
				return result;
			},
			memsp: ast => {
				let result = new Instruction();
				const returner = this.getTempVariable();
				const returningSymbol = this.getTempSymbol(this.semantic.getTypeInfo('void*'));
				const blockRef = this.visitAndRead(ast.arguments[0]);
				const posRef = this.visitAndRead(ast.arguments[1]);
				result.concat(blockRef);
				result.concat(posRef);
				result.concat(InstructionBuilder.set(`${returner}_block`, blockRef.instructionReturn));
				result.concat(InstructionBuilder.set(`${returner}_pos`, posRef.instructionReturn));
				result.concat(this.generateSymbolWrite(returningSymbol, returner));
				result.instructionReturn = returningSymbol.getAssemblySymbol();
				result.setAttribute('isPointer', true);
				return result;
			}
		};

		if (handlers[node.functionName]) {
			try {
				return handlers[node.functionName](node);
			} catch (err) {
				this.addError(err.toString(), node.getAttribute('location'));
				return new Instruction();
			}
		}

		const extra = this.extraConfig.getAttribute('extraHandler') ?? new Map();
		if (extra.has(node.functionName)) {
			try {
				return extra.get(node.functionName)(this, node);
			} catch (err) {
				this.addError(err.toString(), node.getAttribute('location'));
				return new Instruction();
			}
		}

		this.addWarning(`Calling unimplemented builtin function ${node.functionName} function`, node.location);
		return new Instruction({
			content: '{not_implemented}',
			referrer: []
		});
	}

    // 变量管理
	// This is about 'temporary registry' I guess
	
	/**
	 * @deprecated
	 * @returns {string}
	 */
    getTempVariable() {
        // 获取临时变量名
		return '__register_' + (this.registryId++);
    }

	/**
	 * @param {string | null} [kind=null] 
	 * @param {TypeInfo | null} [type=null] 
	 * @remark Visit comes first, so the symbols will be added normally!
	 * @remark Notice: there's no "naturally-in-memory" symbol now as they are costly.
	 * @returns {SymbolEntry}
	 */
	getTempSymbol(type = null, kind = null) {
		let symbolType = type;
		if (!symbolType) {
			symbolType = this.semantic.getTypeInfo("null_t");
		}
		if (!kind) {
			kind = symbolType.kind;
		}
		const isPointer = kind === 'pointer';
		let symbol = new SymbolEntry(`__tempsym_${isPointer ? `ptr_${this.registryPointerSymbolId++}` : this.registrySymbolId++}`, symbolType, this.currentScope, kind, null, symbolType.size);

		symbol.isVirtualSymbol = true;
		// The only thing necessary is to restore them after function's exiting
		if (this.currentScope) {
			this.currentScope.addSymbol(symbol);
		}

		if (this.recuriveInfo.has(this.currentFunction)) {
			//symbol.accessThroughPointer = true;
			//symbol.needMemoryAllocation = true;
			symbol.isRecursiveSymbol = true;
			let sTarget = this.functionManagement.functionCollection.get(this.currentFunction).stackSymbols;
			/*if (!sTarget.map(symb => symb.describe()).includes(symbol.describe())) {
				sTarget.push(symbol);
			}*/
			const sNames = sTarget.filter(symb => symb.name == symbol.name);
			// Given that memory allocation is essential in this environment, there's no need to compare
			if (sNames.length == 0 
				|| Math.max(...sNames.map(symb => symb.size)) < symbol.size) {
				if (sNames.length > 0)
					sTarget = sTarget.filter(symb => symb.name != symbol.name);
				sTarget.push(symbol);	// So the type information doesn't really matter!
			}
		}

		if (this.guaranteeTemporarySymbolReg && (symbol.accessThroughPointer || symbol.needMemoryAllocation)) {
			throw new InternalGenerationFailure(`Error in temporary symbol allocation configuration!`);
		}
		
		return symbol;
	}

	releaseTempSymbol() {
		this.registrySymbolId = 0;
		this.registryPointerSymbolId = 0;
	}

	getTempSpace() {
        // 获取临时变量名
		return '__memory_' + (this.temporarySpaceId++);
    }

	releaseTempVariable() {
		this.registryId = 0;
		this.releaseTempSymbol();
	}
    
}

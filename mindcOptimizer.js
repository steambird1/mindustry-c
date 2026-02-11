import { ASTNodeType, AttributeClass, ASTNode, CompilationPhase,
	ProgramNode, FunctionDeclarationNode, FunctionCallNode, AssignmentExpressionNode, 
    WhileStatementNode, ASTBuilder, ASTVisitor, __convert, objectList,
	liquidList, unitList, buildingList
 } from "./mindcBase.js";

import { SymbolEntry, Scope, TypeInfo, MemberInfo, SemanticAnalyzer } from "./mindcSemantic.js";

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

export class Optimizer extends ASTVisitor {
    constructor(compiler = null) {
		super(compiler);
        this.modified = false;
        this.errors = [];
        this.warnings = [];
        
        // 优化状态
        this.functionCallGraph = new Map();
        this.functionInfo = new Map();
        this.constants = new Map(); // 全局常量表
        this.localConstants = new Map(); // 局部常量表 
        this.variableUses = new Map(); // 变量使用统计 {scopePath: Map<varName, {reads, writes}>}
		//this.visitedSideEffects = false;
        
        // 作用域分析
        this.currentScope = null;
		this.currentFunction = null;
        this.scopeStack = [];
        this.analyzedScopes = new Set();
    }

    optimize(analysisResult, ast) {
        const { globalScope, symbolTable, typeTable, structTable, typedefTable } = analysisResult;
        
        this.ast = ast;
        this.globalScope = globalScope;
        this.symbolTable = symbolTable;
        this.typeTable = typeTable;
        this.structTable = structTable;
        this.typedefTable = typedefTable;
        
        this.modified = false;
        this.errors = [];
        this.warnings = [];
        this.analyzedScopes.clear();
        
        // 多轮优化
        let iteration = 0;
        const maxIterations = 10;
        
        do {
            this.modified = false;
            this.optimizationPass();
			this.analyzedScopes.clear();
            iteration++;
			// debug:
			//console.log(iteration + ' -th optimize:');
			//console.log(this);
        } while (this.modified && iteration < maxIterations);
        
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
        // 1. 收集全局信息（函数调用图等）
		this.functionInfo.clear();
		//this.localConstants.clear();
		this.localConstants.forEach(cs => cs.clear());
        this.collectGlobalInfo();
        
        // 3. 按作用域递归分析
        this.analyzeScope(this.globalScope);

		// Do scope optimizing:
		const optimizingScope = scope => {
			// First optimize children scopes!
			scope.children.forEach(child => optimizingScope(child));
			this.optimizeScope(scope);
		};
		optimizingScope(this.globalScope);
		
		// 2. 删除无用函数
        this.removeUnusedFunctions();
        
        // 4. 函数内联
        this.inlineFunctions();
        
        // 5. 全局死代码消除
        this.globalDeadCodeElimination();
    }

    // =============== 作用域分析 ===============

	// ! Manually adjusted !
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
        //this.optimizeScope(scope);
        
        // 退出作用域
        this.exitScope();
    }

    enterScope(scope) {
        this.scopeStack.push(this.currentScope);
        this.currentScope = scope;
		// 初始化当前作用域的常量表和变量使用统计
        const scopePath = scope.getPath();
        this.localConstants.set(scopePath, new Map());
        this.variableUses.set(scopePath, new Map());
    }

    exitScope() {
		const scopePath = this.currentScope.getPath();
		// Cleanup information, which is not necessary at all
		//this.localConstants.delete(scopePath);
		//this.variableUses.delete(scopePath);
        this.currentScope = this.scopeStack.pop();
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
	 */
	duplicateVariable(scope, varName, targetScope, targetName) {
		let varRef = scope.lookupCurrent(varName);
		if (!varRef) {
			return;
		}
		varRef = varRef.duplicate();
		varRef.name = targetName;
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
				writes: 1
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
        
		if (info && info.disposeReturn) {
			node.setAttribute('disposeReturn', true);
			let copiedInfo = { ...info };
			copiedInfo.disposeReturn = false;
			copiedInfo.parentInfo = info;
			info = copiedInfo;
		}

        switch (node.type) {
			case 'Program':
				if (node.functions) {
					node.functions.forEach(func => this.analyzeNode(func, info));
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
			
			case 'BuiltinCall':
				const builtins = info ? { ...info } : {};
				builtins.callNode = node;
				node.arguments.forEach(args => {
					this.analyzeNode(args, builtins);
				});
				break;
				
			case 'MemberExpression':
				// Don't further analyze them!!!
				this.analyzeNode(node.children[0]);
				if (node.getAttribute('computed')) {
					this.analyzeNode(node.children[1]);
				}
				// Otherwise it is NOT a valid variable!!
				return;
				break;
                
            default:
                // 递归分析子节点
				
                break;
        }

		if (node.declarators) {
			this.analyzeVariableDeclaration(node, info);
		}
		if (node.children) {
			node.children.forEach(child => this.analyzeNode(child, {
				...info,
				disposeReturn: (node.type === 'ExpressionStatement' 
					&& child != node.children[node.children.length - 1]
				)
			}));
		}
    }

    // =============== 具体分析函数 ===============
	
	// These functions are manually implemented as I have reached maximum conversation length of Deepseek:
	
	// ! Manually added scope entrance !
	analyzeCompoundStatement(node, info = null) {
		let enteredScope = false;
		if (node.scope && node.scope.name !== this.currentScope.name) {
			enteredScope = true;
			this.enterScope(node.scope);
		}
		if (node.statements) {
			node.statements.forEach(child => this.analyzeNode(child, {
				...info,
				disposeReturn: true
			}));
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
	
	// TODO: Value assignment might be a kind of writing !!!
	analyzeBinaryExpression(node, info = null) {
		//const scopePath = this.getCurrentScopePath();
		//const localConstants = this.localConstants.get(scopePath);
		this.analyzeNode(node.left, info);
		this.analyzeNode(node.right, info);
		if (node.operator.includes('=') && node.operator !== '==' && node.operator !== '>=' && node.operator !== '<=' && node.operator !== '!=') {
			// Assignment
			if (node.left.type === 'Identifier') {
				this.addWriteForCurrentScope(node.left.name, this.evaluateConstantExpression(node), this.shouldRunEvaluation(node, info));
			} else {
				// Something special -- this component will have side effect
				// ! New attribute label !
				node.setAttribute('maybeHasSideEffects', true);
			}
		}
	}
	
	// Return whether value should be updated right away (the exception is in the pre-analysis of loop)
	// This is a manually-added function
	shouldRunEvaluation(node, info = null) {
		if (info) {
			if (info.isLoop || info.isUnsure) return false;
		}
		return true;
	}
	
	// Some unary expressions have side	effects of writing (++ and --):
	// ! Manually adjusted !
	analyzeUnaryExpression(node, info = null) {
		
		if ((node.operator === '++' || node.operator === '--')) {
			if (node.argument.type === 'Identifier') {
				const scopePath = this.currentScope.lookupScopeOf(node.argument.name).getPath();
				const localConstants = this.localConstants.get(scopePath);
				let newConstant = localConstants.get(node.argument.name);
				if (newConstant != null) {
					// ! TODO: Only do this for: !
					if (node.operator === '++') newConstant++;
					if (node.operator === '--') newConstant--;
				}
				this.addWriteForCurrentScope(node.argument.name, newConstant, this.shouldRunEvaluation(node, info));
			}
		} else {
			this.analyzeNode(node.argument, info);
		}
	}
	
	
	// (End)
	// ! Manually added scope entrance !
	// The condition for inlining has been adjusted.
    analyzeFunction(funcNode, info = null) {
        const funcName = funcNode.name;
        this.currentFunction = funcName;
        // 记录函数信息
        if (!this.functionInfo.has(funcName)) {
            this.functionInfo.set(funcName, {
                calls: 0,
                hasSideEffects: false,
				isAddressed: false,
                isDefinition: funcNode.body !== null,
                isInline: funcNode.isInline || false,
				canInline: true,
                node: funcNode,
                scope: funcNode.scope
            });
        }
        
		let enteredScope = false;
		if (funcNode.scope && funcNode.scope.name !== this.currentScope.name) {
			enteredScope = true;
			this.enterScope(funcNode.scope);
		}
		
		const funcInfo = {
			inFunction: true,
			returnCount: 0,
			parentInfo: info
		};
		
        // 分析函数体
        if (funcNode.body) {
            this.analyzeNode(funcNode.body, funcInfo);
        }
		if (enteredScope) {
			this.exitScope();
		}
		
		// Consider whether this can be inlined
		if (!this.functionInfo.get(funcName).isAddressed) {
			if (funcInfo.returnCount > 1) {
				this.functionInfo.get(funcName).canInline = false;
			} else if (funcInfo.returnCount == 1) {
				this.functionInfo.get(funcName).canInline = funcNode.body.statements[funcNode.body.statements.length - 1].type === 'ReturnStatement';
			} else {
				this.functionInfo.get(funcName).canInline = true;
			}
		}
		
		this.currentFunction = null;	// Given that functions won't be embedded
    }

    analyzeVariableDeclaration(node, info = null) {
        const scopePath = this.getCurrentScopePath();
        const localConstants = this.localConstants.get(scopePath);
        const variableUses = this.variableUses.get(scopePath);
        // Must be in current scope, so it's correct
        node.declarators.forEach(declarator => {
            if (!declarator.name) return;
			/**
			 * @type {SymbolEntry}
			 */
			const symbol = this.currentScope.lookup(declarator.name);
			const canOptimizeThis = !(symbol.isVolatile || symbol.isStatic || symbol.isExtern);
            
            // 初始化使用统计
            variableUses.set(declarator.name, {
                reads: 0,
                writes: 1, // 初始化算作一次写入
				canOptimize: canOptimizeThis,	// False: already symbolized as no-optimize
                location: declarator.location,
                node: declarator
            });
            
            // 检查常量初始化
            if (declarator.initializer) {
                this.analyzeNode(declarator.initializer, info);
                if (canOptimizeThis) {
					const constValue = this.evaluateConstantExpression(declarator.initializer);
					if (constValue !== undefined) {
						localConstants.set(declarator.name, constValue);
						
						// 标记符号为常量
						if (symbol && this.shouldRunEvaluation(node, info)) {
							symbol.markAsConstant(constValue);
						}
					}
				}
                
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
        const scopePath = this.currentScope.lookupScopeOf(node.name).getPath();
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

	// !! This has manual changes !!
    analyzeAssignmentExpression(node, info = null) {
        
        // 分析右值
        this.analyzeNode(node.left, info);
        this.analyzeNode(node.right, info);
		
		// 分析左值
		// If there's no side effect, still consider optimizing.
		// Logic manually adjusted !
        if (node.left.type === 'Identifier') {
			if (this.hasSideEffects(node.right)) {
				this.addWriteForCurrentScope(node.left.name);
			} else {
				const scopePath = this.currentScope.lookupScopeOf(node.left.name).getPath();
				const localConstants = this.localConstants.get(scopePath);
				const variableUses = this.variableUses.get(scopePath);
				const usage = variableUses.get(node.left.name);
				if (usage && usage.canOptimize) {
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
						if (symbol && this.shouldRunEvaluation(node, info)) {
							symbol.markAsConstant(constValue);
						}
					}
					// Manually added
					this.addWriteForCurrentScope(node.left.name, constValue, this.shouldRunEvaluation(node, info));
				}
				
			}
		}
		if (node.right.type === 'Identifier' && node.right.symbol && node.right.symbol.kind === 'function') {
			this.recordFunctionCall(this.currentFunction, node.right.name);
		}
    }

	recordFunctionCall(callerName, funcName) {
		if (!this.functionCallGraph.has(callerName)) {
			this.functionCallGraph.set(callerName, new Set());
		}
		this.functionCallGraph.get(callerName).add(funcName);
	}

    analyzeFunctionCall(node, info = null) {
		const valid = this.shouldRunEvaluation(node, info);
        if (node.callee.type === 'Identifier') {
            const funcName = node.callee.name;
            
            // 更新函数调用计数
            const info = this.functionInfo.get(funcName);
            if (info) {
                if (valid) info.calls++;
                
                // 记录调用关系
                if (this.currentScope && this.currentScope.astNode) {
                    const callerName = this.currentFunction || 'anonymous';
                    this.recordFunctionCall(callerName, funcName);
                }
            }
        }
        
        // 分析参数
        node.arguments.forEach(arg => this.analyzeNode(arg, info));
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
			if (!info || (!info.isLoop && !info.optimizingLoop)) {
				if (conditionValue !== undefined) {
					node.setAttribute('constantCondition', conditionValue);
					
					if (conditionValue) {
						// 条件总是为真，可以标记then分支总是执行
						this.warnings.push(`Condition always true in if statement`, node.test.location);
					} else {
						// 条件总是为假，可以标记else分支总是执行（如果有）
						this.warnings.push(`Condition always false in if statement`, node.test.location);
					}
				}
			}
			
			
			// 检查条件是否有副作用
			if (this.hasSideEffects(node.test)) {
				node.setAttribute('conditionHasSideEffects', true);
			}
		}
		if (conditionValue !== undefined && info && info.optimizingLoop) {
			if (conditionValue == 0) {
				if (node.alternate) this.analyzeNode(node.alternate, info);
			} else {
				if (node.consequent) this.analyzeNode(node.consequent, info);
			}
		} else {
			// 分析then分支
			const ifInfo = {
				isUnsure: conditionValue == null || conditionValue == undefined,
				parentInfo: info
			};
			if (node.consequent) {
				this.analyzeNode(node.consequent, ifInfo);
				
				// 检查then分支是否有副作用
				if (this.hasSideEffects(node.consequent)) {
					node.setAttribute('consequentHasSideEffects', true);
				}
			}
			
			// 分析else分支
			if (node.alternate) {
				this.analyzeNode(node.alternate, ifInfo);
				
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
				parentLoop: node,
				hasBreak: false,
				hasContinue: false,
				parentInfo: info
			};
		node.setAttribute('hasSideEffects', false);
		// 分析条件表达式
		if (node.test) {
			this.analyzeNode(node.test, loopInfo);
			
			// 检查条件是否为常量
			const conditionValue = this.evaluateConstantExpression(node.test);
			
			// 记录条件信息用于优化
			if (conditionValue !== undefined) {
				//node.setAttribute('constantCondition', conditionValue);
				
				if (conditionValue == 0) {
					// 条件总是为假，循环永远不会执行
					node.setAttribute('neverExecuted', true);
					this.warnings.push(`Loop never executed (while(false))`, node.test.location);
				}
			}
			
			// 检查条件是否有副作用
			if (this.hasSideEffects(node.test)) {
				node.setAttribute('conditionHasSideEffects', true);
			}
		}
		
		// 分析循环体
		if (node.body) {
			// 进入循环体作用域
			
			
			// 分析循环体
			this.analyzeNode(node.body, loopInfo);
			
			// 如果循环永远不会执行，不需要分析循环体
			if (node.getAttribute('neverExecuted')) {
				return;
			}
			
			// 检查循环体是否有副作用
			node.setAttribute('bodyHasSideEffects', node.getAttribute('confirmedSideEffects') || this.hasSideEffects(node.body));
			
			
			// 记录是否有break/continue
			if (loopInfo.hasBreak) {
				node.setAttribute('hasBreak', true);
				node.setAttribute('infiniteLoop', false);
			}
			if (loopInfo.hasContinue) {
				node.setAttribute('hasContinue', true);
			}
		}

		// 尝试分析while循环是否可以优化
		if (!node.getAttribute('bodyHasSideEffects')) {
			if (!info || !info.isLoop) {
				if (this.analyzeWhileLoopForOptimization(node, info)) {
					if ((!info || !info.optimizingLoop) && this.optimizeDeterministicWhileLoop(node)) {
						this.modified = true;
					}
				}
			}
		} else {
			this.markLoopAsPotentiallyWithSideEffects(node);
		}
		
		if (node.getAttribute('infiniteLoop')) {
			this.warnings.push(`Infinite loop detected (while(true))`, node.test.location);
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
			
			if (whileNode.test.left.type === 'Identifier' &&
				whileNode.test.right.type === 'Identifier') {
				// 两边都是变量，难以优化
				return;
			}
			
			if (whileNode.test.left.type === 'Identifier') {
				loopVarName = whileNode.test.left.name;
				comparisonOp = whileNode.test.operator;
				constValue = this.evaluateConstantExpression(whileNode.test.right);
			} else if (whileNode.test.right.type === 'Identifier') {
				loopVarName = whileNode.test.right.name;
				comparisonOp = this.reverseComparisonOperator(whileNode.test.operator);
				constValue = this.evaluateConstantExpression(whileNode.test.left);
			}
			
			if (loopVarName && constValue !== undefined) {
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
						// 尝试计算循环次数
						const iterationCount = this.calculateLoopIterations(
							initValue,
							conditionInfo,
							updateInfo
						);
						
						if (iterationCount !== null && iterationCount >= 0) {
							whileNode.setAttribute('iterationCount', iterationCount);
							
							if (iterationCount === 0) {
								whileNode.setAttribute('neverExecuted', true);
								return true;
							} else if (iterationCount <= 100) {
								// 可以优化
								whileNode.setAttribute('canOptimize', true);
								if (!this.simulateLoop(whileNode, loopVarName, initValue, conditionInfo, updateInfo, iterationCount, info)) {
									whileNode.setAttribute('optimized', false);
									return false;
								}
								whileNode.setAttribute('optimized', true);
								return true;
							}
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
			parentLoop: node,
			hasBreak: false,
			hasContinue: false,
			parentInfo: info
		};
		// 分析初始化部分
		if (node.init) {
			this.analyzeNode(node.init, {
				...loopInfo,
				disposeReturn: true
			});
			
			// 检查初始化是否有副作用
			node.setAttribute('initHasSideEffects', this.hasSideEffects(node.init));
		}
		
		// 分析条件部分
		if (node.test) {
			// ! Always make changes for initialization !
			this.analyzeNode(node.test, loopInfo);
			
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
					this.warnings.push(`Loop never executed (for with constant false condition)`, node.test.location);
				}
			}
			
			// 检查条件是否有副作用
			node.setAttribute('conditionHasSideEffects', this.hasSideEffects(node.test));
		} else {
			// 没有条件表达式，是无限循环 (for(;;))
			// ! Notice that there might be 'break' !
			node.setAttribute('infiniteLoop', true);
		}
		
		// 分析更新部分
		if (node.update) {
			this.analyzeNode(node.update, {
				...loopInfo,
				disposeReturn: true
			});
			
			// 检查更新是否有副作用
			node.setAttribute('updateHasSideEffects', this.hasSideEffects(node.update));
		}
		
		// 分析循环体
		if (node.body) {
			// 分析循环体
			this.analyzeNode(node.body, loopInfo);// Don't do changes
			
			// 检查循环体是否有副作用
			node.setAttribute('bodyHasSideEffects', node.getAttribute('confirmedSideEffects') || this.hasSideEffects(node.body));
			
			// 记录是否有break/continue
			if (loopInfo.hasBreak) {
				node.setAttribute('hasBreak', true);
				node.setAttribute('infiniteLoop', false);
			}
			if (loopInfo.hasContinue) {
				node.setAttribute('hasContinue', true);
			}
		}
		
		// (Manually added) Do optimize afterwards
		if (!node.getAttribute('bodyHasSideEffects')) {
			if (!info || !info.isLoop) {
				if (this.analyzeAndOptimizeForLoop(node, info)) {
					if ((!info || !info.optimizingLoop) && this.optimizeDeterministicForLoop(node)) {
						this.modified = true;
					}
				}
			}
		} else {
			this.markLoopAsPotentiallyWithSideEffects(node);
		}
		
		if (node.getAttribute('infiniteLoop')) {
			this.warnings.push(`Infinite loop detected (for with constant true condition)`, node.test.location);
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
			const MAX_OPTIMIZE_ITERATIONS = 100; // 最大优化循环次数
			if (iterationCount <= MAX_OPTIMIZE_ITERATIONS) {
				// 可以优化这个循环
				forNode.setAttribute('canOptimize', true);
				
				// 模拟循环，更新变量的常量值
				if (iterationCount > 0) {
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
		const configuredMaxSimulation = 100;
		const configuredMaxGrossSimulation = 50000;
		// End
		
		const scopePath = this.currentScope.lookupScopeOf(loopVarName).getPath();
		const localConstants = this.localConstants.get(scopePath);
		const variableUses = this.variableUses.get(scopePath);
		
		// 保存循环前的常量状态
		const savedConstants = new Map(localConstants);
		
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
			grossIterationCount: iterationCount
		};
		
		if (envInfo && envInfo.grossIterationCount) {
			info.grossIterationCount *= envInfo.grossIterationCount;
		}
		if (info.grossIterationCount > configuredMaxGrossSimulation) {
			return false;
		}
		
		// 初始化循环变量
		localConstants.set(loopVarName, initValue);
		loopNode.setAttribute('loopVarName', loopVarName);
		
		// 如果有循环体，尝试分析它
		if (loopNode.body && iterationCount > 0) {
			// 对于简单循环，我们可以模拟少量迭代来传播常量
			const maxSimulatedIterations = Math.min(iterationCount, configuredMaxSimulation);
			let i = 0;
			let broken = false, continuing = false;
			for (i = 0; i < maxSimulatedIterations; i++) {
				// 分析循环体
				// This will result in an execution
				const currentValue = localConstants.get(loopVarName) || initValue;
				
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
				
				const newValue = currentValue + updateInfo.step;
				localConstants.set(loopVarName, newValue);
				
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
			
			if ((!broken) && loopNode.test) {
				const testEval = this.evaluateConstantExpression(loopNode.test);
				if (testEval != 0) {
					// Still can be evaluated!!!
					loopNode.setAttribute('maybeHasSideEffects', true);
				}
			}
			
			// 设置循环后的变量值
			const finalValue = initValue + (iterationCount * updateInfo.step);
			localConstants.set(loopVarName, finalValue);
			
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
	 * 标记循环可能有副作用
	 * @param {ASTNode} loopNode - 循环节点
	 */
	markLoopAsPotentiallyWithSideEffects(loopNode) {

		// 收集循环中可能被修改的变量
		const modifiedVars = this.collectModifiedVariablesInLoop(loopNode);
		
		// 标记这些变量为不可优化
		modifiedVars.forEach(varInfo => {
			const varName = varInfo.varName;
			const scopePath = (varInfo.scope ?? this.currentScope).lookupScopeOf(varName).getPath();
			const variableUses = this.variableUses.get(scopePath);
			const constantList = this.localConstants.get(scopePath);
			const usage = variableUses.get(varName);
			if (usage) {
				usage.canOptimize = false;
			}
			if (constantList.has(varName)) {
				constantList.delete(varName);
			}
		});
		
		// 标记循环有副作用
		loopNode.setAttribute('hasSideEffects', true);
	}

	/**
	 * 收集循环中可能被修改的变量
	 * @param {ASTNode} loopNode - 循环节点
	 * @returns {Set<{varName: string, scope: Scope}>} - 被修改的变量集合
	 * @remark - Manually modified for side effect checkers
	 */
	collectModifiedVariablesInLoop(loopNode) {
		const modifiedVars = new Set();

		// 递归收集赋值表达式中的变量
		// There's no need to make this closure :(
		const collectFromNode = (node) => {
			if (!node) return;
			
			switch (node.type) {
				case 'AssignmentExpression':
					if (node.left.type === 'Identifier') {
						modifiedVars.add({
							varName: node.left.name,
							scope: node.scope
						});
					} else {
						// Unrecognized...
						console.log('Internal Error: Unrecognized variable in loop optimizer', node.left);
					}
					break;
				
				case 'BinaryExpression':
					if (node.operator.includes('=') && node.operator !== '==' && node.operator !== '!='
					&& node.operator !== '>=' && node.operator !== '<=') {
						if (node.left.type === 'Identifier') {
							modifiedVars.add({
								varName: node.left.name,
								scope: node.scope
							});
						} else {
							console.log('Internal Error: Unrecognized variable in loop optimizer',node.left);
						}
					}
					break;
				case 'UnaryExpression':
					if ((node.operator === '++' || node.operator === '--')) {
						if (node.argument.type === 'Identifier') {
							modifiedVars.add({
								varName: node.argument.name,
								scope: node.scope
							});
						} else {
							console.log('Internal Error: Unrecognized variable in loop optimizer',node.left);
						}
					}
					break;
					
				case 'FunctionCall':
				case 'BuiltinCall':
					// 函数调用可能修改全局变量
					// 这里简化处理：如果调用非纯函数，认为所有变量都可能被修改
					// Replaced manually !!!
					if (this.hasSideEffects(node)) {
						// 标记循环有副作用
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
			return this.partiallyOptimizeLoop(forNode, loopVarName, iterationCount);
		}
		
		// 完全可优化的循环
		return this.fullyOptimizeDeterministicLoop(forNode, iterationCount);
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
		this.warnings.push(`Replaced never-executed loop with initialization only`);
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
			return this.partiallyOptimizeLoop(whileNode, loopVarName, iterationCount);
		}
		
		// 完全可优化的循环
		return this.fullyOptimizeDeterministicLoop(whileNode, iterationCount);
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
			const varName = varInfo.varName;

			const scopePath = this.currentScope.lookupScopeOf(varName).getPath();
			const localConstants = this.localConstants.get(scopePath);
			const variableUses = this.variableUses.get(scopePath);
			const finalValue = localConstants.get(varName);
			if (finalValue === undefined) {
				// 变量没有确定的最终值，不能完全优化
				return false;
			}
			varFinalValues.set(varName, finalValue);
			
			// 检查变量的使用情况
			const usage = variableUses.get(varName);
			if (usage && !usage.canOptimize) {
				// 变量标记为不可优化
				return false;
			}
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
		
		this.warnings.push(`Optimized deterministic loop with ${iterationCount} iterations to direct assignments`);
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
	 * @param {Set<string>} modifiedVars - 修改的变量集合
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
			
			modifiedVars.forEach(varName => {
				const scopePath = this.currentScope.lookupScopeOf(varName).getPath();
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
			this.warnings.push(`Partially unrolled loop: ${firstSideEffect} side-effect-free iterations converted to direct assignments`);
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
			info.returnCount++;
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
	addWriteForCurrentScope(varName, noSideEffectValue = null, doUpdate = true) {
		const scopePath = this.currentScope.lookupScopeOf(varName).getPath();
		const variableUses = this.variableUses.get(scopePath);
		const localConstants = this.localConstants.get(scopePath);
		
		const usage = variableUses.get(varName);
		if (usage) {
			usage.writes++;
			if (noSideEffectValue != null) {
				usage.canOptimize = true;
			} else {
				usage.canOptimize = false;
			}
		}
		
		// 赋值会改变变量值，从常量表中移除
		// TODO: Consider removal only if the
		// assigned value is NOT a constant or
		// inside a conditional statement !!
		if (noSideEffectValue == null) {
			localConstants.delete(varName);
		} else if (doUpdate) {
			localConstants.set(varName, noSideEffectValue);
		}
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
        this.removeUnusedVariablesInScope(scope, variableUses);
        
        // 2. 常量传播
        this.constantPropagationInScope(scope, localConstants);
        
        // 3. 控制流优化（使用分析阶段收集的信息）
		//this.optimizeControlFlowInScope(scope);	// Not implemented
		
		// 4. 死代码消除
		this.deadCodeEliminationInScope(scope);
    }

	/**
	 * 
	 * @param {string} varName 
	 */
	variableCanBeRemoved(varName) {
		let symbol = varName;
		if (typeof varName === 'string') {
			symbol = this.currentScope.lookup(varName);
		}
		return (!symbol.isVolatile) && (!symbol.isStatic) && (!symbol.isAddressed);
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
			if (symbol.accessThroughPointer) return;
			if (symbol.type && symbol.type.type && symbol.type.type.kind !== 'basic') return;
			if (!this.variableCanBeRemoved(symbol)) return;
            
            const usage = variableUses.get(symbol.name);
            if (!usage) return;
            
            // 检查变量是否被使用
            const isUsed = usage.reads > 0 || usage.writes > 0;
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
                    this.warnings.push(`Removed unused variable '${symbol.name}' in scope ${scope.getPath()}`);
                }
            }
        });
    }

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
		} else if (typeof value === 'object') {
			if (value.isBuiltinConstant) {
				const result = ASTBuilder.identifier(value.name);
				result.dataType = this.typeTable.get('content_t');
				return result;
			} else {
				return null;
			}
		} else if (value == null) {
			const result = ASTBuilder.nullLiteral();
			result.dataType = this.typeTable.get('null_t');
			return result;
		} else {
			return null;
		}
	}
	/**
	 * 
	 * @param {ASTNode} node 
	 * @param {Map<string, any>} constants 
	 * @bug Notice its time complexity !
	 */
    traverseAndReplaceConstants(node, constants) {
        if (!node) return;
        // Maybe not numeric?
		let rightConstValue;
        switch (node.type) {
            case 'Identifier':
                const constValue = constants.get(node.name);
                if (constValue !== undefined) {
                    // 替换为常量值
                    const replacement = this.getLiteral(constValue);
                    replacement.location = node.location;
					replacement.dataType = node.dataType;
                    this.replaceNode(node, replacement);
                    this.modified = true;
                }
                break;
                
            case 'BinaryExpression':
                // 常量折叠
                const leftValue = this.evaluateConstantExpression(node.left);
                const rightValue = this.evaluateConstantExpression(node.right);
				const isAssignment = node.operator.includes('=') && node.operator !== '==' && node.operator !== '!=' && node.operator !== '>=' && node.operator !== '<=';
                
				if (isAssignment) {
					if (node.left.type === 'Identifier' && this.variableCanBeRemoved(node.left.name) && leftValue !== undefined) {
						this.replaceNode(node, null);
						break;
					}
				}

                if ((isAssignment || leftValue !== undefined) && rightValue !== undefined) {
                    const result = this.evaluateBinaryOperation(node.operator, leftValue, rightValue);
                    if (result !== undefined) {
                        const replacement = this.getLiteral(result);
                        replacement.location = node.location;
						replacement.dataType = node.dataType;
                        this.replaceNode(node, replacement);
                        this.modified = true;
                    }
                }
                
                // 代数简化
                if (!this.modified) {
                    this.simplifyBinaryExpression(node, leftValue, rightValue);
                }
                break;
                
            case 'UnaryExpression':
                const argValue = this.evaluateConstantExpression(node.argument);
                if (argValue !== undefined) {
                    const result = this.evaluateUnaryOperation(node.operator, argValue);
                    if (result !== undefined) {
                        const replacement = this.getLiteral(result);
                        replacement.location = node.location;
						replacement.dataType = node.dataType;
                        this.replaceNode(node, replacement);
                        this.modified = true;
                    }
                }
                break;
                
            case 'IfStatement':
                const testValue = this.evaluateConstantExpression(node.test);
                if (testValue !== undefined) {
                    if (testValue) {
                        // 条件为真，只保留then分支
                        this.replaceNode(node, node.consequent);
                    } else {
                        // 条件为假，只保留else分支（如果有）
                        this.replaceNode(node, node.alternate || null);
                    }
                    this.modified = true;
                }
                break;
                
            case 'WhileStatement':
                const whileTestValue = this.evaluateConstantExpression(node.test);
                if (whileTestValue !== undefined && !whileTestValue) {
                    // 条件为假的while循环，直接删除
                    this.replaceNode(node, null);
                    this.modified = true;
					break;
                }
                break;
			
			case 'AssignmentExpression':
				// It is possible that assign value is given a constant
				if (node.left.type === 'Identifier' && constants.has(node.left.name) && this.variableCanBeRemoved(node.left.name)) {
					this.replaceNode(node, null);
					break;
				}
				rightConstValue = this.evaluateConstantExpression(node.right);
				if (rightConstValue != null) {
					// This is a constant, add it into constant set
					this.replaceNode(node.right, this.getLiteral(rightConstValue));
				} else {
					// Consider removal
					this.traverseAndReplaceConstants(node.right, constants);
				}
				break;
			
			case 'VariableDeclarator':
				// Some problems are here
				if (constants.has(node.name) && this.variableCanBeRemoved(node.name)) {
					this.replaceNode(node, null);
					break;
				}
				rightConstValue = this.evaluateConstantExpression(node.initializer);
				if (rightConstValue != null) {
					// This is a constant, add it into constant set
					this.replaceNode(node.initializer, this.getLiteral(rightConstValue));
				} else {
					// Consider removal
					this.traverseAndReplaceConstants(node.initializer, constants);
				}
				break;

			case 'CompoundStatement':
				node.statements.forEach(stmt => this.traverseAndReplaceConstants(stmt, constants));
        }

		if (!node) {
			return;	// Already modified
		}
        
        // 递归处理子节点
        if (node.children) {
            node.children.forEach(child => this.traverseAndReplaceConstants(child, constants));
        }

        const fieldsToCheck = [
				'functions',
				'statements', 'expression', 'test', 'consequent', 'alternate',
				'body', 'init', 'update', 'argument',
				'declarators', 'arguments', 'callee'
			];	// A little bit special

		for (const field of fieldsToCheck) {
			if (Array.isArray(node[field])) {
				node[field].forEach(elem => this.traverseAndReplaceConstants(elem, constants));
			} else if (node[field] && typeof node[field] === 'object') {
				this.traverseAndReplaceConstants(node[field], constants);
			}
		}
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
                    size: this.estimateFunctionSize(funcNode)
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
						
						// Manually added: also remove symbols
						this.globalScope.removeSymbol(funcName);
						
						// 从AST中删除函数定义
						this.removeNodeFromParent(funcNode);
						this.modified = true;
						this.warnings.push(`Removed unused function '${funcName}'`);
					}
				}
                
            }
        });
    }

	// Has manual changes on 'isAddressed'
    inlineFunctions() {
        // 收集适合内联的函数
        const inlineCandidates = [];
        
        this.functionInfo.forEach((info, funcName) => {
			if (info.isAddressed) return;	// Can't be inlined!
			if (!info.node || !info.node.body) return;
			const paraSize = (info.node.parameters ? (info.node.parameters.length ?? 0) : 0);
			const inlineEstimate = info.calls * info.size;
			const noInlineEstimate = (info.size + 16 + (8 + paraSize) * info.calls + paraSize * 2);
            if (info.calls === 1 || info.isInline || inlineEstimate <= noInlineEstimate) {
                inlineCandidates.push(funcName);
            }
        });
        
		this.recuriveInfo = this.findRecursiveFunctions(this.functionCallGraph);

        // 尝试内联候选函数
        inlineCandidates.forEach(funcName => {
            if (this.shouldInlineFunction(funcName)) {
                this.performFunctionInlining(funcName);
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
	 * 
	 * @param {ASTNode} node 
	 * @param {boolean} [allowConstants=true] Whether to allow builtin constants to be regarded as constants
	 * @returns {number | string | extBuiltinConstant}
	 */
    evaluateConstantExpression(node, allowConstants = true) {
        if (!node) return undefined;
        
        switch (node.type) {
            case 'NumericLiteral':
                return node.value;
            case 'Identifier':
				// Try evaluating in current environment
				const scope = this.currentScope.lookupScopeOf(node.name);
				if (node.name[0] === '@' && allowConstants) {
					// Regard this as a constant value...
					return {
						isBuiltinConstant: true,
						category: ConstantManager.categorize(node.name),
						name: node.name
					};
				}
				if (scope != null) {
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
				}
            case 'BinaryExpression':
			case 'AssignmentExpression':	// Manually added
                const left = this.evaluateConstantExpression(node.left);
                const right = this.evaluateConstantExpression(node.right);
                if (left !== undefined && right !== undefined) {
                    return this.evaluateBinaryOperation(node.operator, left, right);
                }
                return undefined;
            case 'UnaryExpression':
                const arg = this.evaluateConstantExpression(node.argument);
                if (arg !== undefined) {
                    return this.evaluateUnaryOperation(node.operator, arg);
                }
                return undefined;
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
    evaluateBinaryOperation(operator, left, right) {
        switch (operator) {
			case '=': return right;
            case '+': case '+=': return left + right;
            case '-': case '-=': return left - right;
            case '*': case '*=': return left * right;
            case '/': case '/=':
				if (right === 0) {
					this.errors.push('Explicit division by zero');
					return undefined;
				}
				return left / right;
            case '%': case '%=':
				if (right === 0) {
					this.errors.push('Explicit modulo by zero');
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
            case '==': return left == right;
            case '!=': return left != right;
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
    hasSideEffects(node, special = null) {
        if (!node) return false;
		const selectedScope = node.scope ?? this.currentScope;
		const inFunction = special && special.inFunction;

		const functionBuiltin = ['set', 'op', 'jump', 'asm'];

        // 检查节点是否有副作用
        switch (node.type) {
			case 'VariableDeclarator':
			case 'Declarator':
				if (node.dataType && (
					node.dataType.qualifiers.includes('volatile') 
					|| node.dataType.qualifiers.includes('static')
					|| node.dataType.qualifiers.includes('extern'))) {	// 'extern' means that the variable might be modified from the outside
					return true;
				}
				if (node.initializer) {
					return this.hasSideEffects(node.initializer, special);
				}
				break;
            case 'AssignmentExpression':
				if (node.left.dataType && (
					node.left.dataType.qualifiers.includes('volatile') 
					|| node.left.dataType.qualifiers.includes('static')
					|| node.left.dataType.qualifiers.includes('extern'))) {
					return true;
				}
			case 'BinaryExpression':
				return (this.hasSideEffects(node.left, special)) || (this.hasSideEffects(node.right, special));
				// TODO: Double-check logic here
				break;
			//case 'InitializerList':	// Depending on its children
			case 'FunctionCall':
				//return !inFunction;
				//break;
			case 'AsmStatement':
				// There won't be cross-function optimizing
				return true;
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
				
			case 'ReturnStatement':
				// return可能有副作用（返回值表达式）
				// Abortion of control is dealt with elsewhere
				return node.argument ? this.hasSideEffects(node.argument, special) : false;
			case 'IfStatement':
			case 'WhileStatement':
			case 'ForStatement':
				// 控制语句可能有副作用
				return (node.test && this.hasSideEffects(node.test, special)) ||
					   (node.consequent && this.hasSideEffects(node.consequent, special)) ||
					   (node.alternate && this.hasSideEffects(node.alternate, special)) ||
					   (node.body && this.hasSideEffects(node.body, special)) ||
					   (node.init && this.hasSideEffects(node.init, special)) ||
					   (node.update && this.hasSideEffects(node.update, special));
			case 'CompoundStatement':
				// 如果其任何子语句有副作用，复合语句有副作用
				if (node.statements) {
					for (const stmt of node.statements) {
						if (this.hasSideEffects(stmt, special)) {
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
				if (node.name.length > 0 && node.name[0] === '@') return true;
				const scopeInfo = selectedScope.lookupScopeOf(node.name);
				if (!scopeInfo) {
					return true;	// Cannot get information
				}
				const scopePath = scopeInfo.getPath();
				const variableUses = this.variableUses.get(scopePath);
				const variableState = variableUses.get(node.name);
				if (variableState && (!variableState.canOptimize)) {
					return true;
				}
				break;
        }
        
        // 递归检查子节点
		if (node.declarators) {
			let hasSideEffects = false;
			node.declarators.forEach(declarator => {
				hasSideEffects = hasSideEffects || this.hasSideEffects(declarator, special);
			});
			return hasSideEffects;
		}
        if (node.children) {
            for (const child of node.children) {
                if (this.hasSideEffects(child, special)) {
                    return true;
                }
            }
        }
        
        return false;
    }

    simplifyBinaryExpression(node) {
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
				
			case 'ExpressionStatement':
				if (oldNode === parent.expression) {
					if (newNode) {
						newNode.parent = parent;
						parent.expression = newNode;
					} else {
						// 删除整个表达式语句
						this.removeNodeFromParent(parent);
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
				break;
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
        
        let size = 0;
        const countStatements = (node) => {
            if (!node) return;
            
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
                node.children.forEach(child => countStatements(child));
            }

			const fieldsToCheck = [
				'functions', 'globalDeclarations', 'typeDefinitions',
				'statements', 'expression', 'test', 'consequent', 'alternate',
				'body', 'init', 'update', 'argument', 'left', 'right',
				'declarators', 'arguments', 'callee', 'initializer'
			];

			for (const field of fieldsToCheck) {
				if (Array.isArray(node[field])) {
					node[field].forEach(item => countStatements(item));
				} else if (node[field] && typeof node[field] === 'object') {
					countStatements(node[field]);
				}
			}

        };
        
        countStatements(funcNode.body);
        return size;
    }

    shouldInlineFunction(funcName) {
        const info = this.functionInfo.get(funcName);
		if (funcName === 'main') return false;
        if (!info || !info.node.body) return false;
		if (!info.canInline) return false;
		if (this.hasSideEffects(info.node.body, {
			inFunction: true
		})) return false;
        
        // 检查递归调用
        //const calledFunctions = this.functionCallGraph.get(funcName) || new Set();
        if (this.recuriveInfo.has(funcName)) return false; // 递归函数不内联
        
        return true;
    }

	/**
	 * 执行函数内联
	 */
	performFunctionInlining(funcName) {
		const funcInfo = this.functionInfo.get(funcName);
		if (!funcInfo || !funcInfo.node || !funcInfo.node.body) {
			return;
		}
		
		// 找到所有调用点
		const callSites = [];
		this.collectFunctionCallSites(this.ast, funcName, callSites);
		
		if (callSites.length === 0) return;
		
		// 对每个调用点进行内联
		for (const {callNode, parentNode, context} of callSites) {
			this.inlineFunctionAtSite(funcInfo.node, callNode, parentNode, context);
		}
		
		// 如果函数现在没有被调用，可以删除它
		if (funcInfo.calls === 0) {
			this.globalScope.removeSymbol(funcName);	// Manually added
			this.removeNodeFromParent(funcInfo.node);
			this.warnings.push(`Removed unused function '${funcName}' after inlining`);
		}
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
	 * @param {Scope} scope 
	 * @param {Scope} target 
	 */
	reRootScope(scope, target) {
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
			this.variableUses.delete(originalPath);
			this.localConstants.set(newPath, this.localConstants.get(originalPath));
			this.localConstants.delete(originalPath);
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
	inlineFunctionAtSite(funcNode, callNode, parentNode, context) {
		// Cloned body must be a compound statement
		// 检查是否适合内联
		if (!this.isSuitableForInlining(funcNode, callNode, context)) {
			return false;
		}
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
		const clonedBody = this.cloneAST(funcNode.body);
		if (!clonedBody) return false;
		
		// 处理返回语句
		// (This function is no longer used)
		//const returnValue = this.extractAndReplaceReturns(clonedBody, parentNode, callNode);
		
		const changeParameter = () => {
			// Insert parameters after determining where the parent is!
			// 创建参数映射
			const paramMap = new Map();
			/**
			 * @type {AssignmentExpressionNode[]}
			 */
			let unshifts = [];
			funcNode.parameters.forEach((param, index) => {
				if (index < callNode.arguments.length && param.name) {
					//const paramSymbol = funcNode.scope.lookup(param.name).duplicate();
					const symbolName = funcNode.name + ':' + param.name;
					const ident = ASTBuilder.identifier(symbolName);
					const value = this.cloneAST(callNode.arguments[index]);
					const assigner = ASTBuilder.assignmentExpression('=', ident, value);
					ident.dataType = callNode.arguments[index].dataType;
					ident.parent = assigner;
					value.parent = assigner;
					//clonedBody.statements.unshift(assigner);
					unshifts.push(assigner);
					assigner.parent = clonedBody;
					assigner.dataType = ident.dataType;
					ident.scope = value.scope = assigner.scope = funcNode.scope;// parentNode.scope ?? callNode.scope;
					//paramMap.set(param.name, callNode.arguments[index]);
					paramMap.set(param.name, ident);

					if (parentNode.scope) {
						// TODO: Add new symbol into caller's symbol table!
						this.duplicateVariable(funcNode.scope, param.name, parentNode.scope, symbolName);
					}
				}
			});
			
			// 替换参数
			this.replaceParametersInAST(clonedBody, paramMap, funcNode);
			clonedBody.statements.unshift(...unshifts);
		};

		// TODO: ! See whether the return value is got somewhere !
		const doInsertInto = (targetNode, lvalOfReturn, doDeletion) => {
			changeParameter();

			const index = targetNode.statements.indexOf(parentNode);
			if (index !== -1) {
				// 删除原表达式语句，插入函数体内容
				//if (doDeletion) targetNode.statements.splice(index, 1);
				let spliceFix = 0;
				let newCompound = ASTBuilder.compoundStatement();
				newCompound.scope = funcNode.scope;					// Apply similar structure...
				clonedBody.statements.forEach((stmt, i) => {
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
								this.replaceNode(lvalOfReturn.left, compStmt);
							}
							if (lvalOfReturn.right && lvalOfReturn.right === callNode) {
								this.replaceNode(lvalOfReturn.right, compStmt);
							}
							if (lvalOfReturn.argument && lvalOfReturn.argument === callNode) {
								this.replaceNode(lvalOfReturn.argument, compStmt);
							}
							if (lvalOfReturn.initializer && lvalOfReturn.initializer === callNode) {
								this.replaceNode(lvalOfReturn.initializer, compStmt);
							}
							//actualStmt = lvalOfReturn;	// There's no need to insert
							compStmt.parent = lvalOfReturn;
						} else if (!actualStmt) {
							actualStmt = stmt.argument;
							// Might have side effects
						}
					}
					if (actualStmt) {
						//targetNode.statements.splice(index + i + spliceFix, 0, actualStmt);
						//actualStmt.parent = targetNode;
						newCompound.statements.push(actualStmt);
						actualStmt.parent = newCompound;
					}
					
				});
				targetNode.statements.splice(index, 0, newCompound);
				newCompound.parent = targetNode;

				// Deliver the scope:
				if (targetNode.scope && funcNode.scope) {
					this.reRootScope(funcNode.scope, targetNode.scope);
					funcNode.scope.symbols = new Map(
						[...funcNode.scope.symbols].filter(([key, symb]) => symb.kind !== 'parameter')
					);	// Remove all parameters!
				}

				return true;
			} else {
				return false;
			}
		};
		
		let parentOfParent = parentNode.parent, result = true;
		const directParent = parentNode;	// Not modified ver
		// Get first compound statement
		while (parentOfParent && parentOfParent.type !== 'CompoundStatement') {
			parentNode = parentOfParent;
			parentOfParent = parentOfParent.parent;
		}

		// There should be only one call in the 'parent node'
		/**
		 * @type {number}
		 */
		let totalCalls = 0;
		const countTotalCalls = node => {
			if (node.name === callNode.name) totalCalls++;
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
		} else if (directParent.type === 'AssignmentExpression' || directParent.type === 'BinaryExpression' || directParent.type === 'UnaryExpression' || directParent.type === 'VariableDeclarator') {
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
		this.warnings.push(`Inlined function '${funcNode.name}'`);
		return true;
	}

	/**
	 * 检查函数是否适合内联
	 * @remark This function is probably not needed
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
		
		// 检查函数体大小
		const functionSize = this.estimateFunctionSize(funcNode);
		if (functionSize > 25) { // 阈值，可根据需要调整
			return false;
		}
		
		return true;
	}

	/**
	 * 
	 * @param {ASTNode} node 
	 * @param {Map<string, ASTNode>} paramMap 
	 * @param {FunctionDeclarationNode} funcNode 
	 * @returns 
	 */
	replaceParametersInAST(node, paramMap, funcNode) {
		if (!node || !paramMap) return;
		
		switch (node.type) {
			case 'Identifier':
				const replacement = paramMap.get(node.name);
				if (replacement) {
					const clonedReplacement = this.cloneAST(replacement);
					clonedReplacement.location = node.location;
					this.replaceNode(node, clonedReplacement);
				}
				break;
		}
		
		// 递归处理子节点
		if (node.children) {
			node.children.forEach(child => this.replaceParametersInAST(child, paramMap));
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
						this.replaceParametersInAST(item, paramMap);
					}
				});
			} else if (node[field] && typeof node[field] === 'object') {
				this.replaceParametersInAST(node[field], paramMap);
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
	 * 清理全局声明
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
					this.warnings.push('Removed empty global variable declaration');
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
					this.warnings.push(`Removed global variable declaration with no used variables`);
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
					this.warnings.push(`Removed unused type definition '${typeName}'`);
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
					this.warnings.push(`Removed unused function declaration '${func.name}'`);
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
	cloneAST(node) {
		if (!node) return null;
		
		// 创建新节点
		const clone = new ASTNode(node.type, node.location);
		clone.parent = node.parent;
		clone.scope = node.scope;
		
		// 复制属性
		for (const [key, value] of node._attributes) {
			clone.setAttribute(key, value);
		}
		
		// 复制特定字段
		switch (node.type) {
			case 'Program':
				if (node.functions) clone.functions = [];
				if (node.globalDeclarations) clone.globalDeclarations = [];
				if (node.typeDefinitions) clone.typeDefinitions = [];
				break;
				
			case 'FunctionDeclaration':
				clone.name = node.name;
				clone.returnType = node.returnType ? this.cloneAST(node.returnType) : null;
				clone.parameters = node.parameters ? node.parameters.map(p => this.cloneAST(p)) : [];
				clone.isInline = node.isInline || false;
				break;
				
			case 'VariableDeclaration':
				clone.type = node.type ? this.cloneAST(node.type) : null;
				clone.declarators = node.declarators ? node.declarators.map(d => this.cloneAST(d)) : [];
				clone.storageClass = node.storageClass;
				break;
				
			case 'VariableDeclarator':
				clone.name = node.name;
				if (node.initializer) clone.initializer = this.cloneAST(node.initializer);
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
				clone.left = node.left ? this.cloneAST(node.left) : null;
				clone.right = node.right ? this.cloneAST(node.right) : null;
				break;
				
			case 'UnaryExpression':
				clone.operator = node.operator;
				clone.argument = node.argument ? this.cloneAST(node.argument) : null;
				clone.prefix = node.prefix !== undefined ? node.prefix : true;
				break;
				
			case 'IfStatement':
				clone.test = node.test ? this.cloneAST(node.test) : null;
				clone.consequent = node.consequent ? this.cloneAST(node.consequent) : null;
				clone.alternate = node.alternate ? this.cloneAST(node.alternate) : null;
				break;
				
			case 'WhileStatement':
				clone.test = node.test ? this.cloneAST(node.test) : null;
				clone.body = node.body ? this.cloneAST(node.body) : null;
				break;
				
			case 'ForStatement':
				clone.init = node.init ? this.cloneAST(node.init) : null;
				clone.test = node.test ? this.cloneAST(node.test) : null;
				clone.update = node.update ? this.cloneAST(node.update) : null;
				clone.body = node.body ? this.cloneAST(node.body) : null;
				break;
				
			case 'ReturnStatement':
				clone.argument = node.argument ? this.cloneAST(node.argument) : null;
				break;
				
			case 'CompoundStatement':
				clone.statements = node.statements ? node.statements.map(s => this.cloneAST(s)) : [];
				break;
				
			case 'ExpressionStatement':
				clone.expression = node.expression ? this.cloneAST(node.expression) : null;
				break;
				
			case 'FunctionCall':
				clone.callee = node.callee ? this.cloneAST(node.callee) : null;
				clone.arguments = node.arguments ? node.arguments.map(a => this.cloneAST(a)) : [];
				break;
				
			case 'BuiltinCall':
				clone.functionName = node.functionName;
				clone.arguments = node.arguments ? node.arguments.map(a => this.cloneAST(a)) : [];
				break;
				
			case 'MemberExpression':
				if (node.getChild(0)) clone.addChild(this.cloneAST(node.getChild(0)));
				if (node.getChild(1)) clone.addChild(this.cloneAST(node.getChild(1)));
				clone.setAttribute('computed', node.getAttribute('computed') || false);
				clone.setAttribute('operator', node.getAttribute('operator'));
				break;
				
			default:
				// 默认处理：复制子节点
				break;
		}

		if (node.children) {
			node.children.forEach(child => clone.addChild(this.cloneAST(child)));
		}
		
		const fieldsToCheck = [
				'functions', 'globalDeclarations', 'typeDefinitions',
				'statements', 'expression', 'test', 'consequent', 'alternate',
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
				this.warnings.push(`Removed dead code after unconditional control flow in compound statement`);
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
					this.warnings.push(`Removed else branch from always-true if statement`);
				}
			} else {
				// 条件为假，只保留else分支（如果有）
				if (ifStmt.alternate) {
					// 用else分支替换整个if语句
					// 注意：这里我们只标记修改，实际替换在调用者中处理
					this.warnings.push(`If statement with always-false condition can be replaced with else branch`);
				} else {
					// 没有else分支，整个if语句都可以删除
					// 注意：这里我们只标记修改，实际删除在调用者中处理
					this.warnings.push(`If statement with always-false condition and no else branch can be removed`);
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
					this.warnings.push(`Removing else branch from always-true if statement`);
					
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
					this.warnings.push(`Removing then branch from always-false if statement`);
					
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
			this.warnings.push(`Removing never-executed loop`);
			
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
			this.warnings.push(`Removing never-executed for loop`);
			
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
				this.warnings.push(`Unrolling single-iteration for loop`);
				
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
						this.warnings.push(`Removed empty statements from compound statement`);
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

}

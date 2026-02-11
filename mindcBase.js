
// 抽象语法树节点类型
// AST节点类型枚举
export const ASTNodeType = {
    // 程序结构
    PROGRAM: 'Program',
    FUNCTION_DECLARATION: 'FunctionDeclaration',
    FUNCTION_CALL: 'FunctionCall',
    PARAMETER_LIST: 'ParameterList',
    
    // 语句
    EXPRESSION_STATEMENT: 'ExpressionStatement',
    COMPOUND_STATEMENT: 'CompoundStatement',
    IF_STATEMENT: 'IfStatement',
    WHILE_STATEMENT: 'WhileStatement',
    DO_WHILE_STATEMENT: 'DoWhileStatement',
    FOR_STATEMENT: 'ForStatement',
    RETURN_STATEMENT: 'ReturnStatement',
    BREAK_STATEMENT: 'BreakStatement',
    CONTINUE_STATEMENT: 'ContinueStatement',
    DECLARATION_STATEMENT: 'DeclarationStatement',
    
    // 声明
    VARIABLE_DECLARATION: 'VariableDeclaration',
    VARIABLE_DECLARATOR: 'VariableDeclarator',
	
	// 类型定义相关
    TYPEDEF_DECLARATION: 'TypedefDeclaration',
    STRUCT_DEFINITION: 'StructDefinition',
    UNION_DEFINITION: 'UnionDefinition',
    STRUCT_MEMBER: 'StructMember',
    UNION_MEMBER: 'UnionMember',
    
    // 类型限定符
    TYPE_QUALIFIER: 'TypeQualifier',
	
	// 指针类型相关
    POINTER_TYPE: 'PointerType',
    DECLARATOR: 'Declarator',
    
    // 表达式
    BINARY_EXPRESSION: 'BinaryExpression',
	CAST_EXPRESSION: 'CastExpression',
    UNARY_EXPRESSION: 'UnaryExpression',
    ASSIGNMENT_EXPRESSION: 'AssignmentExpression',
    LOGICAL_EXPRESSION: 'LogicalExpression',
    CONDITIONAL_EXPRESSION: 'ConditionalExpression',
    MEMBER_EXPRESSION: 'MemberExpression',
    ARRAY_EXPRESSION: 'ArrayExpression',
    CALL_EXPRESSION: 'CallExpression',
    
    // 字面量
    IDENTIFIER: 'Identifier',
    NUMERIC_LITERAL: 'NumericLiteral',
    STRING_LITERAL: 'StringLiteral',
    CHARACTER_LITERAL: 'CharacterLiteral',
    NULL_LITERAL: 'NullLiteral',
	INITIALIZER_LIST: 'InitializerList',
    
    // 类型
    TYPE_SPECIFIER: 'TypeSpecifier',
    POINTER_TYPE: 'PointerType',
    
    // 特殊
    ASM_STATEMENT: 'AsmStatement',
    BUILTIN_CALL: 'BuiltinCall',
	
	// For optimizer
	DELETED: 'DeletedStatement'
};

export class AttributeClass {
	constructor() {
		this._attributes = new Map();
	}

	setAttribute(key, value) {
        this._attributes.set(key, value);
        return this;
    }
    
    getAttribute(key) {
        return this._attributes.get(key);
    }
    
    hasAttribute(key) {
        return this._attributes.has(key);
    }
}

// AST节点基类
export class ASTNode extends AttributeClass {
    constructor(type, location = null) {
		super();
        this.type = type;
        this.location = location; // { start: { line, column }, end: { line, column } }
        this.parent = null;
        this.children = [];
        this._attributes = new Map(); // 用于存储额外属性
    }
    
    addChild(node) {
        if (node instanceof ASTNode) {
            node.parent = this;
            this.children.push(node);
        }
        return this;
    }
    
    addChildren(nodes) {
        nodes.forEach(node => this.addChild(node));
        return this;
    }
    
    getChild(index) {
        return this.children[index] || null;
    }
    
    getChildren() {
        return this.children.slice();
    }
    
    // 遍历方法
    traverse(visitor, depth = 0) {
        visitor(this, depth);
        this.children.forEach(child => child.traverse(visitor, depth + 1));
    }
    
    // 查找特定类型的子节点
    findNodesOfType(nodeType) {
        const results = [];
        this.traverse((node) => {
            if (node.type === nodeType) {
                results.push(node);
            }
        });
        return results;
    }
    
    toString() {
        return `${this.type}${this.location ? ` at ${this.location.start.line}:${this.location.start.column}` : ''}`;
    }
}

// 编译阶段基类
export class CompilationPhase {
	/**
	 * 
	 * @param {Compiler} compiler 
	 */
    constructor(compiler) {
        this.compiler = compiler;
		this.extraConfig = compiler ? compiler.extraConfig : new AttributeClass();
        this.errors = [];
        this.warnings = [];
    }
    
    addError(message, line = null) {
        this.errors.push({ message, line });
    }
    
    addWarning(message, line = null) {
        this.warnings.push({ message, line });
    }

	// A tool for the compiler!
	/**
	 * 找到所有递归的函数
	 * @param {Map<string, Set<string>>} callGraph - 函数调用关系图
	 * @returns {Set<string>} - 递归的函数集合
	 * @remark By deepseek
	 */
	findRecursiveFunctions(callGraph) {
		// 收集所有函数节点
		const allFunctions = new Set();
		
		for (const [caller, callees] of callGraph) {
			allFunctions.add(caller);
			for (const callee of callees) {
				allFunctions.add(callee);
			}
		}
		
		// 初始化数据结构
		const visited = new Map(); // 访问状态：0=未访问，1=访问中，2=已访问
		const recursive = new Map(); // 是否为递归函数
		const callStack = []; // DFS调用栈
		
		for (const func of allFunctions) {
			visited.set(func, 0);
			recursive.set(func, false);
		}
		
		// DFS搜索，检测环
		function dfs(node) {
			// 将节点标记为访问中
			visited.set(node, 1);
			callStack.push(node);
			
			// 获取当前函数调用的函数集合
			const neighbors = callGraph.get(node) || new Set();
			
			for (const neighbor of neighbors) {
				const neighborState = visited.get(neighbor);
				
				if (neighborState === 0) {
					// 邻居未访问，递归访问
					dfs(neighbor);
					// 如果邻居是递归的，当前节点也是递归的
					if (recursive.get(neighbor)) {
						recursive.set(node, true);
					}
				} else if (neighborState === 1) {
					// 发现环！当前路径上的节点都在环中
					const cycleStartIndex = callStack.indexOf(neighbor);
					for (let i = cycleStartIndex; i < callStack.length; i++) {
						recursive.set(callStack[i], true);
					}
					// 当前节点在环中
					recursive.set(node, true);
				} else { // neighborState === 2
					// 邻居已访问完成
					if (recursive.get(neighbor)) {
						recursive.set(node, true);
					}
				}
			}
			
			// 从栈中弹出节点
			callStack.pop();
			// 标记节点为已访问
			visited.set(node, 2);
		}
		
		// 对每个未访问的节点进行DFS
		for (const func of allFunctions) {
			if (visited.get(func) === 0) {
				dfs(func);
			}
		}
		
		// 收集所有递归函数
		const result = new Set();
		for (const [func, isRecursive] of recursive) {
			if (isRecursive) {
				result.add(func);
			}
		}
		
		return result;
	}
}

// 具体AST节点类
export class ProgramNode extends ASTNode {
    constructor() {
        super(ASTNodeType.PROGRAM);
        this.functions = [];
        this.globalDeclarations = [];
    }
}

// !! Manually modified !!
export class FunctionDeclarationNode extends ASTNode {
    constructor(name, returnType) {
        super(ASTNodeType.FUNCTION_DECLARATION);
        this.name = name;
        this.returnType = returnType;
        this.parameters = [];
        this.body = null;
        this.isBuiltin = false;
		this.storageClass = null;
		this.isInline = false;
		// For function pointer!!!
		this.isFunctionPointer = false;
		this.pointerLayer = 0;
		this.pointerQualifiers = [];
    }
}

export class FunctionCallNode extends ASTNode {
    constructor(callee) {
        super(ASTNodeType.FUNCTION_CALL);
        this.callee = callee;
        this.arguments = [];
    }
}

export class BuiltinCallNode extends ASTNode {
    constructor(functionName) {
        super(ASTNodeType.BUILTIN_CALL);
        this.functionName = functionName;
        this.arguments = [];
    }
}

export class VariableDeclarationNode extends ASTNode {
    constructor(type, declarators) {
        super(ASTNodeType.VARIABLE_DECLARATION);
        this.type = type;
        this.declarators = declarators || [];
        this.storageClass = null; // auto, register, static, extern
    }
}

// 修改VariableDeclaratorNode以支持指针信息
export class VariableDeclaratorNode extends ASTNode {
    constructor(name) {
        super(ASTNodeType.VARIABLE_DECLARATOR);
        this.name = name;
        this.initializer = null;
        this.pointerDepth = 0; // 指针深度
        this.pointerQualifiers = []; // 指针限定符数组，每个元素对应一级指针
        this.arrayDimensions = []; // 数组维度
    }
}

// Initializer uses children! (and has no type information)
export class InitializerListNode extends ASTNode {
	constructor() {
		super(ASTNodeType.INITIALIZER_LIST);
	}
}

export class BinaryExpressionNode extends ASTNode {
    constructor(operator, left, right) {
        super(ASTNodeType.BINARY_EXPRESSION);
        this.operator = operator;
        this.left = left;
        this.right = right;
    }
}

export class UnaryExpressionNode extends ASTNode {
    constructor(operator, argument) {
        super(ASTNodeType.UNARY_EXPRESSION);
        this.operator = operator;
        this.argument = argument;
        this.prefix = true; // 默认为前缀操作符
    }
}

export class AssignmentExpressionNode extends ASTNode {
    constructor(operator, left, right) {
        super(ASTNodeType.ASSIGNMENT_EXPRESSION);
        this.operator = operator;
        this.left = left;
        this.right = right;
    }
}

export class CastExpression extends ASTNode {
	constructor(typeNode, expression) {
		super(ASTNodeType.CAST_EXPRESSION);
		this.typeNode = typeNode;
		this.expression = expression;
	}
}

export class LogicalExpressionNode extends ASTNode {
    constructor(operator, left, right) {
        super(ASTNodeType.LOGICAL_EXPRESSION);
        this.operator = operator;
        this.left = left;
        this.right = right;
    }
}

export class ConditionalExpressionNode extends ASTNode {
    constructor(test, consequent, alternate) {
        super(ASTNodeType.CONDITIONAL_EXPRESSION);
        this.test = test;
        this.consequent = consequent;
        this.alternate = alternate;
    }
}

export class IfStatementNode extends ASTNode {
    constructor(test, consequent, alternate = null) {
        super(ASTNodeType.IF_STATEMENT);
        this.test = test;
        this.consequent = consequent;
        this.alternate = alternate;
    }
}

export class WhileStatementNode extends ASTNode {
    constructor(test, body) {
        super(ASTNodeType.WHILE_STATEMENT);
        this.test = test;
        this.body = body;
    }
}

export class ForStatementNode extends ASTNode {
    constructor(init, test, update, body) {
        super(ASTNodeType.FOR_STATEMENT);
        this.init = init;
        this.test = test;
        this.update = update;
        this.body = body;
    }
}

export class ReturnStatementNode extends ASTNode {
    constructor(argument = null) {
        super(ASTNodeType.RETURN_STATEMENT);
        this.argument = argument;
    }
}

export class CompoundStatementNode extends ASTNode {
    constructor() {
        super(ASTNodeType.COMPOUND_STATEMENT);
        this.statements = [];
    }
}

export class IdentifierNode extends ASTNode {
    constructor(name) {
        super(ASTNodeType.IDENTIFIER);
        this.name = name;
    }
}

export class NumericLiteralNode extends ASTNode {
    constructor(value) {
        super(ASTNodeType.NUMERIC_LITERAL);
        this.value = value;
        this.raw = String(value);
    }
}

export class StringLiteralNode extends ASTNode {
    constructor(value, raw = null) {
        super(ASTNodeType.STRING_LITERAL);
        this.value = value;
        this.raw = raw ?? `"${value}"`;
    }
}

export class CharacterLiteralNode extends ASTNode {
    constructor(value, raw = null) {
        super(ASTNodeType.CHARACTER_LITERAL);
        this.value = value;
        this.raw = raw ?? `'${value}'`;
    }
}

export class NullLiteralNode extends ASTNode {
    constructor() {
        super(ASTNodeType.NULL_LITERAL);
        this.value = null;
        this.raw = 'null';
    }
}

// 修改TypeSpecifierNode以存储限定符信息
// ! Also modified for function pointer !
export class TypeSpecifierNode extends ASTNode {
    constructor(typeName) {
        super(ASTNodeType.TYPE_SPECIFIER);
        this.typeName = typeName;
		// qualifiers are in attributes
        //this.qualifiers = []; // 类型限定符（const, volatile）
		this.storageClass = null;
        this.pointerDepth = 0; // 指针深度
        this.pointerQualifiers = []; // 指针限定符
		this.isFunctionType = false;
		this.correspondingFunction = null;	// With a FunctionDeclarationNode, if is a function type
    }
}

// 添加新的AST节点类
export class TypedefDeclarationNode extends ASTNode {
    constructor(type, declarators) {
        super(ASTNodeType.TYPEDEF_DECLARATION);
        this.type = type; // 基础类型
        this.declarators = declarators || []; // 类型别名列表
        this.isStruct = false;
        this.isUnion = false;
		this.isFunction = false;
        this.structDefinition = null; // 如果是结构体/联合体类型定义
		this.functionDefinition = null;	// Reserved for function pointer
    }
}

export class StructDefinitionNode extends ASTNode {
    constructor(name) {
        super(ASTNodeType.STRUCT_DEFINITION);
        this.name = name;
        this.members = [];
        this.isDefinition = false; // 是否完整定义（有成员列表）
    }
}

export class UnionDefinitionNode extends ASTNode {
    constructor(name) {
        super(ASTNodeType.UNION_DEFINITION);
        this.name = name;
        this.members = [];
        this.isDefinition = false; // 是否完整定义（有成员列表）
    }
}

export class StructMemberNode extends ASTNode {
    constructor(type, name) {
        super(ASTNodeType.STRUCT_MEMBER);
        this.type = type;
        this.name = name;
        this.bitField = null; // 位域大小（如果有）
    }
}

export class TypeQualifierNode extends ASTNode {
    constructor(qualifier) {
        super(ASTNodeType.TYPE_QUALIFIER);
        this.qualifier = qualifier; // 'const' 或 'volatile'
    }
}

export class AsmStatementNode extends ASTNode {
    constructor(code) {
        super(ASTNodeType.ASM_STATEMENT);
        this.code = code;
    }
}

export class PointerTypeNode extends ASTNode {
    constructor(baseType, qualifiers = [], pointerDepth = 1) {
        super(ASTNodeType.POINTER_TYPE);
        this.baseType = baseType; // 指向的类型
        this.qualifiers = qualifiers; // 指针本身的限定符
        this.pointerDepth = pointerDepth; // 指针深度（1表示单级指针）
        this.innerPointer = null; // 内层指针（用于多级指针）
    }
}

export class DeclaratorNode extends ASTNode {
    constructor(name) {
        super(ASTNodeType.DECLARATOR);
        this.name = name; // 变量名
        this.pointerDepth = 0; // 指针深度
        this.pointerQualifiers = []; // 指针限定符数组，每个元素对应一级指针
        this.arrayDimensions = []; // 数组维度
        this.functionParams = null; // 函数参数（用于函数指针）
		this.initializer = null;
    }
}

// Added in 2-2
// 在优化器类中添加一个内部标记节点类型
export class DeletedStatement extends ASTNode {
    constructor() {
        super('DeletedStatement');
    }
}

// AST构建器工具类
export class ASTBuilder {
    static program() {
        return new ProgramNode();
    }
    
    static functionDeclaration(name, returnType) {
        return new FunctionDeclarationNode(name, returnType);
    }
    
    static functionCall(callee) {
        return new FunctionCallNode(callee);
    }
    
    static builtinCall(functionName) {
        return new BuiltinCallNode(functionName);
    }
    
    static variableDeclaration(type, declarators) {
        return new VariableDeclarationNode(type, declarators);
    }
    
    static variableDeclarator(name) {
        return new VariableDeclaratorNode(name);
    }
    
    static binaryExpression(operator, left, right) {
        return new BinaryExpressionNode(operator, left, right);
    }
    
    static unaryExpression(operator, argument) {
        return new UnaryExpressionNode(operator, argument);
    }
    
    static assignmentExpression(operator, left, right) {
        return new AssignmentExpressionNode(operator, left, right);
    }
    
    static logicalExpression(operator, left, right) {
        return new LogicalExpressionNode(operator, left, right);
    }
    
    static conditionalExpression(test, consequent, alternate) {
        return new ConditionalExpressionNode(test, consequent, alternate);
    }
    
    static ifStatement(test, consequent, alternate = null) {
        return new IfStatementNode(test, consequent, alternate);
    }
    
    static whileStatement(test, body) {
        return new WhileStatementNode(test, body);
    }
    
    static forStatement(init, test, update, body) {
        return new ForStatementNode(init, test, update, body);
    }
    
    static returnStatement(argument = null) {
        return new ReturnStatementNode(argument);
    }
    
    static compoundStatement() {
        return new CompoundStatementNode();
    }
    
    static identifier(name) {
        return new IdentifierNode(name);
    }
    
    static numericLiteral(value) {
        return new NumericLiteralNode(value);
    }
    
    static stringLiteral(value, raw = null) {
        return new StringLiteralNode(value, raw);
    }
    
    static characterLiteral(value, raw = null) {
        return new CharacterLiteralNode(value, raw);
    }
    
    static nullLiteral() {
        return new NullLiteralNode();
    }

	static initializerList() {
		return new InitializerListNode();
	}
    
    static typeSpecifier(typeName) {
        return new TypeSpecifierNode(typeName);
    }
	
	static typedefDeclaration(type, declarators) {
        return new TypedefDeclarationNode(type, declarators);
    }
    
    static structDefinition(name) {
        return new StructDefinitionNode(name);
    }
    
    static unionDefinition(name) {
        return new UnionDefinitionNode(name);
    }
    
    static structMember(type, name) {
        return new StructMemberNode(type, name);
    }
    
    static typeQualifier(qualifier) {
        return new TypeQualifierNode(qualifier);
    }
    
	static pointerType(baseType, qualifiers = [], pointerDepth = 1) {
        return new PointerTypeNode(baseType, qualifiers, pointerDepth);
    }
    
    static declarator(name) {
        return new DeclaratorNode(name);
    }
	
    static asmStatement(code) {
        return new AsmStatementNode(code);
    }
}

// AST访问者模式基类
export class ASTVisitor extends CompilationPhase {
    visit(node, param = null) {
		// This modification is manually done.
		let extracted = node.type;
		if (typeof extracted != "string") {
			extracted = extracted.type;
			if (extracted == "TypeSpecifier") {
				extracted = "VariableDeclaration";
			}
		}
			
        const methodName = `visit${extracted}`;
        if (this[methodName]) {
            return this[methodName](node, param);
        }
		// default visitor has no param, but we still add it
        return this.visitDefault(node, param);
    }
    
    visitDefault(node, param = null) {
        // 默认遍历所有子节点
        node.children.forEach(child => this.visit(child, param));
    }

	/**
	 * @callback ASTVisitorCallback
	 * @param {ASTNode} node
	 * 
	 * @callback ASTVisitorHalt
	 * @returns {boolean} Whether to stop visiting.
	 */
	/**
	 * 
	 * @param {ASTNode} node 
	 * @param {ASTVisitorCallback} callee 
	 * @param {ASTVisitorHalt | null} [halt=null]
	 */
	further(node, callee, halt = null) {
		const fields = [
			'functions', 'globalDeclarations', 'typeDefinitions',
			'statements', 'expression', 'test', 'consequent', 'alternate',
			'body', 'init', 'update', 'argument', 'left', 'right',
			'declarators', 'arguments', 'callee', 'initializer'
		];
		
		for (const field of fields) {
			if (node[field]) {
				if (Array.isArray(node[field])) {
					for (const item of node[field]) {
						callee(item);
						if (halt && halt()) break;
					}
				} else if (typeof node[field] === 'object') {
					callee(node[field]);
				}
			}
			if (halt && halt()) break;
		}

		node.children.forEach(child => {
			if (halt && (!halt())) callee(child);
		});
	}
}

export class ConstantManager {
	static categorize(token) {
		if (objectList.includes(token)) return 'object';
		else if (liquidList.includes(token)) return 'liquid';
		else if (unitList.includes(token)) return 'unit';
		else if (buildingList.includes(token)) return 'building';
		else return 'unknown';
	}
}

/**
 * 
 * @param {string} s 
 */
export const __convert = s => s.split(',').map(t => t.trim());
export const objectList = __convert(
	'@copper, @lead, @coal, @graphite, @scrap, @sand, @pyratite, @silicon, @spore-pod, @blast-compound, @titanium, @plastanium, @thorium, @phase-fabric, @surge-alloy, @beryllium, @tungsten, @oxide, @carbide'
);
export const liquidList = __convert(
	'@water, @slag, @oil, @cryofluid, @neoplasm, @gallium, @ozone, @hydrogen, @cyanogen, @nitrogen'
);
export const unitList = __convert(
	'@Aegires, @Alpha, @Anthicus, @Antumbra, @Arkyid, @Atrax, @Avert, @Beta, @Bryde, @Cleroi, @Collaris, @Conquer, @Corvus, @Crawler, @Cyerce, @Dagger, @Disrupt, @Eclipse, @Elude, @Emanate, @Evoke, @Flare, @Fortress, @Gamma, @Guardian, @Horizon, @Incite, @Latum, @Locus, @Mace, @Manifold, @Mega, @Merui, @Minke, @Mono, @Navanax, @Nova, @Obviate, @Oct, @Omura, @Oxynoe, @Poly, @Precept, @Pulsar, @Quad, @Quasar, @Quell, @Reign, @Renale, @Retusa, @Risso, @Scepter, @Sei, @Spiroct, @Stell, @Tecta, @Toxopid, @Vanquish, @Vela, @Zenith'
).map(s => s.toLowerCase());
export const buildingList = __convert(
	[
		'@core-shard, @core-foundation, @core-nucleus, @core-bastion, @core-acropolis',
		'@mechanical-drill, @pneumatic-drill, @laser-drill, @blast-drill, @water-extractor, @cultivator, @oil-extractor',
		'@conveyor, @titanium-conveyor, @plastanium-conveyor, @armored-conveyor, @junction, @bridge-conveyor, @phase-conveyor, @sorter, @inverted-sorter, @router, @distributor, @overflow-gate, @underflow-gate, @mass-driver',
		'@conduit, @pulse-conveyor, @plated-conveyor, @rotary-pump, @thermal-pump, @conduit, @pipeline, @plated-pipeline, @liquid-router, @liquid-tank, @liquid-junction, @bridge-conduit, @phase-conveyor',
		'@combustion-generator, @thermal-generator, @steam-generator, @differential-generator, @rtg-generator, @solar-panel, @large-solar-panel, @thorium-reactor, @impact-reactor, @battery, @large-battery, @power-node, @power-node-large, @surge-tower, @diode',
		'@copper-wall, @large-copper-wall, @titanium-wall, @large-titanium-wall, @durium-wall, @large-durium-wall, @thorium-wall, @large-thorium-wall, @phase-wall, @large-phase-wall, @surge-wall, @large-surge-wall, @door, @large-door, @mender, @mend-projector, @force-projector, @overdrive-projector, @overdrive-dome, @repair-point, @repair-turret',
		'@duo, @scatter, @scorch, @hail, @arc, @wave, @lancer, @swarmer, @salvo, @fuse, @ripple, @cyclone, @foreshadow, @spectre, @meltdown, @segment, @parallax, @tsunami',
		'@dagger-factory, @mace-factory, @fortress-factory, @scepter-factory, @reign-factory',
		'@nova-factory, @pulsar-factory, @quasar-factory, @vela-factory, @corvus-factory',
		'@crawler-factory, @atrax-factory, @spiroct-factory, @arkyid-factory, @toxopid-factory',
		'@silicon-smelter, @silicon-smelter2, @silicon-smelter3, @kiln, @plastanium-compressor, @phase-weaver, @surge-smelter, @cryofluid-mixer, @pyratite-mixer, @blast-mixer, @melter, @separator, @disassembler, @spore-press, @pulverizer, @coal-centrifuge, @multi-press',
		'@container, @vault, @core-shard, @core-foundation, @core-nucleus, @unloader',
		'@liquid-tank, @conduit, @liquid-router',
		'@container, @vault, @unloader',
		'@illuminator, @payload-conveyor, @payload-router, @incinerator, @power-source, @power-void, @item-source, @item-void, @liquid-source, @liquid-void, @payload-source, @payload-void, @message, @switch, @micro-processor, @logic-processor, @hyper-processor, @memory-cell, @memory-bank, @logic-display, @large-logic-display',
		'@oxidis'
	].join(',')
);

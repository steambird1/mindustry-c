/*
MinCCompiler on JS
By Seabird Starch Gunnhildr and Deepseek
(Mainly the latter :)

WARNING and DISCLAIMER:

This code is only tested by some very small and simple samples
so far.
*/

class C89ToMindustryCompiler {
    constructor() {
        // 编译器配置
        this.config = {
            targetLanguage: 'mindustry_low_level',
            standard: 'C89',
            memoryBlocks: ['cell1', 'cell2', 'cell3', 'cell4'], // 可扩展
            maxMemorySize: 1024
        };
        
        // 编译状态
        this.state = {
            currentScope: null,
            variables: new Map(),      // 变量符号表
            functions: new Map(),      // 函数符号表
            labels: new Map(),         // 标签管理
            memoryAllocations: new Map(), // 内存分配
            tempVarCounter: 0,         // 临时变量计数
            labelCounter: 0            // 标签计数
        };
        
        // 类型系统
        this.types = {
            'int': { size: 1, baseType: 'numeric' },
            'float': { size: 1, baseType: 'numeric' },
            'char': { size: 1, baseType: 'numeric' },
            'char*': { size: 1, baseType: 'pointer' },
            'void': { size: 0, baseType: 'void' },
            'device': { size: 1, baseType: 'device' },
            'null_t': { size: 1, baseType: 'null' }
        };
        
        // 操作符映射
        this.operatorMap = {
            '+': 'add',
            '-': 'sub',
            '*': 'mul',
            '/': 'div',
            '%': 'mod',
            '&': 'and',
            '|': 'or',
            '^': 'xor',
            '<<': 'shl',
            '>>': 'shr',
            '==': 'equal',
            '!=': 'notEqual',
            '<': 'lessThan',
            '>': 'greaterThan',
            '<=': 'lessThanEq',
            '>=': 'greaterThanEq'
        };
        
        // 内建函数映射
        this.builtinFunctions = new Set([
            'draw', 'print', 'drawflush', 'printflush', 'control',
            'radar', 'sensor', 'lookup', 'wait', 'stop', 'end', 'asm'
        ]);
    }
}

// 抽象语法树节点类型
// AST节点类型枚举
const ASTNodeType = {
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
    
    // 类型
    TYPE_SPECIFIER: 'TypeSpecifier',
    POINTER_TYPE: 'PointerType',
    
    // 特殊
    ASM_STATEMENT: 'AsmStatement',
    BUILTIN_CALL: 'BuiltinCall',
	
	// For optimizer
	DELETED: 'DeletedStatement'
};

// AST节点基类
class ASTNode {
    constructor(type, location = null) {
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

// 具体AST节点类
class ProgramNode extends ASTNode {
    constructor() {
        super(ASTNodeType.PROGRAM);
        this.functions = [];
        this.globalDeclarations = [];
    }
}

// !! Manually modified !!
class FunctionDeclarationNode extends ASTNode {
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

class FunctionCallNode extends ASTNode {
    constructor(callee) {
        super(ASTNodeType.FUNCTION_CALL);
        this.callee = callee;
        this.arguments = [];
    }
}

class BuiltinCallNode extends ASTNode {
    constructor(functionName) {
        super(ASTNodeType.BUILTIN_CALL);
        this.functionName = functionName;
        this.arguments = [];
    }
}

class VariableDeclarationNode extends ASTNode {
    constructor(type, declarators) {
        super(ASTNodeType.VARIABLE_DECLARATION);
        this.type = type;
        this.declarators = declarators || [];
        this.storageClass = null; // auto, register, static, extern
    }
}

// 修改VariableDeclaratorNode以支持指针信息
class VariableDeclaratorNode extends ASTNode {
    constructor(name) {
        super(ASTNodeType.VARIABLE_DECLARATOR);
        this.name = name;
        this.initializer = null;
        this.pointerDepth = 0; // 指针深度
        this.pointerQualifiers = []; // 指针限定符数组，每个元素对应一级指针
        this.arrayDimensions = []; // 数组维度
    }
}

class BinaryExpressionNode extends ASTNode {
    constructor(operator, left, right) {
        super(ASTNodeType.BINARY_EXPRESSION);
        this.operator = operator;
        this.left = left;
        this.right = right;
    }
}

class UnaryExpressionNode extends ASTNode {
    constructor(operator, argument) {
        super(ASTNodeType.UNARY_EXPRESSION);
        this.operator = operator;
        this.argument = argument;
        this.prefix = true; // 默认为前缀操作符
    }
}

class AssignmentExpressionNode extends ASTNode {
    constructor(operator, left, right) {
        super(ASTNodeType.ASSIGNMENT_EXPRESSION);
        this.operator = operator;
        this.left = left;
        this.right = right;
    }
}

class CastExpression extends ASTNode {
	constructor(typeNode, expression) {
		super(ASTNodeType.CAST_EXPRESSION);
		this.typeNode = typeNode;
		this.expression = expression;
	}
}

class LogicalExpressionNode extends ASTNode {
    constructor(operator, left, right) {
        super(ASTNodeType.LOGICAL_EXPRESSION);
        this.operator = operator;
        this.left = left;
        this.right = right;
    }
}

class ConditionalExpressionNode extends ASTNode {
    constructor(test, consequent, alternate) {
        super(ASTNodeType.CONDITIONAL_EXPRESSION);
        this.test = test;
        this.consequent = consequent;
        this.alternate = alternate;
    }
}

class IfStatementNode extends ASTNode {
    constructor(test, consequent, alternate = null) {
        super(ASTNodeType.IF_STATEMENT);
        this.test = test;
        this.consequent = consequent;
        this.alternate = alternate;
    }
}

class WhileStatementNode extends ASTNode {
    constructor(test, body) {
        super(ASTNodeType.WHILE_STATEMENT);
        this.test = test;
        this.body = body;
    }
}

class ForStatementNode extends ASTNode {
    constructor(init, test, update, body) {
        super(ASTNodeType.FOR_STATEMENT);
        this.init = init;
        this.test = test;
        this.update = update;
        this.body = body;
    }
}

class ReturnStatementNode extends ASTNode {
    constructor(argument = null) {
        super(ASTNodeType.RETURN_STATEMENT);
        this.argument = argument;
    }
}

class CompoundStatementNode extends ASTNode {
    constructor() {
        super(ASTNodeType.COMPOUND_STATEMENT);
        this.statements = [];
    }
}

class IdentifierNode extends ASTNode {
    constructor(name) {
        super(ASTNodeType.IDENTIFIER);
        this.name = name;
    }
}

class NumericLiteralNode extends ASTNode {
    constructor(value) {
        super(ASTNodeType.NUMERIC_LITERAL);
        this.value = value;
        this.raw = String(value);
    }
}

class StringLiteralNode extends ASTNode {
    constructor(value) {
        super(ASTNodeType.STRING_LITERAL);
        this.value = value;
        this.raw = `"${value}"`;
    }
}

class CharacterLiteralNode extends ASTNode {
    constructor(value) {
        super(ASTNodeType.CHARACTER_LITERAL);
        this.value = value;
        this.raw = `'${value}'`;
    }
}

class NullLiteralNode extends ASTNode {
    constructor() {
        super(ASTNodeType.NULL_LITERAL);
        this.value = null;
        this.raw = 'null';
    }
}

// 修改TypeSpecifierNode以存储限定符信息
// ! Also modified for function pointer !
class TypeSpecifierNode extends ASTNode {
    constructor(typeName) {
        super(ASTNodeType.TYPE_SPECIFIER);
        this.typeName = typeName;
        this.qualifiers = []; // 类型限定符（const, volatile）
        this.pointerDepth = 0; // 指针深度
        this.pointerQualifiers = []; // 指针限定符
		this.isFunctionType = false;
		this.correspondingFunction = null;	// With a FunctionDeclarationNode, if is a function type
    }
}

// 添加新的AST节点类
class TypedefDeclarationNode extends ASTNode {
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

class StructDefinitionNode extends ASTNode {
    constructor(name) {
        super(ASTNodeType.STRUCT_DEFINITION);
        this.name = name;
        this.members = [];
        this.isDefinition = false; // 是否完整定义（有成员列表）
    }
}

class UnionDefinitionNode extends ASTNode {
    constructor(name) {
        super(ASTNodeType.UNION_DEFINITION);
        this.name = name;
        this.members = [];
        this.isDefinition = false; // 是否完整定义（有成员列表）
    }
}

class StructMemberNode extends ASTNode {
    constructor(type, name) {
        super(ASTNodeType.STRUCT_MEMBER);
        this.type = type;
        this.name = name;
        this.bitField = null; // 位域大小（如果有）
    }
}

class TypeQualifierNode extends ASTNode {
    constructor(qualifier) {
        super(ASTNodeType.TYPE_QUALIFIER);
        this.qualifier = qualifier; // 'const' 或 'volatile'
    }
}

class AsmStatementNode extends ASTNode {
    constructor(code) {
        super(ASTNodeType.ASM_STATEMENT);
        this.code = code;
    }
}

class PointerTypeNode extends ASTNode {
    constructor(baseType, qualifiers = [], pointerDepth = 1) {
        super(ASTNodeType.POINTER_TYPE);
        this.baseType = baseType; // 指向的类型
        this.qualifiers = qualifiers; // 指针本身的限定符
        this.pointerDepth = pointerDepth; // 指针深度（1表示单级指针）
        this.innerPointer = null; // 内层指针（用于多级指针）
    }
}

class DeclaratorNode extends ASTNode {
    constructor(name) {
        super(ASTNodeType.DECLARATOR);
        this.name = name; // 变量名
        this.pointerDepth = 0; // 指针深度
        this.pointerQualifiers = []; // 指针限定符数组，每个元素对应一级指针
        this.arrayDimensions = []; // 数组维度
        this.functionParams = null; // 函数参数（用于函数指针）
    }
}

// Added in 2-2
// 在优化器类中添加一个内部标记节点类型
class DeletedStatement extends ASTNode {
    constructor() {
        super('DeletedStatement');
    }
}

// AST构建器工具类
class ASTBuilder {
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
    
    static stringLiteral(value) {
        return new StringLiteralNode(value);
    }
    
    static characterLiteral(value) {
        return new CharacterLiteralNode(value);
    }
    
    static nullLiteral() {
        return new NullLiteralNode();
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
class ASTVisitor {
    visit(node) {
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
            return this[methodName](node);
        }
        return this.visitDefault(node);
    }
    
    visitDefault(node) {
        // 默认遍历所有子节点
        node.children.forEach(child => this.visit(child));
    }
}

// 符号表条目
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

// 编译阶段基类
class CompilationPhase {
    constructor(compiler) {
        this.compiler = compiler;
        this.errors = [];
        this.warnings = [];
    }
    
    addError(message, line = null) {
        this.errors.push({ message, line });
    }
    
    addWarning(message, line = null) {
        this.warnings.push({ message, line });
    }
}

// 词法分析器
// Token类型枚举
const TokenType = {
    // 标识符和字面量
    IDENTIFIER: 'IDENTIFIER',
    NUMBER: 'NUMBER',
    STRING: 'STRING',
    CHARACTER: 'CHARACTER',
    
    // 关键字
    AUTO: 'AUTO',
    BREAK: 'BREAK',
    CASE: 'CASE',
    CHAR: 'CHAR',
    CONST: 'CONST',
    CONTINUE: 'CONTINUE',
    DEFAULT: 'DEFAULT',
    DO: 'DO',
    DOUBLE: 'DOUBLE',
    ELSE: 'ELSE',
    ENUM: 'ENUM',
    EXTERN: 'EXTERN',
    FLOAT: 'FLOAT',
    FOR: 'FOR',
    GOTO: 'GOTO',
    IF: 'IF',
    INT: 'INT',
	INLINE: 'INLINE',
    LONG: 'LONG',
    REGISTER: 'REGISTER',
    RETURN: 'RETURN',
    SHORT: 'SHORT',
    SIGNED: 'SIGNED',
    SIZEOF: 'SIZEOF',
    STATIC: 'STATIC',
    STRUCT: 'STRUCT',
    SWITCH: 'SWITCH',
    TYPEDEF: 'TYPEDEF',
    UNION: 'UNION',
    UNSIGNED: 'UNSIGNED',
    VOID: 'VOID',
    VOLATILE: 'VOLATILE',
    WHILE: 'WHILE',
    
    // 特殊关键字
    NULL: 'NULL',
    
    // 运算符
    PLUS: 'PLUS',
    MINUS: 'MINUS',
    MULTIPLY: 'MULTIPLY',
    DIVIDE: 'DIVIDE',
    MODULO: 'MODULO',
    
    // 赋值运算符
    ASSIGN: 'ASSIGN',
    PLUS_ASSIGN: 'PLUS_ASSIGN',
    MINUS_ASSIGN: 'MINUS_ASSIGN',
    MULTIPLY_ASSIGN: 'MULTIPLY_ASSIGN',
    DIVIDE_ASSIGN: 'DIVIDE_ASSIGN',
    MODULO_ASSIGN: 'MODULO_ASSIGN',
    
    // 比较运算符
    EQUAL: 'EQUAL',
    NOT_EQUAL: 'NOT_EQUAL',
    LESS_THAN: 'LESS_THAN',
    LESS_EQUAL: 'LESS_EQUAL',
    GREATER_THAN: 'GREATER_THAN',
    GREATER_EQUAL: 'GREATER_EQUAL',
    
    // 逻辑运算符
    AND: 'AND',
    OR: 'OR',
    NOT: 'NOT',
    
    // 位运算符
    BITWISE_AND: 'BITWISE_AND',
    BITWISE_OR: 'BITWISE_OR',
    BITWISE_XOR: 'BITWISE_XOR',
    BITWISE_NOT: 'BITWISE_NOT',
    LEFT_SHIFT: 'LEFT_SHIFT',
    RIGHT_SHIFT: 'RIGHT_SHIFT',
    
    // 增量/减量
    INCREMENT: 'INCREMENT',
    DECREMENT: 'DECREMENT',
    
    // 标点符号
    SEMICOLON: 'SEMICOLON',
    COMMA: 'COMMA',
    DOT: 'DOT',
    ARROW: 'ARROW',
    
    // 括号
    LEFT_PAREN: 'LEFT_PAREN',
    RIGHT_PAREN: 'RIGHT_PAREN',
    LEFT_BRACE: 'LEFT_BRACE',
    RIGHT_BRACE: 'RIGHT_BRACE',
    LEFT_BRACKET: 'LEFT_BRACKET',
    RIGHT_BRACKET: 'RIGHT_BRACKET',
    
    // 其他
    COLON: 'COLON',
    QUESTION: 'QUESTION',
    
    // 特殊指令和内建函数
    ASM: 'ASM',
    DRAW: 'DRAW',
    PRINT: 'PRINT',
    DRAWFLUSH: 'DRAWFLUSH',
    PRINTFLUSH: 'PRINTFLUSH',
    GETLINK: 'GETLINK',
    CONTROL: 'CONTROL',
    RADAR: 'RADAR',
    SENSOR: 'SENSOR',
    SET: 'SET',
    OP: 'OP',
    LOOKUP: 'LOOKUP',
    WAIT: 'WAIT',
    STOP: 'STOP',
    END: 'END',
    JUMP: 'JUMP',
    READ: 'READ',
    WRITE: 'WRITE',
    
    // 特殊常量
    PREDEFINED_CONSTANT: 'PREDEFINED_CONSTANT', // @开头的常量
    
    // 结束标记
    EOF: 'EOF'
};

// Token类
class Token {
    constructor(type, value, location = null) {
        this.type = type;
        this.value = value;
        this.location = location; // { line, column, index }
    }
    
    toString() {
        return `Token(${this.type}, ${JSON.stringify(this.value)})`;
    }
    
    isOperator() {
        const operators = [
            TokenType.PLUS, TokenType.MINUS, TokenType.MULTIPLY, TokenType.DIVIDE, TokenType.MODULO,
            TokenType.ASSIGN, TokenType.EQUAL, TokenType.NOT_EQUAL, TokenType.LESS_THAN,
            TokenType.LESS_EQUAL, TokenType.GREATER_THAN, TokenType.GREATER_EQUAL,
            TokenType.AND, TokenType.OR, TokenType.NOT, TokenType.BITWISE_AND,
            TokenType.BITWISE_OR, TokenType.BITWISE_XOR, TokenType.LEFT_SHIFT, TokenType.RIGHT_SHIFT
        ];
        return operators.includes(this.type);
    }
    
    isKeyword() {
        const keywords = [
            TokenType.AUTO, TokenType.BREAK, TokenType.CASE, TokenType.CHAR, TokenType.CONST,
            TokenType.CONTINUE, TokenType.DEFAULT, TokenType.DO, TokenType.DOUBLE, TokenType.ELSE,
            TokenType.ENUM, TokenType.EXTERN, TokenType.FLOAT, TokenType.FOR, TokenType.GOTO,
            TokenType.IF, TokenType.INT, TokenType.LONG, TokenType.REGISTER, TokenType.RETURN,
            TokenType.SHORT, TokenType.SIGNED, TokenType.SIZEOF, TokenType.STATIC, TokenType.STRUCT,
            TokenType.SWITCH, TokenType.TYPEDEF, TokenType.UNION, TokenType.UNSIGNED, TokenType.VOID,
            TokenType.VOLATILE, TokenType.WHILE, TokenType.NULL
        ];
        return keywords.includes(this.type);
    }
}

// 关键字映射
const KEYWORDS = {
    'auto': TokenType.AUTO,
    'break': TokenType.BREAK,
    'case': TokenType.CASE,
    'char': TokenType.CHAR,
    'const': TokenType.CONST,
    'continue': TokenType.CONTINUE,
    'default': TokenType.DEFAULT,
    'do': TokenType.DO,
    'double': TokenType.DOUBLE,
    'else': TokenType.ELSE,
    'enum': TokenType.ENUM,
    'extern': TokenType.EXTERN,
    'float': TokenType.FLOAT,
    'for': TokenType.FOR,
    'goto': TokenType.GOTO,
    'if': TokenType.IF,
    'int': TokenType.INT,
	'inline': TokenType.INLINE,	/* Added, 4 Dec */
    'long': TokenType.LONG,
    'register': TokenType.REGISTER,
    'return': TokenType.RETURN,
    'short': TokenType.SHORT,
    'signed': TokenType.SIGNED,
    'sizeof': TokenType.SIZEOF,
    'static': TokenType.STATIC,
    'struct': TokenType.STRUCT,
    'switch': TokenType.SWITCH,
    'typedef': TokenType.TYPEDEF,
    'union': TokenType.UNION,
    'unsigned': TokenType.UNSIGNED,
    'void': TokenType.VOID,
    'volatile': TokenType.VOLATILE,
    'while': TokenType.WHILE,
    'null': TokenType.NULL
};

// 特殊指令映射
const SPECIAL_INSTRUCTIONS = {
    'asm': TokenType.ASM,
    'draw': TokenType.DRAW,
    'print': TokenType.PRINT,
    'drawflush': TokenType.DRAWFLUSH,
    'printflush': TokenType.PRINTFLUSH,
    'getlink': TokenType.GETLINK,
    'control': TokenType.CONTROL,
    'radar': TokenType.RADAR,
    'sensor': TokenType.SENSOR,
    'set': TokenType.SET,
    'op': TokenType.OP,
    'lookup': TokenType.LOOKUP,
    'wait': TokenType.WAIT,
    'stop': TokenType.STOP,
    'end': TokenType.END,
    'jump': TokenType.JUMP,
    'read': TokenType.READ,
    'write': TokenType.WRITE
};

// 运算符映射
const OPERATORS = {
    '+': TokenType.PLUS,
    '-': TokenType.MINUS,
    '*': TokenType.MULTIPLY,
    '/': TokenType.DIVIDE,
    '%': TokenType.MODULO,
    '=': TokenType.ASSIGN,
    '==': TokenType.EQUAL,
    '!=': TokenType.NOT_EQUAL,
    '<': TokenType.LESS_THAN,
    '<=': TokenType.LESS_EQUAL,
    '>': TokenType.GREATER_THAN,
    '>=': TokenType.GREATER_EQUAL,
    '&&': TokenType.AND,
    '||': TokenType.OR,
    '!': TokenType.NOT,
    '&': TokenType.BITWISE_AND,
    '|': TokenType.BITWISE_OR,
    '^': TokenType.BITWISE_XOR,
    '~': TokenType.BITWISE_NOT,
    '<<': TokenType.LEFT_SHIFT,
    '>>': TokenType.RIGHT_SHIFT,
    '+=': TokenType.PLUS_ASSIGN,
    '-=': TokenType.MINUS_ASSIGN,
    '*=': TokenType.MULTIPLY_ASSIGN,
    '/=': TokenType.DIVIDE_ASSIGN,
    '%=': TokenType.MODULO_ASSIGN,
	'<<=': TokenType.LEFT_SHIFT_ASSIGN,
    '>>=': TokenType.RIGHT_SHIFT_ASSIGN,
    '&=': TokenType.BITWISE_AND_ASSIGN,
    '|=': TokenType.BITWISE_OR_ASSIGN,
    '^=': TokenType.BITWISE_XOR_ASSIGN,
    '++': TokenType.INCREMENT,
    '--': TokenType.DECREMENT,
	// 成员访问运算符
    '.': TokenType.DOT,
    '->': TokenType.ARROW
};

// 标点符号映射
const PUNCTUATORS = {
    ';': TokenType.SEMICOLON,
    ',': TokenType.COMMA,
    '.': TokenType.DOT,
    '->': TokenType.ARROW,
    '(': TokenType.LEFT_PAREN,
    ')': TokenType.RIGHT_PAREN,
    '{': TokenType.LEFT_BRACE,
    '}': TokenType.RIGHT_BRACE,
    '[': TokenType.LEFT_BRACKET,
    ']': TokenType.RIGHT_BRACKET,
    ':': TokenType.COLON,
    '?': TokenType.QUESTION
};

class Lexer {
    constructor(sourceCode) {
        this.sourceCode = sourceCode;
        this.position = 0;
        this.line = 1;
        this.column = 1;
        this.tokens = [];
        this.errors = [];
    }

    tokenize() {
        this.tokens = [];
        this.errors = [];
        this.position = 0;
        this.line = 1;
        this.column = 1;

        while (this.position < this.sourceCode.length) {
            const char = this.sourceCode[this.position];
            
            if (this.isWhitespace(char)) {
                this.skipWhitespace();
            } else if (char === '/' && this.peek() === '/') {
                this.skipSingleLineComment();
            } else if (char === '/' && this.peek() === '*') {
                this.skipMultiLineComment();
            } else if (char === '"') {
                this.readString();
            } else if (char === "'") {
                this.readCharacter();
            } else if (this.isDigit(char)) {
                this.readNumber();
            } else if (this.isIdentifierStart(char)) {
                this.readIdentifier();
            } else if (char === '@') {
                this.readPredefinedConstant();
            } else if (this.isOperator(char)) {
                this.readOperator();
            } else if (this.isPunctuator(char)) {
                this.readPunctuator();
            } else {
                this.addError(`Unexpected character: '${char}'`);
                this.advance();
            }
        }

        this.tokens.push(new Token(TokenType.EOF, '', this.getLocation()));
        return {
            tokens: this.tokens,
            errors: this.errors
        };
    }

    isWhitespace(char) {
        return char === ' ' || char === '\t' || char === '\n' || char === '\r';
    }

    skipWhitespace() {
        while (this.position < this.sourceCode.length && this.isWhitespace(this.sourceCode[this.position])) {
            if (this.sourceCode[this.position] === '\n') {
                this.line++;
                this.column = 1;
            } else {
                this.column++;
            }
            this.position++;
        }
    }

    skipSingleLineComment() {
        const startLocation = this.getLocation();
        this.advance(); // 跳过第一个 '/'
        this.advance(); // 跳过第二个 '/'
        
        while (this.position < this.sourceCode.length && this.sourceCode[this.position] !== '\n') {
            this.advance();
        }
        
        // 可选：将注释作为token存储
        // const comment = this.sourceCode.substring(startLocation.index, this.position);
        // this.tokens.push(new Token(TokenType.COMMENT, comment, startLocation));
    }

    skipMultiLineComment() {
        const startLocation = this.getLocation();
        this.advance(); // 跳过 '/'
        this.advance(); // 跳过 '*'
        
        while (this.position < this.sourceCode.length) {
            if (this.sourceCode[this.position] === '*' && this.peek() === '/') {
                this.advance(); // 跳过 '*'
                this.advance(); // 跳过 '/'
                break;
            }
            if (this.sourceCode[this.position] === '\n') {
                this.line++;
                this.column = 1;
            } else {
                this.column++;
            }
            this.position++;
        }
        
        if (this.position >= this.sourceCode.length) {
            this.addError('Unterminated multi-line comment', startLocation);
        }
        
        // 可选：将注释作为token存储
        // const comment = this.sourceCode.substring(startLocation.index, this.position);
        // this.tokens.push(new Token(TokenType.COMMENT, comment, startLocation));
    }

    readString() {
        const startLocation = this.getLocation();
        this.advance(); // 跳过开头的 '"'
        let value = '';
        let escaped = false;

        while (this.position < this.sourceCode.length) {
            const char = this.sourceCode[this.position];
            
            if (escaped) {
                switch (char) {
                    case 'n': value += '\n'; break;
                    case 't': value += '\t'; break;
                    case 'r': value += '\r'; break;
                    case '0': value += '\0'; break;
                    case '"': value += '"'; break;
                    case '\\': value += '\\'; break;
                    default: value += char;
                }
                escaped = false;
            } else if (char === '\\') {
                escaped = true;
            } else if (char === '"') {
                this.advance(); // 跳过结尾的 '"'
                this.tokens.push(new Token(TokenType.STRING, value, startLocation));
                return;
            } else {
                value += char;
            }
            
            this.advance();
        }

        this.addError('Unterminated string literal', startLocation);
        this.tokens.push(new Token(TokenType.STRING, value, startLocation));
    }

    readCharacter() {
        const startLocation = this.getLocation();
        this.advance(); // 跳过开头的 "'"
        let value = '';
        let escaped = false;

        if (this.position < this.sourceCode.length) {
            const char = this.sourceCode[this.position];
            
            if (char === '\\') {
                escaped = true;
                this.advance();
                if (this.position < this.sourceCode.length) {
                    const escapeChar = this.sourceCode[this.position];
                    switch (escapeChar) {
                        case 'n': value = '\n'; break;
                        case 't': value = '\t'; break;
                        case 'r': value = '\r'; break;
                        case '0': value = '\0'; break;
                        case "'": value = "'"; break;
                        case '\\': value = '\\'; break;
                        default: value = escapeChar;
                    }
                }
            } else {
                value = char;
            }
            
            this.advance();
            
            if (this.position < this.sourceCode.length && this.sourceCode[this.position] === "'") {
                this.advance(); // 跳过结尾的 "'"
                this.tokens.push(new Token(TokenType.CHARACTER, value, startLocation));
                return;
            }
        }

        this.addError('Unterminated character literal', startLocation);
        this.tokens.push(new Token(TokenType.CHARACTER, value, startLocation));
    }

    readNumber() {
        const startLocation = this.getLocation();
        let value = '';
        let hasDot = false;
        let hasExponent = false;

        while (this.position < this.sourceCode.length) {
            const char = this.sourceCode[this.position];
            
            if (this.isDigit(char)) {
                value += char;
            } else if (char === '.' && !hasDot && !hasExponent) {
                value += char;
                hasDot = true;
            } else if ((char === 'e' || char === 'E') && !hasExponent) {
                value += char;
                hasExponent = true;
                // 检查指数符号
                if (this.peek() === '+' || this.peek() === '-') {
                    value += this.sourceCode[++this.position];
                }
            } else {
                break;
            }
            
            this.advance();
        }

        // 检查后缀
        if (this.position < this.sourceCode.length) {
            const char = this.sourceCode[this.position];
            if (char === 'f' || char === 'F' || char === 'l' || char === 'L') {
                value += char;
                this.advance();
            }
        }

        const numericValue = hasDot || hasExponent ? parseFloat(value) : parseInt(value, 10);
        this.tokens.push(new Token(TokenType.NUMBER, numericValue, startLocation));
    }

    readIdentifier() {
        const startLocation = this.getLocation();
        let value = '';

        while (this.position < this.sourceCode.length && this.isIdentifierPart(this.sourceCode[this.position])) {
            value += this.sourceCode[this.position];
            this.advance();
        }

        // 检查是否是关键字
        if (KEYWORDS[value]) {
            this.tokens.push(new Token(KEYWORDS[value], value, startLocation));
        } else if (SPECIAL_INSTRUCTIONS[value]) {
            this.tokens.push(new Token(SPECIAL_INSTRUCTIONS[value], value, startLocation));
        } else {
            this.tokens.push(new Token(TokenType.IDENTIFIER, value, startLocation));
        }
    }

    readPredefinedConstant() {
        const startLocation = this.getLocation();
        this.advance(); // 跳过 '@'
        let value = '@';

        while (this.position < this.sourceCode.length && this.isIdentifierPart(this.sourceCode[this.position])) {
            value += this.sourceCode[this.position];
            this.advance();
        }

        this.tokens.push(new Token(TokenType.PREDEFINED_CONSTANT, value, startLocation));
    }

    readOperator() {
        const startLocation = this.getLocation();
        let value = this.sourceCode[this.position];
        
        // 检查双字符运算符
        const twoCharOp = value + (this.peek() || '');
        if (OPERATORS[twoCharOp]) {
            value = twoCharOp;
            this.advance(); // 额外前进一次
        }
        
        this.advance(); // 前进到下一个字符
        this.tokens.push(new Token(OPERATORS[value], value, startLocation));
    }

    readPunctuator() {
        const startLocation = this.getLocation();
        let value = this.sourceCode[this.position];
        
        // 检查双字符标点符号（如 ->）
        const twoCharPunct = value + (this.peek() || '');
        if (PUNCTUATORS[twoCharPunct]) {
            value = twoCharPunct;
            this.advance(); // 额外前进一次
        }
        
        this.advance(); // 前进到下一个字符
        this.tokens.push(new Token(PUNCTUATORS[value], value, startLocation));
    }

    isDigit(char) {
        return char >= '0' && char <= '9';
    }

    isIdentifierStart(char) {
        return (char >= 'a' && char <= 'z') || 
               (char >= 'A' && char <= 'Z') || 
               char === '_';
    }

    isIdentifierPart(char) {
        return this.isIdentifierStart(char) || this.isDigit(char);
    }

    isOperator(char) {
        return Object.keys(OPERATORS).some(op => op.startsWith(char));
    }

    isPunctuator(char) {
        return Object.keys(PUNCTUATORS).some(punct => punct.startsWith(char));
    }

    peek(offset = 1) {
        return this.position + offset < this.sourceCode.length 
            ? this.sourceCode[this.position + offset] 
            : null;
    }

    advance() {
        if (this.position < this.sourceCode.length) {
            this.position++;
            this.column++;
        }
    }

    getLocation() {
        return {
            line: this.line,
            column: this.column,
            index: this.position
        };
    }

    addError(message, location = null) {
        const errorLocation = location || this.getLocation();
        this.errors.push({
            message,
            line: errorLocation.line,
            column: errorLocation.column,
            index: errorLocation.index
        });
    }
}

// 语法分析器
class Parser {
    constructor(lexer) {
        this.lexer = lexer;
        this.tokens = [];
        this.currentTokenIndex = 0;
        this.errors = [];
        this.currentScope = null;
		
		// 添加已知类型集合
        this.knownTypeNames = new Set([
            'int', 'char', 'float', 'void', 'double', 'long', 'short',
            'signed', 'unsigned', 'device', 'null_t'
        ]);
    }

    parse() {
        this.lexer.tokenize();
        this.tokens = this.lexer.tokens;
        this.errors = [...this.lexer.errors];
        
        try {
            const program = this.parseProgram();
            return {
                success: this.errors.length === 0,
                ast: program,
                errors: this.errors
            };
        } catch (error) {
            this.addError(`Internal parser error: ${error.message}\n${error.stack}`);
            return {
                success: false,
                ast: null,
                errors: this.errors
            };
        }
    }

    // =============== 辅助方法 ===============

    getCurrentToken() {
        if (this.currentTokenIndex >= this.tokens.length) {
            return this.tokens[this.tokens.length - 1]; // EOF token
        }
        return this.tokens[this.currentTokenIndex];
    }

    peekToken(offset = 1) {
        const index = this.currentTokenIndex + offset;
        if (index >= this.tokens.length) {
            return this.tokens[this.tokens.length - 1]; // EOF token
        }
        return this.tokens[index];
    }

    consumeToken() {
		const gotToken = this.getCurrentToken();
        if (this.currentTokenIndex < this.tokens.length) {
            this.currentTokenIndex++;
        }
        return gotToken;	// This should be the logic...
    }

    matchToken(expectedType, expectedValue = null) {
        const token = this.getCurrentToken();
        if (token.type === expectedType) {
            if (expectedValue === null || token.value === expectedValue) {
                return true;
            }
        }
        return false;
    }

    expectToken(expectedType, expectedValue = null) {
        const token = this.getCurrentToken();
        if (this.matchToken(expectedType, expectedValue)) {
            this.consumeToken();
            return token;
        }
        
        const expected = expectedValue || expectedType;
        this.addError(`Expected ${expected}, but found ${token.type} '${token.value}'`, token.location);
        return null;
    }

    addError(message, location = null) {
        const token = this.getCurrentToken();
        this.errors.push({
            message,
            line: location ? location.line : token.location.line,
            column: location ? location.column : token.location.column
        });
    }

    // =============== 解析方法 ===============

    parseProgram() {
        const program = ASTBuilder.program();
        const startLocation = this.getCurrentToken().location;
		
		// 第一遍：收集类型定义
        const typeDefinitions = [];
        const savedIndex = this.currentTokenIndex;
        
        while (!this.matchToken(TokenType.EOF)) {
            if (this.matchToken(TokenType.SEMICOLON)) {
                this.consumeToken(); // 跳过空语句
                continue;
            }

            // 尝试解析类型定义（typedef）
            if (this.matchToken(TokenType.TYPEDEF)) {
                const typedefDecl = this.parseTypedefDeclaration();
                if (typedefDecl) {
                    typeDefinitions.push(typedefDecl);
					typedefDecl.parent = program;
                    // 记录类型名
                    typedefDecl.declarators.forEach(d => {
                        if (d.name) {
                            this.addTypeName(d.name);
                        }
                    });
                    continue;
                }
            }

            // 尝试解析结构体/联合体定义
            if (this.matchToken(TokenType.STRUCT) || this.matchToken(TokenType.UNION)) {
                const startIdx = this.currentTokenIndex;
                const typeDef = this.parseStructOrUnionDefinition();
                if (typeDef && typeDef.isDefinition) {
                    typeDefinitions.push(typeDef);
					typeDef.parent = program;
                    // 记录类型名
                    if (typeDef.name) {
                        this.addTypeName(typeDef.name);
                    }
                    continue;
                } else {
                    this.currentTokenIndex = startIdx;
                }
            }

            // 跳过其他token
            this.consumeToken();
        }

        // 重置位置进行第二遍解析
        this.currentTokenIndex = savedIndex;

        while (!this.matchToken(TokenType.EOF)) {
            if (this.matchToken(TokenType.SEMICOLON)) {
                this.consumeToken(); // 跳过空语句
                continue;
            }
			
			// 尝试解析类型定义（typedef）
            if (this.matchToken(TokenType.TYPEDEF)) {
                const typedefDecl = this.parseTypedefDeclaration();
                if (typedefDecl) {
					typedefDecl.parent = program;
                    program.typeDefinitions = program.typeDefinitions || [];
                    program.typeDefinitions.push(typedefDecl);
                    continue;
                }
            }

            // 尝试解析结构体/联合体定义（没有变量声明）
            if (this.matchToken(TokenType.STRUCT) || this.matchToken(TokenType.UNION)) {
                // 先查看后面是否是定义
                const startIndex = this.currentTokenIndex;
                const typeDef = this.parseStructOrUnionDefinition();
                if (typeDef && typeDef.isDefinition) {
                    // 这是一个完整的结构体/联合体定义
					typeDef.parent = program;
                    program.typeDefinitions = program.typeDefinitions || [];
                    program.typeDefinitions.push(typeDef);
                    continue;
                } else {
                    // 回退，可能是结构体/联合体变量声明
                    this.currentTokenIndex = startIndex;
                }
            }

            // 解析全局声明或函数定义
            const declaration = this.parseDeclaration();
            if (declaration) {
                if (declaration.type === 'FunctionDeclaration') {
                    program.functions.push(declaration);
                } else {
                    program.globalDeclarations.push(declaration);
                }
				declaration.parent = program;
            } else {
                // 跳过无法解析的token
                this.consumeToken();
            }
			
			// A meaningless makeup:
			/*
			const declaration = this.parseDeclaration();
            if (declaration) {
                if (declaration.type === 'FunctionDeclaration') {
                    program.functions.push(declaration);
                } else if (declaration.type === 'TypedefDeclaration' || 
                          declaration.type === 'StructDefinition' || 
                          declaration.type === 'UnionDefinition') {
                    // 这些应该已经在第一遍处理了，但以防万一
                    program.typeDefinitions = program.typeDefinitions || [];
                    program.typeDefinitions.push(declaration);
                } else {
                    program.globalDeclarations.push(declaration);
                }
            } else {
                // 跳过无法解析的token
                this.consumeToken();
            }
			*/
        }

        program.location = startLocation;
        return program;
    }
	
	matchTokenTypeAt(index, types) {
        if (index >= this.tokens.length) return false;
        const token = this.tokens[index];
        return types.includes(token.type);
    }
	
	parseTypedefDeclaration() {
        const typedefToken = this.expectToken(TokenType.TYPEDEF);
        if (!typedefToken) return null;
		
		const functionLookahead = this.lookaheadForFunction();
		if (functionLookahead.isFunction) {
			this.addError('Cannot declare function through typedef');
			return null;
		} else if (functionLookahead.isFunctionPointer) {
			// TODO: Currently, only single name is implemented for function pointer:
			const parsedFunction = this.parseFunctionDeclaration(true);
			let typeSpecifier = new TypeSpecifierNode();
			typeSpecifier.qualifiers = parsedFunction.storageClass;
			typeSpecifier.pointerDepth = parsedFunction.pointerDepth; //functionLookahead.pointerDepth;
			typeSpecifier.pointerQualifiers = parsedFunction.pointerQualifiers;
			
			typeSpecifier.isFunctionType = true;
			typeSpecifier.correspondingFunction = parsedFunction;
			const typeDeclarator = ASTBuilder.variableDeclarator(parsedFunction.name);
			this.addTypeName(parsedFunction.name);
			typeDeclarator.location = typedefToken.location;
			typeDeclarator.pointerDepth = typeSpecifier.pointerDepth;
			typeDeclarator.pointerQualifiers = typeSpecifier.pointerQualifiers;
			// TODO: No array dimension so far
			// I guess nobody wants that, as it's so complex!
			const typedefDecl = ASTBuilder.typedefDeclaration(typeSpecifier, [typeDeclarator]);
			typedefDecl.location = typedefToken.location;
			typedefDecl.isFunction = true;
			typedefDecl.functionDefinition = parsedFunction;
			
			typeSpecifier.parent = typeDeclarator;
			typeDeclarator.parent = typedefDecl;
			parsedFunction.parent = typedefDecl;
			// Might be TODO: More pointer implementation ?
			
			return typedefDecl;
		}

        // 解析类型说明符
        const baseType = this.parseTypeSpecifier();
        if (!baseType) {
            this.addError('Expected type specifier after typedef');
            return null;
        }
		
		const typedefDecl = ASTBuilder.typedefDeclaration(baseType, []);

        // 解析声明符（类型别名），可以有多个
        const declarators = [];
        do {
            const declarator = this.parseDeclarator(false);
            if (declarator && declarator.name) {
				// Manually merged !!
				this.addTypeName(declarator.name);
                // 创建类型别名声明符
                const typeDeclarator = ASTBuilder.variableDeclarator(declarator.name);
                typeDeclarator.location = declarator.location;
                typeDeclarator.pointerDepth = declarator.pointerDepth;
                typeDeclarator.pointerQualifiers = declarator.pointerQualifiers;
                typeDeclarator.arrayDimensions = declarator.arrayDimensions;
				declarator.parent = typeDeclarator;
				typeDeclarator.parent = typedefDecl;
                declarators.push(typeDeclarator);
            }
        } while (this.matchToken(TokenType.COMMA) && this.consumeToken());

        this.expectToken(TokenType.SEMICOLON);

        typedefDecl.declarators = declarators;
        typedefDecl.location = typedefToken.location;
        
        // 如果声明符有指针信息，需要创建对应的指针类型定义
        if (declarators.some(d => d.pointerDepth > 0)) {
            typedefDecl.hasPointerTypes = true;
        }
        
        return typedefDecl;
    }

    parseTypedefDeclarator() {
        // 简单的类型别名（不支持复杂的声明符如指针、数组等）
        const nameToken = this.expectToken(TokenType.IDENTIFIER);
        if (!nameToken) return null;

        // 创建一个简单的声明符节点
        const declarator = ASTBuilder.variableDeclarator(nameToken.value);
        declarator.location = nameToken.location;
        return declarator;
    }

    parseStructOrUnionDefinition() {
        const isUnion = this.matchToken(TokenType.UNION);
        const structOrUnionToken = this.consumeToken(); // 消耗 'struct' 或 'union'
        
        // 解析结构体/联合体名称（可选）
        let name = null;
        if (this.matchToken(TokenType.IDENTIFIER)) {
            const nameToken = this.consumeToken();
            name = nameToken.value;
        }

        // 检查是否有成员定义
        let members = [];
        let isDefinition = false;
        
        if (this.matchToken(TokenType.LEFT_BRACE)) {
            isDefinition = true;
            this.consumeToken(); // 跳过 '{'
            
            // 解析成员列表
            while (!this.matchToken(TokenType.RIGHT_BRACE) && !this.matchToken(TokenType.EOF)) {
                const member = this.parseStructMember();
                if (member) {
                    members.push(member);
                } else {
                    break;
                }
            }
            
            this.expectToken(TokenType.RIGHT_BRACE);
        }

        // 创建结构体/联合体定义节点
        let typeDef;
        if (isUnion) {
            typeDef = ASTBuilder.unionDefinition(name);
            typeDef.type = 'union';
        } else {
            typeDef = ASTBuilder.structDefinition(name);
            typeDef.type = 'struct';
        }
        
        typeDef.members = members;
        typeDef.isDefinition = isDefinition;
        typeDef.location = structOrUnionToken.location;
        
		members.forEach(member => member.parent = typeDef);
		
		if (name) {
            this.addTypeName(name);
        }
		
        // 检查是否有变量声明部分（如 struct S { ... } var1, var2;）
        // 如果有，需要特殊处理（这里我们暂时不支持）
		// !! Notice this unsupported feature !!
        
        return typeDef;
    }

    // 修改struct/union成员解析，支持指针成员
    parseStructMember() {
        // 解析类型说明符
        const memberType = this.parseTypeSpecifier();
        if (!memberType) return null;

        // 解析成员声明符
        const declarator = this.parseDeclarator(false);
        if (!declarator || !declarator.name) {
            this.addError('Expected member name in struct/union');
            return null;
        }

        // 检查是否有位域
		// TODO: Honestly speaking, I don't think I will remember implementing this at all...
		// (Yes, it's of great importance for Mindustry, if possible.)
        let bitField = null;
        if (this.matchToken(TokenType.COLON)) {
            this.consumeToken(); // 跳过 ':'
            const bitFieldExpr = this.parseExpression();
            if (bitFieldExpr) {
                bitField = bitFieldExpr;
            }
        }

        this.expectToken(TokenType.SEMICOLON);

        const member = ASTBuilder.structMember(memberType, declarator.name);
        member.bitField = bitField;
        member.location = declarator.location;
        member.pointerDepth = declarator.pointerDepth;
        member.pointerQualifiers = declarator.pointerQualifiers;
        member.arrayDimensions = declarator.arrayDimensions;
		declarator.parent = member;
        return member;
    }

	// (This function has manual changes!!!)
	// (Major manual change: support for pointer -- the former ones are really awful !!!)
    parseTypeSpecifier() {
        // 解析类型限定符（const, volatile）
        const qualifiers = this.parseTypeQualifiers();
        /*
        // 检查基本类型
        const typeTokens = [
            TokenType.INT, TokenType.CHAR, TokenType.FLOAT, TokenType.VOID,
            TokenType.DOUBLE, TokenType.LONG, TokenType.SHORT, 
            TokenType.SIGNED, TokenType.UNSIGNED
        ];
		
        for (const typeToken of typeTokens) {
            if (this.matchToken(typeToken)) {
				// 此处应该取当前 token 而不是下一个 token
				// Change withdrawn with changes in consumeToken
                const token = this.consumeToken();
                const typeNode = ASTBuilder.typeSpecifier(token.value);
                typeNode.setAttribute('location', token.location);
                typeNode.setAttribute('qualifiers', qualifiers);
                return typeNode;
            }
        }

        // 检查结构体/联合体类型
        if (this.matchToken(TokenType.STRUCT) || this.matchToken(TokenType.UNION)) {
            return this.parseStructOrUnionTypeSpecifier(qualifiers);
        }
		
        // 检查枚举类型（简单实现）
        if (this.matchToken(TokenType.ENUM)) {
            const enumToken = this.consumeToken();
            const typeNode = ASTBuilder.typeSpecifier('enum');
            typeNode.setAttribute('location', enumToken.location);
            typeNode.setAttribute('qualifiers', qualifiers);
            // 可以进一步解析枚举定义
            return typeNode;
        }
		*/
		
		const typeTokens = [
            TokenType.INT, TokenType.CHAR, TokenType.FLOAT, TokenType.VOID,
            TokenType.DOUBLE, TokenType.LONG, TokenType.SHORT, TokenType.SIGNED, TokenType.UNSIGNED,
            TokenType.STRUCT, TokenType.UNION, TokenType.ENUM
        ];
		
		let checkSpecialTypes = true, doneTypeFetch = false;
		let typeNode = null;

        for (const typeToken of typeTokens) {
            if (this.matchToken(typeToken)) {
                const token = this.consumeToken();
                typeNode = ASTBuilder.typeSpecifier(token.value);
                typeNode.setAttribute('location', token.location);
                typeNode.setAttribute('qualifiers', qualifiers);
                
                // 处理struct/union/enum类型
                if (token.type === TokenType.STRUCT || token.type === TokenType.UNION) {
                    this.parseStructOrUnionType(typeNode);
                } else if (token.type === TokenType.ENUM) {
                    this.parseEnumType(typeNode);
                }
                
				checkSpecialTypes = false;
				doneTypeFetch = true;
                //return typeNode;
            }
        }

        // 特殊类型：device 和 null_t
        if (checkSpecialTypes && this.matchToken(TokenType.IDENTIFIER)) {
            const token = this.getCurrentToken();
            if (token.value === 'device' || token.value === 'null_t') {
                this.consumeToken();
                typeNode = ASTBuilder.typeSpecifier(token.value);
                typeNode.setAttribute('location', token.location);
                typeNode.setAttribute('qualifiers', qualifiers);
                return typeNode;
            }
            
            // 可能是通过typedef定义的类型别名
            typeNode = ASTBuilder.typeSpecifier(token.value);
            typeNode.setAttribute('location', token.location);
            typeNode.setAttribute('qualifiers', qualifiers);
            typeNode.setAttribute('isTypedef', true);
			typeNode.setAttribute('isCustomType', true);
            this.consumeToken();
			doneTypeFetch = true;
        }
		
		if (!doneTypeFetch) {
			this.addError('Expected type specifier');
			return null;
		}
		
		// Fetch pointers
		while (this.matchToken(TokenType.MULTIPLY)) {
			this.consumeToken();
			typeNode.pointerQualifiers.push(this.parseTypeQualifiers());
			typeNode.pointerDepth++;
		}

        return typeNode;
    }

	// 修改函数参数解析
    parseParameter() {
        // 解析类型说明符
        const paramType = this.parseTypeSpecifier();
        if (!paramType) return null;
        
        // 解析声明符
		let decName = null;
        const declarator = this.parseDeclarator(true);
        if (declarator) decName = declarator.name;	// Function declaration / function pointer has no name
        
        return {
            type: paramType,
            name: decName,
            pointerDepth: declarator.pointerDepth,
            pointerQualifiers: declarator.pointerQualifiers,
            arrayDimensions: declarator.arrayDimensions,
            location: declarator.location || paramType.location
        };
    }

	parseDeclarator(isFunctionParam = false) {
        let declarator = null;
        
        // 解析指针部分
        const pointerQualifiers = [];
        let pointerDepth = 0;
        
        while (this.matchToken(TokenType.MULTIPLY)) {
            this.consumeToken(); // 跳过 '*'
            pointerDepth++;
            
            // 解析指针限定符
            const qualifiers = this.parseTypeQualifiers();
            pointerQualifiers.push(qualifiers);
        }
        
        // 解析直接声明符
        if (this.matchToken(TokenType.LEFT_PAREN)) {
            // 处理括号声明符，如：int (*a);
            this.consumeToken(); // 跳过 '('
            declarator = this.parseDeclarator(isFunctionParam);
            this.expectToken(TokenType.RIGHT_PAREN);
        } else {
            // 解析标识符
            const nameToken = this.getCurrentToken();
			if (!isFunctionParam) this.expectToken(TokenType.IDENTIFIER);
            if (nameToken.type != TokenType.IDENTIFIER) {
                // 如果是函数参数且没有名称（如：void func(int))
                if (isFunctionParam) {
                    return ASTBuilder.declarator(null);
                }
                return null;
            }
            
            declarator = ASTBuilder.declarator(nameToken.value);
            declarator.location = nameToken.location;
        }
        
        // 解析数组和函数后缀
        while (true) {
            // 数组维度
            if (this.matchToken(TokenType.LEFT_BRACKET)) {
                this.consumeToken(); // 跳过 '['
                let dimension = null;
                if (!this.matchToken(TokenType.RIGHT_BRACKET)) {
                    dimension = this.parseExpression();
                }
                this.expectToken(TokenType.RIGHT_BRACKET);
                declarator.arrayDimensions.push(dimension);
            }
            // 函数参数列表
            else if (this.matchToken(TokenType.LEFT_PAREN)) {
                this.consumeToken(); // 跳过 '('
                const params = [];
                if (!this.matchToken(TokenType.RIGHT_PAREN)) {
                    do {
                        const param = this.parseParameter();
                        if (param) {
                            params.push(param);
                        }
                    } while (this.matchToken(TokenType.COMMA) && this.consumeToken());
                }
				if (this.matchToken(TokenType.IDENTIFIER))
					this.consumeToken();	// Last parameter identifier is to be consumed !!!
                this.expectToken(TokenType.RIGHT_PAREN);
                declarator.functionParams = params;
            }
            else {
                break;
            }
        }
        
        // 设置指针信息
        declarator.pointerDepth = pointerDepth;
        declarator.pointerQualifiers = pointerQualifiers;
        
        return declarator;
    }

    parseTypeQualifiers() {
        const qualifiers = [];
        while (this.matchToken(TokenType.CONST) || this.matchToken(TokenType.VOLATILE)) {
            const qualifierToken = this.consumeToken();
            qualifiers.push(qualifierToken.value);
        }
        return qualifiers;
    }

	// ****
	// THIS FUNCTION HAS A MANUAL CHANGE
	// ****
    parseStructOrUnionTypeSpecifier(qualifiers) {
        const isUnion = this.matchToken(TokenType.UNION);
		// Withdrawn
        const structOrUnionToken = this.consumeToken();
        
        let name = null;
        if (this.matchToken(TokenType.IDENTIFIER)) {
			// Withdrawn
            const nameToken = this.consumeToken();
            name = nameToken.value;
        }

        // 创建类型说明符节点
        const typeNode = ASTBuilder.typeSpecifier(isUnion ? 'union' : 'struct');
        typeNode.setAttribute('location', structOrUnionToken.location);
        typeNode.setAttribute('qualifiers', qualifiers);
        typeNode.setAttribute('structOrUnionName', name);
        
        return typeNode;
    }
	
	// Is this really useful:
	// 添加解析struct/union类型的方法
    parseStructOrUnionType(structNode) {
        // 检查是否有结构体/联合体名称
        if (this.matchToken(TokenType.IDENTIFIER)) {
            const nameToken = this.consumeToken();
            structNode.setAttribute('structOrUnionName', nameToken.value);
        }

        // 检查是否有结构体体（可选）
        if (this.matchToken(TokenType.LEFT_BRACE)) {
            this.consumeToken(); // 跳过 '{'
            
            while (!this.matchToken(TokenType.RIGHT_BRACE) && !this.matchToken(TokenType.EOF)) {
                // 跳过成员定义（在类型说明符中不解析成员）
                if (this.matchToken(TokenType.SEMICOLON)) {
                    this.consumeToken();
                } else {
                    this.consumeToken();
                }
            }
            
            if (this.matchToken(TokenType.RIGHT_BRACE)) {
                this.consumeToken(); // 跳过 '}'
            }
        }
        
        return structNode;
    }

    // 添加解析enum类型的方法
    parseEnumType(enumNode) {
        // 检查是否有枚举名称
        if (this.matchToken(TokenType.IDENTIFIER)) {
            const nameToken = this.consumeToken();
            enumNode.setAttribute('enumName', nameToken.value);
        }

        // 检查是否有枚举体（可选）
        if (this.matchToken(TokenType.LEFT_BRACE)) {
            this.consumeToken(); // 跳过 '{'
            
            while (!this.matchToken(TokenType.RIGHT_BRACE) && !this.matchToken(TokenType.EOF)) {
                // 跳过枚举值定义
                if (this.matchToken(TokenType.SEMICOLON)) {
                    this.consumeToken();
                } else {
                    this.consumeToken();
                }
            }
            
            if (this.matchToken(TokenType.RIGHT_BRACE)) {
                this.consumeToken(); // 跳过 '}'
            }
        }
        
        return enumNode;
    }

    // 修改lookaheadForFunction方法以考虑类型定义
	// ****
	// THIS FUNCTION HAS A MANUAL CHANGE
	// ****
	// This function now features function-pointer recognition!
    lookaheadForFunction() {
        let startIndex = this.currentTokenIndex;
        let currentIndex = startIndex;
        
        // 跳过类型限定符和存储类说明符
        while (this.matchTokenTypeAt(currentIndex, [
            TokenType.CONST, TokenType.VOLATILE,
            TokenType.STATIC, TokenType.EXTERN, TokenType.AUTO, TokenType.REGISTER
        ])) {
            currentIndex++;
        }

		// (手动编写)
		// 注意：如果最后一个是 Identifier，是函数名！
		let endedAsIdentifier = false;

        // 跳过类型说明符
        while (this.matchTokenTypeAt(currentIndex, [
            TokenType.INT, TokenType.CHAR, TokenType.FLOAT, TokenType.VOID,
            TokenType.DOUBLE, TokenType.LONG, TokenType.SHORT, TokenType.SIGNED, TokenType.UNSIGNED,
            TokenType.STRUCT, TokenType.UNION, TokenType.ENUM,
            TokenType.IDENTIFIER // typedef定义的类型
        ])) {
			endedAsIdentifier = this.matchTokenTypeAt(currentIndex, [TokenType.IDENTIFIER]);
            currentIndex++;
        }
		
		// 最后一个是函数名称
		if (endedAsIdentifier) currentIndex--;
		
		// Might be a function pointer's type
		if (this.matchTokenTypeAt(currentIndex, [TokenType.LEFT_PAREN])) {
			currentIndex++;	// Simply skip it
			let pointerLayer = 0;
			while (this.matchTokenTypeAt(currentIndex + pointerLayer, [TokenType.MULTIPLY])) {
				pointerLayer++;
			}
			if (this.matchTokenTypeAt(currentIndex + pointerLayer + 1, [TokenType.RIGHT_PAREN])) {
				if (pointerLayer > 0) {
					return { isFunction: false, isFunctionPointer: true, functionPointerLayer: pointerLayer };
				}
				currentIndex++;
			}
		}
        
        // 跳过指针（可能有多个）
        while (this.matchTokenTypeAt(currentIndex, [TokenType.MULTIPLY])) {
            currentIndex++;
            // 跳过指针限定符
            while (this.matchTokenTypeAt(currentIndex, [TokenType.CONST, TokenType.VOLATILE])) {
                currentIndex++;
            }
        }
		
        // 接下来应该是标识符
        if (this.matchTokenTypeAt(currentIndex, [TokenType.IDENTIFIER])) {
            // 再接下来是 '(' 说明是函数
            if (this.matchTokenTypeAt(currentIndex + 1, [TokenType.LEFT_PAREN])) {
                return { isFunction: true };
            }
        }

        return { isFunction: false };
    }

    // 修改parseDeclaration以处理结构体/联合体变量声明
    parseDeclaration() {
		/*
        // 检查是否是结构体/联合体变量声明
        const lookahead = this.lookaheadForStructOrUnionVariable();
        if (lookahead.isStructOrUnionVar) {
            return this.parseStructOrUnionVariableDeclaration();
        }

        // 检查是否为函数声明
        const funcLookahead = this.lookaheadForFunction();
        if (funcLookahead.isFunction) {
            return this.parseFunctionDeclaration();
        } else {
            return this.parseVariableDeclaration();
        }
		*/
		// 检查是否为函数声明
        const lookahead = this.lookaheadForFunction();
        if (lookahead.isFunction) {
            return this.parseFunctionDeclaration();
        }
        
        // 尝试解析变量声明
        const startIndex = this.currentTokenIndex;
        const varDecl = this.parseVariableDeclaration();
        if (varDecl) {
            return varDecl;
        }
        
        // 回退并尝试解析typedef或结构体定义
        this.currentTokenIndex = startIndex;
        
        // 检查是否为typedef
        if (this.matchToken(TokenType.TYPEDEF)) {
            return this.parseTypedefDeclaration();
        }
        
        // 检查是否为结构体/联合体定义
        if (this.matchToken(TokenType.STRUCT) || this.matchToken(TokenType.UNION)) {
            const startIndex2 = this.currentTokenIndex;
            const typeDef = this.parseStructOrUnionDefinition();
            if (typeDef && typeDef.isDefinition) {
                // 这是一个完整的结构体/联合体定义
                return typeDef;
            } else {
                // 回退，可能是结构体/联合体变量声明
                this.currentTokenIndex = startIndex2;
                const varDecl2 = this.parseVariableDeclaration();
                if (varDecl2) {
                    return varDecl2;
                }
            }
        }
        
        return null;
    }

    lookaheadForStructOrUnionVariable() {
        const startIndex = this.currentTokenIndex;
        let currentIndex = startIndex;
        let isStructOrUnionVar = false;

        // 检查是否是struct或union
        if (this.matchTokenTypeAt(currentIndex, [TokenType.STRUCT, TokenType.UNION])) {
            // 跳过struct/union和可能的名称
            currentIndex++;
            if (this.matchTokenTypeAt(currentIndex, [TokenType.IDENTIFIER])) {
                currentIndex++;
            }
            
            // 检查是否有左花括号（定义）
            if (this.matchTokenTypeAt(currentIndex, [TokenType.LEFT_BRACE])) {
                // 跳过成员定义
                currentIndex++;
                let braceCount = 1;
                while (braceCount > 0 && currentIndex < this.tokens.length) {
                    if (this.tokens[currentIndex].type === TokenType.LEFT_BRACE) braceCount++;
                    if (this.tokens[currentIndex].type === TokenType.RIGHT_BRACE) braceCount--;
                    currentIndex++;
                }
            }
            
            // 跳过可能的指针
            while (this.matchTokenTypeAt(currentIndex, [TokenType.MULTIPLY])) {
                currentIndex++;
                // 跳过指针限定符
                while (this.matchTokenTypeAt(currentIndex, [TokenType.CONST, TokenType.VOLATILE])) {
                    currentIndex++;
                }
            }
            
            // 检查后面是否有标识符（变量名）
            if (this.matchTokenTypeAt(currentIndex, [TokenType.IDENTIFIER])) {
                isStructOrUnionVar = true;
            }
        }

        return { isStructOrUnionVar };
    }

    parseStructOrUnionVariableDeclaration() {
        const type = this.parseTypeSpecifier();
        if (!type) return null;

        const declarators = [];
		const varDecl = ASTBuilder.variableDeclaration(type, []);
        do {
            const declarator = this.parseVariableDeclarator();
            if (declarator) {
				declarator.parent = varDecl;
                declarators.push(declarator);
            }
        } while (this.matchToken(TokenType.COMMA) && this.consumeToken());

        this.expectToken(TokenType.SEMICOLON);

        varDecl.location = type.location;
        varDecl.isStructOrUnion = true;
		varDecl.declarators = declarators;
		type.parent = varDecl;
        return varDecl;
    }

    // 更新isBuiltinFunction方法，添加更多内建函数
    isBuiltinFunction(name) {
        const builtins = [
            'draw', 'print', 'drawflush', 'printflush', 'getlink',
            'control', 'radar', 'sensor', 'set', 'op', 'lookup',
            'wait', 'stop', 'end', 'jump', 'read', 'write', 'asm'
        ];
        return builtins.includes(name);
    }

    // 修改函数声明解析中的参数列表处理
	// This is expected to work for function pointers as well
	// Also, this function always return function declaration node, EVEN IF IT IS ACTUALLY A FUNCTION POINTER
    parseFunctionDeclaration(allowFunctionPointer = false) {
		let storageClass = null;
        let isInline = false;
        
        while (this.matchToken(TokenType.STATIC) || 
               this.matchToken(TokenType.INLINE) || 
               this.matchToken(TokenType.EXTERN) ||
               this.matchToken(TokenType.VOLATILE) ||
               this.matchToken(TokenType.CONST)) {
            const token = this.consumeToken();
            if (token.type === TokenType.INLINE) {
                isInline = true;
            } else if (token.type === TokenType.STATIC || 
                      token.type === TokenType.EXTERN) {
                storageClass = token.value;
            }
        }
		
        const returnType = this.parseTypeSpecifier();
        if (!returnType) return null;

        // 解析函数名
        const declarator = this.parseDeclarator(false);
        if (!declarator || !declarator.name) {
            this.addError('Expected function name');
            return null;
        }

        // 函数不能有数组维度
        if (declarator.arrayDimensions.length > 0) {
            this.addError('Function cannot have array dimensions');
        }

        const functionDecl = ASTBuilder.functionDeclaration(declarator.name, returnType);
        functionDecl.location = returnType.location;
		functionDecl.storageClass = storageClass;
        functionDecl.isInline = isInline;
		
		returnType.parent = functionDecl;
		declarator.parent = functionDecl;
		
		// Actually a function pointer
		if (declarator.pointerDepth > 0) {
			if (allowFunctionPointer) {
				functionDecl.isFunctionPointer = true;
				functionDecl.pointerLayer = declarator.pointerDepth;
				functionDecl.pointerQualifiers = declarator.pointerQualifiers;
			} else {
				this.addError('Cannot use function pointer in function declaration or implementation');
				return null;
			}
		}
        
        // 解析函数参数
        if (declarator.functionParams) {
            functionDecl.parameters = declarator.functionParams;
        } else {
            // 没有参数列表，使用空的参数
            functionDecl.parameters = [];
        }

        // 解析函数体
		if (!functionDecl.isFunctionPointer) {
			if (this.matchToken(TokenType.SEMICOLON)) {
				// 函数声明，没有函数体
				this.consumeToken();
			} else {
				// 函数定义，有函数体
				functionDecl.body = this.parseCompoundStatement();
				functionDecl.body.parent = functionDecl;
			}
		}

        return functionDecl;
    }

	// 添加struct定义解析
	parseStructDefinition(structNode) {
		// 检查是否有结构体名称
		if (this.matchToken(TokenType.IDENTIFIER)) {
			const nameToken = this.consumeToken();
			structNode.setAttribute('name', nameToken.value);
		}

		// 检查是否有结构体体
		if (this.matchToken(TokenType.LEFT_BRACE)) {
			this.consumeToken(); // 跳过 '{'
			const members = [];
			
			while (!this.matchToken(TokenType.RIGHT_BRACE) && !this.matchToken(TokenType.EOF)) {
				const memberType = this.parseTypeSpecifier();
				if (!memberType) break;

				const memberName = this.expectToken(TokenType.IDENTIFIER);
				if (!memberName) break;

				members.push({
					type: memberType,
					name: memberName.value,
					location: memberName.location
				});

				this.expectToken(TokenType.SEMICOLON);
			}

			this.expectToken(TokenType.RIGHT_BRACE);
			structNode.setAttribute('members', members);
		}
		
		return structNode;
	}

	// It used to have a label/bookmark here...
    parseVariableDeclaration() {
        let type = this.parseTypeSpecifier();
        if (!type) return null;

        const declarators = [];
		const varDecl = ASTBuilder.variableDeclaration(type, []);
        do {
            const declarator = this.parseDeclarator(false);
			
            if (!declarator || !declarator.name) {
				// Uncovered on 5 Dec
                this.addError('Expected variable name in declaration');
                break;
				//return null;
            }
            declarator.parent = varDecl;
            // 创建变量声明符节点
            const varDeclarator = ASTBuilder.variableDeclarator(declarator.name);
            varDeclarator.location = declarator.location;
            varDeclarator.pointerDepth = declarator.pointerDepth;
            varDeclarator.pointerQualifiers = declarator.pointerQualifiers;
            varDeclarator.arrayDimensions = declarator.arrayDimensions;
			
			if (type.pointerDepth > 0) {
				// Only for the first
				varDeclarator.pointerDepth += type.pointerDepth;
				type.pointerDepth = 0;
				varDeclarator.pointerQualifiers = varDeclarator.pointerQualifiers.concat(type.pointerQualifiers);
				type.pointerQualifiers = [];
			}
            
			// This is a little bit different from pure C89
            // 处理初始化
            if (this.matchToken(TokenType.ASSIGN)) {
                this.consumeToken();
                varDeclarator.initializer = this.parseExpression();
            }
            
            declarators.push(varDeclarator);
        } while (this.matchToken(TokenType.COMMA) && this.consumeToken());

        this.expectToken(TokenType.SEMICOLON);

        varDecl.location = type.location;
		varDecl.declarators = declarators;
		type.parent = varDecl;
        return varDecl;
    }

    parseVariableDeclarator() {
        const nameToken = this.expectToken(TokenType.IDENTIFIER);
        if (!nameToken) return null;

        const declarator = ASTBuilder.variableDeclarator(nameToken.value);
        declarator.location = nameToken.location;

        if (this.matchToken(TokenType.ASSIGN)) {
            this.consumeToken();
            declarator.initializer = this.parseExpression();
        }

        return declarator;
    }

    parseCompoundStatement() {
        const leftBrace = this.expectToken(TokenType.LEFT_BRACE);
        if (!leftBrace) return null;

        const compoundStmt = ASTBuilder.compoundStatement();
        compoundStmt.location = leftBrace.location;

        while (!this.matchToken(TokenType.RIGHT_BRACE) && !this.matchToken(TokenType.EOF)) {
            const statement = this.parseStatement();
            if (statement) {
				statement.parent = compoundStmt;
                compoundStmt.statements.push(statement);
            } else {
                break;
            }
        }

        this.expectToken(TokenType.RIGHT_BRACE);
        return compoundStmt;
    }

    parseStatement() {
        const token = this.getCurrentToken();

        switch (token.type) {
            case TokenType.LEFT_BRACE:
                return this.parseCompoundStatement();
            
            case TokenType.IF:
                return this.parseIfStatement();
            
            case TokenType.WHILE:
                return this.parseWhileStatement();
            
            case TokenType.FOR:
                return this.parseForStatement();
            
            case TokenType.RETURN:
                return this.parseReturnStatement();
            
            case TokenType.BREAK:
                return this.parseBreakStatement();
            
            case TokenType.CONTINUE:
                return this.parseContinueStatement();
            
            case TokenType.ASM:
                return this.parseAsmStatement();
            
			case TokenType.CONST:
			case TokenType.VOLATILE:
            case TokenType.INT:
            case TokenType.CHAR:
            case TokenType.FLOAT:
            case TokenType.VOID:
            case TokenType.DOUBLE:
            case TokenType.LONG:
            case TokenType.SHORT:
            case TokenType.SIGNED:
            case TokenType.UNSIGNED: {
                // 可能是变量声明
                const startIndex = this.currentTokenIndex;
                const declaration = this.parseVariableDeclaration();
                if (declaration) return declaration;
                
                // 如果不是变量声明，回退并解析表达式
                this.currentTokenIndex = startIndex;
                return this.parseExpressionStatement();
            }
            
            default:
                // 检查是否为已知类型名（包括自定义类型）
                if (this.isTypeName(token.value)) {
                    // 尝试解析为变量声明
                    const savedIndex = this.currentTokenIndex;
                    const savedErrorsLength = this.errors.length;
                    
                    // 尝试解析为变量声明
                    const declaration = this.tryParseVariableDeclaration();
                    if (declaration) {
                        return declaration;
                    }
                    
                    // 解析失败，恢复状态
                    this.currentTokenIndex = savedIndex;
                    // 移除尝试解析期间产生的错误
                    if (this.errors.length > savedErrorsLength) {
                        this.errors.splice(savedErrorsLength);
                    }
                }
                return this.parseExpressionStatement();
        }
    }
	
	// 添加尝试解析变量声明的方法，不产生永久性错误
    tryParseVariableDeclaration() {
        // 临时禁用错误报告
        const originalAddError = this.addError;
        let hadError = false;
        
        // 临时替换addError方法，只标记错误而不记录
        this.addError = (message, location) => {
			// debugging:
			//console.log(message, location);
            hadError = true;
        };
        
        try {
            // 尝试解析变量声明
            const type = this.parseTypeSpecifier();
            if (!type || hadError) {
                return null;
            }
            
            // 保存当前位置，因为parseDeclarator可能会失败
            const savedIndex = this.currentTokenIndex;
            
            const declarators = [];
            let firstDeclarator = null;
            
            // 尝试解析第一个声明符
            firstDeclarator = this.parseDeclarator(false);
            if (!firstDeclarator || !firstDeclarator.name || hadError) {
                // 解析失败，恢复位置
                this.currentTokenIndex = savedIndex;
                return null;
            }
            
            declarators.push(firstDeclarator);
			const varDecl = ASTBuilder.variableDeclaration(type, []);
			
			firstDeclarator.parent = varDecl;
			type.parent = varDecl;
            
            // 检查是否有更多声明符
            while (this.matchToken(TokenType.COMMA) && !hadError) {
                this.consumeToken();
                const declarator = this.parseDeclarator(false);
                if (!declarator || !declarator.name) {
                    break;
                }
				declarator.parent = varDecl;
                declarators.push(declarator);
            }
            
            // 检查分号
            if (!this.matchToken(TokenType.SEMICOLON) || hadError) {
                return null;
            }
            
            varDecl.location = type.location;
			varDecl.declarators = declarators;
			type.parent = varDecl;
            
            // 解析分号
            this.consumeToken();
            
            return varDecl;
        } finally {
            // 恢复原始的错误报告方法
            this.addError = originalAddError;
        }
    }
	
	// 添加辅助方法判断是否为类型名
	// In short, this is a function that currently always
	// returning true
    isTypeName(name) {
        // 检查是否为已知的内置类型
        const builtinTypes = [
            'int', 'char', 'float', 'void', 'double', 'long', 'short',
            'signed', 'unsigned', 'device', 'null_t'
        ];
        
        if (builtinTypes.includes(name)) {
            return true;
        }
        
        // 检查是否为已知的结构体/联合体名称
        // 注意：这里我们无法在解析时知道所有自定义类型，
        // 但我们可以通过解析过程中收集的类型名来判断
        
        // 对于struct/union/enum类型，它们已经通过关键字处理了
        // 这里主要处理typedef定义的类型名
        
        // 在完整实现中，应该维护一个类型名符号表
        // 检查是否已经在已知类型名集合中
        if (this.knownTypeNames.has(name)) {
            return true;
        }
        
        // 对于未知的标识符，我们需要检查它是否可能是类型名
        // 这里我们采用一个简单的启发式方法：检查后面的token模式
        
        const savedIndex = this.currentTokenIndex;
        let isLikelyType = false;
        
        // 跳过当前标识符
        this.consumeToken();
        
        // 检查常见的类型名后跟的模式
        if (this.matchToken(TokenType.IDENTIFIER)) {
            // 类型名后跟标识符（变量名） - 很可能是变量声明
            isLikelyType = true;
        } else if (this.matchToken(TokenType.MULTIPLY)) {
            // 类型名后跟*（指针） - 很可能是变量声明
            isLikelyType = true;
        } else if (this.matchToken(TokenType.LEFT_PAREN)) {
            // 类型名后跟( - 可能是函数声明或强制类型转换
            // 我们需要进一步检查
            const afterParen = this.tokens[this.currentTokenIndex + 1];
            if (afterParen && afterParen.type === TokenType.IDENTIFIER) {
                // (后跟标识符 - 可能是强制类型转换
                isLikelyType = true;
            }
        }
        
        // 恢复位置
        this.currentTokenIndex = savedIndex;
        
        return isLikelyType;
    }

	// 添加方法来记录已知类型名
    addTypeName(typeName) {
        this.knownTypeNames.add(typeName);
    }

    parseIfStatement() {
        const ifToken = this.expectToken(TokenType.IF);
        if (!ifToken) return null;

        this.expectToken(TokenType.LEFT_PAREN);
        const test = this.parseExpression();
        this.expectToken(TokenType.RIGHT_PAREN);

        const consequent = this.parseStatement();
        let alternate = null;

        if (this.matchToken(TokenType.ELSE)) {
            this.consumeToken();
            alternate = this.parseStatement();
        }

        const ifStmt = ASTBuilder.ifStatement(test, consequent, alternate);
		test.parent = ifStmt;
		consequent.parent = ifStmt;
		if (alternate) alternate.parent = ifStmt;
        ifStmt.location = ifToken.location;
        return ifStmt;
    }

    parseWhileStatement() {
        const whileToken = this.expectToken(TokenType.WHILE);
        if (!whileToken) return null;

        this.expectToken(TokenType.LEFT_PAREN);
        const test = this.parseExpression();
        this.expectToken(TokenType.RIGHT_PAREN);

        const body = this.parseStatement();

        const whileStmt = ASTBuilder.whileStatement(test, body);
        whileStmt.location = whileToken.location;
		test.parent = whileStmt;
		body.parent = whileStmt;
        return whileStmt;
    }

    parseForStatement() {
        const forToken = this.expectToken(TokenType.FOR);
        if (!forToken) return null;

        this.expectToken(TokenType.LEFT_PAREN);
        
        const init = this.matchToken(TokenType.SEMICOLON) 
            ? null 
            : this.parseExpressionStatement();
        
        const test = this.matchToken(TokenType.SEMICOLON)
            ? null
            : this.parseExpression();
        this.expectToken(TokenType.SEMICOLON);
        
        const update = this.matchToken(TokenType.RIGHT_PAREN)
            ? null
            : this.parseExpression();
        this.expectToken(TokenType.RIGHT_PAREN);

        const body = this.parseStatement();

        const forStmt = ASTBuilder.forStatement(init, test, update, body);
        forStmt.location = forToken.location;
		if (init) init.parent = forStmt;
		if (test) test.parent = forStmt;
		if (update) update.parent = forStmt;
        return forStmt;
    }

    parseReturnStatement() {
        const returnToken = this.expectToken(TokenType.RETURN);
        if (!returnToken) return null;

        let argument = null;
        if (!this.matchToken(TokenType.SEMICOLON)) {
            argument = this.parseExpression();
        }

        this.expectToken(TokenType.SEMICOLON);

        const returnStmt = ASTBuilder.returnStatement(argument);
        returnStmt.location = returnToken.location;
		argument.parent = returnStmt;
        return returnStmt;
    }

    parseBreakStatement() {
        const breakToken = this.expectToken(TokenType.BREAK);
        if (!breakToken) return null;

        this.expectToken(TokenType.SEMICOLON);

        const breakStmt = new ASTNode('BreakStatement');
        breakStmt.location = breakToken.location;
        return breakStmt;
    }

    parseContinueStatement() {
        const continueToken = this.expectToken(TokenType.CONTINUE);
        if (!continueToken) return null;

        this.expectToken(TokenType.SEMICOLON);

        const continueStmt = new ASTNode('ContinueStatement');
        continueStmt.location = continueToken.location;
        return continueStmt;
    }

    parseAsmStatement() {
        const asmToken = this.expectToken(TokenType.ASM);
        if (!asmToken) return null;

        this.expectToken(TokenType.LEFT_PAREN);
        const codeToken = this.expectToken(TokenType.STRING);
        this.expectToken(TokenType.RIGHT_PAREN);
        this.expectToken(TokenType.SEMICOLON);

        if (!codeToken) return null;

        const asmStmt = ASTBuilder.asmStatement(codeToken.value);
        asmStmt.location = asmToken.location;
        return asmStmt;
    }

    parseExpressionStatement() {
        const expression = this.parseExpression();
        this.expectToken(TokenType.SEMICOLON);

        if (!expression) return null;

        const exprStmt = new ASTNode('ExpressionStatement');
        exprStmt.addChild(expression);
        exprStmt.location = expression.location;
        return exprStmt;
    }

    parseExpression() {
        return this.parseAssignmentExpression();
    }

    parseAssignmentExpression() {
        const left = this.parseConditionalExpression();
        if (!left) return null;

        if (this.isAssignmentOperator()) {
            const operatorToken = this.consumeToken();
            const right = this.parseAssignmentExpression();
            
            if (!right) {
                this.addError('Expected expression after assignment operator');
                return left;
            }

            const assignment = ASTBuilder.assignmentExpression(operatorToken.value, left, right);
            assignment.location = left.location;
			left.parent = assignment;
			right.parent = assignment;
            return assignment;
        }

        return left;
    }

    isAssignmentOperator() {
        const token = this.getCurrentToken();
        return [
            TokenType.ASSIGN, TokenType.PLUS_ASSIGN, TokenType.MINUS_ASSIGN,
            TokenType.MULTIPLY_ASSIGN, TokenType.DIVIDE_ASSIGN, TokenType.MODULO_ASSIGN,
            TokenType.LEFT_SHIFT_ASSIGN, TokenType.RIGHT_SHIFT_ASSIGN,
            TokenType.BITWISE_AND_ASSIGN, TokenType.BITWISE_OR_ASSIGN, TokenType.BITWISE_XOR_ASSIGN
        ].includes(token.type);
    }

    parseConditionalExpression() {
        const test = this.parseLogicalOrExpression();
        if (!test) return null;

        if (this.matchToken(TokenType.QUESTION)) {
            this.consumeToken();
            const consequent = this.parseExpression();
            this.expectToken(TokenType.COLON);
            const alternate = this.parseConditionalExpression();

            const conditional = ASTBuilder.conditionalExpression(test, consequent, alternate);
			consequent.parent = conditional;
			if (alternate) alternate.parent = conditional;
            conditional.location = test.location;
            return conditional;
        }

        return test;
    }

    parseLogicalOrExpression() {
        let left = this.parseLogicalAndExpression();
        if (!left) return null;

        while (this.matchToken(TokenType.OR)) {
            const operatorToken = this.consumeToken();
            const right = this.parseLogicalAndExpression();
            
            if (!right) {
                this.addError('Expected expression after logical OR operator');
                break;
            }

            const logicalExpr = ASTBuilder.logicalExpression(operatorToken.value, left, right);
			left.parent = logicalExpr;
			right.parent = logicalExpr;
            logicalExpr.location = left.location;
            left = logicalExpr;
        }

        return left;
    }

    parseLogicalAndExpression() {
        let left = this.parseEqualityExpression();
        if (!left) return null;

        while (this.matchToken(TokenType.AND)) {
            const operatorToken = this.consumeToken();
            const right = this.parseEqualityExpression();
            
            if (!right) {
                this.addError('Expected expression after logical AND operator');
                break;
            }

            const logicalExpr = ASTBuilder.logicalExpression(operatorToken.value, left, right);
			left.parent = logicalExpr;
			right.parent = logicalExpr;
            logicalExpr.location = left.location;
            left = logicalExpr;
        }

        return left;
    }

    parseEqualityExpression() {
        let left = this.parseRelationalExpression();
        if (!left) return null;

        while (this.matchToken(TokenType.EQUAL) || this.matchToken(TokenType.NOT_EQUAL)) {
            const operatorToken = this.consumeToken();
            const right = this.parseRelationalExpression();
            
            if (!right) {
                this.addError('Expected expression after equality operator');
                break;
            }

            const binaryExpr = ASTBuilder.binaryExpression(operatorToken.value, left, right);
			left.parent = binaryExpr;
			right.parent = binaryExpr;
            binaryExpr.location = left.location;
            left = binaryExpr;
        }

        return left;
    }

    parseRelationalExpression() {
        let left = this.parseAdditiveExpression();
        if (!left) return null;

        while (
            this.matchToken(TokenType.LESS_THAN) ||
            this.matchToken(TokenType.LESS_EQUAL) ||
            this.matchToken(TokenType.GREATER_THAN) ||
            this.matchToken(TokenType.GREATER_EQUAL)
        ) {
            const operatorToken = this.consumeToken();
            const right = this.parseAdditiveExpression();
            
            if (!right) {
                this.addError('Expected expression after relational operator');
                break;
            }

            const binaryExpr = ASTBuilder.binaryExpression(operatorToken.value, left, right);
			left.parent = binaryExpr;
			right.parent = binaryExpr;
            binaryExpr.location = left.location;
            left = binaryExpr;
        }

        return left;
    }

    parseAdditiveExpression() {
        let left = this.parseMultiplicativeExpression();
        if (!left) return null;

        while (this.matchToken(TokenType.PLUS) || this.matchToken(TokenType.MINUS)) {
            const operatorToken = this.consumeToken();
            const right = this.parseMultiplicativeExpression();
            
            if (!right) {
                this.addError('Expected expression after additive operator');
                break;
            }

            const binaryExpr = ASTBuilder.binaryExpression(operatorToken.value, left, right);
			left.parent = binaryExpr;
			right.parent = binaryExpr;
            binaryExpr.location = left.location;
            left = binaryExpr;
        }

        return left;
    }

    parseMultiplicativeExpression() {
        let left = this.parseCastExpression(); // 修改这里
		if (!left) return null;

        while (
            this.matchToken(TokenType.MULTIPLY) ||
            this.matchToken(TokenType.DIVIDE) ||
            this.matchToken(TokenType.MODULO)
        ) {
            const operatorToken = this.consumeToken();
            const right = this.parseCastExpression();
            
            if (!right) {
                this.addError('Expected expression after multiplicative operator');
                break;
            }

            const binaryExpr = ASTBuilder.binaryExpression(operatorToken.value, left, right);
			left.parent = binaryExpr;
			right.parent = binaryExpr;
            binaryExpr.location = left.location;
            left = binaryExpr;
        }

        return left;
    }
	
	// 修改 parseCastExpression 方法，使其更健壮
	parseCastExpression() {
		// 检查是否可能是类型转换
		if (this.matchToken(TokenType.LEFT_PAREN)) {
			const startIndex = this.currentTokenIndex;
			
			// 尝试解析类型说明符
			this.consumeToken(); // 跳过 '('
			
			// 解析类型限定符
			const qualifiers = this.parseTypeQualifiers();
			
			// 尝试解析类型说明符
			const typeTokens = [
				TokenType.INT, TokenType.CHAR, TokenType.FLOAT, TokenType.VOID,
				TokenType.DOUBLE, TokenType.LONG, TokenType.SHORT, 
				TokenType.SIGNED, TokenType.UNSIGNED,
				TokenType.STRUCT, TokenType.UNION, TokenType.ENUM
			];
			
			let isType = false;
			let typeNode = null;
			
			// 检查基本类型
			for (const typeToken of typeTokens) {
				if (this.matchToken(typeToken)) {
					isType = true;
					const token = this.consumeToken();
					typeNode = ASTBuilder.typeSpecifier(token.value);
					typeNode.setAttribute('location', token.location);
					typeNode.setAttribute('qualifiers', qualifiers);
					
					// 处理struct/union/enum类型
					if (token.type === TokenType.STRUCT || token.type === TokenType.UNION) {
						this.parseStructOrUnionType(typeNode);
					} else if (token.type === TokenType.ENUM) {
						this.parseEnumType(typeNode);
					}
					break;
				}
			}
			
			// 检查特殊类型：device 和 null_t
			if (!isType && this.matchToken(TokenType.IDENTIFIER)) {
				const token = this.getCurrentToken();
				if (token.value === 'device' || token.value === 'null_t' || this.knownTypeNames.has(token.value)) {
					isType = true;
					this.consumeToken();
					typeNode = ASTBuilder.typeSpecifier(token.value);
					typeNode.setAttribute('location', token.location);
					typeNode.setAttribute('qualifiers', qualifiers);
					typeNode.setAttribute('isCustomType', true);
				}
			}
			
			if (isType && typeNode) {
				// 解析指针部分
				while (this.matchToken(TokenType.MULTIPLY)) {
					this.consumeToken();
					typeNode.pointerQualifiers.push(this.parseTypeQualifiers());
					typeNode.pointerDepth++;
				}
				
				// 检查是否有右括号
				if (this.matchToken(TokenType.RIGHT_PAREN)) {
					this.consumeToken(); // 跳过 ')'
					
					// 解析被转换的表达式
					const expression = this.parseCastExpression(); // 递归解析，允许嵌套类型转换
					if (expression) {
						// 创建类型转换节点
						const castExpr = new ASTNode('CastExpression', typeNode, expression);
						//castExpr.addChild(typeNode);
						castExpr.setAttribute('pointerDepth', typeNode.pointerDepth);
						castExpr.setAttribute('qualifiers', typeNode.pointerQualifiers);
						//castExpr.addChild(expression);
						castExpr.location = typeNode.location;
						typeNode.parent = castExpr;
						expression.parent = castExpr;
						return castExpr;
					} else {
						this.addError('Expected expression after cast type');
						return null;
					}
				} else {
					this.addError('Expected right parenthesis in type cast');
					// 回退
					this.currentTokenIndex = startIndex;
					return null;
				}
			} else {
				// 不是类型转换，回退
				this.currentTokenIndex = startIndex;
			}
		}
		
		// 如果不是类型转换，继续解析其他表达式
		return this.parseUnaryExpression();
	}
	
	// Currently, we always allow cast
	isValidCast(targetType, sourceType) {
		return true;
	}
	
	// Consider overriding 
    parseUnaryExpression() {
		
        // 检查一元操作符
        const unaryOperators = [
            TokenType.PLUS, TokenType.MINUS, TokenType.NOT, TokenType.BITWISE_NOT,
            TokenType.INCREMENT, TokenType.DECREMENT, TokenType.MULTIPLY, TokenType.BITWISE_AND
        ];

		// AI does have special styles. If I were the programmer, I would not implement this like that:
        for (const opType of unaryOperators) {
			if (this.matchToken(opType)) {
				const operatorToken = this.consumeToken();
				
				// 特殊处理：*和&可能是解引用和取地址操作符
				let operatorValue = operatorToken.value;
				
				// 解析参数
				const argument = this.parseUnaryExpression();
				
				if (!argument) {
					this.addError('Expected expression after unary operator');
					return null;
				}
				
				const unaryExpr = ASTBuilder.unaryExpression(operatorValue, argument);
				unaryExpr.location = operatorToken.location;
				argument.parent = unaryExpr;
				// 为解引用和取地址操作添加特殊标记
				if (operatorValue === '*') {
					unaryExpr.setAttribute('isDereference', true);
				} else if (operatorValue === '&') {
					unaryExpr.setAttribute('isAddressOf', true);
				}
				
				return unaryExpr;
			}
		}

        return this.parsePostfixExpression();
    }

    parsePostfixExpression() {
        let expression = this.parsePrimaryExpression();
        if (!expression) return null;

        while (true) {
            // 函数调用
            if (this.matchToken(TokenType.LEFT_PAREN)) {
                this.consumeToken();
                
                // 检查是否是内建函数
                if (expression.type === 'Identifier' && this.isBuiltinFunction(expression.name)) {
                    const builtinCall = ASTBuilder.builtinCall(expression.name);
                    builtinCall.location = expression.location;
					expression.parent = builtinCall;
                    
                    if (!this.matchToken(TokenType.RIGHT_PAREN)) {
                        do {
                            const arg = this.parseExpression();
                            if (arg) builtinCall.arguments.push(arg);
                        } while (this.matchToken(TokenType.COMMA) && this.consumeToken());
                    }
                    
                    this.expectToken(TokenType.RIGHT_PAREN);
                    expression = builtinCall;
                } else {
                    const callExpr = ASTBuilder.functionCall(expression);
                    callExpr.location = expression.location;
					expression.parent = callExpr;
                    
                    if (!this.matchToken(TokenType.RIGHT_PAREN)) {
                        do {
                            const arg = this.parseExpression();
                            if (arg) callExpr.arguments.push(arg);
                        } while (this.matchToken(TokenType.COMMA) && this.consumeToken());
                    }
                    
                    this.expectToken(TokenType.RIGHT_PAREN);
                    expression = callExpr;
                }
            }
            // 数组下标
            else if (this.matchToken(TokenType.LEFT_BRACKET)) {
                this.consumeToken();
                const index = this.parseExpression();
                this.expectToken(TokenType.RIGHT_BRACKET);
                
                // 创建成员表达式（数组访问）
                const memberExpr = new ASTNode('MemberExpression');
                memberExpr.addChild(expression);
                memberExpr.addChild(index);
                memberExpr.setAttribute('computed', true);
                memberExpr.location = expression.location;
				expression.parent = memberExpr;
                expression = memberExpr;
            }
			// 结构体成员访问（点操作符）
            else if (this.matchToken(TokenType.DOT)) {
                this.consumeToken();
                const memberName = this.expectToken(TokenType.IDENTIFIER);
                
                if (!memberName) {
                    this.addError('Expected member name after . operator');
                    return expression;
                }
                
                const memberExpr = new ASTNode('MemberExpression');
                memberExpr.addChild(expression);
                memberExpr.addChild(ASTBuilder.identifier(memberName.value));
                memberExpr.setAttribute('computed', false);
                memberExpr.setAttribute('operator', '.');
                memberExpr.location = expression.location;
				expression.parent = memberExpr;
                expression = memberExpr;
            }
            // 结构体指针成员访问（箭头操作符）
            else if (this.matchToken(TokenType.ARROW)) {
                this.consumeToken();
                const memberName = this.expectToken(TokenType.IDENTIFIER);
                
                if (!memberName) {
                    this.addError('Expected member name after -> operator');
                    return expression;
                }
                
                const memberExpr = new ASTNode('MemberExpression');
                memberExpr.addChild(expression);
                memberExpr.addChild(ASTBuilder.identifier(memberName.value));
                memberExpr.setAttribute('computed', false);
                memberExpr.setAttribute('operator', '->');
                memberExpr.location = expression.location;
				expression.parent = memberExpr;
                expression = memberExpr;
            }
            // 后置递增/递减
            else if (this.matchToken(TokenType.INCREMENT) || this.matchToken(TokenType.DECREMENT)) {
                const operatorToken = this.consumeToken();
                const unaryExpr = ASTBuilder.unaryExpression(operatorToken.value, expression);
                unaryExpr.prefix = false;
                unaryExpr.location = expression.location;
				expression.parent = unaryExpr;
                expression = unaryExpr;
            }
            else {
                break;
            }
        }

        return expression;
    }

    parsePrimaryExpression() {
        const token = this.getCurrentToken();

        switch (token.type) {
            case TokenType.IDENTIFIER: {
                this.consumeToken();
                return ASTBuilder.identifier(token.value).setAttribute('location', token.location);
            }
            
            case TokenType.NUMBER: {
                this.consumeToken();
                return ASTBuilder.numericLiteral(token.value).setAttribute('location', token.location);
            }
            
            case TokenType.STRING: {
                this.consumeToken();
                return ASTBuilder.stringLiteral(token.value).setAttribute('location', token.location);
            }
            
            case TokenType.CHARACTER: {
                this.consumeToken();
                return ASTBuilder.characterLiteral(token.value).setAttribute('location', token.location);
            }
            
            case TokenType.NULL: {
                this.consumeToken();
                return ASTBuilder.nullLiteral().setAttribute('location', token.location);
            }
            
            case TokenType.PREDEFINED_CONSTANT: {
                this.consumeToken();
                // @开头的预定义常量作为标识符处理
                return ASTBuilder.identifier(token.value).setAttribute('location', token.location);
            }
            
            case TokenType.LEFT_PAREN: {
                this.consumeToken();
                const expression = this.parseExpression();
                this.expectToken(TokenType.RIGHT_PAREN);
                return expression;
            }
            
            default:
                this.addError(`Unexpected token in expression: ${token.type} '${token.value}'`);
                this.consumeToken();
                return null;
        }
    }

    isBuiltinFunction(name) {
        const builtins = [
			'draw', 'print', 'drawflush', 'printflush', 'getlink',
			'control', 'radar', 'sensor', 'set', 'op', 'lookup',
			'wait', 'stop', 'end', 'jump', 'read', 'write', 'asm'
		];
        return builtins.includes(name);
    }
	
	// Added at 2nd fixing conversation
	// 添加特殊内建函数处理
	parseBuiltinCall(functionName, startLocation) {
		const builtinCall = ASTBuilder.builtinCall(functionName);
		builtinCall.location = startLocation;

		this.expectToken(TokenType.LEFT_PAREN);
		
		// 根据不同的内建函数处理参数
		switch (functionName) {
			case 'draw':
			case 'control':
				// 第一个参数是操作名称（字符串）
				const opNameToken = this.expectToken(TokenType.STRING);
				if (opNameToken) {
					builtinCall.arguments.push(ASTBuilder.stringLiteral(opNameToken.value));
				}
				break;
				
			case 'radar':
				// 四个操作名称参数
				for (let i = 0; i < 4; i++) {
					const radarOpToken = this.expectToken(TokenType.STRING);
					if (radarOpToken) {
						builtinCall.arguments.push(ASTBuilder.stringLiteral(radarOpToken.value));
					}
					if (i < 3 && !this.matchToken(TokenType.COMMA)) break;
					if (i < 3) this.consumeToken(); // 跳过逗号
				}
				break;
				
			default:
				// 默认参数处理
				break;
		}
		
		// 继续解析剩余参数
		if (!this.matchToken(TokenType.RIGHT_PAREN)) {
			do {
				// 对于set和op等函数，可能有特殊的参数要求
				if ((functionName === 'set' || functionName === 'op') && 
					builtinCall.arguments.length === 0) {
					// 第一个参数是目标变量名
					const targetVar = this.expectToken(TokenType.IDENTIFIER);
					if (targetVar) {
						builtinCall.arguments.push(ASTBuilder.identifier(targetVar.value));
					}
				} else {
					const arg = this.parseExpression();
					if (arg) builtinCall.arguments.push(arg);
				}
			} while (this.matchToken(TokenType.COMMA) && this.consumeToken());
		}
		
		this.expectToken(TokenType.RIGHT_PAREN);
		return builtinCall;
	}
}

// 7th conv.
// 修改SymbolEntry类以存储更详细的类型信息
class SymbolEntry {
    constructor(name, type, scope, kind, location = null) {
        this.name = name;
        this.type = type; // 类型信息（现在是TypeInfo对象）
        this.scope = scope;
		this.owningScope = null;	// The scope that the function (if it points to one) owns
        this.kind = kind; // 'variable', 'function', 'parameter', 'type'
        this.location = location;
        this.initialized = false;
        this.used = false;
		this.readCount = 0; // 读取次数
        this.writeCount = 0; // 写入次数
        this.memoryLocation = null;
        this.isGlobal = false;
		this.isRegister = false; // 标记是否为register变量
		// Note: this is also available for functions:
		// (for functions, once 'true', it can't be inlined !)
        this.isAddressed = false; // 标记是否被取地址
		this.isConst = false;
        this.isVolatile = false;
		// 常量传播相关
        this.isConstant = false;
        this.constantValue = null;
        
        // 优化标记
        this.canBeEliminated = false;
        this.isDead = false;
    }
	
	// 标记为常量
    markAsConstant(value) {
        this.isConstant = true;
        this.constantValue = value;
        if (this.type && this.type.qualifiers) {
            this.type.qualifiers.push('const');
        }
    }
}

// 作用域类
class Scope {
    constructor(parent = null, type = 'block', astNode = null, name = '') {
        this.parent = parent;
        this.type = type; // 'global', 'function', 'block'
		this.name = name;
		this.astNode = astNode; // 关联的AST节点
        this.symbols = new Map();
        this.children = [];
		
		if (parent) {
            parent.children.push(this);
        }
    }

    addSymbol(symbol) {
        this.symbols.set(symbol.name, symbol);
		symbol.scope = this; // 建立双向关联
    }
	
	removeSymbol(name) {
		this.symbols.delete(name);
	}

    lookup(name) {
        return this.symbols.get(name) || (this.parent ? this.parent.lookup(name) : null);
    }
	
	// lookup scope for a symbol
	lookupScopeOf(name) {
		if (this.symbols.has(name)) {
			return this;
		}
		return this.parent ? this.parent.lookupScopeOf(name) : null;
	}

    lookupCurrent(name) {
        return this.symbols.get(name) || null;
    }
	
	// 获取当前作用域的所有符号
    getAllSymbols() {
        return Array.from(this.symbols.values());
    }
	
	// 获取作用域的完整路径
    getPath() {
        const path = [];
        let current = this;
        while (current) {
            path.unshift(current.type + ':' + current.name);
            current = current.parent;
        }
        return path.join('.');
    }
}

// 首先，添加一些辅助类
class TypeInfo {
    constructor(name, kind, size = 1, members = null) {
        this.name = name; // 类型名称
        this.kind = kind; // 'basic', 'struct', 'union', 'pointer', 'array', 'function', 'device', 'null'
        this.size = size; // 类型大小（按1字节对齐）
        this.members = members || []; // 结构体/联合体成员
        this.pointerTo = null; // 对于指针类型，指向的类型 (MUST BE A TYPE, NOT A FUNCTION)
        this.arraySize = null; // 对于数组类型，数组大小
        this.qualifiers = []; // 类型限定符（const, volatile）
		this.functionTo = null;	// Pointing to what function for function type
    }
    
    isConst() {
        return this.qualifiers.includes('const');
    }
    
    isVolatile() {
        return this.qualifiers.includes('volatile');
    }
    
    toString() {
        let result = this.qualifiers.length > 0 ? this.qualifiers.join(' ') + ' ' : '';
        result += this.name;
        if (this.kind === 'pointer' && this.pointerTo) {
            result = this.pointerTo.toString() + '*';
        }
        return result;
    }
}

class MemberInfo {
    constructor(name, type, offset, bitField = null) {
        this.name = name;
        this.type = type;
        this.offset = offset; // 成员偏移量
        this.bitField = bitField; // 位域信息
    }
}

class SemanticAnalyzer extends ASTVisitor {
	// !! Global scope is being manually established !!
    constructor() {
        super();
        this.errors = [];
        this.warnings = [];
        this.currentScope = null;
        this.currentFunction = null;
        this.symbolTable = new Map();
        this.typeTable = new Map();
        this.structTable = new Map(); // 专门存储结构体/联合体定义
        this.typedefTable = new Map(); // 存储typedef定义的类型别名
        
		this.globalScope = null;	// ???
		this.scopeCounter = 0;
		// Cannot do initialization in construction !!!
    }

    analyze(ast) {
        this.errors = [];
        this.warnings = [];
        this.currentScope = new Scope(null, 'global');
        this.currentFunction = null;
		this.globalScope = this.currentScope;
		
		// 初始化内建类型
        this.initializeBuiltinTypes();
        this.initializeBuiltinFunctions();
		
		// 建立AST节点和作用域的关联
        this.linkScopesToAST(ast);
		
		// 第一遍：收集类型定义（typedef, struct, union）
        this.collectTypeDefinitions(ast);

        this.visit(ast);

        return {
            success: this.errors.length === 0,
            ast: ast,
            errors: this.errors,
            warnings: this.warnings,
            symbolTable: this.symbolTable
        };
    }
	
	// !! Notice: new scopes can't be created afterwards !!
	// 建立AST节点和作用域的关联
    linkScopesToAST(node) {
        if (!node) return;
        
        // 将当前作用域关联到节点
        node.scope = this.currentScope;
        
        switch (node.type) {
            case 'Program':
                // 程序节点关联全局作用域
                node.scope = this.globalScope;
                this.globalScope.astNode = node;
				// This is the actual thing...
				if (node.functions) {
					node.functions.forEach(child => this.linkScopesToAST(child));
				}
                break;
                
            case 'FunctionDeclaration':
                // 函数作用域
                const functionScope = new Scope(this.currentScope, 'function', node, node.name);
                this.currentScope = functionScope;
                node.scope = functionScope;
                break;
                
            case 'CompoundStatement':
                // 块作用域
				// TODO: There are some bugs here...
                if (this.currentScope.type !== 'function' || 
                    (this.currentScope.astNode && this.currentScope.astNode.type !== 'CompoundStatement')) {
                    const blockScope = new Scope(this.currentScope, 'block', node, '' + (++this.scopeCounter));
                    this.currentScope = blockScope;
                    node.scope = blockScope;
                } else {
                    // 函数体的第一个复合语句不创建新作用域
                    node.scope = this.currentScope;
                }
				if (node.statements) {
					node.statements.forEach(child => this.linkScopesToAST(child));
				}
                break;
        }
        
        // 递归处理子节点
        if (node.children) {
            node.children.forEach(child => this.linkScopesToAST(child));
        }
		
        // 处理特殊节点的子节点
        switch (node.type) {
            case 'IfStatement':
                if (node.test) this.linkScopesToAST(node.test);
                if (node.consequent) this.linkScopesToAST(node.consequent);
                if (node.alternate) this.linkScopesToAST(node.alternate);
                break;
                
            case 'WhileStatement':
                if (node.test) this.linkScopesToAST(node.test);
                if (node.body) this.linkScopesToAST(node.body);
                break;
                
            case 'ForStatement':
                if (node.init) this.linkScopesToAST(node.init);
                if (node.test) this.linkScopesToAST(node.test);
                if (node.update) this.linkScopesToAST(node.update);
                if (node.body) this.linkScopesToAST(node.body);
                break;
                
            case 'FunctionDeclaration':
                // 处理参数
                node.parameters.forEach(param => {
                    if (param.type && param.name) {
                        param.scope = this.currentScope;
                    }
                });
                if (node.body) this.linkScopesToAST(node.body);
                break;
                
            case 'VariableDeclaration':
                node.declarators.forEach(declarator => {
                    declarator.scope = this.currentScope;
                    if (declarator.initializer) {
                        this.linkScopesToAST(declarator.initializer);
                    }
                });
                break;
        }
        
        // 退出作用域
        switch (node.type) {
            case 'FunctionDeclaration':
            case 'CompoundStatement':
                if (node.scope && node.scope !== this.globalScope) {
                    this.currentScope = node.scope.parent;
                }
                break;
        }
    }
	
	// !! MANUAL CHANGES HERE !!
	collectTypeDefinitions(node) {
        if (!node) return;
        
        // 收集程序中的类型定义
        if (node.type === 'Program') {
            // 收集typedef定义
			// Modified here
            if (node.typeDefinitions) {
                node.typeDefinitions.forEach(def => {
                    if (def.type === 'struct' || def.type === 'union') {
                        this.processStructOrUnionDefinition(def);
                    } else {
						if ((!def.isStruct) && (!def.isUnion)) {
							this.processTypedefDeclaration(def);
						}
						else {
							this.addError(`Internal error: Unknown parsed structure`, node.location);
						}
					}
                });
            }
            
            // 收集全局结构体/联合体定义
            node.globalDeclarations.forEach(decl => {
                if (decl.type === 'VariableDeclaration' && decl.isStructOrUnion) {
                    // 处理结构体/联合体变量声明中的类型定义
                    //this.processTypeFromDeclaration(decl.type);
					// 检查是否是结构体/联合体类型
					const typeName = this.getTypeNameFromTypeNode(decl.type);
					if (typeName === 'struct' || typeName === 'union') {
						this.processTypeFromDeclaration(decl.type);
					}
				}
            });
        }
    }

    initializeBuiltinTypes() {
        // 基本类型
        const basicTypes = ['int', 'char', 'float', 'double', 'void', 'long', 'short', 'signed', 'unsigned'];
        basicTypes.forEach(type => {
            this.typeTable.set(type, new TypeInfo(type, 'basic', 1));
        });

        // 特殊类型
        this.typeTable.set('device', new TypeInfo('device', 'device', 1));
        this.typeTable.set('null_t', new TypeInfo('null_t', 'null', 1));
        
        // 指针类型的基础（void*）
		// Pointer has a size of 2 !!
        const voidPtrType = new TypeInfo('void*', 'pointer', 2);
        voidPtrType.pointerTo = this.typeTable.get('void');
        this.typeTable.set('void*', voidPtrType);
    }

    initializeBuiltinFunctions() {
		// TODO: Must be modified...
        const builtins = [
            { name: 'draw', returnType: 'void', parameters: ['char*'] }, // 操作名称 + 变长参数
			{ name: 'print', returnType: 'void', parameters: [] }, // 变长参数
			{ name: 'drawflush', returnType: 'void', parameters: ['device'] },
			{ name: 'printflush', returnType: 'void', parameters: ['device'] },
			{ name: 'getlink', returnType: 'device', parameters: ['int'] },
			{ name: 'control', returnType: 'void', parameters: ['char*', 'device'] }, // 操作名称 + 设备 + 变长参数
			{ name: 'radar', returnType: 'int', parameters: ['char*', 'char*', 'char*', 'char*', 'int'] },
			{ name: 'sensor', returnType: 'int', parameters: ['device', 'int'] },
			{ name: 'set', returnType: 'void', parameters: [] }, // 特殊处理
			{ name: 'op', returnType: 'void', parameters: [] }, // 特殊处理
			{ name: 'lookup', returnType: 'int', parameters: ['char*', 'int'] },
			{ name: 'wait', returnType: 'void', parameters: ['int'] },
			{ name: 'stop', returnType: 'void', parameters: [] },
			{ name: 'end', returnType: 'void', parameters: [] },
			{ name: 'jump', returnType: 'void', parameters: [] }, // 特殊处理
			{ name: 'read', returnType: 'void', parameters: [] }, // 特殊处理
			{ name: 'write', returnType: 'void', parameters: [] }, // 特殊处理
			{ name: 'asm', returnType: 'void', parameters: ['char*'] } // TODO: I'm unsure about whether this can be correctly done now.
        ];

        builtins.forEach(func => {
            const symbol = new SymbolEntry(
                func.name,
                { returnType: func.returnType, parameters: func.parameters },
                this.currentScope,
                'function'
            );
			// Builtin symbol doesn't own scope
            this.currentScope.addSymbol(symbol);
        });
    }

	// Deepseek has some branches here
	// ! There has some manual changes !
	processTypedefDeclaration(typedefNode) {
		// Deepseek says yes
		// 临时解决方案：检查节点是否具有declarators属性
		if (!typedefNode.declarators) {
			this.addError(`Internal Error: Invalid typedef declaration`, typedefNode.location);
			return;
		}
		
		// Some special process for function pointer type:
		// ! TODO: Functions will need 'isAddressed' (to suppress inlining) !
		if (typedefNode.isFunction) {
			typedefNode.declarators.forEach(declarator => {
				const aliasName = declarator.name;
				// Directly create a new type instead of go aliases
				// TODO: Add temporary name, if necessary?
				// Function has a size of 1 (its address)
				// (Different from pointer's size 2 due to the need of storing its block position
				// This will make them have a name
				const aliasType = new TypeInfo(aliasName, 'function', 1);
				aliasType.qualifiers = typedefNode.functionDefinition.pointerQualifiers;
				aliasType.functionTo = new SymbolEntry(
					aliasName,
					{
						returnType: typedefNode.functionDefinition.returnType.typeName,
						parameters: typedefNode.functionDefinition.parameters.map(param => ({
							name: param.name,
							type: this.getTypeNameFromTypeNode(param.type)
						}))
					},
					this.globalScope,
					'function',
					typedefNode.location
				);
				
				this.typeTable.set(aliasName, aliasType);
				this.typedefTable.set(aliasName, {
					originalType: aliasType,
					aliasType: aliasType,
					location: declarator.location
				});
			});
		} else {
			// Traditional process
			let baseTypeName = this.getTypeNameFromTypeNode(typedefNode.type);
			let baseType = this.getTypeInfo(baseTypeName);
			
			if (!baseType) {
				this.addError(`Unknown base type '${baseTypeName}' in typedef`, typedefNode.location);
				return;
			}
			
			typedefNode.declarators.forEach(declarator => {
				const aliasName = declarator.name;
				const aliasType = this.createAliasType(baseType, declarator);
				
				// 添加到类型表和typedef表
				this.typeTable.set(aliasName, aliasType);
				this.typedefTable.set(aliasName, {
					originalType: baseType,
					aliasType: aliasType,
					location: declarator.location
				});
			});
		}
    }
	
	processStructOrUnionDefinition(defNode) {
        const typeName = defNode.name || `anonymous_${defNode.type}_${Date.now()}`;
        const isUnion = defNode.type === 'UnionDefinition';
        const kind = isUnion ? 'union' : 'struct';
        
        // 创建结构体/联合体类型
        const structType = new TypeInfo(typeName, kind, 0); // 初始大小为0
        structType.members = [];
        
        // 处理成员
        let currentOffset = 0;
        let maxMemberSize = 0;
        
        if (defNode.members && defNode.members.length > 0) {
            defNode.members.forEach(member => {
                const memberTypeName = this.getTypeNameFromTypeNode(member.type);
                const memberType = this.getTypeInfo(memberTypeName);
                
                if (!memberType) {
                    this.addError(`Unknown member type '${memberTypeName}'`, member.location);
                    return;
                }
                
                // 计算成员大小（所有基本类型大小为1）
                const memberSize = this.calculateTypeSize(memberType);
                
                // 对于结构体，累加偏移量；对于联合体，取最大成员大小
                if (isUnion) {
                    maxMemberSize = Math.max(maxMemberSize, memberSize);
                } else {
                    // 结构体成员：按1字节对齐
                    const memberInfo = new MemberInfo(
                        member.name,
                        memberType,
                        currentOffset,
                        member.bitField
                    );
                    structType.members.push(memberInfo);
                    currentOffset += memberSize;
                }
            });
            
            // 计算最终大小
            if (isUnion) {
                structType.size = maxMemberSize;
                // 联合体所有成员偏移量都是0
                defNode.members.forEach((member, index) => {
                    const memberTypeName = this.getTypeNameFromTypeNode(member.type);
                    const memberType = this.getTypeInfo(memberTypeName);
                    if (memberType) {
                        const memberInfo = new MemberInfo(
                            member.name,
                            memberType,
                            0, // 所有成员偏移量都是0
                            member.bitField
                        );
                        structType.members.push(memberInfo);
                    }
                });
            } else {
                structType.size = currentOffset;
            }
        } else {
            // 空结构体/联合体
            structType.size = 0;
        }
        
        // 存储到类型表
        this.typeTable.set(typeName, structType);
        this.structTable.set(typeName, structType);
        
        // 如果结构体有名称，添加到当前作用域
        if (defNode.name) {
            const structSymbol = new SymbolEntry(
                defNode.name,
                { type: structType },
                this.currentScope,
                'type'
            );
            this.currentScope.addSymbol(structSymbol);
        }
    }
	
	processTypeFromDeclaration(typeNode) {
        // 从声明中提取类型信息
        const typeName = this.getTypeNameFromTypeNode(typeNode);
        if (!this.typeTable.has(typeName)) {
            const typeInfo = this.createTypeInfoFromNode(typeNode);
            if (typeInfo) {
                this.typeTable.set(typeName, typeInfo);
            }
        }
    }
	
	createTypeInfoFromNode(typeNode) {
        const typeName = this.getTypeNameFromTypeNode(typeNode);
        const qualifiers = typeNode.getAttribute('qualifiers') || [];
        
        // 检查是否是结构体/联合体类型
        if (typeName === 'struct' || typeName === 'union') {
            const structName = typeNode.getAttribute('structOrUnionName');
            if (structName && this.structTable.has(structName)) {
                const structType = this.structTable.get(structName);
                const result = new TypeInfo(structName, typeName, structType.size, structType.members);
                result.qualifiers = qualifiers;
                return result;
            } else {
                // 匿名结构体/联合体
                const anonymousType = new TypeInfo(`anonymous_${typeName}`, typeName, 1);
                anonymousType.qualifiers = qualifiers;
                return anonymousType;
            }
        }
        
        // 基本类型或typedef类型
        const baseType = this.typeTable.get(typeName);
        if (baseType) {
            const result = new TypeInfo(typeName, baseType.kind, baseType.size);
            result.qualifiers = qualifiers;
            return result;
        }
        
        return null;
    }
	
	createAliasType(baseType, declarator) {
        // 创建typedef别名类型
        const aliasType = new TypeInfo(baseType.name, baseType.kind, baseType.size, [...baseType.members]);
        aliasType.qualifiers = [...baseType.qualifiers];
        
        // 处理指针
        if (declarator.pointerDepth > 0) {
            let currentType = aliasType;
            for (let i = 0; i < declarator.pointerDepth; i++) {
                const ptrType = new TypeInfo('', 'pointer', 2);
                ptrType.pointerTo = currentType;
                ptrType.qualifiers = declarator.pointerQualifiers[i] || [];
                currentType = ptrType;
            }
            return currentType;
        }
        
        // 处理数组
        if (declarator.arrayDimensions && declarator.arrayDimensions.length > 0) {
            // 计算数组类型
            let arrayType = aliasType;
            for (const dim of declarator.arrayDimensions.reverse()) {
                const arrType = new TypeInfo('', 'array', arrayType.size * (dim || 1));
                arrType.arraySize = dim;
                arrType.pointerTo = arrayType; // 使用pointerTo指向元素类型
                arrayType = arrType;
            }
            return arrayType;
        }
        
        return aliasType;
    }
	
	// Manually modified to support pointers!
	getTypeInfo(typeNameRaw) {
		
		let typeName = typeNameRaw;
		const ptrLocate = typeName.indexOf('*');
		const ptrSize = (ptrLocate < 0) ? 0 : typeName.length - ptrLocate;
		if (ptrLocate != -1) {
			typeName = typeName.slice(0, ptrLocate);
		}
		let result = null;
		
        // 从类型表获取类型信息
        if (this.typeTable.has(typeName)) {
            result = this.typeTable.get(typeName);
        } else if (this.structTable.has(typeName)) {
            result = this.structTable.get(typeName);
        }
		
		for (let i = 0; i < ptrSize; i++) {
			const ptrType = new TypeInfo('', 'pointer', '2');
			ptrType.pointerTo = result;
			result = ptrType;
		}
        
        return result;
    }
	
	calculateTypeSize(typeInfo) {
        if (!typeInfo) return 1; // 默认大小
        
        switch (typeInfo.kind) {
            case 'basic':
            case 'device':
            case 'null':
            case 'pointer':
                return 1;
            case 'struct':
            case 'union':
                return typeInfo.size || 1;
            case 'array':
                if (typeInfo.arraySize) {
                    const elementSize = this.calculateTypeSize(typeInfo.pointerTo);
                    return elementSize * typeInfo.arraySize;
                }
				// TODO: Might be adding warning
                return 1; // 未知大小的数组
            default:
                return 1;
        }
    }

    addError(message, location = null) {
        this.errors.push({
            message,
            line: location ? location.line : 0,
            column: location ? location.column : 0
        });
    }

    addWarning(message, location = null) {
        this.warnings.push({
            message,
            line: location ? location.line : 0,
            column: location ? location.column : 0
        });
    }

    // =============== 访问者方法 ===============

    visitProgram(node) {
        // 遍历所有全局声明和函数
        node.functions.forEach(func => this.visit(func));
        node.globalDeclarations.forEach(decl => this.visit(decl));
    }

    visitFunctionDeclaration(node) {
        // 检查函数是否已声明
        const existingSymbol = this.currentScope.lookupCurrent(node.name);
        if (existingSymbol) {
            this.addError(`Function '${node.name}' is already declared`, node.location);
            return;
        }

		// 获取返回类型名称
		// Added after correctional conversation.
		const returnTypeName = this.getTypeNameFromTypeNode(node.returnType);
		if (!returnTypeName) {
			this.addError(`Invalid return type for function '${node.name}'`, node.returnType.location);
			return;
		}

        // 创建函数符号
        const funcSymbol = new SymbolEntry(
            node.name,
            {
                returnType: this.typeToString(this.getTypeNameFromTypeNode(node.returnType)),
                parameters: node.parameters.map(param => ({
                    name: param.name,
                    type: this.getTypeNameFromTypeNode(param.type) /*param.type.typeName*/
					/* Modified in 5th conversation. Whether this is correct remains to be seen! */
                }))
            },
            this.currentScope,
            'function',
            node.location
        );
		funcSymbol.owningScope = node.scope;

        this.currentScope.addSymbol(funcSymbol);
        this.currentFunction = funcSymbol;

        // 进入函数作用域
		let functionScope = node.scope;
		if (!functionScope) {
			functionScope = new Scope(this.currentScope, 'function');
			this.currentScope.children.push(functionScope);
		}
		this.currentScope = functionScope;

        // 添加参数到作用域
        node.parameters.forEach(param => {
            const paramSymbol = new SymbolEntry(
                param.name,
                { type: param.type.typeName },
                this.currentScope,
                'parameter',
                param.location
            );
            this.currentScope.addSymbol(paramSymbol);
        });

        // 分析函数体
        if (node.body) {
            this.visit(node.body);
        }

        // 返回父作用域
        this.currentScope = this.currentScope.parent;
        this.currentFunction = null;
    }

	// Modified on 1 Dec, to be verified
    visitVariableDeclaration(node) {
        const baseTypeName = this.getTypeNameFromTypeNode(node.type);
        const qualifiers = node.type.getAttribute('qualifiers') || [];
		
		// 检查是否有存储类说明符
		const storageClass = node.storageClass; // auto, register, static, extern
        
        // 获取基础类型信息
        let baseType = this.getTypeInfo(baseTypeName);
        if (!baseType) {
            this.addError(`Unknown type '${baseTypeName}'`, node.type.location);
            return;
        }
        
        // 创建带有限定符的类型
        const varType = new TypeInfo(baseType.name, baseType.kind, baseType.size, [...baseType.members]);
        varType.qualifiers = [...baseType.qualifiers, ...qualifiers];
		
        node.declarators.forEach(declarator => {
            // 检查变量是否已声明
            const existingSymbol = this.currentScope.lookupCurrent(declarator.name);
            if (existingSymbol) {
                this.addError(`Variable '${declarator.name}' is already declared in this scope`, declarator.location);
                return;
            }

            // 创建变量类型（处理指针和数组）
            let finalType = varType;
            
            // 处理指针
            if (declarator.pointerDepth > 0) {
                let currentType = finalType;
                for (let i = 0; i < declarator.pointerDepth; i++) {
                    const ptrType = new TypeInfo('', 'pointer', 2);
                    ptrType.pointerTo = currentType;
                    ptrType.qualifiers = declarator.pointerQualifiers[i] || [];
                    currentType = ptrType;
                }
                finalType = currentType;
            } else {
				if (baseType.functionTo) {
					finalType.functionTo = baseType.functionTo;
				}
				// TODO: Check array support for typedef
				if (baseType.arrayDimensions && baseType.arrayDimensions.length > 0) {
					// baseType seems immutable so far
					finalType.arraySize = baseType.arraySize;
					finalType.arrayDimensions = baseType.arrayDimensions;
				}
			}
            
            // 处理数组
            if (declarator.arrayDimensions && declarator.arrayDimensions.length > 0) {
                let arrayType = finalType;
                for (const dim of declarator.arrayDimensions.reverse()) {
                    const arrType = new TypeInfo('', 'array', arrayType.size * (dim || 1));
                    arrType.arraySize = dim;
                    arrType.pointerTo = arrayType;
                    arrayType = arrType;
                }
                finalType = arrayType;
            }

            // 创建变量符号
            const varSymbol = new SymbolEntry(
                declarator.name,
                { 
                    type: finalType,
                    isConst: finalType.isConst(),
                    isVolatile: finalType.isVolatile()
                },
                this.currentScope,
                'variable',
                declarator.location
            );
			
			// 设置register标记
			if (storageClass === 'register') {
				varSymbol.isRegister = true;
			}
			
			// 添加const和volatile信息
            const qualifiers = node.type.getAttribute('qualifiers') || [];
            varSymbol.isConst = qualifiers.includes('const') || finalType.isConst();
            varSymbol.isVolatile = qualifiers.includes('volatile') || finalType.isVolatile();
			
			// 检查是否为全局变量
            varSymbol.isGlobal = this.currentScope === this.globalScope;

            this.currentScope.addSymbol(varSymbol);

            // 检查初始化表达式
            if (declarator.initializer) {
                this.visit(declarator.initializer);
                
                // 类型检查初始化表达式
                const initType = this.getExpressionType(declarator.initializer);
                if (initType && !this.isTypeCompatible(finalType, initType)) {
                    this.addError(
                        `Cannot initialize '${this.typeToString(finalType)}' with '${this.typeToString(initType)}'`,
                        declarator.initializer.location
                    );
                }

                // 检查const变量初始化
                if (finalType.isConst() && !declarator.initializer) {
                    this.addError(`Const variable '${declarator.name}' must be initialized`, declarator.location);
                }

                varSymbol.initialized = true;
            } else if (finalType.isConst()) {
                this.addError(`Const variable '${declarator.name}' must be initialized`, declarator.location);
            }
        });
    }

	// 5th conversation
	// 新增辅助方法：从类型节点中提取类型名称
	// 7th (1 Dec)
	// 修改getTypeNameFromTypeNode以处理指针和结构体
	// Priority adjusted manually !!!
	// Also add pointer dereference manually
	// This function doesn't case const qualifier
    getTypeNameFromTypeNode(typeNode) {
        if (!typeNode) return null;
        
		const ptrSuffix = "*".repeat(typeNode.pointerDepth);
		
		// 处理结构体/联合体类型
        if (typeNode.type === 'TypeSpecifier' && 
            (typeNode.typeName === 'struct' || typeNode.typeName === 'union')) {
            const structName = typeNode.getAttribute('structOrUnionName');
            return (structName || typeNode.typeName) + ptrSuffix;
        }
		
        // 如果是TypeSpecifier节点
        if (typeNode.typeName) {
            return typeNode.typeName + ptrSuffix;
        }
        
        // 如果是标识符节点（处理typedef定义的类型）
        if (typeNode.type === 'Identifier') {
            return typeNode.name + ptrSuffix;
        }
        
        return null;
    }

    visitIdentifier(node) {
        const symbol = this.currentScope.lookup(node.name);
        if (!symbol) {
            this.addError(`Undeclared identifier '${node.name}'`, node.location);
            return;
        }

        symbol.used = true;
        node.symbol = symbol; // 将符号关联到节点

        // 设置节点类型
        if (symbol.kind === 'variable' || symbol.kind === 'parameter') {
            node.dataType = symbol.type.type;
        }
    }

    visitBinaryExpression(node) {
        this.visit(node.left);
        this.visit(node.right);

        const leftType = this.getExpressionType(node.left);
        const rightType = this.getExpressionType(node.right);

        if (!leftType || !rightType) {
            return;
        }

        // 检查操作符的类型兼容性
        if (!this.isTypeCompatibleForOperator(node.operator, leftType, rightType)) {
            this.addError(
                `Operator '${node.operator}' cannot be applied to types '${leftType}' and '${rightType}'`,
                node.location
            );
            return;
        }

        // 设置表达式结果类型
        node.dataType = this.getResultType(node.operator, leftType, rightType);
    }

    visitUnaryExpression(node) {
        this.visit(node.argument);
		const argType = this.getExpressionType(node.argument);
		
		if (!argType) return;
		
		const operator = node.operator;
		
		switch (operator) {
			case '*': // 解引用操作
				if (!this.isValidDereference(argType)) {
					this.addError(`Cannot dereference non-pointer type '${this.typeToString(argType)}'`, node.location);
					return;
				}
				
				// 设置表达式结果类型
				node.dataType = argType.pointerTo;
				break;
				
			case '&': // 取地址操作
				// 检查操作数是否为左值
				if (!this.isLValue(node.argument)) {
					this.addError(`Cannot take address of non-lvalue`, node.location);
					return;
				}
				
				// 检查是否为register变量
				if (this.isRegisterVariable(node.argument)) {
					this.addError(`Cannot take address of register variable`, node.location);
					return;
				}
				
				// 标记变量已被取地址
				this.markVariableAsAddressed(node.argument);
				
				// 创建指针类型
				const ptrType = new TypeInfo('', 'pointer', 2);
				ptrType.pointerTo = argType;
				node.dataType = ptrType;
				break;
				
			case '+': case '-': // 正负号
				if (!this.isNumericType(argType.name)) {
					this.addError(`Unary '${operator}' requires numeric type`, node.location);
					return;
				}
				node.dataType = argType;
				break;
				
			case '~': // 按位取反
				if (!this.isIntegerType(argType.name)) {
					this.addError(`Unary '${operator}' requires integer type`, node.location);
					return;
				}
				node.dataType = argType;
				break;
				
			case '!': // 逻辑非
				node.dataType = this.typeTable.get('int'); // 返回int类型（0或1）
				break;
				
			case '++': case '--': // 递增递减
				if (!this.isLValue(node.argument)) {
					this.addError(`'${operator}' requires lvalue`, node.location);
					return;
				}
				
				if (!this.isNumericType(argType.name)) {
					this.addError(`'${operator}' requires numeric type`, node.location);
					return;
				}
				
				// 检查const变量
				if (argType.isConst()) {
					this.addError(`Cannot modify const variable with '${operator}'`, node.location);
				}
				
				node.dataType = argType;
				break;
				
			default:
				this.addError(`Unknown unary operator '${operator}'`, node.location);
				break;
		}
    }
	
	// 添加辅助方法检查是否有效的解引用操作
	isValidDereference(type) {
		return type && (type.kind === 'pointer' || type.kind === 'array');
	}
	
	// 添加辅助方法检查是否为register变量
	isRegisterVariable(node) {
		if (node.type === 'Identifier') {
			const symbol = this.currentScope.lookup(node.name);
			return symbol && symbol.isRegister;
		}
		return false;
	}
	
	// 添加辅助方法标记变量已被取地址
	markVariableAsAddressed(node) {
		if (node.type === 'Identifier') {
			const symbol = this.currentScope.lookup(node.name);
			if (symbol && symbol.kind === 'variable') {
				symbol.isAddressed = true;
				
				// 被取地址的变量不能放在寄存器中
				if (symbol.isRegister) {
					this.addWarning(`Register variable '${node.name}' has its address taken, will be stored in memory`, node.location);
					symbol.isRegister = false; // 强制取消register优化
				}
			}
		} else if (node.type === 'MemberExpression') {
			// 处理结构体成员被取地址的情况
			const object = node.getChild(0);
			this.markVariableAsAddressed(object);
		} else if (node.type === 'UnaryExpression' && node.operator === '*') {
			// 解引用表达式的地址（如&*ptr）等于ptr
			const arg = node.argument;
			if (arg.type === 'Identifier') {
				const symbol = this.currentScope.lookup(arg.name);
				if (symbol && symbol.kind === 'variable') {
					symbol.isAddressed = true;
				}
			}
		}
	}

    visitAssignmentExpression(node) {
        this.visit(node.left);
        this.visit(node.right);

        const leftType = this.getExpressionType(node.left);
        let rightType = this.getExpressionType(node.right);

        if (!leftType || !rightType) {
            return;
        }

        // 检查赋值兼容性
        if (!this.isTypeCompatible(leftType, rightType)) {
			let funcFlag = false;
			if (node.right.type === 'Identifier' && node.right.symbol.kind === 'function') {
				// Try function pointer
				const funcType = new TypeInfo('', 'function', 1);
				funcType.functionTo = new SymbolEntry(
					'',
					{
						returnType: node.right.symbol.type.returnType,
						parameters: node.right.symbol.type.parameters
					},
					this.globalScope,
					'function',
					node.right.location
				);
				// Identifier itself can be assigned this:
				node.dataType = funcType;
				rightType = funcType;
				funcFlag = this.isTypeCompatible(leftType, rightType);
				const rightTypeReadable = this.typeToString(rightType);
				if (!funcFlag) {
					this.addError(`Incorrect type '${rightTypeReadable}' in assignment of function pointer '${leftType}'`,
					node.location);
				}
			} else {
				this.addError(
                `Cannot assign '${rightType}' to '${leftType}'`,
                node.location
				);
			}
			if (!funcFlag) {
				
				return;
			}
        }
		
		// TODO: Some issues about "const int*" / "int* const" might exist
		
		// 检查左值是否为const
        if (leftType.isConst()) {
            this.addError(`Cannot assign to const variable`, node.location);
        }

        // 检查左值
        if (!this.isLValue(node.left)) {
            this.addError(`Assignment requires lvalue`, node.location);
        }

        node.dataType = leftType;
    }
	
	// 添加成员访问支持
    visitMemberExpression(node) {
        const object = node.getChild(0);
        const member = node.getChild(1);
        const computed = node.getAttribute('computed');
		const operator = node.getAttribute('operator'); // '.' 或 '->'
        
        this.visit(object);
        const objectType = this.getExpressionType(object);
        
        if (!objectType) return;
        
        // 数组下标访问
        if (computed) {
            if (objectType.kind !== 'array' && objectType.kind !== 'pointer') {
                this.addError(`Cannot use subscript on non-array type`, node.location);
                return;
            }
            
            this.visit(member);
            const indexType = this.getExpressionType(member);
            if (indexType && indexType.name !== 'int') {
                this.addWarning(`Array index should be of type 'int'`, member.location);
            }
            
            if (objectType.kind === 'array') {
                node.dataType = objectType.pointerTo;
            } else if (objectType.kind === 'pointer') {
                node.dataType = objectType.pointerTo;
            }
            return;
        }
        
		// 结构体/联合体成员访问
		let targetType = objectType;
		
		// 处理指针操作符 '->'
		if (operator === '->') {
			if (objectType.kind !== 'pointer') {
				this.addError(`'->' operator requires pointer to struct/union`, node.location);
				return;
			}
			
			if (!objectType.pointerTo) {
				this.addError(`Cannot dereference incomplete pointer type`, node.location);
				return;
			}
			
			// 解引用指针，获取指向的类型
			targetType = objectType.pointerTo;
		}
		// 处理点操作符 '.'
		else if (operator === '.') {
			// 点操作符要求对象是结构体/联合体类型，不能是指针
			if (objectType.kind === 'pointer') {
				this.addError(`Cannot use '.' operator on pointer type, use '->' instead`, node.location);
				return;
			}
		}
		
		// 检查目标类型是否为结构体或联合体
		if (targetType.kind !== 'struct' && targetType.kind !== 'union') {
			this.addError(`Member access requires struct/union type, got '${this.typeToString(targetType)}'`, node.location);
			return;
		}
		
		
		if (member.type !== 'Identifier') {
			this.addError(`Member name must be an identifier`, member.location);
			return;
		}
		
		const memberName = member.name;
		const memberInfo = targetType.members.find(m => m.name === memberName);
		
		if (!memberInfo) {
			this.addError(`'${targetType.name}' has no member named '${memberName}'`, member.location);
			return;
		}
		
		// 记录成员信息
		node.dataType = memberInfo.type;
		node.memberInfo = memberInfo;
		
		// 设置节点属性，便于代码生成阶段使用
		node.setAttribute('memberOffset', memberInfo.offset);
		node.setAttribute('memberName', memberName);
		
		// 如果是通过指针访问，记录解引用信息
		if (operator === '->') {
			node.setAttribute('dereferenced', true);
		}
		
    }

    visitFunctionCall(node) {
        this.visit(node.callee);

        // 检查参数
        node.arguments.forEach(arg => this.visit(arg));

        // 检查函数是否存在
        if (node.callee.type !== ASTNodeType.IDENTIFIER) {
            this.addError(`Function call target must be an identifier`, node.callee.location);
            return;
        }

        const funcSymbol = this.currentScope.lookup(node.callee.name);
        if (!funcSymbol || funcSymbol.kind !== 'function') {
            this.addError(`Undeclared function '${node.callee.name}'`, node.callee.location);
            return;
        }

        // 检查参数数量
        const expectedParams = funcSymbol.type.parameters || [];
        if (node.arguments.length !== expectedParams.length) {
            this.addError(
                `Function '${node.callee.name}' expects ${expectedParams.length} arguments, but ${node.arguments.length} were provided`,
                node.location
            );
            return;
        }

        // 检查参数类型
        node.arguments.forEach((arg, index) => {
            const argType = this.getExpressionType(arg);
            const expectedType = expectedParams[index].type;
            
            if (argType && expectedType && !this.isTypeCompatible(expectedType, argType)) {
                this.addError(
                    `Argument ${index + 1} of '${node.callee.name}' expects '${expectedType}', but got '${argType}'`,
                    arg.location
                );
            }
        });

        node.dataType = funcSymbol.type.returnType;
    }

    visitBuiltinCall(node) {
		const funcSymbol = this.currentScope.lookup(node.functionName);
		if (!funcSymbol || funcSymbol.kind !== 'function') {
			this.addError(`Unknown builtin function '${node.functionName}'`, node.location);
			return;
		}

		// 检查参数
		node.arguments.forEach(arg => this.visit(arg));

		// 特殊的内建函数参数检查
		switch (node.functionName) {
			case 'set':
				if (node.arguments.length < 2) {
					this.addError(`'set' requires at least 2 arguments`, node.location);
				}
				break;
				
			case 'op':
				if (node.arguments.length < 3) {
					this.addError(`'op' requires at least 3 arguments`, node.location);
				}
				break;
				
			case 'read':
			case 'write':
				if (node.arguments.length !== 3) {
					this.addError(`'${node.functionName}' requires exactly 3 arguments`, node.location);
				}
				break;
				
			default:
				// 对于变长参数函数，跳过严格的参数数量检查
				if (!funcSymbol.type.hasVarArgs) {
					const expectedParams = funcSymbol.type.parameters || [];
					if (node.arguments.length !== expectedParams.length) {
						this.addError(
							`Function '${node.functionName}' expects ${expectedParams.length} arguments, but ${node.arguments.length} were provided`,
							node.location
						);
					}
				}
				break;
		}

		node.dataType = funcSymbol.type.returnType;
	}

	// !! Has manual changes !!
    visitIfStatement(node) {
        this.visit(node.test);
        
        // 检查条件表达式类型
        const testType = this.getExpressionType(node.test);
        if (testType && testType.name !== 'int') {
            this.addWarning(`Condition expression should be of type 'int'`, node.test.location);
        }

        this.visit(node.consequent);
        if (node.alternate) {
            this.visit(node.alternate);
        }
    }

    visitWhileStatement(node) {
        this.visit(node.test);
        
        // 检查条件表达式类型
        const testType = this.getExpressionType(node.test);
        if (testType && testType.name !== 'int') {
            this.addWarning(`Loop condition should be of type 'int'`, node.test.location);
        }

        this.visit(node.body);
    }

	// FOR LOOP DOESN'T HAVE ITS SCOPE
    visitForStatement(node) {
        // 进入新的作用域
        //const forScope = new Scope(this.currentScope, 'block');
        //this.currentScope.children.push(forScope);
        //this.currentScope = forScope;

        if (node.init) this.visit(node.init);
        if (node.test) {
            this.visit(node.test);
            const testType = this.getExpressionType(node.test);
            if (testType && testType.name !== 'int') {
                this.addWarning(`Loop condition should be of type 'int'`, node.test.location);
            }
        }
        if (node.update) this.visit(node.update);
        if (node.body) this.visit(node.body);

        // 退出作用域
        //this.currentScope = this.currentScope.parent;
    }

    visitReturnStatement(node) {
        if (node.argument) {
            this.visit(node.argument);
            
            if (this.currentFunction) {
                const returnType = this.currentFunction.type.returnType;
                const argType = this.getExpressionType(node.argument);
                
                if (returnType !== 'void' && argType && !this.isTypeCompatible(returnType, argType)) {
                    this.addError(
                        `Return type mismatch: expected '${returnType}', but got '${argType}'`,
                        node.argument.location
                    );
                }
            }
        } else if (this.currentFunction && this.currentFunction.type.returnType !== 'void') {
            this.addError(`Function must return a value`, node.location);
        }
    }

    visitCompoundStatement(node) {
        // 进入新的块作用域
        let blockScope = node.scope;
		if (!blockScope) {
			blockScope = new Scope(this.currentScope, 'block');
			this.currentScope.children.push(blockScope);
		}
        this.currentScope = blockScope;

        // 遍历块内的所有语句
        node.statements.forEach(stmt => this.visit(stmt));

        // 退出作用域
        this.currentScope = this.currentScope.parent;
    }

    // =============== 类型检查辅助方法 ===============
	getExpressionType(node) {
        if (!node) return null;

        // 如果节点已经有数据类型，直接返回
        if (node.dataType) {
            return node.dataType;
			//return this.typeTable.get(node.dataType) ?? node.dataType;
        }

        // 根据节点类型推断类型
        switch (node.type) {
            case 'Identifier':
                const symbol = this.currentScope.lookup(node.name);
                return symbol ? symbol.type : null;

            case 'MemberExpression':
                return node.dataType;

            case 'UnaryExpression':
                if (node.operator === '*') {
                    // 解引用操作
                    const argType = this.getExpressionType(node.argument);
                    if (argType && argType.kind === 'pointer') {
                        return argType.pointerTo;
                    }
                    return null;
                } else if (node.operator === '&') {
                    // 取地址操作
                    const argType = this.getExpressionType(node.argument);
                    if (argType) {
                        const ptrType = new TypeInfo('', 'pointer', 2);
                        ptrType.pointerTo = argType;
                        return ptrType;
                    }
                    return null;
                }
                // 其他一元操作符
				/*
				// 其他一元操作符
				const argType = this.getExpressionType(node.argument);
				if (argType) {
					// 对于递增递减操作，返回与参数相同的类型
					if (node.operator === '++' || node.operator === '--') {
						return argType;
					}
					// 对于其他一元操作符，返回数值类型
					return this.typeTable.get('int');
				}
				return null;
				*/
                return this.getExpressionType(node.argument);

            case 'BinaryExpression':
                // 对于指针算术等操作，需要特殊处理
                const leftType = this.getExpressionType(node.left);
                const rightType = this.getExpressionType(node.right);
                
                if (node.operator === '+' || node.operator === '-') {
                    // 指针算术
                    if (leftType && leftType.kind === 'pointer') {
                        return leftType;
                    }
                    if (rightType && rightType.kind === 'pointer') {
                        return rightType;
                    }
                }
                
                // 默认返回左操作数类型
                return leftType;
				
			case 'AssignmentExpression':
				// 赋值表达式的类型是左操作数的类型
				return this.getExpressionType(node.left);
			
			case 'CastExpression':
				// 类型转换表达式的类型是转换后的类型
				if (node.children && node.children[0]) {
					const typeNode = node.children[0];
					const typeName = this.getTypeNameFromTypeNode(typeNode);
					let baseType = this.getTypeInfo(typeName);
					
					// 处理指针转换
					const pointerDepth = node.getAttribute('pointerDepth') || 0;
					if (pointerDepth > 0) {
						let currentType = baseType;
						for (let i = 0; i < pointerDepth; i++) {
							const ptrType = new TypeInfo('', 'pointer', 2);
							ptrType.pointerTo = currentType;
							currentType = ptrType;
						}
						return currentType;
					}
					return baseType;
				}
				return null;

            case 'NumericLiteral':
                return this.typeTable.get('int');

            case 'StringLiteral':
                const charPtrType = new TypeInfo('', 'pointer', 2);
                charPtrType.pointerTo = this.typeTable.get('char');
                return charPtrType;

            case 'CharacterLiteral':
                return this.typeTable.get('char');

            case 'NullLiteral':
                return this.typeTable.get('null_t');

            default:
                return null;
        }
    }

	// Currently unused
	isValidMemberAccessOperator(operator) {
		return operator === '.' || operator === '->';
	}

    // 修改类型兼容性检查以支持指针和结构体
    isTypeCompatible(targetTypeRaw, sourceTypeRaw) {
		
		let targetType = targetTypeRaw, sourceType = sourceTypeRaw;
		// TODO: Might be sth like '*'
		if (typeof targetType === 'string') {
			targetType = this.getTypeInfo(targetType);
		}
		if (typeof sourceType === 'string') {
			sourceType = this.getTypeInfo(sourceType);
		}
		
		if (!targetType || !sourceType) return false;
        
		// Remove const qualifier for both sides:
		
		
        // 相同类型总是兼容
        if (this.typeToString(targetType) === this.typeToString(sourceType)) {
            return true;
        }
        
        // void指针可以接受任何指针类型
        if (targetType.kind === 'pointer' && sourceType.kind === 'pointer') {
            if (targetType.pointerTo && targetType.pointerTo.name === 'void') {
                return true;
            }
            if (sourceType.pointerTo && sourceType.pointerTo.name === 'void') {
                return true;
            }
            // 指针类型兼容性检查
            return this.isTypeCompatible(targetType.pointerTo, sourceType.pointerTo);
        }
        
        // 数组到指针的转换
        if (targetType.kind === 'pointer' && sourceType.kind === 'array') {
            return this.isTypeCompatible(targetType.pointerTo, sourceType.pointerTo);
        }
        
        // 数值类型之间的兼容性
        const numericTypes = ['int', 'char', 'short', 'long', 'float', 'double', 'signed', 'unsigned'];
        if (numericTypes.includes(targetType.name) && numericTypes.includes(sourceType.name)) {
            return true;
        }
        
        // null_t可以赋值给指针类型
		// TODO: Let's regard this as a feature...
        if (sourceType.name === 'null_t' && targetType.kind === 'pointer') {
            return true;
        }
        
        // 结构体/联合体类型兼容性
        if (targetType.kind === 'struct' || targetType.kind === 'union') {
            if (sourceType.kind === targetType.kind && targetType.name === sourceType.name) {
                return true;
            }
        }
        
        return false;
    }

	// ! Has manual content !
    isTypeCompatibleForOperator(operator, leftTypeRaw, rightTypeRaw) {
		
		let leftType = leftTypeRaw, rightType = rightTypeRaw;
		// TODO: Might be sth like '*'
		if (typeof leftType === 'string') {
			leftType = this.getTypeInfo(leftType);
		}
		if (typeof rightType === 'string') {
			rightType = this.getTypeInfo(rightType);
		}
		
		if (!leftType || !rightType) return false;
		
        const arithmeticOps = ['+', '-', '*', '/', '%'];
        const comparisonOps = ['==', '!=', '<', '<=', '>', '>='];
        const logicalOps = ['&&', '||'];
        const bitwiseOps = ['&', '|', '^', '<<', '>>'];

		// No operator is allowed for struct / union
		// (Manually written)
		if (leftType.kind === 'struct' || leftType.kind === 'union' || leftType.kind === 'function')
			return false;

		if (rightType.kind === 'struct' || rightType.kind === 'union' || leftType.kind === 'function')
			return false;
		// (End)
		
		// (Manually written) (They should BE NAMES)
		// (As well as pointer execution)

        // 算术运算符要求数值类型
        if (arithmeticOps.includes(operator)) {
			if (leftType.kind === 'pointer' && rightType.kind === 'pointer') {
				return this.isTypeCompatible(leftType.pointerTo, rightType.pointerTo);
			}
            return this.isNumericType(leftType.name) && this.isNumericType(rightType.name);
        }

        // 比较运算符要求兼容类型
        if (comparisonOps.includes(operator)) {
			if (leftType.kind === 'pointer' && rightType.kind === 'pointer') {
				return this.isTypeCompatible(leftType.pointerTo, rightType.pointerTo);
			}
            return this.isTypeCompatible(leftType, rightType);
        }

        // 逻辑运算符要求布尔上下文（这里简化为int类型）
        if (logicalOps.includes(operator)) {
            return leftType === 'int' && rightType === 'int';
        }

        // 位运算符要求整型
        if (bitwiseOps.includes(operator)) {
            return this.isIntegerType(leftType.name) && this.isIntegerType(rightType.name);
        }

        return true; // 其他操作符暂时宽松处理
    }
	
	// 添加类型转字符串方法
    typeToString(typeInfo) {
        if (!typeInfo) return 'unknown';
		if (typeof typeInfo === 'string') {
			return typeInfo;
		}
        
        let result = '';
        if (typeInfo.qualifiers && typeInfo.qualifiers.length > 0) {
            result += typeInfo.qualifiers.join(' ') + ' ';
        }
        
        if (typeInfo.kind === 'pointer') {
            result += this.typeToString(typeInfo.pointerTo) + '*';
        } else if (typeInfo.kind === 'array') {
            result += this.typeToString(typeInfo.pointerTo) + `[${typeInfo.arraySize || ''}]`;
        } else if (typeInfo.kind === 'function') {
			result += '{' + this.typeToString(typeInfo.functionTo.type.returnType) + ' || ';
			result += (typeInfo.functionTo.type.parameters.map(param => (this.typeToString(param.type)))).join(',') + '}';
		} else {
            result += typeInfo.name;
        }
        
        return result;
    }

	// Check for LVal beforehand
    isTypeCompatibleForUnaryOperator(operator, argTypeRaw) {
		
		let argType = argTypeRaw;
		if (typeof argType === 'string') {
			argType = this.getTypeInfo(argType);
		}
		
        const arithmeticOps = ['+', '-'];
        const logicalOps = ['!'];
        const bitwiseOps = ['~'];
        const incrementOps = ['++', '--'];

        // 算术一元运算符要求数值类型
        if (arithmeticOps.includes(operator)) {
            return this.isNumericType(argType);
        }

        // 逻辑非要求布尔上下文（简化为int）
        if (logicalOps.includes(operator)) {
            return argType === 'int';
        }

        // 位非要求整型
        if (bitwiseOps.includes(operator)) {
            return this.isIntegerType(argType);
        }

        // 递增递减要求左值且为数值类型
        if (incrementOps.includes(operator)) {
            return this.isNumericType(argType);
        }
		
		if (operator === '*') {
			return argType.kind === 'pointer';
		}

        return true;
    }

    getResultType(operator, leftType, rightType) {
        // 简化处理：大多数情况返回左操作数的类型
        // 在实际编译器中需要更精确的类型推导
        return leftType;
    }

    getUnaryResultType(operator, argType) {
        // 一元表达式通常保持操作数类型
		
		if (operator === '&') {
			// Return a pointer
			const ptrType = new TypeInfo('', 'pointer', 2);
			ptrType.pointerTo = argType;
			return ptrType;
		}
		
		if (operator === '*') {
			// Return a pointer's pointing-to
			return argType.pointerTo;
		}
		
        return argType;
    }

    // 修改isLValue以支持成员访问
    isLValue(node) {
        // 标识符是左值
        if (node.type === 'Identifier') {
            return true;
        }

        // 成员访问是左值
        if (node.type === 'MemberExpression') {
            return true;
        }

        // 解引用是左值
        if (node.type === 'UnaryExpression' && node.operator === '*') {
            return true;
        }

        // 数组下标是左值
        if (node.type === 'MemberExpression' && node.getAttribute('computed')) {
            return true;
        }

        // 其他情况暂时认为不是左值
        return false;
    }

    isNumericType(type) {
        const numericTypes = ['int', 'char', 'short', 'long', 'float', 'double', 'signed', 'unsigned'];
        return numericTypes.includes(type);
    }

    isIntegerType(type) {
        const integerTypes = ['int', 'char', 'short', 'long', 'signed', 'unsigned'];
        return integerTypes.includes(type);
    }

    // 默认访问方法
    visitDefault(node) {
        if (node.children) {
            node.children.forEach(child => this.visit(child));
        }
    }
}

// 优化器
// As what is given, this only does very simple optimizing (constant evaluation)
// Optimizer comes first?
// I MUST DEBUG THIS ON MY OWN :(

class Optimizer {
    constructor() {
        this.modified = false;
        this.errors = [];
        this.warnings = [];
        
        // 优化状态
        this.functionCallGraph = new Map();
        this.functionInfo = new Map();
        this.constants = new Map(); // 全局常量表
        this.localConstants = new Map(); // 局部常量表 {scopePath: Map<varName, value>}
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
            iteration++;
			// debug:
			console.log(iteration + ' -th optimize:');
			console.log(this);
        } while (this.modified && iteration < maxIterations);
        
        return {
            success: this.errors.length === 0,
            ast: this.ast,
            errors: this.errors,
            warnings: this.warnings,
            optimized: iteration > 1
        };
    }

    optimizationPass() {
        // 1. 收集全局信息（函数调用图等）
        this.collectGlobalInfo();
        
        // 3. 按作用域递归分析
        this.analyzeScope(this.globalScope);
		
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
        this.optimizeScope(scope);
        
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
        return this.currentScope ? this.currentScope.getPath() : 'global';
    }

    // =============== 节点分析 ===============

    analyzeNode(node, info = null) {
        if (!node) return;
        
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
                this.analyzeExpressionStatement(node, info);
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
                
            default:
                // 递归分析子节点
				if (node.declarators) {
					this.analyzeVariableDeclaration(node, info);
					break;
				}
                if (node.children) {
                    node.children.forEach(child => this.analyzeNode(child, info));
                }
                break;
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
			node.statements.forEach(child => this.analyzeNode(child, info));
		}
		if (enteredScope) {
			this.exitScope();
		}
	}
	
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
			if (info.isLoop) return false;
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
    analyzeFunction(funcNode, info = null) {
        const funcName = funcNode.name;
        this.currentFunction = funcName;
        // 记录函数信息
        if (!this.functionInfo.has(funcName)) {
            this.functionInfo.set(funcName, {
                calls: 0,
                hasSideEffects: false,
                isDefinition: funcNode.body !== null,
                isInline: funcNode.isInline || false,
                node: funcNode,
                scope: funcNode.scope
            });
        }
        
		let enteredScope = false;
		if (funcNode.scope && funcNode.scope.name !== this.currentScope.name) {
			enteredScope = true;
			this.enterScope(funcNode.scope);
		}
		
        // 分析函数体
        if (funcNode.body) {
            this.analyzeNode(funcNode.body, info);
        }
		if (enteredScope) {
			this.exitScope();
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
            
            // 初始化使用统计
            variableUses.set(declarator.name, {
                reads: 0,
                writes: 1, // 初始化算作一次写入
				canOptimize: true,	// False: already symbolized as no-optimize
                location: declarator.location,
                node: declarator
            });
            
            // 检查常量初始化
            if (declarator.initializer) {
                this.analyzeNode(declarator.initializer, info);
                
                const constValue = this.evaluateConstantExpression(declarator.initializer);
                if (constValue !== undefined) {
                    localConstants.set(declarator.name, constValue);
                    
                    // 标记符号为常量
                    const symbol = this.currentScope.lookup(declarator.name);
                    if (symbol) {
                        symbol.markAsConstant(constValue);
                    }
                }
            }
        });
    }

	// Something might be to do here:
    analyzeIdentifier(node, info = null) {
        const scopePath = this.currentScope.lookupScopeOf(node.name).getPath();
        const variableUses = this.variableUses.get(scopePath);
        
        // 记录变量读取
        const usage = variableUses.get(node.name);
        if (usage) {
            usage.reads++;
        }
        
        // 检查是否为函数调用
        const symbol = this.currentScope.lookup(node.name);
        if (symbol && symbol.kind === 'function') {
            // 函数调用统计在analyzeFunctionCall中处理
			// Debug:
			console.log('identifier being called on function: node:');
			console.log(node);
			symbol.isAddressed = true;
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
						if (symbol) {
							symbol.markAsConstant(constValue);
						}
					}
					// Manually added
					this.addWriteForCurrentScope(node.left.name, constValue, this.shouldRunEvaluation(node, info));
				}
				
			}
		}
    }

    analyzeFunctionCall(node, info = null) {
        if (node.callee.type === 'Identifier') {
            const funcName = node.callee.name;
            
            // 更新函数调用计数
            const info = this.functionInfo.get(funcName);
            if (info) {
                info.calls++;
                
                // 记录调用关系
                if (this.currentScope && this.currentScope.astNode) {
                    const callerName = this.currentFunction || 'anonymous';
                    if (!this.functionCallGraph.has(callerName)) {
                        this.functionCallGraph.set(callerName, new Set());
                    }
                    this.functionCallGraph.get(callerName).add(funcName);
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
			if (node.consequent) {
				this.analyzeNode(node.consequent, info);
				
				// 检查then分支是否有副作用
				if (this.hasSideEffects(node.consequent)) {
					node.setAttribute('consequentHasSideEffects', true);
				}
			}
			
			// 分析else分支
			if (node.alternate) {
				this.analyzeNode(node.alternate, info);
				
				// 检查else分支是否有副作用
				if (this.hasSideEffects(node.alternate)) {
					node.setAttribute('alternateHasSideEffects', true);
				}
			}
		}
	}
	
	analyzeWhileStatement(node, info = null) {
		node.setAttribute('hasSideEffects', false);
		// 分析条件表达式
		if (node.test) {
			this.analyzeNode(node.test, info);
			
			// 检查条件是否为常量
			const conditionValue = this.evaluateConstantExpression(node.test);
			
			// 记录条件信息用于优化
			if (conditionValue !== undefined) {
				node.setAttribute('constantCondition', conditionValue);
				
				if (conditionValue) {
					// 条件总是为真，是无限循环
					node.setAttribute('infiniteLoop', true);
					
				} else {
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
			const loopInfo = {
				isLoop: true,
				parentLoop: node,
				hasBreak: false,
				hasContinue: false
			};
			
			// 分析循环体
			this.analyzeNode(node.body, loopInfo);
			
			// 如果循环永远不会执行，不需要分析循环体
			if (node.getAttribute('neverExecuted')) {
				return;
			}
			
			// 检查循环体是否有副作用
			node.setAttribute('bodyHasSideEffects', this.hasSideEffects(node.body));
			
			
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
				if (this.analyzeWhileLoopForOptimization(node)) {
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
	analyzeWhileLoopForOptimization(whileNode) {
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
						
						// 尝试计算循环次数
						const iterationCount = this.calculateLoopIterations(
							initValue,
							{ comparisonOp, constValue },
							updateInfo
						);
						
						if (iterationCount !== null && iterationCount >= 0) {
							whileNode.setAttribute('iterationCount', iterationCount);
							
							if (iterationCount === 0) {
								whileNode.setAttribute('neverExecuted', true);
							} else if (iterationCount <= 100) {
								// 可以优化
								whileNode.setAttribute('canOptimize', true);
								whileNode.setAttribute('optimized', true);
							}
						}
					}
				}
			}
		}
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
			if (node.children && !updateInfo) {
				for (const child of node.children) {
					searchForUpdate(child);
					if (updateInfo) break;
				}
			}
			
			// 搜索特定字段
			if (!updateInfo) {
				const fields = ['statements', 'consequent', 'alternate', 'body', 
							  'init', 'test', 'update', 'argument', 'left', 'right'];
				
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
			this.analyzeNode(node.init, null);	// Do changes !!
			
			// 检查初始化是否有副作用
			node.setAttribute('initHasSideEffects', this.hasSideEffects(node.init));
		}
		
		// 分析条件部分
		if (node.test) {
			// ! Always make changes for initialization !
			this.analyzeNode(node.test, null);
			
			// 检查条件是否为常量
			const conditionValue = this.evaluateConstantExpression(node.test);
			
			// 记录条件信息用于优化
			if (conditionValue !== undefined) {
				node.setAttribute('constantCondition', conditionValue);
				
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
			this.analyzeNode(node.update, loopInfo);
			
			// 检查更新是否有副作用
			node.setAttribute('updateHasSideEffects', this.hasSideEffects(node.update));
		}
		
		// 分析循环体
		if (node.body) {
			// 分析循环体
			this.analyzeNode(node.body, loopInfo);// Don't do changes
			
			// 检查循环体是否有副作用
			if (this.hasSideEffects(node.body)) {
				node.setAttribute('bodyHasSideEffects', true);
			}
			
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
				forNode.setAttribute('iterationOptimized', true);
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
			for (i = 0; i < maxSimulatedIterations; i++) {
				// 分析循环体
				// This will result in an execution
				const currentValue = localConstants.get(loopVarName) || initValue;
				
				info.currentRound = i;
				info.currentLoopVar = currentValue;
				this.analyzeNode(loopNode.body, info);
				
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
				
				// 如果循环变量不再可优化，停止模拟
				const usage = variableUses.get(loopVarName);
				if (usage && !usage.canOptimize) {
					loopNode.setAttribute('maybeHasSideEffects', true);
					break;
				}
			}
			
			if (loopNode.test) {
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
		modifiedVars.forEach(varName => {
			const scopePath = this.currentScope.lookupScopeOf(varName).getPath();
			const variableUses = this.variableUses.get(scopePath);
			const usage = variableUses.get(varName);
			if (usage) {
				usage.canOptimize = false;
			}
		});
		
		// 标记循环有副作用
		loopNode.setAttribute('hasSideEffects', true);
	}

	/**
	 * 收集循环中可能被修改的变量
	 * @param {ASTNode} loopNode - 循环节点
	 * @returns {Set<string>} - 被修改的变量集合
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
						modifiedVars.add(node.left.name);
					} else {
						// Unrecognized...
						this.errors.push('Internal Error: Unrecognized variable in loop optimizer', node.left);
					}
					break;
				
				case 'BinaryExpression':
					if (node.operator.includes('=') && node.operator !== '==' && node.operator !== '!=') {
						if (node.left.type === 'Identifier') {
							modifiedVars.add(node.left.name);
						} else {
							this.errors.push('Internal Error: Unrecognized variable in loop optimizer',node.left);
						}
					}
					break;
				case 'UnaryExpression':
					if ((node.operator === '++' || node.operator === '--')) {
						if (node.argument.type === 'Identifier') {
							modifiedVars.add(node.argument.name);
						} else {
							this.errors.push('Internal Error: Unrecognized variable in loop optimizer',node.left);
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
				
				case 'CompoundStatement':
					node.statements.forEach(stmt => collectFromNode(stmt));
					break;
				case 'ForStatement':
				case 'WhileStatement':
				case 'IfStatement':
					if (node.init) collectFromNode(node.init);
					if (node.test) collectFromNode(node.test);
					if (node.update) collectFromNode(node.update);
					if (node.body) collectFromNode(node.body);
					if (node.consequent) collectFromNode(node.consequent);
					if (node.alternate) collectFromNode(node.alternate);
					break;
				case 'ReturnStatement':
					if (node.argument) collectFromNode(node.argument);
			}
			
			// 递归处理子节点
			if (node.children) {
				node.children.forEach(child => collectFromNode(child));
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
			exprStmt.expression = forNode.test;
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
		
		if (!canOptimize && !neverExecuted) return false;
		
		// 如果循环永远不会执行
		if (neverExecuted) {
			// while循环没有初始化部分，直接删除
			// 但需要保留条件的副作用（如果有）
			const conditionHasSideEffects = whileNode.getAttribute('conditionHasSideEffects');
			if (conditionHasSideEffects) {
				// 用条件表达式替换整个while循环
				const exprStmt = new ASTNode('ExpressionStatement');
				exprStmt.expression = whileNode.test;
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
				exprStmt.expression = loopNode.init;
				replacementStatements.push(exprStmt);
			}
		}
		
		// After init!
		// 检查所有修改的变量在循环结束后是否有确定的常量值
		const varFinalValues = new Map();
		for (const varName of modifiedVars) {
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
			exprStmt.expression = assignmentExpr;
			
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
		
		if (firstSideEffectIteration >= totalIterations) {
			// 所有迭代都没有副作用，可以完全优化
			// Debug:
			console.log('unexpected side effect call');
			return this.fullyOptimizeDeterministicLoop(loopNode, totalIterations);
		}
		
		// 部分优化：展开没有副作用的部分，保留有副作用的部分
		return this.partiallyUnrollLoop(loopNode, loopVariable, totalIterations, firstSideEffectIteration, modifiedVars);
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
				exprStmt.expression = assignmentExpr;
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
		const remainingIterations = totalIterations - firstSideEffect;
		
		if (remainingIterations > 0) {
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
	 */
	createModifiedForLoop(originalLoop, loopVariable, newInitValue) {
		// 复制原始循环
		const newLoop = new ASTNode('ForStatement');
		newLoop.init = originalLoop.init;
		newLoop.test = originalLoop.test;
		newLoop.update = originalLoop.update;
		newLoop.body = originalLoop.body;
		
		// 修改初始化部分：将循环变量设置为已完成的迭代后的值
		if (loopVar) {
			const initValue = this.getVariableInitialValue(loopVar);
			if (initValue !== undefined) {
				const numericLiteral = ASTBuilder.numericLiteral(newInitValue);
				const identifier = ASTBuilder.identifier(loopVariable);
				const assignmentExpr = ASTBuilder.assignmentExpression('=', identifier, numericLiteral);
				
				if (originalLoop.init.type === 'VariableDeclaration') {
					// 修改变量声明的初始值
					const newDeclarator = ASTBuilder.variableDeclarator(loopVar);
					newDeclarator.initializer = numericLiteral;
					newLoop.init.declarators = [newDeclarator];
				} else if (originalLoop.init.type === 'ExpressionStatement') {
					// To be verified
					originalLoop.addChild(assignmentExpr);
				} else {
					// 创建赋值表达式语句
					const exprStmt = new ASTNode('ExpressionStatement');
					exprStmt.expression = assignmentExpr;
					newLoop.init = exprStmt;
				}
			}
		}
		
		
		return newLoop;
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
	}

	analyzeBreakStatement(node, info = null) {
		// 标记在循环或switch中有break
		if (info && info.isLoop) {
			info.hasBreak = true;
		}
		// 也可以记录在其他结构中，如switch
	}

	analyzeContinueStatement(node, info = null) {
		// 标记在循环中有continue
		if (info && info.isLoop) {
			info.hasContinue = true;
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
    optimizeScope(scope) {
        const scopePath = scope.getPath();
        const variableUses = this.variableUses.get(scopePath) || new Map();
        const localConstants = this.localConstants.get(scopePath) || new Map();
        
        // 1. 删除无用变量
        this.removeUnusedVariablesInScope(scope, variableUses);
        
        // 2. 常量传播
        this.constantPropagationInScope(scope, localConstants);
        
        // 3. 控制流优化（使用分析阶段收集的信息）
		//this.optimizeControlFlowInScope(scope);
		
		// 4. 死代码消除
		this.deadCodeEliminationInScope(scope);
    }

    removeUnusedVariablesInScope(scope, variableUses) {
        const symbols = scope.getAllSymbols();
        
        symbols.forEach(symbol => {
            if (symbol.kind !== 'variable') return;
            
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
        
        // 遍历作用域内的所有节点，传播常量
        this.traverseAndReplaceConstants(scope.astNode, localConstants);
    }

    traverseAndReplaceConstants(node, constants) {
        if (!node) return;
        
        switch (node.type) {
            case 'Identifier':
                const constValue = constants.get(node.name);
                if (constValue !== undefined) {
                    // 替换为常量值
                    const replacement = ASTBuilder.numericLiteral(constValue);
                    replacement.location = node.location;
                    this.replaceNode(node, replacement);
                    this.modified = true;
                }
                break;
                
            case 'BinaryExpression':
                // 常量折叠
                const leftValue = this.evaluateConstantExpression(node.left);
                const rightValue = this.evaluateConstantExpression(node.right);
                
                if (leftValue !== undefined && rightValue !== undefined) {
                    const result = this.evaluateBinaryOperation(node.operator, leftValue, rightValue);
                    if (result !== undefined) {
                        const replacement = ASTBuilder.numericLiteral(result);
                        replacement.location = node.location;
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
                        const replacement = ASTBuilder.numericLiteral(result);
                        replacement.location = node.location;
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
                }
                break;
			/*	
			case 'AssignmentExpression':
				// It is possible that assign value is given a constant
				const rightValue = this.evaluateConstantExpression(node.right);
				if (rightValue !== undefined && !rightValue) {
					// This is a constant, add it into constant set
				} else {
					// Consider removal
					
				}
				break;
			*/
			case 'CompoundStatement':
				node.statements.forEach(stmt => this.traverseAndReplaceConstants(stmt, constants));
        }
        
        // 递归处理子节点
        if (node.children) {
            node.children.forEach(child => this.traverseAndReplaceConstants(child, constants));
        }
        
        // 处理特殊节点的子节点
        switch (node.type) {
            case 'IfStatement':
                if (node.test) this.traverseAndReplaceConstants(node.test, constants);
                if (node.consequent) this.traverseAndReplaceConstants(node.consequent, constants);
                if (node.alternate) this.traverseAndReplaceConstants(node.alternate, constants);
                break;
                
            case 'WhileStatement':
                if (node.test) this.traverseAndReplaceConstants(node.test, constants);
                if (node.body) this.traverseAndReplaceConstants(node.body, constants);
                break;
                
            case 'ForStatement':
                if (node.init) this.traverseAndReplaceConstants(node.init, constants);
                if (node.test) this.traverseAndReplaceConstants(node.test, constants);
                if (node.update) this.traverseAndReplaceConstants(node.update, constants);
                if (node.body) this.traverseAndReplaceConstants(node.body, constants);
                break;
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
            if (info.calls === 1 || info.isInline || info.size <= 3) {
                inlineCandidates.push(funcName);
            }
        });
        
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

    evaluateConstantExpression(node) {
        if (!node) return undefined;
        
        switch (node.type) {
            case 'NumericLiteral':
                return node.value;
            case 'Identifier':
				// Try evaluating in current environment
				const scopePath = this.currentScope.lookupScopeOf(node.name).getPath();
				const localConstantRef = this.localConstants.get(scopePath).get(node.name);
				return localConstantRef ?? this.constants.get(node.name);
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
            case '<<': case '<<=': return left << right;
            case '>>': case '>>=': return left >> right;
            case '==': return left == right ? 1 : 0;
            case '!=': return left != right ? 1 : 0;
            case '<': return left < right ? 1 : 0;
            case '>': return left > right ? 1 : 0;
            case '<=': return left <= right ? 1 : 0;
            case '>=': return left >= right ? 1 : 0;
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
    hasSideEffects(node) {
        if (!node) return false;
        
        // 检查节点是否有副作用
        switch (node.type) {
			case 'BinaryExpression':
            case 'AssignmentExpression':
				return (this.hasSideEffects(node.left)) || (this.hasSideEffects(node.right));
				// TODO: Double-check logic here
				break;
			case 'FunctionCall':
            case 'BuiltinCall':
			case 'AsmStatement':
				// There won't be cross-function optimizing
				return true;
				break;
            case 'UnaryExpression':
				// ++ and -- itself actually have no side effect.
				// side effect of '&' depends
				if (node.operator === '&' || node.operator === '*') {
					return this.hasSideEffects(node.argument);
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
				return node.argument ? this.hasSideEffects(node.argument) : false;
			case 'IfStatement':
			case 'WhileStatement':
			case 'ForStatement':
				// 控制语句可能有副作用
				return (node.test && this.hasSideEffects(node.test)) ||
					   (node.consequent && this.hasSideEffects(node.consequent)) ||
					   (node.alternate && this.hasSideEffects(node.alternate)) ||
					   (node.body && this.hasSideEffects(node.body)) ||
					   (node.init && this.hasSideEffects(node.init)) ||
					   (node.update && this.hasSideEffects(node.update));
			case 'CompoundStatement':
				// 如果其任何子语句有副作用，复合语句有副作用
				if (node.statements) {
					for (const stmt of node.statements) {
						if (this.hasSideEffects(stmt)) {
							return true;
						}
					}
				}
				break;
			case 'StructMemberNode':
			case 'PointerTypeNode':
				// Not considering struct members
				return true;
			case 'Identifier':
				const scopePath = this.currentScope.lookupScopeOf(node.name).getPath();
				const variableUses = this.variableUses.get(scopePath);
				const variableState = variableUses.get(node.name);
				if (variableState && (!variableState.canOptimize)) {
					return true;
				}
				break;
        }
        
        // 递归检查子节点
        if (node.children) {
            for (const child of node.children) {
                if (this.hasSideEffects(child)) {
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
				if (parent.children) {
					const childIndex = parent.children.indexOf(oldNode);
					if (childIndex !== -1) {
						if (newNode) {
							newNode.parent = parent;
							parent.children[childIndex] = newNode;
						} else {
							parent.children.splice(childIndex, 1);
						}
						return;
					}
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
            }
            
            if (node.children) {
                node.children.forEach(child => countStatements(child));
            }
        };
        
        countStatements(funcNode.body);
        return size;
    }

    shouldInlineFunction(funcName) {
        const info = this.functionInfo.get(funcName);
        if (!info || !info.node.body) return false;
        
        // 检查递归调用
        const calledFunctions = this.functionCallGraph.get(funcName) || new Set();
        if (calledFunctions.has(funcName)) return false; // 递归函数不内联
        
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
			this.globalSymbols.removeSymbol(funcName);	// Manually added
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
			'declarators', 'arguments', 'callee'
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
	 * 在特定调用点内联函数
	 */
	inlineFunctionAtSite(funcNode, callNode, parentNode, context) {
		// 检查是否适合内联
		if (!this.isSuitableForInlining(funcNode, callNode, context)) {
			return;
		}
		
		// 复制函数体
		const clonedBody = this.cloneAST(funcNode.body);
		if (!clonedBody) return;
		
		// 创建参数映射
		const paramMap = new Map();
		funcNode.parameters.forEach((param, index) => {
			if (index < callNode.arguments.length && param.name) {
				paramMap.set(param.name, callNode.arguments[index]);
			}
		});
		
		// 替换参数
		this.replaceParametersInAST(clonedBody, paramMap);
		
		// 处理返回语句
		const returnValue = this.extractAndReplaceReturns(clonedBody, parentNode, callNode);
		
		if (returnValue !== undefined) {
			// 函数有返回值，用返回值替换调用
			this.replaceNode(callNode, returnValue);
		} else if (parentNode.type === 'ExpressionStatement') {
			// 没有返回值的函数调用，用函数体替换整个表达式语句
			if (clonedBody.type === 'CompoundStatement') {
				// 将复合语句的内容插入到父节点中
				const parentOfParent = parentNode.parent;
				if (parentOfParent && parentOfParent.statements) {
					const index = parentOfParent.statements.indexOf(parentNode);
					if (index !== -1) {
						// 删除原表达式语句，插入函数体内容
						parentOfParent.statements.splice(index, 1);
						clonedBody.statements.forEach((stmt, i) => {
							parentOfParent.statements.splice(index + i, 0, stmt);
							stmt.parent = parentOfParent;
						});
					}
				}
			}
		}
		
		// 更新调用计数
		const funcInfo = this.functionInfo.get(funcNode.name);
		if (funcInfo) {
			funcInfo.calls--;
		}
		
		this.modified = true;
		this.warnings.push(`Inlined function '${funcNode.name}'`);
	}

	/**
	 * 检查函数是否适合内联
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
		if (functionSize > 20) { // 阈值，可根据需要调整
			return false;
		}
		
		// 检查是否有副作用
		if (this.functionHasSideEffects(funcNode)) {
			return false;
		}
		
		return true;
	}

	/**
	 * 替换AST中的参数引用
	 */
	replaceParametersInAST(node, paramMap) {
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
			'declarators', 'arguments', 'callee'
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
		let hasSideEffects = false;
		
		const checkNode = (node) => {
			if (!node) return;
			
			switch (node.type) {
				case 'FunctionCall':
				case 'BuiltinCall':
				case 'AssignmentExpression':
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
				'declarators', 'arguments', 'callee'
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
				if (node.children) {
					node.children.forEach(child => clone.addChild(this.cloneAST(child)));
				}
				break;
		}
		
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
			case 'ExpressionStatement':
				// 表达式语句的副作用就是表达式本身
				return stmt.expression ? stmt.expression : null;
				
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
								exprStmt.expression = preserved;
								sideEffectStatements.push(exprStmt);
							}
						}
					});
				}
				
				if (sideEffectStatements.length === 0) {
					return null;
				} else if (sideEffectStatements.length === 1) {
					return sideEffectStatements[0].expression || sideEffectStatements[0];
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
					exprStmt.expression = forStmt.test;
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
					updateStmt.expression = forStmt.update;
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


// !! NOTICE: SINCE THIS, THERE ARE NOT-SYNCED CHANGES !!
// (On campus)

// 内建函数处理器
// This should be written WITH code generator
// (Actually, there should be even more... / merged with Code Generator...)
class BuiltinFunctionHandler {
    static handleDraw(args) {
        // 处理draw函数调用
    }
    
    static handleControl(args) {
        // 处理control函数调用
    }
    
    static handleAsm(args) {
        // 处理内联汇编
    }
    
    // 其他内建函数...
}

// 代码生成器
class CodeGenerator extends CompilationPhase {
    constructor(compiler) {
        super(compiler);
        this.outputCode = [];
        this.currentMemoryBlock = 'cell1';
        this.currentMemoryOffset = 0;
    }
    
    generate(ast) {
        // 将AST转换为目标代码
        // 返回目标代码字符串数组
    }
    
    // 内存管理方法
	// (-- To prompt: it is about assigned MEMORY THAT WE USE)
    allocateMemory(variable, size = 1) {
        // 在内存块中分配空间
    }
    
    freeMemory(variable) {
        // 释放内存空间
    }
    
    // 变量管理
    getTempVariable() {
        // 获取临时变量名
    }
    
    // 标签管理
    generateLabel(prefix = 'label') {
        // 生成唯一标签
    }
}


// 主编译器接口
class Compiler {
    constructor() {
        this.lexer = new Lexer(this);
        this.parser = new Parser(this);
        this.semanticAnalyzer = new SemanticAnalyzer(this);
        this.optimizer = new Optimizer(this);
        this.codeGenerator = new CodeGenerator(this);
    }
    
    compile(sourceCode) {
        try {
            // 编译管道
            const tokens = this.lexer.tokenize(sourceCode);
            const ast = this.parser.parse(tokens);
            const analyzedAst = this.semanticAnalyzer.analyze(ast);
            const optimizedAst = this.optimizer.optimize(analyzedAst);
            const targetCode = this.codeGenerator.generate(optimizedAst);
            
            return {
                success: true,
                code: targetCode,
                errors: this.getAllErrors(),
                warnings: this.getAllWarnings()
            };
        } catch (error) {
            return {
                success: false,
                error: error.message,
                errors: this.getAllErrors(),
                warnings: this.getAllWarnings()
            };
        }
    }
    
    getAllErrors() {
        // 收集所有阶段的错误
    }
    
    getAllWarnings() {
        // 收集所有阶段的警告
    }
}

// 导出主要接口
// Not done now
// NO LONGER USED (1 Dec)
if (false) {
	module.exports = {
		Compiler,
		C89ToMindustryCompiler,
		BuiltinFunctionHandler,			/* From then on: merged in 2nd conv (AST Tree). */
		ASTNodeType,
		ASTNode,
		ProgramNode,
		FunctionDeclarationNode,
		FunctionCallNode,
		BuiltinCallNode,
		VariableDeclarationNode,
		VariableDeclaratorNode,
		BinaryExpressionNode,
		UnaryExpressionNode,
		AssignmentExpressionNode,
		LogicalExpressionNode,
		ConditionalExpressionNode,
		IfStatementNode,
		WhileStatementNode,
		ForStatementNode,
		ReturnStatementNode,
		CompoundStatementNode,
		IdentifierNode,
		NumericLiteralNode,
		StringLiteralNode,
		CharacterLiteralNode,
		NullLiteralNode,
		TypeSpecifierNode,
		AsmStatementNode,
		ASTBuilder,
		ASTVisitorm,					/* From then on: merged in 3rd conv (Lexer) */
		TokenType,
		Token,
		Lexer,
		KEYWORDS,
		SPECIAL_INSTRUCTIONS,
		OPERATORS,
		PUNCTUATORS,
		Parser,							/* merged in 4nd conv (Parser) */
		SemanticAnalyzer,
		SymbolEntry,
		Scope,
		Optimizer
	};
}
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
    BUILTIN_CALL: 'BuiltinCall'
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
class TypeSpecifierNode extends ASTNode {
    constructor(typeName) {
        super(ASTNodeType.TYPE_SPECIFIER);
        this.typeName = typeName;
        this.qualifiers = []; // 类型限定符（const, volatile）
        this.pointerDepth = 0; // 指针深度（向后兼容）
        this.pointerQualifiers = []; // 指针限定符（向后兼容）
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
        this.structDefinition = null; // 如果是结构体/联合体类型定义
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

        // 解析类型说明符
        const baseType = this.parseTypeSpecifier();
        if (!baseType) {
            this.addError('Expected type specifier after typedef');
            return null;
        }

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
                declarators.push(typeDeclarator);
            }
        } while (this.matchToken(TokenType.COMMA) && this.consumeToken());

        this.expectToken(TokenType.SEMICOLON);

        const typedefDecl = ASTBuilder.typedefDeclaration(baseType, declarators);
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
        return member;
    }

	// (This function has manual changes!!!)
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

        for (const typeToken of typeTokens) {
            if (this.matchToken(typeToken)) {
                const token = this.consumeToken();
                const typeNode = ASTBuilder.typeSpecifier(token.value);
                typeNode.setAttribute('location', token.location);
                typeNode.setAttribute('qualifiers', qualifiers);
                
                // 处理struct/union/enum类型
                if (token.type === TokenType.STRUCT || token.type === TokenType.UNION) {
                    this.parseStructOrUnionType(typeNode);
                } else if (token.type === TokenType.ENUM) {
                    this.parseEnumType(typeNode);
                }
                
                return typeNode;
            }
        }

        // 特殊类型：device 和 null_t
        if (this.matchToken(TokenType.IDENTIFIER)) {
            const token = this.getCurrentToken();
            if (token.value === 'device' || token.value === 'null_t') {
                this.consumeToken();
                const typeNode = ASTBuilder.typeSpecifier(token.value);
                typeNode.setAttribute('location', token.location);
                typeNode.setAttribute('qualifiers', qualifiers);
                return typeNode;
            }
            
            // 可能是通过typedef定义的类型别名
            const typeNode = ASTBuilder.typeSpecifier(token.value);
            typeNode.setAttribute('location', token.location);
            typeNode.setAttribute('qualifiers', qualifiers);
            typeNode.setAttribute('isTypedef', true);
			typeNode.setAttribute('isCustomType', true);
            this.consumeToken();
            return typeNode;
        }

        this.addError('Expected type specifier');
        return null;
    }

	// 修改函数参数解析
    parseParameter() {
        // 解析类型说明符
        const paramType = this.parseTypeSpecifier();
        if (!paramType) return null;
        
        // 解析声明符
        const declarator = this.parseDeclarator(true);
        if (!declarator) return null;
        
        return {
            type: paramType,
            name: declarator.name,
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
            const nameToken = this.expectToken(TokenType.IDENTIFIER);
            if (!nameToken) {
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
        do {
            const declarator = this.parseVariableDeclarator();
            if (declarator) {
                declarators.push(declarator);
            }
        } while (this.matchToken(TokenType.COMMA) && this.consumeToken());

        this.expectToken(TokenType.SEMICOLON);

        const varDecl = ASTBuilder.variableDeclaration(type, declarators);
        varDecl.location = type.location;
        varDecl.isStructOrUnion = true;
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
    parseFunctionDeclaration() {
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
        
        // 解析函数参数
        if (declarator.functionParams) {
            functionDecl.parameters = declarator.functionParams;
        } else {
            // 没有参数列表，使用空的参数
            functionDecl.parameters = [];
        }

        // 解析函数体
        if (this.matchToken(TokenType.SEMICOLON)) {
            // 函数声明，没有函数体
            this.consumeToken();
        } else {
            // 函数定义，有函数体
            functionDecl.body = this.parseCompoundStatement();
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
        const type = this.parseTypeSpecifier();
        if (!type) return null;

        const declarators = [];
        do {
            const declarator = this.parseDeclarator(false);
            if (!declarator || !declarator.name) {
				/*
                this.addError('Expected variable name in declaration');
                break;
				*/
				return null;
            }
            
            // 创建变量声明符节点
            const varDeclarator = ASTBuilder.variableDeclarator(declarator.name);
            varDeclarator.location = declarator.location;
            varDeclarator.pointerDepth = declarator.pointerDepth;
            varDeclarator.pointerQualifiers = declarator.pointerQualifiers;
            varDeclarator.arrayDimensions = declarator.arrayDimensions;
            
            // 处理初始化
            if (this.matchToken(TokenType.ASSIGN)) {
                this.consumeToken();
                varDeclarator.initializer = this.parseExpression();
            }
            
            declarators.push(varDeclarator);
        } while (this.matchToken(TokenType.COMMA) && this.consumeToken());

        this.expectToken(TokenType.SEMICOLON);

        const varDecl = ASTBuilder.variableDeclaration(type, declarators);
        varDecl.location = type.location;
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
            
            // 检查是否有更多声明符
            while (this.matchToken(TokenType.COMMA) && !hadError) {
                this.consumeToken();
                const declarator = this.parseDeclarator(false);
                if (!declarator || !declarator.name) {
                    break;
                }
                declarators.push(declarator);
            }
            
            // 检查分号
            if (!this.matchToken(TokenType.SEMICOLON) || hadError) {
                return null;
            }
            
            // 成功解析，创建变量声明节点
            const varDecl = ASTBuilder.variableDeclaration(type, declarators);
            varDecl.location = type.location;
            
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
            binaryExpr.location = left.location;
            left = binaryExpr;
        }

        return left;
    }

    parseMultiplicativeExpression() {
        let left = this.parseUnaryExpression();
        if (!left) return null;

        while (
            this.matchToken(TokenType.MULTIPLY) ||
            this.matchToken(TokenType.DIVIDE) ||
            this.matchToken(TokenType.MODULO)
        ) {
            const operatorToken = this.consumeToken();
            const right = this.parseUnaryExpression();
            
            if (!right) {
                this.addError('Expected expression after multiplicative operator');
                break;
            }

            const binaryExpr = ASTBuilder.binaryExpression(operatorToken.value, left, right);
            binaryExpr.location = left.location;
            left = binaryExpr;
        }

        return left;
    }
	
	// 添加parseCastExpression方法处理类型转换
    parseCastExpression() {
        // 检查是否可能是类型转换
        if (this.matchToken(TokenType.LEFT_PAREN)) {
            const startIndex = this.currentTokenIndex;
            
            // 尝试解析类型说明符
            this.consumeToken(); // 跳过 '('
            const type = this.parseTypeSpecifier();
            
            if (type) {
                // 检查是否有指针声明符
                let pointerDepth = 0;
                while (this.matchToken(TokenType.MULTIPLY)) {
                    this.consumeToken();
                    pointerDepth++;
                }
                
                // 检查是否有右括号
                if (this.matchToken(TokenType.RIGHT_PAREN)) {
                    this.consumeToken(); // 跳过 ')'
                    
                    // 解析被转换的表达式
                    const expression = this.parseCastExpression();
                    if (expression) {
                        // 创建类型转换节点
                        const castExpr = new ASTNode('CastExpression');
                        castExpr.addChild(type);
                        castExpr.setAttribute('pointerDepth', pointerDepth);
                        castExpr.addChild(expression);
                        castExpr.location = type.location;
                        return castExpr;
                    }
                }
            }
            
            // 不是类型转换，回退
            this.currentTokenIndex = startIndex;
        }
        
        return this.parseUnaryExpression();
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
                expression = memberExpr;
            }
            // 后置递增/递减
            else if (this.matchToken(TokenType.INCREMENT) || this.matchToken(TokenType.DECREMENT)) {
                const operatorToken = this.consumeToken();
                const unaryExpr = ASTBuilder.unaryExpression(operatorToken.value, expression);
                unaryExpr.prefix = false;
                unaryExpr.location = expression.location;
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
        this.kind = kind; // 'variable', 'function', 'parameter', 'type'
        this.location = location;
        this.initialized = false;
        this.used = false;
        this.memoryLocation = null;
        this.isGlobal = false;
		this.isRegister = false; // 标记是否为register变量
        this.isAddressed = false; // 标记是否被取地址
    }
}

// 作用域类
class Scope {
    constructor(parent = null, type = 'block') {
        this.parent = parent;
        this.type = type; // 'global', 'function', 'block'
        this.symbols = new Map();
        this.children = [];
    }

    addSymbol(symbol) {
        this.symbols.set(symbol.name, symbol);
    }

    lookup(name) {
        return this.symbols.get(name) || (this.parent ? this.parent.lookup(name) : null);
    }

    lookupCurrent(name) {
        return this.symbols.get(name) || null;
    }
}

// 首先，添加一些辅助类
class TypeInfo {
    constructor(name, kind, size = 1, members = null) {
        this.name = name; // 类型名称
        this.kind = kind; // 'basic', 'struct', 'union', 'pointer', 'array', 'function', 'device', 'null'
        this.size = size; // 类型大小（按1字节对齐）
        this.members = members || []; // 结构体/联合体成员
        this.pointerTo = null; // 对于指针类型，指向的类型
        this.arraySize = null; // 对于数组类型，数组大小
        this.qualifiers = []; // 类型限定符（const, volatile）
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
        
		// Cannot do initialization in construction !!!
    }

    analyze(ast) {
        this.errors = [];
        this.warnings = [];
        this.currentScope = new Scope(null, 'global');
        this.currentFunction = null;
		
		// 初始化内建类型
        this.initializeBuiltinTypes();
        this.initializeBuiltinFunctions();
		
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
        const voidPtrType = new TypeInfo('void*', 'pointer', 1);
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
		
		// I say no
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
                const ptrType = new TypeInfo('', 'pointer', 1);
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
	
	getTypeInfo(typeName) {
        // 从类型表获取类型信息
        if (this.typeTable.has(typeName)) {
            return this.typeTable.get(typeName);
        }
        
        // 检查是否是结构体/联合体
        if (this.structTable.has(typeName)) {
            return this.structTable.get(typeName);
        }
        
        return null;
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
                returnType: node.returnType.typeName,
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

        this.currentScope.addSymbol(funcSymbol);
        this.currentFunction = funcSymbol;

        // 进入函数作用域
        const functionScope = new Scope(this.currentScope, 'function');
        this.currentScope.children.push(functionScope);
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
                    const ptrType = new TypeInfo('', 'pointer', 1);
                    ptrType.pointerTo = currentType;
                    ptrType.qualifiers = declarator.pointerQualifiers[i] || [];
                    currentType = ptrType;
                }
                finalType = currentType;
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
    getTypeNameFromTypeNode(typeNode) {
        if (!typeNode) return null;
        
		// 处理结构体/联合体类型
        if (typeNode.type === 'TypeSpecifier' && 
            (typeNode.typeName === 'struct' || typeNode.typeName === 'union')) {
            const structName = typeNode.getAttribute('structOrUnionName');
            return structName || typeNode.typeName;
        }
		
        // 如果是TypeSpecifier节点
        if (typeNode.typeName) {
            return typeNode.typeName;
        }
        
        // 如果是标识符节点（处理typedef定义的类型）
        if (typeNode.type === 'Identifier') {
            return typeNode.name;
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
            const ptrType = new TypeInfo('', 'pointer', 1);
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
        const rightType = this.getExpressionType(node.right);

        if (!leftType || !rightType) {
            return;
        }

        // 检查赋值兼容性
        if (!this.isTypeCompatible(leftType, rightType)) {
            this.addError(
                `Cannot assign '${rightType}' to '${leftType}'`,
                node.location
            );
            return;
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

    visitForStatement(node) {
        // 进入新的作用域
        const forScope = new Scope(this.currentScope, 'block');
        this.currentScope.children.push(forScope);
        this.currentScope = forScope;

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
        this.currentScope = this.currentScope.parent;
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
        const blockScope = new Scope(this.currentScope, 'block');
        this.currentScope.children.push(blockScope);
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
                        const ptrType = new TypeInfo('', 'pointer', 1);
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
							const ptrType = new TypeInfo('', 'pointer', 1);
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
                const charPtrType = new TypeInfo('', 'pointer', 1);
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
    isTypeCompatible(targetType, sourceType) {
        if (!targetType || !sourceType) return false;
        
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
    isTypeCompatibleForOperator(operator, leftType, rightType) {
        const arithmeticOps = ['+', '-', '*', '/', '%'];
        const comparisonOps = ['==', '!=', '<', '<=', '>', '>='];
        const logicalOps = ['&&', '||'];
        const bitwiseOps = ['&', '|', '^', '<<', '>>'];

		// No operator is allowed for struct / union
		// (Manually written)
		if (leftType === 'struct' || leftType === 'union')
			return false;

		if (rightType === 'struct' || rightType === 'union')
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
        
        let result = '';
        if (typeInfo.qualifiers && typeInfo.qualifiers.length > 0) {
            result += typeInfo.qualifiers.join(' ') + ' ';
        }
        
        if (typeInfo.kind === 'pointer') {
            result += this.typeToString(typeInfo.pointerTo) + '*';
        } else if (typeInfo.kind === 'array') {
            result += this.typeToString(typeInfo.pointerTo) + `[${typeInfo.arraySize || ''}]`;
        } else {
            result += typeInfo.name;
        }
        
        return result;
    }

    isTypeCompatibleForUnaryOperator(operator, argType) {
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

        return true;
    }

    getResultType(operator, leftType, rightType) {
        // 简化处理：大多数情况返回左操作数的类型
        // 在实际编译器中需要更精确的类型推导
        return leftType;
    }

    getUnaryResultType(operator, argType) {
        // 一元表达式通常保持操作数类型
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
        this.optimizedAst = null;
        this.modified = false;
        this.errors = [];
        this.warnings = [];
        
        // 优化状态
        this.functionCallGraph = new Map(); // 函数调用图 {functionName: Set<calledFunctions>}
        this.functionInfo = new Map(); // 函数信息 {functionName: {calls: number, hasSideEffects: boolean}}
        this.constants = new Map(); // 常量映射 {variableKey: value}
        this.variableUses = new Map(); // 变量使用统计 {variableKey: {reads: number, writes: number}}
        this.currentFunction = null;
        this.currentScope = null;
    }

	// This can be improved only when we have the referrer
    optimize(analysisResult, currentAst) {
        this.optimizedAst = currentAst;
        this.symbolTable = analysisResult.symbolTable;
        this.typeTable = analysisResult.typeTable;
        this.structTable = analysisResult.structTable;
        this.typedefTable = analysisResult.typedefTable;
        this.modified = false;
        this.errors = [];
        this.warnings = [];

        // 进行多轮优化直到不再变化
        let iterations = 0;
        const maxIterations = 5;
        
        do {
            this.modified = false;
            this.optimizationPass();
            iterations++;
        } while (this.modified && iterations < maxIterations);

        return {
            success: this.errors.length === 0,
            ast: this.optimizedAst,
            errors: this.errors,
            warnings: this.warnings,
            optimized: iterations > 1
        };
    }

    optimizationPass() {
        // 收集函数信息
        this.collectFunctionInfo();
        
        // 构建调用图
        this.buildCallGraph();
        
        // 删除无用函数
        this.removeUnusedFunctions();
        
        // 删除无用变量
        this.removeUnusedVariables();
        
        // 常量传播和折叠
        this.constantPropagation();
        
        // 函数内联
        this.inlineFunctions();
    }

    // =============== 函数信息收集 ===============

    collectFunctionInfo() {
        this.functionCallGraph.clear();
        this.functionInfo.clear();
        
        // 遍历AST收集函数信息
        this.traverseAST(this.optimizedAst, {
            FunctionDeclaration: (node) => {
                const funcName = node.name;
                this.functionInfo.set(funcName, {
                    calls: 0,
                    hasSideEffects: false,
                    isDefinition: node.body !== null,
                    isInline: node.isInline || false,
                    node: node
                });
                this.functionCallGraph.set(funcName, new Set());
            },
            FunctionCall: (node) => {
                if (node.callee.type === 'Identifier') {
                    const funcName = node.callee.name;
                    const info = this.functionInfo.get(funcName);
                    if (info) {
                        info.calls++;
                    }
                    
                    // 记录调用关系
                    if (this.currentFunction) {
                        const calls = this.functionCallGraph.get(this.currentFunction) || new Set();
                        calls.add(funcName);
                        this.functionCallGraph.set(this.currentFunction, calls);
                    }
                }
            },
            BuiltinCall: (node) => {
                // 内建函数调用被认为有副作用
                if (this.currentFunction) {
                    const info = this.functionInfo.get(this.currentFunction);
                    if (info) {
                        info.hasSideEffects = true;
                    }
                }
            }
        }, {
            enterFunction: (node) => {
                this.currentFunction = node.name;
            },
            exitFunction: () => {
                this.currentFunction = null;
            }
        });
    }

    // =============== 调用图构建 ===============

    buildCallGraph() {
        // 已经在上面的collectFunctionInfo中构建了调用图
        // 这里可以添加额外的调用图分析
    }

    // =============== 无用函数删除 ===============

    removeUnusedFunctions() {
        const usedFunctions = new Set();
        
        // 从main函数开始标记使用的函数
        const markUsed = (funcName) => {
            if (usedFunctions.has(funcName)) return;
            
            usedFunctions.add(funcName);
            const calledFunctions = this.functionCallGraph.get(funcName) || new Set();
            for (const called of calledFunctions) {
                markUsed(called);
            }
        };
        
        // 从main函数开始
        markUsed('main');
        
        // 删除未使用的函数
        if (this.optimizedAst.functions) {
            const originalLength = this.optimizedAst.functions.length;
            this.optimizedAst.functions = this.optimizedAst.functions.filter(func => {
                const funcName = func.name;
                const isUsed = usedFunctions.has(funcName);
                
                // 保留main函数和inline函数（即使未使用）
                if (funcName === 'main' || func.isInline) {
                    return true;
                }
                
                if (!isUsed && func.isDefinition) {
                    this.modified = true;
                    this.warnings.push(`Removing unused function '${funcName}'`);
                    return false;
                }
                
                return true;
            });
            
            if (this.optimizedAst.functions.length !== originalLength) {
                this.modified = true;
            }
        }
    }

    // =============== 无用变量删除 ===============

    removeUnusedVariables() {
        this.variableUses.clear();
        
        // 收集变量使用信息
        this.traverseAST(this.optimizedAst, {
            VariableDeclaration: (node) => {
                node.declarators.forEach(declarator => {
                    if (declarator.name) {
                        const varKey = this.getVariableKey(declarator.name);
                        if (!this.variableUses.has(varKey)) {
                            this.variableUses.set(varKey, { reads: 0, writes: 0 });
                        }
                    }
                });
            },
            Identifier: (node) => {
                if (this.isVariable(node.name)) {
                    const varKey = this.getVariableKey(node.name);
                    const usage = this.variableUses.get(varKey);
                    if (usage) {
                        usage.reads++;
                    }
                }
            },
            AssignmentExpression: (node) => {
                if (node.left.type === 'Identifier') {
                    const varKey = this.getVariableKey(node.left.name);
                    const usage = this.variableUses.get(varKey);
                    if (usage) {
                        usage.writes++;
                    }
                }
            }
        });
        
        // 删除未使用的变量声明
        this.traverseAST(this.optimizedAst, {
            VariableDeclaration: (node) => {
                const usefulDeclarators = [];
                
                for (const declarator of node.declarators) {
                    if (!declarator.name) continue;
                    
                    const varKey = this.getVariableKey(declarator.name);
                    const usage = this.variableUses.get(varKey);
                    
                    // 保留有初始化副作用或实际使用的变量
                    if (usage && (usage.reads > 0 || usage.writes > 0)) {
                        usefulDeclarators.push(declarator);
                    } else if (declarator.initializer && this.hasSideEffects(declarator.initializer)) {
                        // 有副作用的初始化表达式需要保留
                        usefulDeclarators.push(declarator);
                    } else {
                        this.modified = true;
                    }
                }
                
                if (usefulDeclarators.length === 0) {
                    // 整个声明都是无用的
                    this.replaceNode(node, null);
                } else if (usefulDeclarators.length !== node.declarators.length) {
                    node.declarators = usefulDeclarators;
                    this.modified = true;
                }
            }
        });
    }

    // =============== 常量传播 ===============

    constantPropagation() {
        this.constants.clear();
        
        // 收集常量
        this.traverseAST(this.optimizedAst, {
            VariableDeclaration: (node) => {
                node.declarators.forEach(declarator => {
                    if (declarator.name && declarator.initializer) {
                        const value = this.evaluateConstantExpression(declarator.initializer);
                        if (value !== undefined) {
                            const varKey = this.getVariableKey(declarator.name);
                            this.constants.set(varKey, value);
                        }
                    }
                });
            },
            AssignmentExpression: (node) => {
                if (node.left.type === 'Identifier') {
                    const varKey = this.getVariableKey(node.left.name);
                    const value = this.evaluateConstantExpression(node.right);
                    if (value !== undefined) {
                        this.constants.set(varKey, value);
                    } else {
                        // 非常量赋值会改变变量值
                        this.constants.delete(varKey);
                    }
                }
            }
        }, {
            enterFunction: (node) => {
                this.currentFunction = node.name;
            },
            exitFunction: () => {
                this.currentFunction = null;
                // 清除函数内的常量
                this.constants.forEach((value, key) => {
                    if (key.startsWith(this.currentFunction + '.')) {
                        this.constants.delete(key);
                    }
                });
            }
        });
        
        // 应用常量传播
        this.traverseAST(this.optimizedAst, {
            Identifier: (node) => {
                const varKey = this.getVariableKey(node.name);
                const constantValue = this.constants.get(varKey);
                
                if (constantValue !== undefined && this.isVariable(node.name)) {
                    // 替换为常量值
                    const replacement = ASTBuilder.numericLiteral(constantValue);
                    replacement.location = node.location;
                    this.replaceNode(node, replacement);
                    this.modified = true;
                }
            },
            BinaryExpression: (node) => {
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
                    this.simplifyBinaryExpression(node);
                }
            },
            UnaryExpression: (node) => {
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
            },
            IfStatement: (node) => {
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
            },
            WhileStatement: (node) => {
                const testValue = this.evaluateConstantExpression(node.test);
                if (testValue !== undefined && !testValue) {
                    // 条件为假的while循环，直接删除
                    this.replaceNode(node, null);
                    this.modified = true;
                }
            }
        });
    }

    // =============== 函数内联 ===============

    inlineFunctions() {
        // 收集适合内联的函数
        const candidates = [];
        this.functionInfo.forEach((info, funcName) => {
            if (info.isDefinition && info.calls === 1) {
                // 只被调用一次的函数
                candidates.push(funcName);
            } else if (info.isInline && info.calls > 0) {
                // 标记为inline的函数
                candidates.push(funcName);
            } else if (this.isSmallFunction(info.node)) {
                // 小函数
                candidates.push(funcName);
            }
        });
        
        // 内联候选函数
        for (const funcName of candidates) {
            this.inlineFunction(funcName);
        }
    }

    inlineFunction(funcName) {
        const funcInfo = this.functionInfo.get(funcName);
        if (!funcInfo || !funcInfo.node.body) return;
        
        const funcNode = funcInfo.node;
        
        // 查找调用点
        let callSite = null;
        let parentNode = null;
        
        this.traverseAST(this.optimizedAst, {
            FunctionCall: (node, parent) => {
                if (node.callee.type === 'Identifier' && 
                    node.callee.name === funcName && 
                    !callSite) {
                    callSite = node;
                    parentNode = parent;
                }
            }
        });
        
        if (!callSite || !parentNode) return;
        
        // 检查是否适合内联
        if (!this.isSuitableForInlining(funcNode, callSite)) {
            return;
        }
        
        // 创建内联副本
        const inlinedBody = this.cloneAST(funcNode.body);
        
        // 替换参数
        const paramMap = new Map();
        funcNode.parameters.forEach((param, index) => {
            if (index < callSite.arguments.length) {
                paramMap.set(param.name, callSite.arguments[index]);
            }
        });
        
        this.replaceParameters(inlinedBody, paramMap);
        
        // 替换返回语句
        if (this.containsReturn(inlinedBody)) {
            const returnValue = this.extractReturnValue(inlinedBody);
            if (returnValue) {
                this.replaceNode(callSite, returnValue);
            } else {
                // 没有返回值的函数调用，用函数体替换
                if (parentNode.type === 'ExpressionStatement') {
                    this.replaceNode(parentNode, inlinedBody);
                }
            }
        } else {
            // 没有返回语句的函数，用函数体替换调用
            if (parentNode.type === 'ExpressionStatement') {
                this.replaceNode(parentNode, inlinedBody);
            }
        }
        
        this.modified = true;
        this.warnings.push(`Inlined function '${funcName}'`);
        
        // 更新函数调用计数
        funcInfo.calls--;
    }

    // =============== 辅助方法 ===============

    traverseAST(node, visitors, callbacks = {}) {
        const traverse = (currentNode, parent = null) => {
            if (!currentNode) return;
            
            // 调用进入节点的回调
            if (callbacks.enterFunction && currentNode.type === 'FunctionDeclaration') {
                callbacks.enterFunction(currentNode);
            }
            
            // 调用节点特定的访问者
            if (visitors[currentNode.type]) {
                visitors[currentNode.type](currentNode, parent);
            }
            
            // 遍历子节点
            if (currentNode.children) {
                currentNode.children.forEach(child => traverse(child, currentNode));
            }
			
			// Manually added: traverse functions !!!
			if (currentNode.functions) {
				currentNode.functions.forEach(child => traverse(child, currentNode));
			}
            
            // 处理特殊节点的子节点
            switch (currentNode.type) {
                case 'IfStatement':
                    if (currentNode.consequent) traverse(currentNode.consequent, currentNode);
                    if (currentNode.alternate) traverse(currentNode.alternate, currentNode);
                    if (currentNode.test) traverse(currentNode.test, currentNode);
                    break;
                case 'WhileStatement':
                    if (currentNode.body) traverse(currentNode.body, currentNode);
                    if (currentNode.test) traverse(currentNode.test, currentNode);
                    break;
                case 'ForStatement':
                    if (currentNode.init) traverse(currentNode.init, currentNode);
                    if (currentNode.test) traverse(currentNode.test, currentNode);
                    if (currentNode.update) traverse(currentNode.update, currentNode);
                    if (currentNode.body) traverse(currentNode.body, currentNode);
                    break;
                case 'CompoundStatement':
                    if (currentNode.statements) {
                        currentNode.statements.forEach(stmt => traverse(stmt, currentNode));
                    }
                    break;
                case 'VariableDeclaration':
                    currentNode.declarators.forEach(decl => {
                        if (decl.initializer) traverse(decl.initializer, currentNode);
                    });
                    break;
                case 'FunctionDeclaration':
                    if (currentNode.body) traverse(currentNode.body, currentNode);
                    break;
            }
            
            // 调用退出节点的回调
            if (callbacks.exitFunction && currentNode.type === 'FunctionDeclaration') {
                callbacks.exitFunction(currentNode);
            }
        };
        
        traverse(node);
    }

    replaceNode(oldNode, newNode) {
        // 在实际实现中，需要找到oldNode的父节点并替换
        // 这里简化处理，实际需要遍历AST并修改
        // 由于AST节点的父指针没有完全实现，这里需要更复杂的逻辑
        // 为简化，我们暂时只记录替换，实际替换在遍历时由调用者处理
    }

    getVariableKey(varName) {
        return this.currentFunction ? `${this.currentFunction}.${varName}` : varName;
    }

    isVariable(name) {
        // 检查是否是变量（不是类型名或函数名）
        // 简化实现
        return !this.functionInfo.has(name) && !this.typeTable.has(name);
    }

    evaluateConstantExpression(node) {
        if (!node) return undefined;
        
        switch (node.type) {
            case 'NumericLiteral':
                return node.value;
            case 'Identifier':
                const varKey = this.getVariableKey(node.name);
                return this.constants.get(varKey);
            case 'BinaryExpression':
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

    evaluateBinaryOperation(operator, left, right) {
        switch (operator) {
            case '+': return left + right;
            case '-': return left - right;
            case '*': return left * right;
            case '/': return right !== 0 ? left / right : undefined;
            case '%': return right !== 0 ? left % right : undefined;
            case '&': return left & right;
            case '|': return left | right;
            case '^': return left ^ right;
            case '<<': return left << right;
            case '>>': return left >> right;
            case '==': return left == right ? 1 : 0;
            case '!=': return left != right ? 1 : 0;
            case '<': return left < right ? 1 : 0;
            case '>': return left > right ? 1 : 0;
            case '<=': return left <= right ? 1 : 0;
            case '>=': return left >= right ? 1 : 0;
            default: return undefined;
        }
    }

    evaluateUnaryOperation(operator, arg) {
        switch (operator) {
            case '+': return +arg;
            case '-': return -arg;
            case '~': return ~arg;
            case '!': return !arg ? 1 : 0;
            default: return undefined;
        }
    }

    hasSideEffects(node) {
        if (!node) return false;
        
        // 检查节点是否有副作用
        switch (node.type) {
            case 'FunctionCall':
            case 'BuiltinCall':
            case 'AssignmentExpression':
            case 'UnaryExpression':
                if (node.operator === '++' || node.operator === '--') {
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

    isSmallFunction(funcNode) {
        if (!funcNode.body) return false;
        
        // 检查函数体大小
        let statementCount = 0;
        this.traverseAST(funcNode.body, {
            ExpressionStatement: () => statementCount++,
            VariableDeclaration: () => statementCount++,
            ReturnStatement: () => statementCount++,
            IfStatement: () => statementCount++,
            WhileStatement: () => statementCount++,
            ForStatement: () => statementCount++
        });
        
        // 小函数：不超过3个语句
        return statementCount <= 3;
    }

    isSuitableForInlining(funcNode, callSite) {
        // 检查函数是否适合内联
        if (!funcNode.body) return false;
        
        // 检查参数数量匹配
        if (funcNode.parameters.length !== callSite.arguments.length) {
            return false;
        }
        
        // 检查是否有递归调用
        let hasRecursion = false;
        this.traverseAST(funcNode.body, {
            FunctionCall: (node) => {
                if (node.callee.type === 'Identifier' && 
                    node.callee.name === funcNode.name) {
                    hasRecursion = true;
                }
            }
        });
        
        return !hasRecursion;
    }

    cloneAST(node) {
        // 深度复制AST节点
        // 简化实现，实际需要根据节点类型复制
        if (!node) return null;
        
        const clone = Object.assign({}, node);
        clone.children = node.children ? node.children.map(child => this.cloneAST(child)) : [];
        
        return clone;
    }

    replaceParameters(node, paramMap) {
        // 替换参数标识符
        this.traverseAST(node, {
            Identifier: (identNode) => {
                const replacement = paramMap.get(identNode.name);
                if (replacement) {
                    const clonedReplacement = this.cloneAST(replacement);
                    clonedReplacement.location = identNode.location;
                    // 实际替换需要父节点信息
                }
            }
        });
    }

    containsReturn(node) {
        let hasReturn = false;
        this.traverseAST(node, {
            ReturnStatement: () => {
                hasReturn = true;
            }
        });
        return hasReturn;
    }

    extractReturnValue(node) {
        let returnValue = null;
        
        this.traverseAST(node, {
            ReturnStatement: (retNode) => {
                if (retNode.argument) {
                    returnValue = retNode.argument;
                }
            }
        });
        
        return returnValue ? this.cloneAST(returnValue) : null;
    }
}

// 函数内联器辅助类
class FunctionInliner {
    constructor(optimizer, programNode, inlineCandidates) {
        this.optimizer = optimizer;
        this.programNode = programNode;
        this.inlineCandidates = inlineCandidates;
        this.modified = false;
    }

    applyInlining() {
        // 遍历所有函数，内联调用
        for (const func of this.programNode.functions) {
            this.inlineCallsInFunction(func);
        }
        
        // 删除已被内联的函数
        this.programNode.functions = this.programNode.functions.filter(func => {
            const funcName = func.name;
            const isCandidate = this.inlineCandidates.has(funcName);
            const isUsed = (this.optimizer.functionCallCounts.get(funcName) || 0) > 0;
            
            // 保留未被内联或仍被使用的函数
            return !isCandidate || isUsed || funcName === 'main';
        });
    }

    inlineCallsInFunction(funcNode) {
        if (!funcNode.body || !funcNode.body.statements) return;
        
        const funcName = funcNode.name;
        const optimizedStatements = [];
        
        for (const stmt of funcNode.body.statements) {
            // 检查是否是函数调用表达式语句
            if (stmt.type === 'ExpressionStatement' && 
                stmt.expression && 
                stmt.expression.type === 'FunctionCall') {
                
                const callExpr = stmt.expression;
                const calleeName = callExpr.callee.name;
                
                if (this.inlineCandidates.has(calleeName)) {
                    // 内联这个调用
                    const inlinedCode = this.inlineFunctionCall(callExpr, calleeName, funcName);
                    if (inlinedCode) {
                        this.modified = true;
                        optimizedStatements.push(...inlinedCode);
                        continue;
                    }
                }
            }
            
            optimizedStatements.push(stmt);
        }
        
        funcNode.body.statements = optimizedStatements;
    }

    inlineFunctionCall(callExpr, calleeName, callerName) {
        const calleeNode = this.optimizer.functionDefinitions.get(calleeName);
        if (!calleeNode || !calleeNode.body) return null;
        
        // 克隆函数体
        const clonedBody = this.optimizer.cloneNode(calleeNode.body);
        
        // 创建参数映射
        const paramMap = new Map();
        if (calleeNode.parameters && callExpr.arguments) {
            for (let i = 0; i < calleeNode.parameters.length; i++) {
                const paramName = calleeNode.parameters[i].name;
                const argExpr = callExpr.arguments[i];
                
                if (paramName && argExpr) {
                    // 为参数创建临时变量或直接替换
                    const tempVarName = `__inline_${calleeName}_${paramName}_${Date.now()}`;
                    paramMap.set(paramName, {
                        varName: tempVarName,
                        value: argExpr
                    });
                }
            }
        }
        
        // 转换函数体中的语句
        const inlinedStatements = [];
        
        // 添加参数初始化
        for (const [paramName, paramInfo] of paramMap) {
            const varDecl = ASTBuilder.variableDeclaration(
                ASTBuilder.typeSpecifier('int'), // 简化：假设所有参数都是int类型
                [ASTBuilder.variableDeclarator(paramInfo.varName, paramInfo.value)]
            );
            inlinedStatements.push(varDecl);
        }
        
        // 添加函数体语句（替换参数引用）
        if (clonedBody.statements) {
            for (const stmt of clonedBody.statements) {
                const transformedStmt = this.transformStatement(stmt, paramMap, calleeName, callerName);
                if (transformedStmt) {
                    inlinedStatements.push(transformedStmt);
                }
            }
        }
        
        return inlinedStatements;
    }

    transformStatement(stmt, paramMap, calleeName, callerName) {
        const transformed = this.optimizer.cloneNode(stmt);
        
        // 替换参数引用
        const paramReplacer = {
            visitIdentifier: (node) => {
                const varName = node.name;
                if (paramMap.has(varName)) {
                    const paramInfo = paramMap.get(varName);
                    return ASTBuilder.identifier(paramInfo.varName);
                }
                return node;
            },
            
            visitReturnStatement: (node) => {
                // 简化：将return转换为赋值或表达式
                if (node.argument) {
                    const transformedArg = this.transformExpression(node.argument, paramMap);
                    return ASTBuilder.expressionStatement(transformedArg);
                }
                return null;
            }
        };
        
        this.transformNode = (node) => {
            if (!node) return node;
            
            if (node.type === 'Identifier') {
                return paramReplacer.visitIdentifier(node);
            } else if (node.type === 'ReturnStatement') {
                return paramReplacer.visitReturnStatement(node);
            }
            
            // 递归处理子节点
            if (node.children) {
                for (let i = 0; i < node.children.length; i++) {
                    node.children[i] = this.transformNode(node.children[i]);
                }
            }
            
            return node;
        };
        
        return this.transformNode(transformed);
    }

    transformExpression(expr, paramMap) {
        // 简化：仅替换标识符
        const transformed = this.optimizer.cloneNode(expr);
        
        const replaceVisitor = {
            visitIdentifier: (node) => {
                const varName = node.name;
                if (paramMap.has(varName)) {
                    const paramInfo = paramMap.get(varName);
                    return this.optimizer.cloneNode(paramInfo.value);
                }
                return node;
            }
        };
        
        this.visitExpr = (node) => {
            if (!node) return node;
            
            if (node.type === 'Identifier') {
                return replaceVisitor.visitIdentifier(node);
            }
            
            // 递归处理子节点
            if (node.children) {
                for (let i = 0; i < node.children.length; i++) {
                    node.children[i] = this.visitExpr(node.children[i]);
                }
            }
            
            return node;
        };
        
        return this.visitExpr(transformed);
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
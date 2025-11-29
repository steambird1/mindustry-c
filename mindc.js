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

class FunctionDeclarationNode extends ASTNode {
    constructor(name, returnType) {
        super(ASTNodeType.FUNCTION_DECLARATION);
        this.name = name;
        this.returnType = returnType;
        this.parameters = [];
        this.body = null;
        this.isBuiltin = false;
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

class VariableDeclaratorNode extends ASTNode {
    constructor(name) {
        super(ASTNodeType.VARIABLE_DECLARATOR);
        this.name = name;
        this.initializer = null;
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

class TypeSpecifierNode extends ASTNode {
    constructor(typeName) {
        super(ASTNodeType.TYPE_SPECIFIER);
        this.typeName = typeName;
    }
}

class AsmStatementNode extends ASTNode {
    constructor(code) {
        super(ASTNodeType.ASM_STATEMENT);
        this.code = code;
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
    '++': TokenType.INCREMENT,
    '--': TokenType.DECREMENT
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
        if (this.currentTokenIndex < this.tokens.length) {
            this.currentTokenIndex++;
        }
        return this.getCurrentToken();
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

        while (!this.matchToken(TokenType.EOF)) {
            if (this.matchToken(TokenType.SEMICOLON)) {
                this.consumeToken(); // 跳过空语句
                continue;
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
        }

        program.location = startLocation;
        return program;
    }

    parseDeclaration() {
        // 检查是否为函数声明
        const lookahead = this.lookaheadForFunction();
        if (lookahead.isFunction) {
            return this.parseFunctionDeclaration();
        } else {
            return this.parseVariableDeclaration();
        }
    }

    lookaheadForFunction() {
        let startIndex = this.currentTokenIndex;
        let isFunction = false;

        // 跳过类型说明符
        while (this.matchTokenTypeAt(startIndex, [
            TokenType.INT, TokenType.CHAR, TokenType.FLOAT, TokenType.VOID,
            TokenType.DOUBLE, TokenType.LONG, TokenType.SHORT, TokenType.SIGNED, TokenType.UNSIGNED
        ])) {
            startIndex++;
        }

        // 接下来应该是标识符
        if (this.matchTokenTypeAt(startIndex, [TokenType.IDENTIFIER])) {
            // 再接下来是 '(' 说明是函数
            if (this.matchTokenTypeAt(startIndex + 1, [TokenType.LEFT_PAREN])) {
                isFunction = true;
            }
        }

        return { isFunction };
    }

    matchTokenTypeAt(index, types) {
        if (index >= this.tokens.length) return false;
        const token = this.tokens[index];
        return types.includes(token.type);
    }

    parseFunctionDeclaration() {
        const returnType = this.parseTypeSpecifier();
        if (!returnType) return null;

        const nameToken = this.expectToken(TokenType.IDENTIFIER);
        if (!nameToken) return null;

        this.expectToken(TokenType.LEFT_PAREN);

        const functionDecl = ASTBuilder.functionDeclaration(nameToken.value, returnType);
        functionDecl.location = returnType.location;

        // 解析参数
        if (!this.matchToken(TokenType.RIGHT_PAREN)) {
            do {
                const paramType = this.parseTypeSpecifier();
                if (!paramType) break;

                const paramName = this.expectToken(TokenType.IDENTIFIER);
                if (paramName) {
                    functionDecl.parameters.push({
                        type: paramType,
                        name: paramName.value,
                        location: paramType.location
                    });
                }
            } while (this.matchToken(TokenType.COMMA) && this.consumeToken());
        }

        this.expectToken(TokenType.RIGHT_PAREN);
        functionDecl.body = this.parseCompoundStatement();

        return functionDecl;
    }

    parseTypeSpecifier() {
		// Added in the 2nd correction conversation
		let qualifiers = [];
    
		// 解析类型限定符(const, volatile)
		while (this.matchToken(TokenType.CONST) || this.matchToken(TokenType.VOLATILE)) {
			const qualifierToken = this.consumeToken();
			qualifiers.push(qualifierToken.value);
		}
		
        const typeTokens = [
			TokenType.INT, TokenType.CHAR, TokenType.FLOAT, TokenType.VOID,
			TokenType.DOUBLE, TokenType.LONG, TokenType.SHORT, TokenType.SIGNED, TokenType.UNSIGNED,
			TokenType.STRUCT, TokenType.UNION, TokenType.ENUM
		];

		for (const typeToken of typeTokens) {
			if (this.matchToken(typeToken)) {
				const token = this.getCurrentToken();
				this.consumeToken();
				const typeNode = ASTBuilder.typeSpecifier(token.value);
				typeNode.setAttribute('location', token.location);
				typeNode.setAttribute('qualifiers', qualifiers);
				
				// 处理struct类型
				// It does remind me of CSP-S 2023
				if (token.type === TokenType.STRUCT) {
					this.parseStructDefinition(typeNode);
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
		}

        this.addError('Expected type specifier');
        return null;
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

    parseVariableDeclaration() {
        const type = this.parseTypeSpecifier();
        if (!type) return null;

        const declarators = [];
        do {
            const declarator = this.parseVariableDeclarator();
            if (declarator) {
				const qualifiers = type.getAttribute('qualifiers') || [];
				if (qualifiers.includes('const')) {
					declarator.setAttribute('isConst', true);
				}
				if (qualifiers.includes('volatile')) {
					declarator.setAttribute('isVolatile', true);
				}
                declarators.push(declarator);
            }
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
                return this.parseExpressionStatement();
        }
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
            TokenType.MULTIPLY_ASSIGN, TokenType.DIVIDE_ASSIGN, TokenType.MODULO_ASSIGN
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

    parseUnaryExpression() {
        // 检查一元操作符
        const unaryOperators = [
            TokenType.PLUS, TokenType.MINUS, TokenType.NOT, TokenType.BITWISE_NOT,
            TokenType.INCREMENT, TokenType.DECREMENT
        ];

        for (const opType of unaryOperators) {
            if (this.matchToken(opType)) {
                const operatorToken = this.consumeToken();
                const argument = this.parseUnaryExpression();
                
                if (!argument) {
                    this.addError('Expected expression after unary operator');
                    return null;
                }

                const unaryExpr = ASTBuilder.unaryExpression(operatorToken.value, argument);
                unaryExpr.location = operatorToken.location;
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


// 符号表条目
class SymbolEntry {
    constructor(name, type, scope, kind, location = null) {
        this.name = name;
        this.type = type; // 类型信息
        this.scope = scope; // 作用域
        this.kind = kind; // 'variable', 'function', 'parameter'
        this.location = location; // 声明位置
        this.initialized = false;
        this.used = false;
        this.memoryLocation = null; // 内存位置分配
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

class SemanticAnalyzer extends ASTVisitor {
    constructor() {
        super();
        this.errors = [];
        this.warnings = [];
        this.currentScope = null;
        this.currentFunction = null;
        this.symbolTable = new Map(); // 全局符号表
        this.typeTable = new Map(); // 类型表
        
    }

    analyze(ast) {
        this.errors = [];
        this.warnings = [];
        this.currentScope = new Scope(null, 'global');
        this.currentFunction = null;

		// 初始化内建类型
        this.initializeBuiltinTypes();
        this.initializeBuiltinFunctions();

        this.visit(ast);

        return {
            success: this.errors.length === 0,
            ast: ast,
            errors: this.errors,
            warnings: this.warnings,
            symbolTable: this.symbolTable
        };
    }

    initializeBuiltinTypes() {
        // 基本类型
        const basicTypes = ['int', 'char', 'float', 'double', 'void', 'long', 'short', 'signed', 'unsigned'];
        basicTypes.forEach(type => {
            this.typeTable.set(type, {
                name: type,
                size: 1, // 在目标机器中，所有基本类型都占用1个内存单元
                kind: 'basic'
            });
        });

        // 特殊类型
        this.typeTable.set('device', {
            name: 'device',
            size: 1,
            kind: 'device'
        });

        this.typeTable.set('null_t', {
            name: 'null_t',
            size: 1,
            kind: 'null'
        });
		
		// TODO: According to Deepseek, structural types must be added here dynamically (according to the result of parser)
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

    visitVariableDeclaration(node) {
        const typeName = node.type.typeName;
        
		// Manually merged in the 5th conversation
		if (!this.getTypeNameFromTypeNode(node.type)) {
			this.addError(`Invalid type in variable declaration`, node.type.location);
			return;
		}
		
        // 检查类型是否存在
        if (!this.typeTable.has(typeName)) {
            this.addError(`Unknown type '${typeName}'`, node.type.location);
            return;
        }

        node.declarators.forEach(declarator => {
            // 检查变量是否已声明
            const existingSymbol = this.currentScope.lookupCurrent(declarator.name);
            if (existingSymbol) {
                this.addError(`Variable '${declarator.name}' is already declared in this scope`, declarator.location);
                return;
            }

            // 创建变量符号
            const varSymbol = new SymbolEntry(
                declarator.name,
                { type: typeName },
                this.currentScope,
                'variable',
                declarator.location
            );

            this.currentScope.addSymbol(varSymbol);

            // 检查初始化表达式
            if (declarator.initializer) {
                this.visit(declarator.initializer);
                
                // 类型检查初始化表达式
                const initType = this.getExpressionType(declarator.initializer);
                if (initType && !this.isTypeCompatible(typeName, initType)) {
                    this.addError(
                        `Cannot initialize '${typeName}' with '${initType}'`,
                        declarator.initializer.location
                    );
                }

                varSymbol.initialized = true;
            }
        });
    }
	
	// 5th conversation
	// 新增辅助方法：从类型节点中提取类型名称
	getTypeNameFromTypeNode(typeNode) {
		if (!typeNode) return null;
		
		// 如果是TypeSpecifier节点
		if (typeNode.typeName) {
			return typeNode.typeName;
		}
		
		// 如果是其他类型的类型节点（如指针类型等）
		// 这里可以根据需要扩展处理更复杂的类型
		if (typeNode.type === 'TypeSpecifier') {
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
        if (!argType) {
            return;
        }

        // 检查一元操作符的类型兼容性
        if (!this.isTypeCompatibleForUnaryOperator(node.operator, argType)) {
            this.addError(
                `Unary operator '${node.operator}' cannot be applied to type '${argType}'`,
                node.location
            );
            return;
        }

        // 设置表达式结果类型
        node.dataType = this.getUnaryResultType(node.operator, argType);
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

        // 检查左值
        if (!this.isLValue(node.left)) {
            this.addError(`Assignment requires lvalue`, node.location);
        }

        node.dataType = leftType;
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

    visitIfStatement(node) {
        this.visit(node.test);
        
        // 检查条件表达式类型
        const testType = this.getExpressionType(node.test);
        if (testType && testType !== 'int') {
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
        if (testType && testType !== 'int') {
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
            if (testType && testType !== 'int') {
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
            case ASTNodeType.IDENTIFIER:
                const symbol = this.currentScope.lookup(node.name);
                return symbol ? symbol.type.type : null;

            case ASTNodeType.NUMERIC_LITERAL:
                return 'int'; // 简化处理，所有数字字面量视为int

            case ASTNodeType.STRING_LITERAL:
			// TODO: For the machine, this might need some changes!
                return 'char*'; // 字符串字面量视为char*

            case ASTNodeType.CHARACTER_LITERAL:
                return 'char';

            case ASTNodeType.NULL_LITERAL:
                return 'null_t';

            case ASTNodeType.BINARY_EXPRESSION:
            case ASTNodeType.UNARY_EXPRESSION:
            case ASTNodeType.ASSIGNMENT_EXPRESSION:
            case ASTNodeType.FUNCTION_CALL:
            case ASTNodeType.BUILTIN_CALL:
                // 这些节点应该在访问时已经设置了dataType
                return node.dataType || null;

            default:
                return null;
        }
    }

    isTypeCompatible(targetType, sourceType) {
        // 相同类型总是兼容
        if (targetType === sourceType) {
            return true;
        }

        // 数值类型之间的兼容性（简化处理）
        const numericTypes = ['int', 'char', 'short', 'long', 'float', 'double'];
        if (numericTypes.includes(targetType) && numericTypes.includes(sourceType)) {
            return true;
        }
		/*
        // null_t可以赋值给指针类型
        if (sourceType === 'null_t' && targetType.endsWith('*')) {
            return true;
        }
		*/	// This is actually prohibited, as pointer always refers to a real memory access
		// (different from a variable.)

        // device类型的特殊规则
        if (targetType === 'device' && sourceType === 'device') {
            return true;
        }

        return false;
    }

    isTypeCompatibleForOperator(operator, leftType, rightType) {
        const arithmeticOps = ['+', '-', '*', '/', '%'];
        const comparisonOps = ['==', '!=', '<', '<=', '>', '>='];
        const logicalOps = ['&&', '||'];
        const bitwiseOps = ['&', '|', '^', '<<', '>>'];

        // 算术运算符要求数值类型
        if (arithmeticOps.includes(operator)) {
            return this.isNumericType(leftType) && this.isNumericType(rightType);
        }

        // 比较运算符要求兼容类型
        if (comparisonOps.includes(operator)) {
            return this.isTypeCompatible(leftType, rightType);
        }

        // 逻辑运算符要求布尔上下文（这里简化为int类型）
        if (logicalOps.includes(operator)) {
            return leftType === 'int' && rightType === 'int';
        }

        // 位运算符要求整型
        if (bitwiseOps.includes(operator)) {
            return this.isIntegerType(leftType) && this.isIntegerType(rightType);
        }

        return true; // 其他操作符暂时宽松处理
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

    isLValue(node) {
        // 标识符是左值
        if (node.type === ASTNodeType.IDENTIFIER) {
            return true;
        }

        // 数组下标是左值
        if (node.type === ASTNodeType.MEMBER_EXPRESSION && node.getAttribute('computed')) {
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
class Optimizer extends ASTVisitor {
    constructor() {
        super();
        this.constants = new Map(); // 常量值映射
        this.usedVariables = new Set(); // 使用的变量集合
        this.currentFunction = null;
        this.errors = [];
        this.modified = false;
        this.volatileVariables = new Set(); // volatile变量集合
        this.constVariables = new Set(); // const变量集合
    }

    optimize(ast) {
        this.constants.clear();
        this.usedVariables.clear();
        this.volatileVariables.clear();
        this.constVariables.clear();
        this.currentFunction = null;
        this.errors = [];
        this.modified = false;

        // 多次优化直到不再变化或达到最大迭代次数
        let iterations = 0;
        const maxIterations = 10;
        
        do {
            this.modified = false;
            this.optimizePass(ast);
            iterations++;
        } while (this.modified && iterations < maxIterations);

        return {
            success: this.errors.length === 0,
            ast: ast,
            errors: this.errors,
            warnings: [],
            optimized: iterations > 1 // 如果进行了优化则返回true
        };
    }

    optimizePass(ast) {
        // 第一遍：收集常量信息、变量使用情况和限定符信息
        this.collectInfo(ast);
        
        // 第二遍：应用优化
        this.applyOptimizations(ast);
    }

    collectInfo(node) {
        if (!node) return;

        switch (node.type) {
            case 'FunctionDeclaration':
                this.currentFunction = node.name;
                this.visitChildren(node);
                this.currentFunction = null;
                break;

            case 'VariableDeclaration':
                this.collectVariableInfo(node);
                break;

            case 'AssignmentExpression':
                this.collectAssignmentInfo(node);
                break;

            case 'Identifier':
                this.usedVariables.add(node.name);
                break;

            default:
                this.visitChildren(node);
                break;
        }
    }

    collectVariableInfo(node) {
        const typeName = node.type.typeName;
        const qualifiers = node.type.getAttribute('qualifiers') || [];
        
        node.declarators.forEach(declarator => {
            const varName = this.getVariableKey(declarator.name);
            
            // 记录volatile变量
            if (qualifiers.includes('volatile') || declarator.getAttribute('isVolatile')) {
                this.volatileVariables.add(varName);
            }
            
            // 记录const变量
            if (qualifiers.includes('const') || declarator.getAttribute('isConst')) {
                this.constVariables.add(varName);
            }
            
            // 只有非volatile的const变量才能参与常量传播
            if (declarator.initializer && !this.volatileVariables.has(varName)) {
                const value = this.evaluateConstantExpression(declarator.initializer);
                if (value !== undefined) {
                    this.constants.set(varName, value);
                }
            }
        });
        this.visitChildren(node);
    }

    collectAssignmentInfo(node) {
        if (node.left.type === 'Identifier') {
            const varName = this.getVariableKey(node.left.name);
            
            // 检查是否对const变量赋值（应该在语义分析中捕获，但这里作为防御性检查）
            if (this.constVariables.has(varName)) {
                this.addError(`Cannot assign to const variable '${node.left.name}'`, node.location);
            }
            
            // volatile变量或赋值会改变变量值，从常量表中移除
            if (this.volatileVariables.has(varName)) {
                // volatile变量永远不参与常量传播
                this.constants.delete(varName);
            } else {
                // 非volatile变量，赋值会改变其值
                this.constants.delete(varName);
                
                // 如果赋值是常量表达式，可以重新记录（对于非const变量）
                if (!this.constVariables.has(varName)) {
                    const value = this.evaluateConstantExpression(node.right);
                    if (value !== undefined) {
                        this.constants.set(varName, value);
                    }
                }
            }
        }
        this.visitChildren(node);
    }

    applyOptimizations(node) {
        if (!node) return node;

        // 检查是否涉及volatile访问，如果是则跳过优化
        if (this.involvesVolatileAccess(node)) {
            return this.optimizeChildren(node); // 只优化子节点，不优化当前节点
        }

        switch (node.type) {
            case 'FunctionDeclaration':
                return this.optimizeFunction(node);

            case 'VariableDeclaration':
                return this.optimizeVariableDeclaration(node);

            case 'IfStatement':
                return this.optimizeIfStatement(node);

            case 'BinaryExpression':
                return this.optimizeBinaryExpression(node);

            case 'UnaryExpression':
                return this.optimizeUnaryExpression(node);

            case 'ExpressionStatement':
                return this.optimizeExpressionStatement(node);

            case 'CompoundStatement':
                return this.optimizeCompoundStatement(node);

            case 'AssignmentExpression':
                return this.optimizeAssignmentExpression(node);

            case 'Identifier':
                return this.optimizeIdentifier(node);

            default:
                return this.optimizeChildren(node);
        }
    }

    optimizeFunction(node) {
        this.currentFunction = node.name;
        node.body = this.applyOptimizations(node.body);
        this.currentFunction = null;
        return node;
    }

    optimizeVariableDeclaration(node) {
        // 对于volatile变量，不进行任何优化
        const qualifiers = node.type.getAttribute('qualifiers') || [];
        if (qualifiers.includes('volatile')) {
            return this.optimizeChildren(node);
        }

        // 移除未初始化的无用变量（如果未被使用）
        const usefulDeclarators = [];
        
        for (const declarator of node.declarators) {
            const varName = this.getVariableKey(declarator.name);
            
            // volatile变量不参与优化
            if (this.volatileVariables.has(varName)) {
                usefulDeclarators.push(declarator);
                continue;
            }
            
            if (declarator.initializer) {
                // 优化初始化表达式
                declarator.initializer = this.applyOptimizations(declarator.initializer);
                
                // 常量传播：如果初始化表达式是常量且变量是const，记录常量值
                if (this.constVariables.has(varName)) {
                    const constValue = this.evaluateConstantExpression(declarator.initializer);
                    if (constValue !== undefined) {
                        this.constants.set(varName, constValue);
                    }
                }
                
                // 检查变量是否被使用
                if (this.usedVariables.has(declarator.name) || this.mayHaveSideEffects(declarator.initializer)) {
                    usefulDeclarators.push(declarator);
                } else {
                    this.modified = true;
                }
            } else {
                // 未初始化的变量，如果未被使用则移除
                if (this.usedVariables.has(declarator.name)) {
                    usefulDeclarators.push(declarator);
                } else {
                    this.modified = true;
                }
            }
        }

        if (usefulDeclarators.length === 0) {
            this.modified = true;
            return null; // 整个声明都是无用的
        }

        node.declarators = usefulDeclarators;
        return node;
    }

    optimizeIfStatement(node) {
        node.test = this.applyOptimizations(node.test);
        
        // 常量条件求值（但避免对涉及volatile的表达式进行求值）
        if (!this.involvesVolatileAccess(node.test)) {
            const testValue = this.evaluateConstantExpression(node.test);
            if (testValue !== undefined) {
                this.modified = true;
                if (testValue) {
                    // 条件为真，只保留then分支
                    return this.applyOptimizations(node.consequent);
                } else {
                    // 条件为假，只保留else分支（如果有）
                    return node.alternate ? this.applyOptimizations(node.alternate) : null;
                }
            }
        }

        node.consequent = this.applyOptimizations(node.consequent);
        if (node.alternate) {
            node.alternate = this.applyOptimizations(node.alternate);
        }

        // 如果两个分支都为空，移除整个if语句
        if (this.isEmptyStatement(node.consequent) && 
            (!node.alternate || this.isEmptyStatement(node.alternate))) {
            this.modified = true;
            return null;
        }

        return node;
    }

    optimizeBinaryExpression(node) {
        node.left = this.applyOptimizations(node.left);
        node.right = this.applyOptimizations(node.right);

        // 避免对涉及volatile的表达式进行常量折叠
        if (!this.involvesVolatileAccess(node.left) && !this.involvesVolatileAccess(node.right)) {
            // 常量折叠
            const leftValue = this.evaluateConstantExpression(node.left);
            const rightValue = this.evaluateConstantExpression(node.right);
            
            if (leftValue !== undefined && rightValue !== undefined) {
                const result = this.evaluateBinaryOperation(node.operator, leftValue, rightValue);
                if (result !== undefined) {
                    this.modified = true;
                    return ASTBuilder.numericLiteral(result);
                }
            }

            // 代数恒等式简化
            return this.simplifyBinaryExpression(node, leftValue, rightValue);
        }

        return node;
    }

    optimizeUnaryExpression(node) {
        node.argument = this.applyOptimizations(node.argument);
        
        // 避免对涉及volatile的表达式进行常量折叠
        if (!this.involvesVolatileAccess(node.argument)) {
            // 常量折叠
            const argValue = this.evaluateConstantExpression(node.argument);
            if (argValue !== undefined) {
                const result = this.evaluateUnaryOperation(node.operator, argValue);
                if (result !== undefined) {
                    this.modified = true;
                    return ASTBuilder.numericLiteral(result);
                }
            }
        }

        return node;
    }

    optimizeExpressionStatement(node) {
        const optimizedExpr = this.applyOptimizations(node.expression);
        
        if (!optimizedExpr) {
            this.modified = true;
            return null;
        }

        // 移除没有副作用的表达式语句（但volatile访问被认为有副作用）
        if (!this.mayHaveSideEffects(optimizedExpr) && !this.involvesVolatileAccess(optimizedExpr)) {
            this.modified = true;
            return null;
        }

        node.expression = optimizedExpr;
        return node;
    }

    optimizeAssignmentExpression(node) {
        node.left = this.applyOptimizations(node.left);
        node.right = this.applyOptimizations(node.right);
        
        // 对于volatile变量的赋值，不进行优化
        if (node.left.type === 'Identifier' && this.volatileVariables.has(this.getVariableKey(node.left.name))) {
            return node;
        }
        
        return node;
    }

    optimizeIdentifier(node) {
        // 常量传播（但不传播volatile变量）
        const varName = this.getVariableKey(node.name);
        if (!this.volatileVariables.has(varName) && this.constants.has(varName)) {
            const constantValue = this.constants.get(varName);
            this.modified = true;
            return ASTBuilder.numericLiteral(constantValue);
        }
        
        return node;
    }

    optimizeCompoundStatement(node) {
        const optimizedStatements = [];
        
        for (const stmt of node.statements) {
            const optimizedStmt = this.applyOptimizations(stmt);
            if (optimizedStmt) {
                optimizedStatements.push(optimizedStmt);
            } else {
                this.modified = true;
            }
        }

        node.statements = optimizedStatements;
        
        // 如果复合语句为空，返回null
        if (optimizedStatements.length === 0) {
            return null;
        }

        return node;
    }

    optimizeChildren(node) {
        if (node.children) {
            for (let i = 0; i < node.children.length; i++) {
                const optimizedChild = this.applyOptimizations(node.children[i]);
                if (optimizedChild !== node.children[i]) {
                    this.modified = true;
                    node.children[i] = optimizedChild;
                }
            }
        }
        return node;
    }

    // =============== 新增辅助方法 ===============

    involvesVolatileAccess(node) {
        if (!node) return false;
        
        // 检查标识符是否为volatile变量
        if (node.type === 'Identifier') {
            const varName = this.getVariableKey(node.name);
            return this.volatileVariables.has(varName);
        }
        
        // 检查函数调用（内建函数可能有volatile行为）
        if (node.type === 'FunctionCall' || node.type === 'BuiltinCall') {
            // 假设所有内建函数调用都可能涉及volatile访问
            // 在实际实现中，可以根据具体函数进行细化
            return true;
        }
        
        // 递归检查子节点
        if (node.children) {
            for (const child of node.children) {
                if (this.involvesVolatileAccess(child)) {
                    return true;
                }
            }
        }
        
        return false;
    }

    // =============== 原有辅助方法 ===============

    getVariableKey(varName) {
        return this.currentFunction ? `${this.currentFunction}.${varName}` : varName;
    }

    evaluateConstantExpression(node) {
        if (!node) return undefined;

        // 如果涉及volatile访问，不进行常量求值
        if (this.involvesVolatileAccess(node)) {
            return undefined;
        }

        switch (node.type) {
            case 'NumericLiteral':
                return node.value;

            case 'Identifier':
                const varName = this.getVariableKey(node.name);
                return this.constants.get(varName);

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

    simplifyBinaryExpression(node, leftValue, rightValue) {
        // 代数恒等式简化
        switch (node.operator) {
            case '+':
                if (leftValue === 0) {
                    this.modified = true;
                    return node.right; // 0 + x → x
                }
                if (rightValue === 0) {
                    this.modified = true;
                    return node.left; // x + 0 → x
                }
                break;

            case '-':
                if (rightValue === 0) {
                    this.modified = true;
                    return node.left; // x - 0 → x
                }
                break;

            case '*':
                if (leftValue === 1) {
                    this.modified = true;
                    return node.right; // 1 * x → x
                }
                if (rightValue === 1) {
                    this.modified = true;
                    return node.left; // x * 1 → x
                }
                if (leftValue === 0 || rightValue === 0) {
                    this.modified = true;
                    return ASTBuilder.numericLiteral(0); // 0 * x → 0, x * 0 → 0
                }
                break;

            case '/':
                if (rightValue === 1) {
                    this.modified = true;
                    return node.left; // x / 1 → x
                }
                break;

            case '&':
                if (leftValue === 0 || rightValue === 0) {
                    this.modified = true;
                    return ASTBuilder.numericLiteral(0); // 0 & x → 0, x & 0 → 0
                }
                break;

            case '|':
                if (leftValue === 0) {
                    this.modified = true;
                    return node.right; // 0 | x → x
                }
                if (rightValue === 0) {
                    this.modified = true;
                    return node.left; // x | 0 → x
                }
                break;
        }

        return node;
    }

    mayHaveSideEffects(node) {
        if (!node) return false;

        // 以下情况可能有副作用：
        // 1. 函数调用
        // 2. 赋值操作
        // 3. 内建函数调用
        // 4. 内联汇编
        // 5. 涉及内存访问的操作
        // 6. volatile变量访问

        if (this.involvesVolatileAccess(node)) {
            return true;
        }

        switch (node.type) {
            case 'FunctionCall':
            case 'BuiltinCall':
            case 'AssignmentExpression':
            case 'AsmStatement':
                return true;

            case 'UnaryExpression':
                // ++ 和 -- 操作有副作用
                if (node.operator === '++' || node.operator === '--') {
                    return true;
                }
                break;

            case 'BinaryExpression':
                // 复合赋值操作有副作用
                if (node.operator.includes('=') && node.operator !== '==' && node.operator !== '!=') {
                    return true;
                }
                return this.mayHaveSideEffects(node.left) || this.mayHaveSideEffects(node.right);

            default:
                // 递归检查子节点
                if (node.children) {
                    for (const child of node.children) {
                        if (this.mayHaveSideEffects(child)) {
                            return true;
                        }
                    }
                }
                return false;
        }
    }

    isEmptyStatement(node) {
        if (!node) return true;
        if (node.type === 'CompoundStatement') {
            return node.statements.length === 0;
        }
        return false;
    }

    visitChildren(node) {
        if (node.children) {
            node.children.forEach(child => this.collectInfo(child));
        }
    }

    addError(message, location = null) {
        this.errors.push({
            message,
            line: location ? location.line : 0,
            column: location ? location.column : 0
        });
    }
}


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
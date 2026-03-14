
// 词法分析器
// Token类型枚举
export const TokenType = {
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
	NEAR: 'NEAR',
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
	BOOL: 'BOOL',
	TRUE: 'TRUE',
	FALSE: 'FALSE',
    
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
	LEFT_SHIFT_ASSIGN: 'LEFT_SHIFT_ASSIGN',
	RIGHT_SHIFT_ASSIGN: 'RIGHT_SHIFT_ASSIGN',
	BITWISE_AND_ASSIGN: 'BITWISE_AND_ASSIGN',
	BITWISE_OR_ASSIGN: 'BITWISE_OR_ASSIGN',
	BITWISE_XOR_ASSIGN: 'BITWISE_XOR_ASSIGN',
    
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
	PRINTCHAR: 'PRINTCHAR',
	FORMAT: 'FORMAT',
    DRAWFLUSH: 'DRAWFLUSH',
    PRINTFLUSH: 'PRINTFLUSH',
    GETLINK: 'GETLINK',
    CONTROL: 'CONTROL',
    RADAR: 'RADAR',
    SENSOR: 'SENSOR',
	UNITBIND: 'UNITBIND',
	UNITCONTROL: 'UNITCONTROL',
	UNITRADAR: 'UNITRADAR',
	UNITLOCATE: 'UNITLOCATE',
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
    
    // 预处理器指令
    PREPROCESSOR_DIRECTIVE: 'PREPROCESSOR_DIRECTIVE',
    PREPROCESSOR_DEFINE: 'PREPROCESSOR_DEFINE',
    PREPROCESSOR_UNDEF: 'PREPROCESSOR_UNDEF',
    PREPROCESSOR_IF: 'PREPROCESSOR_IF',
    PREPROCESSOR_IFDEF: 'PREPROCESSOR_IFDEF',
    PREPROCESSOR_IFNDEF: 'PREPROCESSOR_IFNDEF',
    PREPROCESSOR_ELSE: 'PREPROCESSOR_ELSE',
    PREPROCESSOR_ENDIF: 'PREPROCESSOR_ENDIF',
    PREPROCESSOR_INCLUDE: 'PREPROCESSOR_INCLUDE',
    PREPROCESSOR_PRAGMA: 'PREPROCESSOR_PRAGMA',
    PREPROCESSOR_ARGUMENTS: 'PREPROCESSOR_ARGUMENTS',
    
    // 结束标记
    EOF: 'EOF'
};

// Token类
export class Token {
    constructor(type, value, location = null, raw = "") {
        this.type = type;
        this.value = value;
        /**
         * @type {{line: number, column: number, index: number, filename?: string}}
         */
        this.location = location; // { line, column, index }
		this.raw = raw;
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
export const KEYWORDS = {
    'auto': TokenType.AUTO,
    'break': TokenType.BREAK,
	'bool': TokenType.BOOL,
    'case': TokenType.CASE,
    'char': TokenType.CHAR,
    'const': TokenType.CONST,
    'continue': TokenType.CONTINUE,
    'default': TokenType.DEFAULT,
    'do': TokenType.DO,	/* Do-while support not implemented right now */
    'double': TokenType.DOUBLE,
    'else': TokenType.ELSE,
    'enum': TokenType.ENUM,
    'extern': TokenType.EXTERN,
	'false': TokenType.FALSE,
    'float': TokenType.FLOAT,
    'for': TokenType.FOR,
    'goto': TokenType.GOTO,
    'if': TokenType.IF,
    'int': TokenType.INT,
	'inline': TokenType.INLINE,	/* Added, 4 Dec */
    'long': TokenType.LONG,
	'near': TokenType.NEAR,
    'register': TokenType.REGISTER,
    'return': TokenType.RETURN,
    'short': TokenType.SHORT,
    'signed': TokenType.SIGNED,
    'sizeof': TokenType.SIZEOF,
    'static': TokenType.STATIC,
    'struct': TokenType.STRUCT,
    'switch': TokenType.SWITCH, /* Unsupported */
	'true': TokenType.TRUE,
    'typedef': TokenType.TYPEDEF,
    'union': TokenType.UNION,
    'unsigned': TokenType.UNSIGNED,
    'void': TokenType.VOID,
    'volatile': TokenType.VOLATILE,
    'while': TokenType.WHILE,
    'null': TokenType.NULL,
    
    // 预处理器指令关键字
    '#define': TokenType.PREPROCESSOR_DEFINE,
    '#undef': TokenType.PREPROCESSOR_UNDEF,
    '#if': TokenType.PREPROCESSOR_IF,
    '#else': TokenType.PREPROCESSOR_ELSE,
    '#ifdef': TokenType.PREPROCESSOR_IFDEF,
    '#ifndef': TokenType.PREPROCESSOR_IFNDEF,
    '#endif': TokenType.PREPROCESSOR_ENDIF,
    '#include': TokenType.PREPROCESSOR_INCLUDE,
    '#pragma': TokenType.PREPROCESSOR_PRAGMA
};

// 特殊指令映射
// 'printchar' and 'format' is not added for compatibility.
export const SPECIAL_INSTRUCTIONS = {
    'asm': TokenType.ASM,
    'draw': TokenType.DRAW,
    'print': TokenType.PRINT,
	'printchar': TokenType.PRINTCHAR,
	'format': TokenType.FORMAT,
    'drawflush': TokenType.DRAWFLUSH,
    'printflush': TokenType.PRINTFLUSH,
    'getlink': TokenType.GETLINK,
    'control': TokenType.CONTROL,
    'radar': TokenType.RADAR,
    'sensor': TokenType.SENSOR,
	'ubind': TokenType.UNITBIND,
	'ucontrol': TokenType.UNITCONTROL,
	'uradar': TokenType.UNITRADAR,
	'ulocate': TokenType.UNITLOCATE,
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
export const OPERATORS = {
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
export const PUNCTUATORS = {
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


export class Lexer {
    constructor(sourceCode, macros = {}, suppressMacroExpansion = false, macroParams = {}) {
        this.sourceCode = sourceCode;
        this.position = 0;
        this.line = 1;
        this.column = 1;
        this.tokens = [];
        this.errors = [];
        this.suppressMacroExpansion = suppressMacroExpansion;
        
        // 预处理器状态
        this.macros = {...macros}; // 存储宏定义 {name: value}
        this.macroParams = {...macroParams};
        this.conditionalStack = []; // 条件编译栈，存储是否跳过代码块的状态
        this.skipping = false; // 当前是否处于“跳过代码”状态
        this.pragmas = []; // 收集所有 #pragma 指令内容
        this.extraExtensions = [];  // Extra extensions included in the code
    }
    
    // -- Utilities for #if processor --

    // 运算符优先级表
    getOperatorPrecedence(op) {
        const precedence = {
            '||': 1,
            '&&': 2,
            '|': 3,
            '^': 4,
            '&': 5,
            '==': 6, '!=': 6,
            '<': 7, '>': 7, '<=': 7, '>=': 7,
            '<<': 8, '>>': 8,
            '+': 9, '-': 9,
            '*': 10, '/': 10, '%': 10,
            '!': 11, '~': 11
        };
        return precedence[op] || 0;
    }

    // 判断是否为二元运算符
    isBinaryOperator(op) {
        const binaryOps = ['||', '&&', '|', '^', '&', '==', '!=', '<', '>', '<=', '>=', 
                          '<<', '>>', '+', '-', '*', '/', '%'];
        return binaryOps.includes(op);
    }
    
    // 判断是否为单目运算符
    isUnaryOperator(op) {
        const unaryOps = ['+', '-', '!', '~'];
        return unaryOps.includes(op);
    }

    // 表达式词法分析（专门用于解析 #if 后的条件表达式）
    tokenizeExpression(expr) {
        const tokens = [];
        let pos = 0;
        let line = 1;
        let column = 1;
        
        while (pos < expr.length) {
            const char = expr[pos];
            
            // 跳过空白字符
            if (char === ' ' || char === '\t' || char === '\n' || char === '\r') {
                if (char === '\n') {
                    line++;
                    column = 1;
                } else {
                    column++;
                }
                pos++;
                continue;
            }
            
            // 数字
            if (this.isDigit(char) || (char === '.' && pos + 1 < expr.length && this.isDigit(expr[pos + 1]))) {
                let numStr = '';
                let hasDot = false;
                let hasExponent = false;
                
                while (pos < expr.length) {
                    const c = expr[pos];
                    if (this.isDigit(c)) {
                        numStr += c;
                    } else if (c === '.' && !hasDot && !hasExponent) {
                        numStr += c;
                        hasDot = true;
                    } else if ((c === 'e' || c === 'E') && !hasExponent) {
                        numStr += c;
                        hasExponent = true;
                        if (pos + 1 < expr.length && (expr[pos + 1] === '+' || expr[pos + 1] === '-')) {
                            numStr += expr[++pos];
                        }
                    } else {
                        break;
                    }
                    pos++;
                    column++;
                }
                
                // 检查后缀
                if (pos < expr.length) {
                    const suffix = expr[pos].toLowerCase();
                    if (suffix === 'l' || suffix === 'u' || suffix === 'f') {
                        numStr += expr[pos];
                        pos++;
                        column++;
                    }
                }
                
                const numValue = hasDot || hasExponent ? parseFloat(numStr) : parseInt(numStr, 10);
                tokens.push({ type: 'number', value: numValue, line, column: column - numStr.length });
                continue;
            }
            
            // 标识符（宏名）
            if (this.isIdentifierStart(char)) {
                let ident = '';
                const startCol = column;
                
                while (pos < expr.length && this.isIdentifierPart(expr[pos])) {
                    ident += expr[pos];
                    pos++;
                    column++;
                }
                
                tokens.push({ type: 'identifier', value: ident, line, column: startCol });
                continue;
            }
            
            // 字符串字面量（支持在表达式中使用字符串常量）
            if (char === '"') {
                let str = '';
                const startCol = column;
                pos++;
                column++;
                
                while (pos < expr.length && expr[pos] !== '"') {
                    if (expr[pos] === '\\') {
                        pos++;
                        column++;
                        if (pos < expr.length) {
                            switch (expr[pos]) {
                                case 'n': str += '\n'; break;
                                case 't': str += '\t'; break;
                                case 'r': str += '\r'; break;
                                case '0': str += '\0'; break;
                                case '"': str += '"'; break;
                                case '\\': str += '\\'; break;
                                default: str += expr[pos];
                            }
                        }
                    } else {
                        str += expr[pos];
                    }
                    pos++;
                    column++;
                }
                
                if (pos < expr.length && expr[pos] === '"') {
                    pos++;
                    column++;
                }
                
                // 在表达式中，字符串常量被当作0处理（C语言标准）
                tokens.push({ type: 'number', value: 0, line, column: startCol });
                continue;
            }
            
            // 字符字面量
            if (char === "'") {
                const startCol = column;
                pos++;
                column++;
                
                if (pos < expr.length) {
                    if (expr[pos] === '\\') {
                        pos++;
                        column++;
                        if (pos < expr.length) {
                            pos++;
                            column++;
                        }
                    } else {
                        pos++;
                        column++;
                    }
                }
                
                if (pos < expr.length && expr[pos] === "'") {
                    pos++;
                    column++;
                }
                
                // 字符常量转换为对应的ASCII值
                tokens.push({ type: 'number', value: 0, line, column: startCol });
                continue;
            }
            
            // 运算符
            if (this.isOperator(char)) {
                let op = char;
                const startCol = column;
                
                // 检查双字符运算符
                if (pos + 1 < expr.length) {
                    const twoCharOp = char + expr[pos + 1];
                    if (['==', '!=', '<=', '>=', '&&', '||', '<<', '>>'].includes(twoCharOp)) {
                        op = twoCharOp;
                        pos += 2;
                        column += 2;
                        tokens.push({ type: 'operator', value: op, line, column: startCol });
                        continue;
                    }
                }
                
                // 单字符运算符
                if (['+', '-', '*', '/', '%', '!', '~', '&', '|', '^', '<', '>', '=', '?', ':'].includes(char)) {
                    pos++;
                    column++;
                    tokens.push({ type: 'operator', value: op, line, column: startCol });
                    continue;
                }
            }
            
            // 括号
            if (char === '(' || char === ')') {
                tokens.push({ type: 'paren', value: char, line, column });
                pos++;
                column++;
                continue;
            }
            
            // 未知字符
            tokens.push({ type: 'unknown', value: char, line, column });
            pos++;
            column++;
        }
        
        return tokens;
    }

    
    // 将中缀表达式转换为后缀表达式（逆波兰表示法）
    infixToPostfix(infixTokens) {
        const output = [];
        const operators = [];
        
        for (const token of infixTokens) {
            if (token.type === 'number') {
                output.push(token);
            } else if (token.type === 'identifier') {
                output.push(token);
            } else if (token.type === 'operator') {
                const op = token.value;
                
                // 处理单目运算符（优先级最高）
                if (this.isUnaryOperator(op)) {
                    // 单目运算符直接入栈
                    operators.push(token);
                } else if (this.isBinaryOperator(op)) {
                    // 处理双目运算符
                    while (operators.length > 0) {
                        const top = operators[operators.length - 1];
                        if (top.type === 'operator' && 
                            this.getOperatorPrecedence(top.value) >= this.getOperatorPrecedence(op)) {
                            output.push(operators.pop());
                        } else {
                            break;
                        }
                    }
                    operators.push(token);
                }
            } else if (token.type === 'paren') {
                if (token.value === '(') {
                    operators.push(token);
                } else if (token.value === ')') {
                    // 弹出直到遇到左括号
                    while (operators.length > 0 && operators[operators.length - 1].value !== '(') {
                        output.push(operators.pop());
                    }
                    // 弹出左括号
                    if (operators.length > 0 && operators[operators.length - 1].value === '(') {
                        operators.pop();
                    }
                    // 如果栈顶是单目运算符，也弹出
                    if (operators.length > 0 && operators[operators.length - 1].type === 'operator' && 
                        this.isUnaryOperator(operators[operators.length - 1].value)) {
                        output.push(operators.pop());
                    }
                }
            }
        }
        
        // 弹出剩余的运算符
        while (operators.length > 0) {
            output.push(operators.pop());
        }
        
        return output;
    }
    
    // 计算后缀表达式
    evaluatePostfix(postfixTokens) {
        const stack = [];
        
        for (const token of postfixTokens) {
            if (token.type === 'number') {
                stack.push(token.value);
            } else if (token.type === 'identifier') {
                // 宏替换：如果标识符是已定义的宏，则替换为其值，否则替换为0
                const macroValue = this.macros[token.value];
                if (macroValue !== undefined) {
                    // 尝试将宏值转换为数字
                    const num = parseFloat(macroValue);
                    stack.push(isNaN(num) ? 0 : num);
                } else {
                    // 未定义的宏被视为0
                    stack.push(0);
                }
            } else if (token.type === 'operator') {
                const op = token.value;
                
                if (this.isUnaryOperator(op)) {
                    if (stack.length < 1) {
                        throw new Error(`Insufficient operands for unary operator ${op}`);
                    }
                    const operand = stack.pop();
                    
                    switch (op) {
                        case '+': stack.push(+operand); break;
                        case '-': stack.push(-operand); break;
                        case '!': stack.push(operand ? 0 : 1); break;
                        case '~': stack.push(~operand); break;
                        default: throw new Error(`Unknown unary operator: ${op}`);
                    }
                } else if (this.isBinaryOperator(op)) {
                    if (stack.length < 2) {
                        throw new Error(`Insufficient operands for binary operator ${op}`);
                    }
                    const right = stack.pop();
                    const left = stack.pop();
                    
                    switch (op) {
                        case '+': stack.push(left + right); break;
                        case '-': stack.push(left - right); break;
                        case '*': stack.push(left * right); break;
                        case '/': 
                            if (right === 0) {
                                throw new Error('Division by zero');
                            }
                            stack.push(Math.trunc(left / right)); // 整数除法
                            break;
                        case '%': 
                            if (right === 0) {
                                throw new Error('Modulo by zero');
                            }
                            stack.push(left % right);
                            break;
                        case '<<': stack.push(left << right); break;
                        case '>>': stack.push(left >> right); break;
                        case '&': stack.push(left & right); break;
                        case '|': stack.push(left | right); break;
                        case '^': stack.push(left ^ right); break;
                        case '&&': stack.push(left && right ? 1 : 0); break;
                        case '||': stack.push(left || right ? 1 : 0); break;
                        case '==': stack.push(left === right ? 1 : 0); break;
                        case '!=': stack.push(left !== right ? 1 : 0); break;
                        case '<': stack.push(left < right ? 1 : 0); break;
                        case '>': stack.push(left > right ? 1 : 0); break;
                        case '<=': stack.push(left <= right ? 1 : 0); break;
                        case '>=': stack.push(left >= right ? 1 : 0); break;
                        default: throw new Error(`Unknown binary operator: ${op}`);
                    }
                }
            }
        }
        
        if (stack.length !== 1) {
            throw new Error('Invalid expression');
        }
        
        return stack[0];
    }
    
    // 计算表达式
    evaluateExpression(expr) {
        try {
            // 1. 词法分析表达式
            const tokens = this.tokenizeExpression(expr);
            
            // 2. 转换为后缀表达式
            const postfix = this.infixToPostfix(tokens);
            
            // 3. 计算后缀表达式
            const result = this.evaluatePostfix(postfix);
            
            return result;
        } catch (error) {
            this.skipping = false;
            throw new Error(`Expression evaluation error: ${error.message}`);
        }
    }

    // -- End --

    async tokenize() {
        /**
         * @type {Token[]}
         */
        this.tokens = [];
        this.errors = [];
        this.pragmas = [];
        this.position = 0;
        this.line = 1;
        this.column = 1;
        //this.macros = {};
        //this.macroParams = {};
        this.conditionalStack = [];
        this.skipping = false;

        while (this.position < this.sourceCode.length) {
            const char = this.sourceCode[this.position];
            
            if (this.isWhitespace(char)) {
                this.skipWhitespace();
            } else if (char === '/' && this.peek() === '/') {
                this.skipSingleLineComment();
            } else if (char === '/' && this.peek() === '*') {
                this.skipMultiLineComment();
            } else if (char === '#') {
                // 遇到预处理器指令
                await this.readPreprocessorDirective();
            } else if (char === '"') {
                this.readString();
            } else if (char === "'") {
                this.readCharacter();
            } else if (this.isDigit(char)) {
                this.readNumber();
            } else if (this.isIdentifierStart(char)) {
                await this.readIdentifier();
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

            if (this.suppressMacroExpansion) {
                this.skipping = false;  // Always false!
            }
        }

        this.tokens.push(new Token(TokenType.EOF, '', this.getLocation()));

        // Exclude all preprocessing factors afterwards
        this.tokens = this.tokens.filter(token => (!token.type.startsWith("PREPROCESSOR_")));

        return {
            tokens: this.tokens,
            errors: this.errors,
            pragmas: this.pragmas, // 返回收集到的 pragma 列表
            extraExtensions: this.extraExtensions
        };
    }
    
    // 新增：读取预处理器指令
    async readPreprocessorDirective() {
        const startLocation = this.getLocation();
        this.advance(); // 跳过 '#'
        
        // 跳过指令前的空白
        this.skipWhitespaceOnly();
        
        // 读取指令名称
        let directive = '';
        while (this.position < this.sourceCode.length && this.isIdentifierPart(this.sourceCode[this.position])) {
            directive += this.sourceCode[this.position];
            this.advance();
        }
        
        // 读取指令后的剩余部分（参数），直到行尾
        this.skipWhitespaceOnly();
        let args = '';
        while (this.position < this.sourceCode.length && this.sourceCode[this.position] !== '\n') {
            args += this.sourceCode[this.position];
            this.advance();
        }
        // 跳过换行符（它会在外层循环被当作空白处理）
        
        const directiveTokenType = KEYWORDS[`#${directive}`];
        if (!directiveTokenType || !directiveTokenType.startsWith('PREPROCESSOR_')) {
            this.addError(`Unknown preprocessor directive: #${directive}`, startLocation);
            // 仍然生成一个通用指令token
            this.tokens.push(new Token(TokenType.PREPROCESSOR_DIRECTIVE, `#${directive} ${args}`.trim(), startLocation));
            return;
        }
        
        // 根据指令类型调用不同的处理器
        switch (directiveTokenType) {
            case TokenType.PREPROCESSOR_DEFINE:
                this.handleDefine(args, startLocation);
                break;
            case TokenType.PREPROCESSOR_UNDEF:
                this.handleUndef(args, startLocation);
                break;
            case TokenType.PREPROCESSOR_IF:
                this.handleIf(args, startLocation);
                break;
            case TokenType.PREPROCESSOR_ELSE:
                this.handleElse(args, startLocation);
                break;
            case TokenType.PREPROCESSOR_IFDEF:
                this.handleIfdef(args, startLocation);
                break;
            case TokenType.PREPROCESSOR_IFNDEF:
                this.handleIfndef(args, startLocation);
                break;
            case TokenType.PREPROCESSOR_ENDIF:
                this.handleEndif(args, startLocation);
                break;
            case TokenType.PREPROCESSOR_INCLUDE:
                await this.handleInclude(args, startLocation);
                break;
            case TokenType.PREPROCESSOR_PRAGMA:
                this.handlePragma(args, startLocation);
                break;
            default:
                // 不应该到达这里
                this.tokens.push(new Token(directiveTokenType, `#${directive} ${args}`.trim(), startLocation));
        }
    }
    
    // 处理 #define
    // Modified in 2nd conversation
    handleDefine(args, location) {
        if (this.skipping) {
            this.tokens.push(new Token(TokenType.PREPROCESSOR_DEFINE, `#define ${args}`.trim(), location));
            return;
        }
        
        const trimmedArgs = args.trim();
        const firstSpace = trimmedArgs.indexOf(' ');

        const resolveParameters = (text) => {
            const firstParen = trimmedArgs.indexOf('(');
            const endParen = trimmedArgs.substring(0, firstSpace).lastIndexOf(')');
            if (firstParen === -1) {
                return {
                    name: text,
                    parameters: []
                };
            } else {
                return {
                    name: text.substring(0, firstParen),
                    parameters: trimmedArgs.substring(firstParen + 1, endParen).split(',')
                    .map(content => content.trim())
                };
            }
        };
        
        if (firstSpace === -1) {
            // 没有值的宏定义（如 #define DEBUG）
            const macroName = trimmedArgs;
            const macroInfo = resolveParameters(macroName);
            this.macros[macroInfo.name] = '';
            this.macroParams[macroInfo.name] = macroInfo.parameters;
        } else {
            // 有值的宏定义
            const macroName = trimmedArgs.substring(0, firstSpace);
            const macroInfo = resolveParameters(macroName);
            let macroValue = trimmedArgs.substring(firstSpace + 1).trim();
            
            if (macroValue.startsWith('(') && macroValue.endsWith(')')) {
                macroValue = macroValue.substring(1, macroValue.length - 1);
            }
            // Parameter list?
            this.macroParams[macroInfo.name] = macroInfo.parameters;
            this.macros[macroInfo.name] = macroValue;
        }
        
        this.tokens.push(new Token(TokenType.PREPROCESSOR_DEFINE, `#define ${args}`.trim(), location));
    }
    
    // 处理 #undef
    handleUndef(args, location) {
        if (this.skipping) {
            this.tokens.push(new Token(TokenType.PREPROCESSOR_UNDEF, `#undef ${args}`.trim(), location));
            return;
        }
        const macroName = args.trim();
        if (macroName) {
            delete this.macros[macroName];
        }
        this.tokens.push(new Token(TokenType.PREPROCESSOR_UNDEF, `#undef ${args}`.trim(), location));
    }
    
    // Replaced in a new conversation
    handleIf(args, location) {
        const condition = args.trim();
        
        if (this.skipping) {
            // 如果已经在跳过块中，仍然需要处理条件栈
            this.conditionalStack.push(this.skipping);
            this.skipping = true;
            this.tokens.push(new Token(TokenType.PREPROCESSOR_IF, `#if ${args}`.trim(), location));
            return;
        }
        
        try {
            // 计算表达式
            const result = this.evaluateExpression(condition);
            
            // 在C语言中，0为假，非0为真
            const conditionValue = result !== 0;
            
            // 将当前 skipping 状态压栈
            this.conditionalStack.push(this.skipping);
            // 更新 skipping 状态
            this.skipping = !conditionValue;
            
            this.tokens.push(new Token(TokenType.PREPROCESSOR_IF, `#if ${args}`.trim(), location));
        } catch (error) {
            this.addError(`Error evaluating #if expression: ${error.message}`, location);
            // this.conditionalStack.push(this.skipping);
            // this.skipping = true;
            this.skipping = false;
            this.tokens.push(new Token(TokenType.PREPROCESSOR_IF, `#if ${args}`.trim(), location));
        }
    }
    
    // 处理 #ifdef
    handleIfdef(args, location) {
        // Preventing embedding problem
        if (this.skipping) {
            this.conditionalStack.push(this.skipping);
            this.skipping = true;
            this.tokens.push(new Token(TokenType.PREPROCESSOR_IFDEF, `#ifdef ${args}`.trim(), location));
            return;
        }

        const macroName = args.trim();
        const isDefined = this.macros.hasOwnProperty(macroName);
        
        this.conditionalStack.push(this.skipping);
        this.skipping = this.skipping || !isDefined;
        this.tokens.push(new Token(TokenType.PREPROCESSOR_IFDEF, `#ifdef ${args}`.trim(), location));
    }
    
    // 处理 #ifndef
    handleIfndef(args, location) {
        if (this.skipping) {
            this.conditionalStack.push(this.skipping);
            this.skipping = true;
            this.tokens.push(new Token(TokenType.PREPROCESSOR_IFNDEF, `#ifndef ${args}`.trim(), location));
            return;
        }

        const macroName = args.trim();
        const isDefined = this.macros.hasOwnProperty(macroName);
        
        this.conditionalStack.push(this.skipping);
        this.skipping = this.skipping || isDefined;
        this.tokens.push(new Token(TokenType.PREPROCESSOR_IFNDEF, `#ifndef ${args}`.trim(), location));
    }
    
    // 处理 #endif
    handleEndif(args, location) {
        if (this.conditionalStack.length === 0) {
            this.addError(`#endif without matching #if`, location);
        } else {
            this.skipping = this.conditionalStack.pop();
        }
        this.tokens.push(new Token(TokenType.PREPROCESSOR_ENDIF, `#endif ${args}`.trim(), location));
    }

    handleElse(args, location) {
        if (this.conditionalStack.length === 0) {
            this.addError(`#else without matching #if`, location);
        } else if (!this.conditionalStack[this.conditionalStack.length - 1]) {
            this.skipping = !this.skipping;
        }
        this.tokens.push(new Token(TokenType.PREPROCESSOR_ELSE, `#else ${args}`.trim(), location));
    }
    
    // 处理 #pragma - 收集指令内容
    handlePragma(args, location) {
        const fullPragma = `#pragma ${args}`.trim();
        this.pragmas.push(fullPragma);
        this.tokens.push(new Token(TokenType.PREPROCESSOR_PRAGMA, fullPragma, location));
    }
    
    // 处理 #include - 占位实现，实际需要异步网络请求
    /**
     * **To be done.** Personally, I think we can simply use `XMLHttpRequest`. I will do that then
     * @param {string} args 
     * @param {*} location 
     * @todo
     */
    async handleInclude(args, location) {
        this.tokens.push(new Token(TokenType.PREPROCESSOR_INCLUDE, `#include ${args}`.trim(), location));
        
        // 在实际实现中，这里应该：
        // 1. 解析文件名（去除引号或尖括号）
        // 2. 使用 fetch(`/include/${filename}`) 获取文件内容
        // 3. 递归调用词法分析器分析包含的文件内容
        // 4. 将得到的 tokens 插入到当前 tokens 流中的适当位置
        // 注意：这需要将 tokenize 方法改为异步，或在此处进行同步的 XMLHttpRequest（不推荐）。
        const synchronouslyFetch = path => {
            let xhr = new XMLHttpRequest();
            xhr.open('GET', path, false);
            xhr.send(null);
            if (xhr.status >= 400) {
                // Error occurred
                throw `XHR Failure: ${xhr.responseText}`;
            }
            return xhr.responseText;
        };
        
        const match = args.trim().match(/^["<](.+?)[">]$/);
        if (match) {
            const filename = match[1];
            try {
                // 假设有同步获取文本的方法（在实际浏览器中需使用异步 fetch 并配合 async/await）
                
                if (filename.endsWith('.js')) {
                    // You must ensure safety on your own!!
                    /*
                    let responseCode = responseText.split('\n')
                                                    .filter(code => !code.startsWith('import')).join('\n');
                    this.extraExtensions.push(eval(`${responseCode}; extension`));
                    */
                    const module = await import(`/include/${filename}`);
                    this.extraExtensions.push(module.extension);
                    // Errors will be thrown through!
                } else {
                    const responseTextRaw = await fetch(`/include/${filename}`);
                    if (!responseTextRaw.ok) {
                        this.addError(`Cannot include file ${filename}`);
                        return;
                    }
                    const responseText = await responseTextRaw.text();
                    const includedLexer = new Lexer(responseText, this.macros, false, this.macroParams);
                    // 需要合并状态，如宏定义
                    const includedResult = await includedLexer.tokenize();
                    // 合并 tokens, errors, pragmas
                    if (!this.suppressMacroExpansion) {
                        this.tokens.push(...includedResult.tokens.filter(token => token.type != TokenType.EOF)
                            .map(token => {
                            return {
                                ...token,
                                location: {
                                    ...token.location,
                                    filename: token.location.filename ?? filename
                                }
                            }
                        })); // (In fact I can simply append it there)
                    }
                    
                    this.errors.push(...includedResult.errors);
                    this.pragmas.push(...includedResult.pragmas);
                    this.macros = {
                        ...this.macros,
                        ...includedLexer.macros
                    };
                    this.macroParams = {
                        ...this.macroParams,
                        ...includedLexer.macroParams
                    };
                    // 注意：需要调整合并后 token 的位置信息
                }
                
            } catch (e) {
                this.addError(`Cannot open included file: ${filename}`, location);
            }
        } else {
            this.addError(`Invalid #include directive`, location);
        }
        
    }
    
    // 辅助方法：仅跳过空白，不跳过换行（用于预处理器行内）
    skipWhitespaceOnly() {
        while (this.position < this.sourceCode.length && 
               (this.sourceCode[this.position] === ' ' || this.sourceCode[this.position] === '\t')) {
            this.advance();
        }
    }

    // 修改现有方法，在 skipping 状态下跳过普通 token 的生成（但不跳过移动指针和注释）
    readString() {
        if (this.skipping) {
            // 跳过整个字符串字面量而不生成 token
            const start = this.position;
            this.advance(); // 跳过引号
            while (this.position < this.sourceCode.length && this.sourceCode[this.position] !== '"') {
                if (this.sourceCode[this.position] === '\\') {
                    this.advance(); // 跳过转义字符
                }
                this.advance();
            }
            if (this.position < this.sourceCode.length && this.sourceCode[this.position] === '"') {
                this.advance(); // 跳过闭合引号
            }
            return;
        }
        // 原有的 readString 逻辑...
        const startLocation = this.getLocation();
		let rawValue = '"';	// Used for highlighter!
        this.advance(); // 跳过开头的 '"'
        let value = '';
        let escaped = false;

        while (this.position < this.sourceCode.length) {
            const char = this.sourceCode[this.position];
            rawValue += char;
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
                this.tokens.push(new Token(TokenType.STRING, value, startLocation, rawValue));
                return;
            } else {
                value += char;
            }
            
            this.advance();
        }

        this.addError('Unterminated string literal', startLocation);
        this.tokens.push(new Token(TokenType.STRING, value, startLocation, rawValue));
    }

    readCharacter() {
        if (this.skipping) {
            // 跳过字符字面量
            const start = this.position;
            this.advance(); // 跳过单引号
            if (this.position < this.sourceCode.length && this.sourceCode[this.position] === '\\') {
                this.advance(); // 跳过转义反斜杠
                if (this.position < this.sourceCode.length) this.advance(); // 跳过转义字符
            } else if (this.position < this.sourceCode.length) {
                this.advance(); // 跳过普通字符
            }
            if (this.position < this.sourceCode.length && this.sourceCode[this.position] === "'") {
                this.advance(); // 跳过闭合单引号
            }
            return;
        }
        // 原有的 readCharacter 逻辑...
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
        if (this.skipping) {
            // 跳过数字
            while (this.position < this.sourceCode.length && this.isDigit(this.sourceCode[this.position])) {
                this.advance();
            }
            // 可能的后缀和小数点等，这里简化处理
            return;
        }
        // 原有的 readNumber 逻辑...
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

    /**
     * This function has some changes done on my own.
     * @returns {void}
     */
    async readIdentifier() {
        const startLocation = this.getLocation();
        let value = '';

        while (this.position < this.sourceCode.length && this.isIdentifierPart(this.sourceCode[this.position])) {
            value += this.sourceCode[this.position];
            this.advance();
        }

        // 在 skipping 状态下，不生成大多数标识符的 token，但预处理器指令关键字仍需处理（它们已在 readPreprocessorDirective 中处理）
        if (this.skipping) {
            return;
        }

        // Added on my own: check if it is a macro
        if (this.macros[value]) {
            // read braces, etc.
            let repls = {...this.macros};
            //repls[value] = undefined;       // No recursive replacement
            repls[value] = value;
            if (this.position < this.sourceCode.length && this.sourceCode[this.position] == '(') {
                // read parameters split by comma (must consider braces/parents/...)
                const PAREN_OPENERS = ['(', '{', '['];
                const PAREN_CLOSERS = [')', '}', ']'];
                //this.position++;
                this.advance();
                let braceLayer = 1, currentParameter = "", parameters = [];
                while (this.position < this.sourceCode.length && braceLayer > 0) {
                    if (braceLayer === 1 && this.sourceCode[this.position] === ',') {
                        parameters.push(currentParameter);
                        currentParameter = "";
                    } else {
                        if (PAREN_OPENERS.includes(this.sourceCode[this.position])) {
                            braceLayer++;
                        } else if (PAREN_CLOSERS.includes(this.sourceCode[this.position])) {
                            braceLayer--;
                            if (braceLayer <= 0) {
                                parameters.push(currentParameter);
                                this.advance();
                                break;
                            }
                        }
                        currentParameter += this.sourceCode[this.position];
                    }
                    this.advance();
                }
                // Might be irregular if not exited through breaking
                for (let i = 0; i < this.macroParams[value].length && i < parameters.length; i++) {
                    repls[this.macroParams[value][i]] = parameters[i];
                }
            }
            if (!this.suppressMacroExpansion) {
                const macroProcessor = new Lexer(this.macros[value], repls, false, this.macroParams);
                const macroResult = await macroProcessor.tokenize();
                this.tokens.push(...macroResult.tokens.filter(token => token.type != TokenType.EOF));
                this.errors.push(...macroResult.errors);
            }
        } else if (KEYWORDS[value]) {
            this.tokens.push(new Token(KEYWORDS[value], value, startLocation));
        } else if (SPECIAL_INSTRUCTIONS[value]) {
            this.tokens.push(new Token(SPECIAL_INSTRUCTIONS[value], value, startLocation));
        } else {
            this.tokens.push(new Token(TokenType.IDENTIFIER, value, startLocation));
        }
    }

    readPredefinedConstant() {
        if (this.skipping) {
            // 跳过预定义常量
            this.advance(); // 跳过 '@'
            while (this.position < this.sourceCode.length && this.isIdentifierPart(this.sourceCode[this.position])) {
                this.advance();
            }
            return;
        }
        // 原有的 readPredefinedConstant 逻辑...
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
        // 原有的 readOperator 逻辑...
        const startLocation = this.getLocation();
        let value = this.sourceCode[this.position];
        
        // 检查双字符运算符
        const twoCharOp = value + (this.peek() || '');
        if (OPERATORS[twoCharOp]) {
            value = twoCharOp;
            this.advance(); // 额外前进一次
        }
        
        this.advance(); // 前进到下一个字符
        if (this.skipping) return; // 跳过运算符
        this.tokens.push(new Token(OPERATORS[value], value, startLocation));
    }

    readPunctuator() {
        // 原有的 readPunctuator 逻辑...
        const startLocation = this.getLocation();
        let value = this.sourceCode[this.position];
        
        // 检查双字符标点符号（如 ->）
        const twoCharPunct = value + (this.peek() || '');
        if (PUNCTUATORS[twoCharPunct]) {
            value = twoCharPunct;
            this.advance(); // 额外前进一次
        }
        
        this.advance(); // 前进到下一个字符
        if (this.skipping) return; // 跳过标点符号
        this.tokens.push(new Token(PUNCTUATORS[value], value, startLocation));
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
import { AttributeClass, ASTNode,
    CastExpression, TypeSpecifierNode, 
    ASTBuilder, __convert, objectList,
	liquidList, unitList, buildingList
 } from "./mindcBase.js";

import { TokenType, Lexer } from "./mindcLexer.js";

export class Parser {
	/**
	 * 
	 * @param {Lexer} lexer 
	 * @param {AttributeClass} config 
	 */
    constructor(lexer, config = new AttributeClass()) {
        this.lexer = lexer;
        this.tokens = [];
        this.currentTokenIndex = 0;
        this.errors = [];
        this.currentScope = null;
		
		// 添加已知类型集合
        this.knownTypeNames = new Set([
            'int', 'char', 'float', 'void', 'double', 'long', 'short',
            'signed', 'unsigned', 'device', 'null_t', 'content_t',
			'item_t', 'liquid_t', 'unit_t', 'block_t', ...(config.getAttribute('extraTypes').map(
				/**
				 * 
				 * @param {TypeInfo} type 
				 * @returns {string}
				 */
				type => type.name
			) ?? [])
        ]);
		this.extraFunctions = config.getAttribute('extraFunctions') ?? [];
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
			typeSpecifier.setAttribute('qualifiers', parsedFunction.storageClass);
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
				bitFieldExpr.parent = declarator;
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
		
		const storageTokens = ['static', 'register', 'extern', 'auto'];

		const typeTokens = [
            TokenType.INT, TokenType.CHAR, TokenType.FLOAT, TokenType.VOID, TokenType.BOOL,
            TokenType.DOUBLE, TokenType.LONG, TokenType.SHORT, TokenType.SIGNED, TokenType.UNSIGNED,
            TokenType.STRUCT, TokenType.UNION, TokenType.ENUM
        ];
		
		let checkSpecialTypes = true, doneTypeFetch = false;
		let typeNode = null;
		let storageClass = '';

		for (const storageToken of storageTokens) {
			if (qualifiers.includes(storageToken)) {
				storageClass = storageToken;
			}
		}

        for (const typeToken of typeTokens) {
            if (this.matchToken(typeToken)) {
                const token = this.consumeToken();
                typeNode = ASTBuilder.typeSpecifier(token.value);
                typeNode.setAttribute('location', token.location);
                typeNode.setAttribute('qualifiers', qualifiers);
				typeNode.storageClass = storageClass;
                
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
			const specials = ['device', 'null_t', 'content_t', 'item_t', 'liquid_t', 'unit_t', 'block_t']
            if (specials.includes(token.value)) {
                this.consumeToken();
                typeNode = ASTBuilder.typeSpecifier(token.value);
                typeNode.setAttribute('location', token.location);
                typeNode.setAttribute('qualifiers', qualifiers);
				typeNode.storageClass = storageClass;
				if (storageClass === 'auto') {
					typeNode.setAttribute('isAutoDevice', true);
				}
                return typeNode;
            }
            
			if (!this.knownTypeNames.has(token.value)) {
				this.addError(`Unknown type ${token.value}`);
				return null;
			}

            // 可能是通过typedef定义的类型别名
            typeNode = ASTBuilder.typeSpecifier(token.value);
            typeNode.setAttribute('location', token.location);
            typeNode.setAttribute('qualifiers', qualifiers);
            typeNode.setAttribute('isTypedef', true);
			typeNode.setAttribute('isCustomType', true);
			typeNode.storageClass = storageClass;
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
							if (this.matchToken(TokenType.IDENTIFIER))
								this.consumeToken();	// Last parameter identifier is to be consumed !!!
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
        
		if (!declarator) return null;
        // 设置指针信息
        declarator.pointerDepth = pointerDepth;
        declarator.pointerQualifiers = pointerQualifiers;

		if (!isFunctionParam) {
			if (this.matchToken(TokenType.ASSIGN)) {
				this.consumeToken();
				declarator.initializer = this.parseExpression();
				if (declarator.initializer)
					declarator.initializer.parent = declarator;
			}
		}
        
        return declarator;
    }

    parseTypeQualifiers() {
        const qualifiers = [];
        while (this.matchTokenTypeAt(this.currentTokenIndex, [
            TokenType.CONST, TokenType.VOLATILE,
            TokenType.STATIC, TokenType.EXTERN, TokenType.AUTO, TokenType.REGISTER,
			TokenType.NEAR
        ])) {
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
            TokenType.INT, TokenType.CHAR, TokenType.FLOAT, TokenType.VOID, TokenType.BOOL,
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
		varDecl.storageClass = type.storageClass;
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
			varDeclarator.initializer = declarator.initializer;
			
			if (type.pointerDepth > 0) {
				// Only for the first
				varDeclarator.pointerDepth += type.pointerDepth;
				type.pointerDepth = 0;
				varDeclarator.pointerQualifiers = varDeclarator.pointerQualifiers.concat(type.pointerQualifiers);
				type.pointerQualifiers = [];
			}
            
			// This is a little bit different from pure C89
            // 处理初始化
			/*
            if (this.matchToken(TokenType.ASSIGN)) {
                this.consumeToken();
                varDeclarator.initializer = this.parseExpression();
				varDeclarator.initializer.parent = varDeclarator;
            }
            */
            declarators.push(varDeclarator);
			varDeclarator.parent = varDecl;
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
			declarator.initializer.parent = declarator;
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
			case TokenType.BOOL:
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
                //if (this.isTypeName(token.value)) {
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
                //}
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
            'signed', 'unsigned', 'device', 'null_t', 'content_t',
			'item_t', 'liquid_t', 'unit_t', 'block_t'
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

		// A known type is necessary
        if (!this.knownTypeNames.has(name)) {
            return false;
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
            : this.parseExpressionStatement(true, true);
        
        const test = this.matchToken(TokenType.SEMICOLON)
            ? null
            : this.parseExpression();
        this.expectToken(TokenType.SEMICOLON);
        
        const update = this.matchToken(TokenType.RIGHT_PAREN)
            ? null
            : this.parseExpressionStatement(false, true);
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
		if (argument) argument.parent = returnStmt;
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

    parseExpressionStatement(expectSemicolon = true, expectSingle = false) {
        const expression = this.parseExpression();
        if (!expression) return null;

		let sequence = [expression];
		while (this.getCurrentToken().type == TokenType.COMMA) {
			this.consumeToken();
			const right = this.parseExpression();
			if (!right) {
                this.addError('Expected expression after comma');
                return left;
            }
			sequence.push(right);
		}
		if (expectSemicolon) this.expectToken(TokenType.SEMICOLON);
		if (expectSingle && sequence.length == 1) return expression;
        const exprStmt = new ASTNode('ExpressionStatement');
        exprStmt.children = sequence;
		sequence.forEach(ast => ast.parent = exprStmt);
        exprStmt.location = expression.location;
        return exprStmt;
    }

    parseExpression() {
		return this.parseAssignmentExpression();

    }

    parseAssignmentExpression() {
        const left = this.parseInitializerList();
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

	/**
	 * Parse initializer list (manually implemented.)
	 */
	parseInitializerList() {
		if (this.matchToken(TokenType.LEFT_BRACE)) {
			let initList = ASTBuilder.initializerList();
			this.consumeToken();
			while (true) {
				const expr = this.parseInitializerList();
				initList.addChild(expr);
				if (this.matchToken(TokenType.RIGHT_BRACE)) {
					break;	// End of the list
				}
				this.expectToken(TokenType.COMMA);	// already consuming
			}
			this.consumeToken();	// consume '}'
			return initList;
		}

		return this.parseConditionalExpression();
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
			test.parent = conditional;
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

            const logicalExpr = ASTBuilder.binaryExpression(operatorToken.value, left, right);
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

            const logicalExpr = ASTBuilder.binaryExpression(operatorToken.value, left, right);
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
				TokenType.INT, TokenType.CHAR, TokenType.FLOAT, TokenType.VOID, TokenType.BOOL,
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
					//typeNode.qualifiers = qualifiers;

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
					//typeNode.qualifiers = qualifiers;
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
						const castExpr = new CastExpression(typeNode, expression);
						//castExpr.addChild(typeNode);
						castExpr.setAttribute('pointerDepth', typeNode.pointerDepth);
						castExpr.setAttribute('qualifiers', typeNode.pointerQualifiers);	// Because they are pointer qualifiers
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
                            if (arg) {
								builtinCall.arguments.push(arg);
								arg.parent = builtinCall;
							}
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
                            if (arg) {
								callExpr.arguments.push(arg);
								arg.parent = callExpr;
							}
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
			// These cases handles builtin functions.
			// Why aren't they identifiers?
			case TokenType.ASM:
			case TokenType.DRAW:
			case TokenType.PRINT:
			case TokenType.PRINTCHAR:
			case TokenType.FORMAT:
			case TokenType.DRAWFLUSH:
			case TokenType.PRINTFLUSH:
			case TokenType.GETLINK:
			case TokenType.UNITBIND:
			case TokenType.UNITCONTROL:
			case TokenType.UNITRADAR:
			case TokenType.UNITLOCATE:
			case TokenType.CONTROL:
			case TokenType.RADAR:
			case TokenType.SENSOR:
			case TokenType.SET:
			case TokenType.OP:
			case TokenType.LOOKUP:
			case TokenType.WAIT:
			case TokenType.STOP:
			case TokenType.END:
			case TokenType.JUMP:
			case TokenType.READ:
			case TokenType.WRITE:
            case TokenType.IDENTIFIER: {
                this.consumeToken();
                return ASTBuilder.identifier(token.value).setAttribute('location', token.location);
            }
            
            case TokenType.NUMBER: {
                this.consumeToken();
                return ASTBuilder.numericLiteral(token.value).setAttribute('location', token.location);
            }

			case TokenType.TRUE: {
				this.consumeToken();
                return ASTBuilder.numericLiteral(true).setAttribute('location', token.location);
			}

			case TokenType.FALSE: {
				this.consumeToken();
                return ASTBuilder.numericLiteral(false).setAttribute('location', token.location);
			}
            
            case TokenType.STRING: {
                this.consumeToken();
                return ASTBuilder.stringLiteral(token.value, token.raw).setAttribute('location', token.location);
            }
            
            case TokenType.CHARACTER: {
                this.consumeToken();
                return ASTBuilder.characterLiteral(token.value, token.raw).setAttribute('location', token.location);
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
			'draw', 'print', 'drawflush', 'printflush', 'getlink', 'ceil', 'floor', 'sqrt', 'rand', 'abs', 'memcpy',
			'control', 'radar', 'sensor', 'set', 'op', 'lookup', 'ubind', 'ulocate', 'ucontrol', 'uradar', 'min', 'max',
			'wait', 'stop', 'end', 'jump', 'read', 'write', 'asm', 'sin', 'cos', 'tan', 'asin', 'acos', 'atan', 'adiff',
			'memsp', 'printchar', 'format', ...(this.extraFunctions.map(func => func.name))
		];
        return builtins.includes(name);
    }
	
	// Added at 2nd fixing conversation
	// 添加特殊内建函数处理
	parseBuiltinCall(functionName, startLocation) {
		const builtinCall = ASTBuilder.builtinCall(functionName);
		builtinCall.location = startLocation;

		this.expectToken(TokenType.LEFT_PAREN);
		
		// I don't think this is really necessary...
		switch (functionName) {
			case 'draw':
			case 'control':
			case 'ucontrol':
			case 'ulocate':
				// 第一个参数是操作名称（字符串）
				const opNameToken = this.expectToken(TokenType.STRING);
				if (opNameToken) {
					builtinCall.arguments.push(ASTBuilder.stringLiteral(opNameToken.value));
				}
				break;

			case 'radar':
			case 'uradar':
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
					if (arg) {
						builtinCall.arguments.push(arg);
						arg.parent = builtinCall;
					}
				}
			} while (this.matchToken(TokenType.COMMA) && this.consumeToken());
		}
		
		this.expectToken(TokenType.RIGHT_PAREN);
		return builtinCall;
	}
}
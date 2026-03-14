import { CompilerExtensionBase } from "../mindc.js";
import { BuiltinCallNode } from "../mindcBase.js";
import { CodeGenerator, InternalGenerationFailure } from "../mindcGenerator.js";
import { Instruction, SingleInstruction } from "../mindcGeneratorBase.js";

function getFiller(len) {
    let filler = '';
    for (let i = 0; i < len; i++) filler += `{op_r${i}} `;
    filler = filler.trimEnd();
    return filler;
}

/**
 * 
 * @param {string} operName 
 * @param {number} len 
 * @param {string?} [hasReturn=null]
 * @returns 
 */
const singleArgPacker = (operName, len, hasReturn = null) => {
    const filler = getFiller(len);
    if (hasReturn) {
        return (
            /**
             * 
             * @param {CodeGenerator} cg 
             * @param {BuiltinCallNode} ast
             */
            (cg, ast) => cg.operatesWrite(
                    cg.operatesReads(
                        ast.arguments.map(arg => cg.visit(arg)),
                        new SingleInstruction(
                            {
                                content: `${operName} {op_write} ${filler}`,
                                referrer: []
                            }
                        )
                    ),
                    cg.getTempSymbol(cg.semantic.getTypeInfo(hasReturn))
                )
            );
    } else {
        return (
            (cg, ast) => cg.operatesReads(
                    ast.arguments.map(arg => cg.visit(arg)),
                    new SingleInstruction(
                        {
                            content: `${operName} ${filler}`,
                            referrer: []
                        }
                    )
                )
            );
    }
    
}

/**
 * 
 * @param {string} funcName 
 * @param {string} operName 
 * @param {number} len 
 * @param {string[]} skipFirstArg 
 */
const multiArgPacker = (funcName, operName, len, skipFirstArg) => {
    const filler = getFiller(len);
    /**
     * 
     * @param {CodeGenerator} cg 
     * @param {BuiltinCallNode} ast 
     * @returns 
     */
    return (cg, ast) => {
        if (ast.arguments.length < 1 || ast.arguments[0].type !== 'StringLiteral') {
            throw new InternalGenerationFailure(`${funcName}() requires rule name as string literal`);
        }
        const operation = ast.arguments[0].value;
        
        return cg.operatesReads(
            [
                ...(skipFirstArg.includes(operation) ? [new Instruction([], 'x')] : []),
                ...(ast.arguments.slice(1).map(arg => cg.visit(arg))),
                ...(new Array(len).fill(new Instruction([], 'x')))
            ].slice(0, len),
            new SingleInstruction(
                {
                    content: `${operName} ${operation} ${filler}`,
                    referrer: []
                }
            )
        );
    } 
};

export const extension = new CompilerExtensionBase(
    new Map(),
    new Map(
        [['argetblock', {
            name: 'argetblock',
            returnType: 'content_t',
            parameters: ['char', 'int', 'int']
        }],
        ['arsetblock', {
            name: 'arsetblock',
            returnType: 'void',
            parameters: ['char', 'content_t', 'int', 'int', 'content_t', 'float']
        }],
        ['arspawn', {
            name: 'arspawn',
            returnType: 'device',
            parameters: ['content_t', 'int', 'int', 'float', 'content_t']
        }],
        ['arstatus', {
            name: 'arstatus',
            returnType: 'void',
            parameters: ['char'],
            hasVarArgs: true
        }],
        ['arweathersense', {
            name: 'arweathersense',
            returnType: 'bool',
            parameters: ['content_t']
        }],
        ['arweatherset', {
            name: 'arweatherset',
            returnType: 'void',
            parameters: ['content_t', 'bool']
        }],
        ['arspawnwave', {
            name: 'arspawnwave',
            returnType: 'void',
            parameters: ['int', 'int', 'bool']
        }],
        ['arsetrule', {
            name: 'arsetrule',
            returnType: 'void',
            parameters: ['char'],
            hasVarArgs: true
        }],
        ['armessage', {
            name: 'armessage',
            returnType: 'void',
            parameters: ['char', 'char'],
            hasVarArgs: true
        }],
        ['arcutscene', {
            name: 'arcutscene',
            returnType: 'void',
            parameters: ['char'],
            hasVarArgs: true
        }],
        ['areffect', {
            name: 'areffect',
            returnType: 'void',
            parameters: ['char'],
            hasVarArgs: true
        }],
        ['arexplosion', {
            name: 'arexplosion',
            returnType: 'void',
            parameters: ['content_t', 'int', 'int', 'float', 'float', 'bool', 'bool', 'bool', 'bool']
        }],
        ['arfetch', {
            name: 'arfetch',
            returnType: 'null_t',
            parameters: ['char', 'content_t', 'int', 'content_t']
        }],
        ['arsetprop', {
            name: 'arsetprop',
            returnType: 'void',
            parameters: ['content_t', 'device', 'null_t']
        }],
        ['argetflag', {
            name: 'argetflag',
            returnType: 'bool',
            parameters: ['char']
        }],
        ['arsetflag', {
            name: 'arsetflag',
            returnType: 'void',
            parameters: ['char', 'bool']
        }],
        ['arplaysound', {
            name: 'arplaysound',
            returnType: 'void',
            parameters: ['char'],
            hasVarArgs: true
        }],
        ['armakemarker', {
            name: 'armakemarker',
            returnType: 'void',
            parameters: ['char', 'int', 'int', 'int', 'bool']
        }],
        ['arsetmarker', {
            name: 'arsetmarker',
            returnType: 'void',
            parameters: ['char'],
            hasVarArgs: true
        }]]
    ),
    new Map(
        [['argetblock', 
            /**
             * 
             * @param {CodeGenerator} cg 
             * @param {BuiltinCallNode} ast 
             */
            (cg, ast) => {
                if (ast.arguments[0].type !== 'StringLiteral') {
                    throw new InternalGenerationFailure('argetblock() requires string literal block type');
                } else {
                    return cg.operatesWrite(
                        cg.operatesReads(
                            ast.arguments.slice(1).map(arg => cg.visit(arg)),
                            new SingleInstruction(
                                {
                                    content: `getblock ${ast.arguments[0].value} {op_write} {op_r0} {op_r1}`,
                                    referrer: []
                                }
                            )
                        ),
                        cg.getTempSymbol(cg.semantic.getTypeInfo('content_t'))
                    )
                }
            }
        ],
        ['arsetblock',
            /**
             * 
             * @param {CodeGenerator} cg 
             * @param {BuiltinCallNode} ast 
             */
            (cg, ast) => {
                if (ast.arguments[0].type !== 'StringLiteral') {
                    throw new InternalGenerationFailure('argetblock() requires string literal block type');
                } else {
                    return cg.operatesReads(
                        ast.arguments.slice(1).map(arg => cg.visit(arg)),
                        new SingleInstruction(
                            {
                                content: `setblock ${ast.arguments[0].value} {op_r0} {op_r1} {op_r2} {op_r3} {op_r4}`,
                                referrer: []
                            }
                        )
                    )
                }
            }
        ],
        ['arspawn',
            (cg, ast) => cg.operatesWrite(
                cg.operatesReads(
                    ast.arguments.map(arg => cg.visit(arg)),
                    new SingleInstruction(
                        {
                            content: `spawn {op_r0} {op_r1} {op_r2} {op_r3} {op_r4} {op_write}`,
                            referrer: []
                        }
                    )
                ),
                cg.getTempSymbol(cg.semantic.getTypeInfo('device'))
            )
        ],
        ['arstatus',
            (cg, ast) => {
                const prcoessor = {
                    apply: () => {
                        if (ast.arguments.length < 4 || ast.arguments[1].type !== 'StringLiteral') {
                            throw new InternalGenerationFailure('Incorrect call format for arstatus()');
                        }
                        return cg.operatesReads(
                            ast.arguments.slice(2).map(arg => cg.visit(arg)),
                            new SingleInstruction(
                                {
                                    content: `status false ${ast.arguments[1].value} {op_r0} {op_r1}`,
                                    referrer: []
                                }
                            )
                        );
                    },
                    clear: () => {
                        if (ast.arguments.length < 3 || ast.arguments[1].type !== 'StringLiteral') {
                            throw new InternalGenerationFailure('Incorrect call format for arstatus()');
                        }
                        return cg.operatesReads(
                            ast.arguments.slice(2).map(arg => cg.visit(arg)),
                            new SingleInstruction(
                                {
                                    content: `status true ${ast.arguments[1].value} {op_r0}`,
                                    referrer: []
                                }
                            )
                        );
                    }
                };
                if (ast.arguments.length < 1 || ast.arguments[0].type !== 'StringLiteral' || !prcoessor[ast.arguments[0].value]) {
                    throw new InternalGenerationFailure('arstatus() requires either "apply" or "clear" for argument 1');
                } else {
                    return prcoessor[ast.arguments[0].value]();
                }
            }
        ],
        ['arweathersense', singleArgPacker('weathersense', 1, 'bool')],
        ['arweatherset', singleArgPacker('weatherset', 2)],
        ['arspawnwave', singleArgPacker('spawnwave', 3)],
        ['arsetrule', multiArgPacker('arsetrule', 'setrule', 5, ['mapArea'])],
        ['armessage', multiArgPacker('armessage', 'message', 2, ['notify', 'mission'])],
        ['arcutscene', multiArgPacker('arcutscene', 'cutscene', 4, [])],
        ['areffect', 
            /**
             * 
             * @param {CodeGenerator} cg 
             * @param {BuiltinCallNode} ast 
             */
            (cg, ast) => {
                // I don't really understand the principle of argument 4, but now, if you deliver a string literal
                // it will make it raw.
                if (ast.arguments.length < 5 || ast.arguments[0].type !== 'StringLiteral') {
                    throw new InternalGenerationFailure('Incorrect call format for areffect()');
                }
                return cg.operatesReads(
                    [
                        ...ast.arguments.slice(1, 4).map(arg => cg.visit(arg)),
                        ...(
                            ast.arguments[4].type === 'StringLiteral' ? []
                                : [cg.visit(ast.arguments[4])]
                        )
                    ],
                    new SingleInstruction({
                        content: `effect ${ast.arguments[0].value} {op_r0} {op_r1} {op_r2} ${
                            ast.arguments[4].type === 'StringLiteral' ? ast.arguments[4].value : '{op_r3}'
                        }`,
                        referrer: []
                    })
                );
            }
        ],
        ['arexplosion', singleArgPacker('explosion', 9)],
        ['arfetch', 
            /**
             * 
             * @param {CodeGenerator} cg 
             * @param {BuiltinCallNode} ast 
             */
            (cg, ast) => {
                if (ast.arguments.length < 4 || ast.arguments[0].type !== 'StringLiteral') {
                    throw new InternalGenerationFailure('Incorrect call format for areffect()');
                }
                return cg.operatesWrite(
                    cg.operatesReads(
                        ast.arguments.slice(1).map(arg => cg.visit(arg)),
                        new SingleInstruction(
                            {
                                content: `fetch ${ast.arguments[0].value} {op_write} {op_r0} {op_r1} {op_r2}`,
                                referrer: []
                            }
                        )
                    ),
                    cg.getTempSymbol(cg.semantic.getTypeInfo('null_t'))
                );
            }
        ],
        ['arsetprop', singleArgPacker('setprop', 3)],
        ['argetflag', singleArgPacker('getflag', 1, 'bool')],
        ['arsetflag', singleArgPacker('setflag', 2)],
        ['arplaysound',
            (cg, ast) => {
                const prcoessor = {
                    global: () => {
                        if (ast.arguments.length < 5) {
                            throw new InternalGenerationFailure('Incorrect call format for arplaysound()');
                        }
                        return cg.operatesReads(
                            ast.arguments.slice(1).map(arg => cg.visit(arg)),
                            new SingleInstruction(
                                {
                                    content: `playsound false {op_r0} {op_r1} {op_r2} {op_r3} x x {op_r4}`,
                                    referrer: []
                                }
                            )
                        );
                    },
                    positional: () => {
                        if (ast.arguments.length < 6) {
                            throw new InternalGenerationFailure('Incorrect call format for arplaysound()');
                        }
                        return cg.operatesReads(
                            ast.arguments.slice(1).map(arg => cg.visit(arg)),
                            new SingleInstruction(
                                {
                                    content: `playsound true {op_r0} {op_r1} {op_r2} x {op_r3} {op_r4} {op_r5}`,
                                    referrer: []
                                }
                            )
                        );
                    }
                };
                if (ast.arguments.length < 1 || ast.arguments[0].type !== 'StringLiteral' || !prcoessor[ast.arguments[0].value]) {
                    throw new InternalGenerationFailure('arplaysound() requires either "global" or "positional" for argument 1');
                } else {
                    return prcoessor[ast.arguments[0].value]();
                }
            }
        ],
        ['armakemarker', multiArgPacker('armakemarker', 'makemarker', 4, [])],
        ['arsetmarker', multiArgPacker('arsetmarker', 'setmarker', 4, [])]
    ])
);
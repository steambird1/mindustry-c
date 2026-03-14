import { CompilerExtensionBase } from "../mindc.js";
import { BuiltinCallNode } from "../mindcBase.js";
import { CodeGenerator, InternalGenerationFailure } from "../mindcGenerator.js";
import { Instruction, InstructionBuilder, SingleInstruction, FunctionRegisterer } from "../mindcGeneratorBase.js";
import { TypeInfo } from "../mindcSemantic.js";

export const extension = new CompilerExtensionBase(
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
        [['sdlist_t', new TypeInfo('sdlist_t', 'basic', 1)]]
    ),
    new Map(
        [['sdcreate', {
            name: 'sdcreate',
            returnType: 'sdlist_t',
            parameters: ['int', 'int']					// sdlist_t sdcreate(int name, int size)
        }],
        ['sdread', {
            name: 'sdread',
            returnType: 'null_t',
            parameters: ['device', 'sdlist_t', 'int']		// null_t sdread(device target, sdlist_t list, int pos)
        }],
        ['sdwrite', {
            name: 'sdwrite',
            returnType: 'void',
            parameters: ['device', 'sdlist_t', 'int', 'null_t']	// void sdwrite(device target, sdlist_t list, int pos, null_t data)
        }]]
    ),
    new Map(
        [['sdcreate',
            /**
             * 
             * @param {CodeGenerator} cg 
             * @param {BuiltinCallNode} ast 
             */
            (cg, ast) => {
                if (ast.arguments.length != 2 
                    || ast.arguments[0].type !== 'NumericLiteral'
                    || ast.arguments[1].type !== 'NumericLiteral') {
                    throw new InternalGenerationFailure(`Incorrect format`, ast.location);
                }
                /**
                 * @type {number}
                 */
                const arraySize = ast.arguments[1].value;
                if (arraySize <= 0) {
                    throw new InternalGenerationFailure(`Inappropriate array size`, ast.location)
                }
                
                const randomIdentifier = ast.arguments[0].value;
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
                const tmpReturner = cg.getTempSymbol(cg.semantic.getTypeInfo('sdlist_t'));
                const returnedObject = new Instruction([
                    InstructionBuilder.op('mul', '__putmp', `_${readerName}`, 16384),
                    InstructionBuilder.op('add', '__putmp', '__putmp', `_${writerName}`),
                    cg.generateSymbolWrite(tmpReturner, '__putmp')
                ]).set_returns(tmpReturner);

                const reader = generateOperations(0, arraySize - 1, InstructionBuilder.read('__return', '__device', `"_pu_${randomIdentifier}_{sval}"`))
                        .raw_replace('input', '__pos').raw_replace('exit', 
                            InstructionBuilder.set('@counter', '__stackpos').content
                        );
                funcManager.addFunction(readerName, reader, null, [], true);
                const writer = generateOperations(0, arraySize - 1, InstructionBuilder.write('__value', '__device', `"_pu_${randomIdentifier}_{sval}"`))
                        .raw_replace('input', '__pos').raw_replace('exit',
                            InstructionBuilder.set('@counter', '__stackpos').content
                        );
                funcManager.addFunction(writerName, writer, null, [], true);
                // Create a local variable (but the problem is: how to get function info?):
                
                return returnedObject;
            }
        ],
        ['sdread',
            /**
             * 
             * @param {CodeGenerator} cg 
             * @param {BuiltinCallNode} ast 
             */
            (cg, ast) => {
                const tmpSymb = cg.getTempSymbol(cg.semantic.getTypeInfo('int'));
                return cg.operatesReads(
                    ast.arguments.map(arg => cg.visit(arg)),
                    new Instruction([
                        // Evaluate reader part
                        InstructionBuilder.set('__device', '{op_r0}'),
                        InstructionBuilder.set('__pos', '{op_r2}'),
                        InstructionBuilder.op('shr', '__funcpos', '{op_r1}', 14),
                        InstructionBuilder.set('__internal_stackpos', '__stackpos'),
                        InstructionBuilder.op('add', '__stackpos', '@counter', 1),
                        InstructionBuilder.set('@counter', '__funcpos'),
                        InstructionBuilder.set('__stackpos', '__internal_stackpos'),
                        cg.generateSymbolWrite(tmpSymb, '__return')
                    ]).set_returns(tmpSymb)
                );
            }
        ],
        ['sdwrite',
            /**
             * 
             * @param {CodeGenerator} cg 
             * @param {BuiltinCallNode} ast 
             */
            (cg, ast) => {
                return cg.operatesReads(
                    ast.arguments.map(arg => cg.visit(arg)),
                    new Instruction([
                        InstructionBuilder.set('__device', '{op_r0}'),
                        InstructionBuilder.set('__pos', '{op_r2}'),
                        InstructionBuilder.set('__value', '{op_r3}'),
                        InstructionBuilder.op('and', '__funcpos', '{op_r1}', 16383),
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
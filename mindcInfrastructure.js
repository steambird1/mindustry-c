/*
MinCCompiler on JS
By Seabird Starch Gunnhildr and Deepseek, 2025 - 2026.

mindcInfrastructure.js -- the object-oriented "value" layer of the code generator.

WHY THIS FILE EXISTS
--------------------
The original mindcGenerator.js described every intermediate value as an `Instruction`
plus a bag of loosely documented boolean attributes (`isPointer`, `isPointerAccess`,
`disallowReplacement`, `isTemporary`, `isSymbolic`, `isNearPointer`, ...) and a set of
string templates (`{op}`, `{opw}`, `{op_r0}`, `{value_entry}`, `{pointer_block_entry}`).
That made every consumer re-implement the same decision tree, and the templates were
replaced through raw string substitution -- fragile and hard to maintain.

This file replaces that by classes. Every value produced while generating code is a
`VariableRet`, which *is* an `Instruction` (so it can still be concatenated into any
instruction sequence and consumed by not-yet-refactored code) *and* knows what it is:

    VariableRet
      |
      +-- ImmediateRet        常量 / @常量 / null -- 只读，不占存储
      |
      +-- BasicRet            单字值，位于某个名字（寄存器或符号）中，可读写
      |     +-- IntRet -- BoolRet
      |     +-- FloatRet
      |     +-- ContentRet
      |     +-- DeviceRet / NullRet
      |
      +-- PointerRet          指针值，占两个字 P_block / P_pos
      |     +-- RefPointerRet     指针值就在 P_block / P_pos 里
      |     +-- MemPointerRet     指针值本身位于内存中（需要读出来）
      |           +-- VolatilePointerRet
      |
      +-- MemoryRefRet        值位于内存中某个地址 P 处（读写都要经过内存）
      |     +-- StructRefRet      结构体 / 联合体
      |           +-- RegistryStructRet  成员各自占一个寄存器（registry struct）
      |
      +-- (references above are all "ReferenceRet": they have get() and set())

PREPARATION (not wired into the generator yet)
      |
      +-- PackedPointerRet    单字打包地址 block*SPAN+pos -- 只能存地址 / 解引用
      |     +-- PackedSlotRet
      |
      +-- ArrayRet            只支持下标访问的数组表示
            +-- TraditionalArrayRet   内存数组（可转指针）
            +-- PseudoListArrayRet    生成代码（pdread/pdwrite 式），句柄为整数
            +-- PackedArrayRet        位域打包进一个寄存器
            +-- ConstantArrayRet      常量表，常量下标直接折叠

USAGE
-----
    const lval = cg.getLValue(node.left);      // ReferenceRet
    const rval = cg.visitValue(node.right);    // VariableRet
    return lval.assign(rval);                  // Instruction (a VariableRet as well)

IMPORTANT (recursion safety)
----------------------------
Only *single-operand* methods live here. Any operation with two or more operands
(`a + b`, `f(a, b)`, ...) must first put every operand into its own temporary
**symbol** (`getTempSymbol`) -- never into a temporary register (`__register_N`):
registers are not saved on the stack, and one name maps to exactly one register, so
the evaluation of the second operand (which may contain a call) would clobber the
first one. `materialize()` does exactly that, and it is free when the value already
sits in a temporary symbol (it just renames, emitting no instruction).
*/

import { Instruction, InstructionBuilder, InstructionReferrer, MEMORY_RESERVE_SIZE } from "./mindcGeneratorBase.js";
import { SemanticAnalyzer, TypeInfo } from "./mindcSemantic.js";

/* ------------------------------------------------------------------ *
 * Small helpers
 * ------------------------------------------------------------------ */

/**
 * Flatten nested arrays of instructions (nesting happens when Rets are combined).
 * @param {any} parts
 * @returns {any[]}
 */
function flattenParts(parts) {
	const result = [];
	const push = item => {
		if (item === null || item === undefined || item === false) return;
		if (Array.isArray(item)) {
			item.forEach(push);
			return;
		}
		result.push(item);
	};
	push(parts);
	return result;
}

/**
 * @param {any} target string | SymbolEntry
 * @returns {string}
 */
function nameOf(target) {
	if (target === null || target === undefined) return 'null';
	if (typeof target === 'string') return target;
	if (typeof target.getAssemblySymbol === 'function') return target.getAssemblySymbol();
	return `${target}`;
}

/** item_t -> item, liquid_t -> liquid ... */
export const contentTypeOf = typeName => {
	// item_t -> item, liquid_t -> liquid ... everything else (int!) defaults to item
	return (typeName && SPECIAL_CONTENT_TYPES.includes(typeName)) ? typeName.slice(0, typeName.length - 2) : 'item';
};

export const SPECIAL_CONTENT_TYPES = ['item_t', 'liquid_t', 'unit_t', 'block_t'];

/**
 * @param {Globals} glob
 * @param {TypeInfo?} type
 * @returns {boolean}
 */
const isIntegerLike = (glob, type) => !!type && glob.semantic.isIntegerType(type.name);
const isFloatLike = (glob, type) => !!type && ['float', 'double'].includes(type.name);
const isBoolLike = (glob, type) => !!type && type.name === 'bool';
const isContentLike = (glob, type) => !!type && type.name === 'content_t';
const isSpecialLike = (glob, type) => !!type && SPECIAL_CONTENT_TYPES.includes(type.name);

/**
 * How many words a value of this type occupies.
 *
 * NOTE: `TypeInfo.isPointerImpl()` is true for every `volatile` type, but a volatile
 * **scalar** still occupies a single word -- volatile only means "keep it in memory".
 * Only real pointers/arrays (and memory-implemented structs) take two words.
 *
 * @param {TypeInfo?} type
 * @returns {number}
 */
/**
 * The numeric id of a `@name` constant, when the compiler knows it.
 *
 * @param {import('./mindcGeneratorBase.js').Globals?} glob
 * @param {string} name
 * @returns {number?} null when unknown (so a runtime `sensor @id` is needed)
 */
export function knownContentIdOf(glob, name) {
	if (typeof name !== 'string' || name[0] !== '@') return null;
	const table = glob?.semantic?.contentTable ?? null;
	if (!table) return null;
	for (const category of ['item', 'liquid', 'unit', 'block']) {
		const id = table.idOf(category, name);
		if (id !== undefined) return (id === null) ? null : id;
	}
	return null;
}

export function widthOfType(type) {
	if (!type || !type.isTypeInfo) return 1;
	if (type.kind === 'pointer' || type.kind === 'array') return 2;
	if (type.kind === 'struct' || type.kind === 'union') return type.isPointerImpl() ? 2 : 1;
	return 1;
}

/* ------------------------------------------------------------------ *
 * Exceptions
 * ------------------------------------------------------------------ */

export class InfrastructureNotImplementedExpection extends Error {
	constructor(info = '') {
		super(`Feature not implemented in infrastructure: ${info}`);
		this.info = info;
	}
}

export class InvalidCastException extends Error {
	constructor(from, to) {
		const describe = v => (v && v.type) ? v.type.name : (v ? `${v.constructor?.name ?? v}` : '?');
		super(`Invalid cast: cannot store ${describe(from)} into ${describe(to)}`);
		this.from = from;
		this.to = to;
	}
}

/* ------------------------------------------------------------------ *
 * Globals -- everything the value classes need from the outer world
 * ------------------------------------------------------------------ */

/**
 * @callback tempVarAssigner
 * @param {TypeInfo?} type
 * @param {string?} kind
 * @returns {string} The **assembly name** of a freshly allocated temporary symbol.
 */
export class Globals {
	/**
	 * @param {import('./mindcGeneratorBase.js').MemoryManager} mem
	 * @param {tempVarAssigner} tmpCaller
	 * @param {SemanticAnalyzer} semantic
	 * @param {import('./mindcGeneratorBase.js').FunctionRegisterer?} funcReg
	 * @param {any?} generator The CodeGenerator itself (for struct copy, etc.)
	 */
	constructor(mem, tmpCaller, semantic, funcReg = null, generator = null) {
		this.memory = mem;
		this.tmpCaller = tmpCaller;
		this.semantic = semantic;
		this.funcReg = funcReg;
		this.generator = generator;
		this.reservedSize = (mem && mem.reservedSize) ? mem.reservedSize : MEMORY_RESERVE_SIZE;
	}

	/**
	 * Allocate a fresh temporary symbol and return its assembly name.
	 * @param {TypeInfo?} type
	 * @param {string?} kind
	 * @returns {string}
	 */
	tmp(type = null, kind = null) {
		return this.tmpCaller(type, kind);
	}
}

/* ------------------------------------------------------------------ *
 * Memory primitives (pointer arithmetic / indirect access)
 * ------------------------------------------------------------------ */

/**
 * Move a pointer forward (or backward) by `step`.
 * @param {Globals} glob
 * @param {string} pointer base name (P_block / P_pos)
 * @param {number|string} step
 * @param {boolean} isNear
 * @param {boolean} backward
 * @returns {Instruction}
 */
export function pointerStep(glob, pointer, step, isNear = false, backward = false) {
	if (!backward) {
		return glob.memory.outputPointerForwardCall(step, pointer, glob.funcReg, false, true, isNear);
	}
	return glob.memory.outputPointerBackwardCall(step, pointer, glob.funcReg, true);
}

/**
 * Read `width` words from the address stored in pointer `pointer` into `target`.
 * @param {Globals} glob
 * @param {string} pointer
 * @param {string} target
 * @param {number} width 1 = scalar, 2 = pointer
 * @param {boolean} isNear
 * @returns {Instruction}
 */
/**
 * Move `pointer` one cell forward WITHOUT moving the pointer itself.
 *
 * Reading or writing a two-word value needs the address of the second word, but
 * that must not disturb the pointer: it may be a variable's save slot, and
 * `pointerStep` (which writes the advanced address back) would slowly walk it
 * into somebody else's storage.
 *
 * @param {import('./mindcGeneratorBase.js').Globals?} glob
 * @param {string} pointer
 * @param {boolean} isNear
 * @returns {Instruction}
 */
function peekNext(glob, pointer, isNear = false) {
	return glob.memory.outputPointerForwardCall(1, pointer, glob.funcReg, false, false, isNear);
}

export function readThrough(glob, pointer, target, width = 1, isNear = false, blockName = null) {
	if (width <= 1) {
		// A stepped pointer keeps the block half of whatever it stepped from,
		// so the block may be read straight from there instead of a copy.
		return glob.memory.outputPointerFetchOf(pointer, target);
	}
	return new Instruction([
		glob.memory.outputPointerFetchOf(pointer, `${target}_block`),
		peekNext(glob, pointer, isNear),
		InstructionBuilder.reads(`${target}_pos`, '__ptrblock', '__ptrpos')
	]);
}

/**
 * Write `width` words from `value` into the address stored in pointer `pointer`.
 * @param {Globals} glob
 * @param {string} pointer
 * @param {string} value
 * @param {number} width
 * @param {boolean} isNear
 * @returns {Instruction}
 */
export function writeThrough(glob, pointer, value, width = 1, isNear = false, blockName = null) {
	if (width <= 1) {
		return glob.memory.outputPointerStorageOf(pointer, value);
	}
	return new Instruction([
		glob.memory.outputPointerStorageOf(pointer, `${value}_block`),
		peekNext(glob, pointer, isNear),
		InstructionBuilder.writes(`${value}_pos`, '__ptrblock', '__ptrpos')
	]);
}

/**
 * Read the value a reference points at (honouring compile-time known locations).
 * @param {Globals} glob
 * @param {MemoryRefRet | MemPointerRet} ref
 * @param {string} target
 * @param {number} width
 * @returns {Instruction}
 */
function referenceRead(glob, ref, target, width) {
	if (ref.fixedLocation) {
		if (width >= 2) {
			return new Instruction([
				glob.memory.outputFetchOf(ref.fixedLocation, `${target}_block`),
				glob.memory.outputFetchOf(ref.fixedLocation.duplicate().forwarding(1), `${target}_pos`)
			]);
		}
		return glob.memory.outputFetchOf(ref.fixedLocation, target);
	}
	return readThrough(glob, ref.name, target, width, ref.isNear, ref.blockAlias ?? null);
}

/**
 * Write the value a reference points at.
 * @param {Globals} glob
 * @param {MemoryRefRet | MemPointerRet} ref
 * @param {string} value
 * @param {number} width
 * @returns {Instruction}
 */
function referenceWrite(glob, ref, value, width) {
	if (ref.fixedLocation) {
		if (width >= 2) {
			return new Instruction([
				glob.memory.outputStorageOf(ref.fixedLocation, `${value}_block`),
				glob.memory.outputStorageOf(ref.fixedLocation.duplicate().forwarding(1), `${value}_pos`)
			]);
		}
		return glob.memory.outputStorageOf(ref.fixedLocation, value);
	}
	return writeThrough(glob, ref.name, value, width, ref.isNear, ref.blockAlias ?? null);
}

/**
 * Turn a pointer into its linear (integer) address.
 * @param {Globals} glob
 * @param {string} pointer
 * @param {string} target
 * @returns {Instruction}
 */
export function pointerToValue(glob, pointer, target) {
	return new Instruction([
		InstructionBuilder.reads(target, `${pointer}_block`, 3),
		InstructionBuilder.op('sub', target, target, glob.reservedSize),
		InstructionBuilder.op('add', target, target, `${pointer}_pos`)
	]);
}

/**
 * Build the type `inner *`.
 * @param {Globals} glob
 * @param {TypeInfo?} inner
 * @returns {TypeInfo?}
 */
function pointerTypeOf(glob, inner) {
	if (!inner) return null;
	const pointer = new TypeInfo('', 'pointer', 2);
	pointer.pointerTo = inner;
	pointer.qualifiers = [...(inner.qualifiers ?? [])].filter(q => q !== 'volatile');
	return pointer;
}

/**
 * Turn an integer address back into a pointer (starting from the very beginning
 * of the managed memory).
 * @param {Globals} glob
 * @param {string} value
 * @param {string} target
 * @returns {Instruction}
 */
export function valueToPointer(glob, value, target) {
	return glob.memory.outputPointerForwardCall(value, target, glob.funcReg, true);
}

/* ------------------------------------------------------------------ *
 * Operations with more than one operand
 * ------------------------------------------------------------------ */

/**
 * Any operation with two operands must materialize BOTH operands into their own
 * temporary symbols first: the evaluation of the second one may contain a call,
 * which would clobber registers (and registers are not saved on the stack).
 *
 * `pointer + int` / `pointer - int` are handled through the pointer stepping
 * routine instead of a plain `op`.
 *
 * @param {Globals} glob
 * @param {string} op mlog operation name (add, sub, mul, ...)
 * @param {VariableRet} left
 * @param {VariableRet} right
 * @param {TypeInfo?} type Result type
 * @returns {VariableRet}
 */
export function arithmetic(glob, op, left, right, type = null) {
	const resultType = type ?? left.type ?? right.type;
	const isAddLike = op === 'add' || op === 'sub';

	// pointer (+|-) integer  ->  move the pointer
	if ((left instanceof PointerRet) && !(right instanceof PointerRet) && isAddLike) {
		const base = left.get();				// read the pointer out (once!)
		if (base instanceof NamePointerRet || base instanceof NameStructPointerRet
			|| base instanceof RemotePointerRet) {
			throw new InfrastructureNotImplementedExpection(
				'pointer arithmetic on a pointer that holds a name: it names a register, '
				+ 'so it has no address that could be moved');
		}
		const step = right.materialize();	// own temporary symbol: safe against calls
		const target = glob.tmp(resultType, 'pointer');
		return new RefPointerRet(glob, [
			base.instructions,
			InstructionBuilder.set(`${target}_block`, `${base.name}_block`),
			InstructionBuilder.set(`${target}_pos`, `${base.name}_pos`),
			pointerStep(glob, target, step.name, base.isNear, op === 'sub')
		], target, resultType, true);
	}

	// pointer - pointer -> difference of the two addresses, as an integer
	if ((left instanceof PointerRet) && (right instanceof PointerRet) && op === 'sub') {
		const l = left.convertTo(glob.semantic.getTypeInfo('int')).materialize();
		const r = right.convertTo(glob.semantic.getTypeInfo('int')).materialize();
		const target = glob.tmp(glob.semantic.getTypeInfo('int'));
		return new IntRet(glob, [l.instructions, r.instructions,
			InstructionBuilder.op('sub', target, l.name, r.name)], target,
			glob.semantic.getTypeInfo('int'), true);
	}

	/*
	Everything else: BOTH operands are materialized into their own temporary symbol
	first, because evaluating the second one may contain a function call, and a
	temporary register (`__register_N`) is neither saved on the stack nor unique per
	value -- a nested (or recursive) call would happily clobber it.
	*/
	const l = left.materialize();
	const r = right.materialize();
	const target = glob.tmp(resultType, widthOfType(resultType) === 2 ? 'pointer' : null);
	const instruction = InstructionBuilder.op(op, target, l.name, r.name);
	return valueClassOf(glob, resultType).create(glob,
		[l.instructions, r.instructions, instruction], target, resultType, true);
}

/* ------------------------------------------------------------------ *
 * VariableRet -- base class
 * ------------------------------------------------------------------ */

export class VariableRet extends Instruction {

	/**
	 * @param {Globals} glob
	 * @param {any} instructions
	 * @param {string?} returner
	 * @param {TypeInfo?} type
	 * @param {boolean} temporary Whether the value lives in a temporary symbol
	 *                            (which means: it may be renamed for free).
	 */
	constructor(glob, instructions, returner, type, temporary = false) {
		super(flattenParts(instructions), returner);
		this.glob = glob;
		this.type = type ?? null;
		this.temporary = temporary;
		this.setAttribute('isTemporary', temporary);
		if (type && type.isTypeInfo) {
			this.setAttribute('isRegStruct', type.isRegStruct());
			this.setAttribute('isStructRet', ['struct', 'union'].includes(type.kind));
		}
	}

	/** The assembly name carrying the value (or pointing at it). */
	get name() {
		return this.instructionReturn;
	}

	/** How many words the value occupies (2 for pointers, 1 otherwise). */
	get width() {
		return widthOfType(this.type);
	}

	get isNear() {
		return this.getAttribute('isNearPointer') === true;
	}

	/**
	 * Shallow copy. Sub-instructions are shared (that is what the legacy code does
	 * as well -- `replace_variable` mutates them in place).
	 * @returns {VariableRet}
	 */
	clone() {
		const copy = Object.create(Object.getPrototypeOf(this));
		Object.assign(copy, this);
		copy.instructions = [...this.instructions];
		copy._attributes = new Map(this._attributes);
		return copy;
	}

	/**
	 * Copy this Ret while renaming its value. Only legal for temporary values
	 * (or for values that are allowed to be replaced), and never for constants.
	 * @param {string|SymbolEntry} target
	 * @returns {VariableRet}
	 */
	rename(target) {
		if (!this.canRename) {
			// Not allowed to rename (a constant, or a protected name): copy instead.
			return this.storeTo(target, this.temporary);
		}
		const name = nameOf(target);
		const copy = this.clone();
		if (this.instructionReturn && this.instructionReturn !== 'null') {
			copy.replace_variable(name, this.instructionReturn);
		}
		copy.temporary = this.temporary;
		return copy;
	}

	/**
	 * Get the value: the returned Ret is guaranteed to be usable **directly** as an
	 * operand of another instruction (its `name` holds the value -- or, for pointers,
	 * `name_block` / `name_pos` do).
	 * @returns {VariableRet}
	 */
	get() {
		return this;
	}

	/**
	 * Store `other` into the position described by `this`.
	 * @param {VariableRet} other
	 * @returns {VariableRet} A Ret describing the position after the write.
	 */
	set(other) {
		throw new InvalidCastException(other, this);
	}

	/**
	 * Alias of set(), provided because `set` is easy to confuse with AttributeClass
	 * semantics when reading call sites.
	 * @param {VariableRet} other
	 * @returns {VariableRet}
	 */
	assign(other) {
		return this.set(other);
	}

	/**
	 * Uniform constructor entry point, so that the factories in this file do not
	 * need to know how many arguments each subclass takes.
	 *
	 * It dispatches on `this`, which means **every** subclass (including ones added
	 * later) builds an instance of its own class. Classes with extra constructor
	 * arguments override it.
	 *
	 * @param {Globals} glob
	 * @param {any} instructions
	 * @param {string?} returner
	 * @param {TypeInfo?} type
	 * @param {boolean} [temporary=false]
	 * @param {SymbolEntry?} [symbol=null]
	 * @returns {VariableRet}
	 */
	static create(glob, instructions, returner, type, temporary = false, symbol = null) {
		return new this(glob, instructions, returner, type, temporary);
	}

	/**
	 * Whether this value may simply be *renamed* to another name instead of being
	 * copied: that is allowed only for values that own their storage (temporary
	 * symbols) and whose name is not protected (`disallowReplacement`).
	 * @returns {boolean}
	 */
	get canRename() {
		if (this instanceof ImmediateRet) return false;
		return this.temporary && this.getAttribute('disallowRename') !== true;
	}

	/**
	 * Make sure the value can safely be used as an operand of a multi-operand
	 * operation, i.e. that it survives the evaluation of the *other* operands
	 * (which may contain calls, possibly recursive ones).
	 *
	 * This is free of cost when the value
	 *  - owns its storage (a temporary symbol), or
	 *  - is a persistent symbol (a real variable: nested code does not reuse it), or
	 *  - is a constant.
	 * Only values sitting in a **temporary register** (`__register_N`) have to be
	 * copied: a register is neither saved on the stack nor unique per value.
	 * @returns {VariableRet}
	 */
	materialize() {
		const value = this.get();
		if (value.canRename) return value;
		if (value instanceof ImmediateRet) return value;
		if (value.getAttribute('isSymbolic') || value.symbol) return value;
		return value.storeTo(this.glob.tmp(value.type, value.width === 2 ? 'pointer' : null), true);
	}

	/**
	 * Copy the value into the given (fresh) name.
	 * @param {string|SymbolEntry} target
	 * @returns {VariableRet}
	 */
	storeTo(target, temporary = true) {
		const name = nameOf(target);
		const value = this.get();
		if (value.width >= 2) {
			return new RefPointerRet(this.glob, [
				value.instructions,
				InstructionBuilder.set(`${name}_block`, `${value.name}_block`),
				InstructionBuilder.set(`${name}_pos`, `${value.name}_pos`)
			], name, value.type, temporary);
		}
		return valueClassOf(this.glob, value.type).create(
			this.glob,
			[value.instructions, InstructionBuilder.set(name, value.name)],
			name, value.type, temporary);
	}

	/**
	 * The address of this value, as a pointer value.
	 * @returns {PointerRet}
	 */
	address() {
		throw new InfrastructureNotImplementedExpection('address of a value');
	}

	/**
	 * Dereference (pointers only).
	 * @returns {VariableRet}
	 */
	deref() {
		throw new InfrastructureNotImplementedExpection('dereference');
	}

	/**
	 * Convert the value to another type, producing conversion instructions when the
	 * Mindustry VM cannot do it implicitly.
	 * @param {TypeInfo} type
	 * @param {string?} category For conversions to content_t: item / liquid / unit / block
	 * @returns {VariableRet}
	 */
	convertTo(type, category = null) {
		const glob = this.glob;
		if (!type || !this.type || glob.semantic.isSameType(this.type, type)) return this.get();
		const src = this.type, dst = type;
		// Fast path: qualifiers (volatile / static / const) never change the
		// representation, so no instruction has to be emitted at all.
		if (widthOfType(src) === widthOfType(dst)
			&& !isFloatLike(glob, src) && !isFloatLike(glob, dst)
			&& !isContentLike(glob, src) && !isContentLike(glob, dst)
			&& !isBoolLike(glob, src) && !isBoolLike(glob, dst)) {
			return this.get();
		}
		const value = this.get();
		const target = glob.tmp(dst, widthOfType(dst) === 2 ? 'pointer' : null);

		/** @type {any} */
		let conversion = null;
		if (widthOfType(dst) === 2 && widthOfType(src) === 1) {
			conversion = valueToPointer(glob, value.name, target);
		} else if (widthOfType(dst) === 1 && widthOfType(src) === 2) {
			conversion = pointerToValue(glob, value.name, target);
		} else if (isIntegerLike(glob, dst) && isFloatLike(glob, src)) {
			conversion = InstructionBuilder.op('floor', target, value.name);
		} else if (isIntegerLike(glob, dst) && isContentLike(glob, src)) {
			/*
			The id of a `@name` is known at compile time, so write it out instead
			of asking the processor with `sensor x @name @id` -- one instruction
			less, and the value is right even when nothing is linked. Only when
			the id is unknown (declared as `null`) does the sensor stay.
			*/
			const knownId = knownContentIdOf(glob, value.name);
			conversion = knownId === null
				? InstructionBuilder.sensor(target, value.name, '@id')
				: InstructionBuilder.set(target, String(knownId));
		} else if (isContentLike(glob, dst) && (isIntegerLike(glob, src) || isSpecialLike(glob, src))) {
			conversion = InstructionBuilder.lookup(category ?? contentTypeOf(src.name), target, value.name);
		} else if (isBoolLike(glob, dst)) {
			conversion = InstructionBuilder.op('notEqual', target, value.name, 0);
		} else if (isIntegerLike(glob, dst) && isBoolLike(glob, src)) {
			conversion = null;		// bool is already 0/1
		}

		if (!conversion) {
			// Nothing to do but a plain copy (this covers item_t <-> int, etc.)
			return value.storeTo(target, true);
		}
		return valueClassOf(glob, dst).create(glob, [value.instructions, conversion], target, dst, true);
	}

	/**
	 * @param {VariableRet} other
	 * @returns {VariableRet}
	 */
	static wrap(other) {
		return new this(other.glob, other.instructions, other.instructionReturn, other.type, other.temporary);
	}
}

/* ------------------------------------------------------------------ *
 * Immediate values
 * ------------------------------------------------------------------ */

/**
 * A constant: numeric literal, `@copper`-like content constant, `null`, string...
 * It can be read (used as operand) but never written.
 */
export class ImmediateRet extends VariableRet {
	constructor(glob, value, type, builtin = false) {
		super(glob, [], `${value}`, type, false);
		this.setAttribute('disallowReplacement', true);
		if (builtin) this.setAttribute('builtinConstant', true);
	}

	get() {
		return this;
	}

	/** Constants never need materialization: they cannot be clobbered. */
	materialize() {
		return this;
	}

	set(other) {
		throw new InvalidCastException(other, this);
	}

	storeTo(target, temporary = true) {
		const name = nameOf(target);
		return valueClassOf(this.glob, this.type).create(this.glob,
			[InstructionBuilder.set(name, this.name)], name, this.type, temporary);
	}

	address() {
		throw new InfrastructureNotImplementedExpection('address of a constant');
	}
}

/* ------------------------------------------------------------------ *
 * Values living in a name (register / symbol)
 * ------------------------------------------------------------------ */

/**
 * A single-word value that lives in `name`. Reading it is free; writing it is
 * a `set` (or, when the source is temporary, a pure rename).
 */
export class BasicRet extends VariableRet {

	/**
	 * @param {VariableRet} other
	 * @returns {BasicRet}
	 */
	set(other) {
		const glob = this.glob;
		// References (values still living in memory) have to be read out first.
		if ((other instanceof MemoryRefRet) || (other instanceof MemPointerRet)) {
			return this.set(other.get());
		}
		if ((other instanceof BasicRet) || (other instanceof ImmediateRet)) {
			if (other.canRename) {
				// The source owns its storage: just redirect it to us, emitting nothing.
				return valueClassOf(glob, this.type).create(glob,
					[this.instructions, other.rename(this.instructionReturn).instructions],
					this.instructionReturn, this.type, this.temporary);
			}
			return valueClassOf(glob, this.type).create(glob,
				[this.instructions, other.instructions,
					InstructionBuilder.set(this.instructionReturn, other.instructionReturn)],
				this.instructionReturn, this.type, this.temporary);
		}
		if (other instanceof PointerRet) {
			// A pointer stored into a scalar slot: take its linear address.
			const converted = other.convertTo(glob.semantic.getTypeInfo('int'));
			if (converted === other) {
				throw new InvalidCastException(other, this);
			}
			return this.set(converted);
		}
		if (other instanceof StructRefRet) {
			return super.set(other);
		}
		return super.set(other);
	}

	address() {
		/*
		A variable that is only ever reached through name pointers never has to
		be moved into memory: `&x` is simply the NAME of x.
		*/
		if (this.isNameAddressable) {
			const glob = this.glob;
			const target = glob.tmp(pointerTypeOf(glob, this.type), 'pointer');
			return new NamePointerRet(glob,
				[this.instructions, NamePointerRet.pointAt(glob, target, this.name)],
				target, pointerTypeOf(glob, this.type), true, this.name);
		}
		throw new InfrastructureNotImplementedExpection(
			`${this.instructionReturn} has no address (use 'volatile', 'static' or '&')`);
	}

	/**
	 * Whether `&x` may be taken as the name of x.
	 * @returns {boolean}
	 */
	get isNameAddressable() {
		if (this.getAttribute('namePointersOnly') === true
			|| this.symbol?.namePointersOnly === true) return true;
		/*
		The pointer pass only marks variables it could see. A pointer that was
		optimised away (`int *p = &x;` where p is never used) leaves no trace,
		and a variable handed to `ulocate` / `ucontrol` is named by the
		instruction itself -- in both cases `&x` is still just the NAME of x.
		`SemanticAnalyzer.collectAddressUses` records which it is.
		*/
		const symbol = this.symbol;
		if (!symbol) return false;
		const generator = this.glob?.generator ?? null;
		const table = generator?.semantic?.addressUseTable ?? null;
		const uses = table?.get(symbol.getAssemblySymbol?.() ?? symbol.name) ?? null;
		if (!uses || uses.other !== 0) return false;
		if (uses.builtin > 0 && uses.pointers.size === 0) return true;
		if (uses.pointers.size === 0) return false;
		// Stored into a pointer: only a name pointer can hold a name
		return generator?.namePointerAllowed?.() === true;
	}
}

export class IntRet extends BasicRet {
	/**
	 * @param {VariableRet} other
	 * @returns {IntRet}
	 */
	set(other) {
		const glob = this.glob;
		if (other instanceof IntRet || other instanceof BoolRet) {
			return IntRet.wrap(super.set(other));
		}
		if (other instanceof FloatRet) {
			const result = super.set(other);
			return new IntRet(glob, [
				result.instructions,
				InstructionBuilder.op('floor', this.instructionReturn, this.instructionReturn)
			], this.instructionReturn, this.type, this.temporary);
		}
		if (other instanceof ContentRet) {
			const result = super.set(other);
			return new IntRet(glob, [
				result.instructions,
				InstructionBuilder.sensor(this.instructionReturn, this.instructionReturn, '@id')
			], this.instructionReturn, this.type, this.temporary);
		}
		if (other instanceof PointerRet) {
			const converted = other.convertTo(glob.semantic.getTypeInfo('int'));
			if (converted === other) throw new InvalidCastException(other, this);
			return this.set(converted);
		}
		return IntRet.wrap(super.set(other));
	}
}

export class BoolRet extends IntRet {
	/**
	 * @param {VariableRet} other
	 * @returns {BoolRet}
	 */
	set(other) {
		if (other instanceof IntRet || other instanceof FloatRet || other instanceof ContentRet) {
			const result = super.set(other);
			return new BoolRet(this.glob, [
				result.instructions,
				InstructionBuilder.op('notEqual', this.instructionReturn, this.instructionReturn, 0)
			], this.instructionReturn, this.type, this.temporary);
		}
		return BoolRet.wrap(super.set(other));
	}
}

export class FloatRet extends BasicRet {
	set(other) {
		return FloatRet.wrap(super.set(other));
	}
}

/** content_t -- game contents (@copper & friends). Never stored in memory. */
export class ContentRet extends BasicRet {
	set(other) {
		const glob = this.glob;
		if (other instanceof ContentRet || other instanceof ImmediateRet) {
			return ContentRet.wrap(super.set(other));
		}
		if (isIntegerLike(glob, other.type) || isSpecialLike(glob, other.type)) {
			const converted = other.convertTo(this.type, contentTypeOf(other.type?.name));
			return this.set(converted);
		}
		return ContentRet.wrap(super.set(other));
	}
}

/** device / unit_t-like handles. */
export class DeviceRet extends BasicRet {
	set(other) {
		return DeviceRet.wrap(super.set(other));
	}
}

export class NullRet extends BasicRet {
	set(other) {
		return NullRet.wrap(super.set(other));
	}
}

/* ------------------------------------------------------------------ *
 * Pointers
 * ------------------------------------------------------------------ */

/**
 * A pointer value: `name_block` is the memory block number, `name_pos` the offset.
 */
export class PointerRet extends VariableRet {

	get width() {
		return 2;
	}

	/**
	 * Read what the pointer points at.
	 * @param {string|SymbolEntry} target Where to store the result.
	 * @returns {VariableRet}
	 */
	fetch(target) {
		const glob = this.glob;
		const name = nameOf(target);
		const inner = (this.type && this.type.pointerTo) ? this.type.pointerTo : null;
		const width = (inner && inner.isPointerImpl()) ? 2 : 1;
		const instructions = [this.instructions,
			readThrough(glob, this.name, name, width, this.isNear, this.blockAlias ?? null)];
		if (width >= 2) {
			return new RefPointerRet(glob, instructions, name, inner, this.temporary);
		}
		return valueClassOf(glob, inner).create(glob, instructions, name, inner, this.temporary);
	}

	/**
	 * Dereference into a fresh temporary symbol.
	 * @returns {VariableRet}
	 */
	deref() {
		const inner = (this.type && this.type.pointerTo) ? this.type.pointerTo : null;
		return this.fetch(this.glob.tmp(inner, inner && inner.isPointerImpl() ? 'pointer' : null));
	}

	/**
	 * Write into the pointed-to slot.
	 * @param {VariableRet} other
	 * @returns {VariableRet}
	 */
	pointedSet(other) {
		const glob = this.glob;
		const inner = (this.type && this.type.pointerTo) ? this.type.pointerTo : null;
		const value = other.get().convertTo(inner ?? other.type);
		return valueClassOf(glob, inner).create(glob,
			[this.instructions, value.instructions,
				writeThrough(glob, this.name, value.name, value.width, this.isNear, this.blockAlias ?? null)],
			this.name, inner, false);
	}

	/** p + n / p - n */
	/**
	 * The register holding the memory block number.
	 *
	 * Stepping a pointer never changes which block it points into, so the
	 * result can keep reading the block from the pointer it came from and
	 * skip copying it.
	 *
	 * @returns {string}
	 */
	blockOperand() {
		return this.blockAlias ?? `${this.name}_block`;
	}

	offset(step, backward = false) {
		const glob = this.glob;
		const target = glob.tmp(this.type, 'pointer');
		/*
		`this.temporary` means the pointer itself lives in a scratch register
		that a nested call may overwrite; then the block has to be copied. A
		named pointer (a variable, an array base) is stable, so only the offset
		needs a copy.
		*/
		/*
		Both halves are copied: the pointer is stepped in place, and the block
		half cannot be borrowed from `this.name`, because that register is
		re-written when the same base is stepped again (or by a nested call),
		while the stepped copy is still being read through.
		*/
		return new RefPointerRet(glob, [
			this.instructions,
			InstructionBuilder.set(`${target}_block`, `${this.name}_block`),
			InstructionBuilder.set(`${target}_pos`, `${this.name}_pos`),
			pointerStep(glob, target, step, this.isNear, backward)
		], target, this.type, true);
	}

	address() {
		throw new InfrastructureNotImplementedExpection('address of a pointer');
	}
}

/** The pointer itself is stored in `name_block` / `name_pos`. */
export class RefPointerRet extends PointerRet {
	/**
	 * @param {VariableRet} other
	 * @returns {RefPointerRet}
	 */
	set(other) {
		const glob = this.glob;
		const value = other.get();
		if (!(value instanceof PointerRet)) {
			// Storing a plain integer into a pointer: rebuild the pointer from the address.
			const converted = value.convertTo(this.type ?? glob.semantic.getTypeInfo('void*'));
			return this.set(converted);
		}
		if (value.canRename) {
			return new RefPointerRet(glob, [
				this.instructions, value.rename(this.instructionReturn).instructions
			], this.instructionReturn, this.type, this.temporary);
		}
		return new RefPointerRet(glob, [
			this.instructions, value.instructions,
			InstructionBuilder.set(`${this.instructionReturn}_block`, `${value.name}_block`),
			InstructionBuilder.set(`${this.instructionReturn}_pos`, `${value.name}_pos`)
		], this.instructionReturn, this.type, this.temporary);
	}
}

/** The pointer value itself lives in memory: `name` is the *address of the pointer*. */
export class MemPointerRet extends PointerRet {

	/**
	 * @param {Globals} glob
	 * @param {any} instructions
	 * @param {string} pointerName Address of the pointer (${X}.__pointer style)
	 * @param {TypeInfo?} type
	 * @param {boolean} temporary
	 * @param {import('./mindcGeneratorBase.js').MemoryBlockInfo?} fixedLocation
	 * @param {SymbolEntry?} symbol
	 */
	constructor(glob, instructions, pointerName, type, temporary = false, fixedLocation = null, symbol = null) {
		super(glob, instructions, pointerName, type, temporary);
		this.fixedLocation = fixedLocation;
		this.symbol = symbol;
		this.setAttribute('isPointer', true);
		this.setAttribute('isPointerAccess', true);
		this.setAttribute('pointerAccess', true);
		this.setAttribute('disallowReplacement', true);
		if (symbol) {
			this.setAttribute('relevantSymbol', symbol);
			this.setAttribute('relevantSymbolRead', symbol);
			this.setAttribute('isSymbolic', true);
			this.setAttribute('isNearPointer', symbol.isNearPointer);
		}
	}

	/** @returns {MemPointerRet} */
	static create(glob, instructions, returner, type, temporary = false, symbol = null, fixedLocation = null) {
		return new this(glob, instructions, returner, type, temporary, fixedLocation, symbol);
	}

	/**
	 * Read the pointer out of memory.
	 * @returns {RefPointerRet}
	 */
	get() {
		const target = this.glob.tmp(this.type, 'pointer');
		return new RefPointerRet(this.glob,
			[this.instructions, referenceRead(this.glob, this, target, 2)], target, this.type, true);
	}

	/**
	 * @param {VariableRet} other
	 * @returns {VariableRet}
	 */
	set(other) {
		const glob = this.glob;
		const value = other.get();
		if (!(value instanceof PointerRet)) {
			return this.set(value.convertTo(this.type ?? glob.semantic.getTypeInfo('void*')));
		}
		return new RefPointerRet(glob,
			[this.instructions, value.instructions, referenceWrite(glob, this, value.name, 2)],
			this.name, this.type, false);
	}

	fetch(target) {
		return this.get().fetch(target);
	}

	deref() {
		return this.get().deref();
	}

	offset(step, backward = false) {
		return this.get().offset(step, backward);
	}

	/** The address of the pointer is known: it is our own address. */
	address() {
		/*
		The type of an address is a POINTER to what we hold. `TypeInfo.pointerTo`
		is null for a struct or union, which would make the address look like a
		single word -- so build the pointer type explicitly.
		*/
		return new RefPointerRet(this.glob, this.instructions, this.name,
			pointerTypeOf(this.glob, this.type), false);
	}
}

export class VolatilePointerRet extends MemPointerRet {
}

/* ------------------------------------------------------------------ *
 * Values that live in memory (accessed through a pointer)
 * ------------------------------------------------------------------ */

/**
 * The value is stored in memory at the address described by pointer `name`
 * (`name_block` / `name_pos`). Reading emits a read; writing emits a write.
 *
 * `fixedLocation`, when set, is a compile-time known MemoryBlockInfo, which allows
 * direct `read/write block pos` without getlink.
 */
export class MemoryRefRet extends VariableRet {

	/**
	 * @param {Globals} glob
	 * @param {any} instructions
	 * @param {string} pointerName
	 * @param {TypeInfo?} type
	 * @param {boolean} temporary
	 * @param {import('./mindcGeneratorBase.js').MemoryBlockInfo?} fixedLocation
	 * @param {SymbolEntry?} symbol
	 */
	constructor(glob, instructions, pointerName, type, temporary = false, fixedLocation = null, symbol = null) {
		super(glob, instructions, pointerName, type, temporary);
		this.fixedLocation = fixedLocation;
		this.symbol = symbol;
		this.setAttribute('isPointerAccess', true);
		this.setAttribute('pointerAccess', true);
		this.setAttribute('disallowReplacement', true);
		if (symbol) {
			this.setAttribute('relevantSymbol', symbol);
			this.setAttribute('relevantSymbolRead', symbol);
			this.setAttribute('isSymbolic', true);
			this.setAttribute('isNearPointer', symbol.isNearPointer);
			this.setAttribute('isRegStruct', symbol.isRegistryStruct());
		}
	}

	/** @returns {MemoryRefRet} */
	static create(glob, instructions, returner, type, temporary = false, symbol = null, fixedLocation = null) {
		return new this(glob, instructions, returner, type, temporary, fixedLocation, symbol);
	}

	/**
	 * Read the value out of memory.
	 * @returns {VariableRet}
	 */
	get() {
		const glob = this.glob;
		const width = this.width;
		const target = glob.tmp(this.type, width >= 2 ? 'pointer' : null);
		const instructions = [this.instructions, referenceRead(glob, this, target, width)];
		return valueClassOf(glob, this.type).create(glob, instructions, target, this.type, true);
	}

	/**
	 * Write into memory.
	 * @param {VariableRet} other
	 * @returns {VariableRet}
	 */
	set(other) {
		const glob = this.glob;
		const value = other.get().convertTo(this.type ?? other.type);
		const instructions = [this.instructions, value.instructions,
			referenceWrite(glob, this, value.name, value.width)];
		// The value of an assignment expression is the value that has been stored --
		// NOT our own name, which is the *address* of that value.
		return valueClassOf(glob, this.type).create(glob, instructions, value.name, this.type, true);
	}

	/**
	 * The address of the value: exactly the pointer we hold.
	 * @returns {RefPointerRet}
	 */
	address() {
		if (this.fixedLocation) {
			// A compile-time constant address has to be materialized first.
			const target = this.glob.tmp(this.type, 'pointer');
			return new RefPointerRet(this.glob,
				[this.instructions, this.fixedLocation.getAssignmentInstruction(target)],
				target, this.type ? this.type.pointerTo : null, true);
		}
		return new RefPointerRet(this.glob, this.instructions, this.name,
			this.type ? this.type.pointerTo : null, false);
	}

	/** Fetch the address as an integer. */
	addressValue() {
		const glob = this.glob;
		const target = glob.tmp(glob.semantic.getTypeInfo('int'));
		if (this.fixedLocation) {
			const ptr = glob.tmp(this.type, 'pointer');
			return new IntRet(glob, [this.instructions,
				this.fixedLocation.getAssignmentInstruction(ptr),
				pointerToValue(glob, ptr, target)], target, glob.semantic.getTypeInfo('int'), true);
		}
		return new IntRet(glob, [this.instructions, pointerToValue(glob, this.name, target)],
			target, glob.semantic.getTypeInfo('int'), true);
	}
}

/* ------------------------------------------------------------------ *
 * Structs / unions
 * ------------------------------------------------------------------ */

/**
 * A struct or union. Depending on the type it is either a set of registers
 * (registry struct, one name per member) or a block of memory (pointer impl).
 * Both are handled by delegating copy to the generator, which already knows
 * how to memcpy / copy member-wise.
 */
export class StructRefRet extends VariableRet {

	constructor(glob, instructions, name, type, temporary = false, symbol = null) {
		super(glob, instructions, name, type, temporary);
		this.symbol = symbol;
		this.setAttribute('isStructRet', true);
		if (type && type.isTypeInfo) {
			this.setAttribute('isRegStruct', type.isRegStruct());
			this.setAttribute('isPointer', type.isPointerImpl());
			this.setAttribute('isPointerAccess', type.isPointerImpl());
		}
		if (symbol) {
			this.setAttribute('relevantSymbol', symbol);
			this.setAttribute('isSymbolic', true);
			this.setAttribute('isNearPointer', symbol.isNearPointer);
		}
	}

	/** @returns {StructRefRet} */
	static create(glob, instructions, returner, type, temporary = false, symbol = null) {
		return new this(glob, instructions, returner, type, temporary, symbol);
	}

	get() {
		return this;
	}

	/**
	 * Struct assignment: delegate to the generator's copy routine.
	 * @param {VariableRet} other
	 * @returns {StructRefRet}
	 */
	set(other) {
		const gen = this.glob.generator;
		if (!gen) throw new InfrastructureNotImplementedExpection('struct copy without generator');
		const source = other.get();
		const copy = gen.copyStruct(this.instructionReturn, source.instructionReturn,
			this.type, source.type);
		return new StructRefRet(this.glob, [this.instructions, source.instructions, copy],
			this.instructionReturn, this.type, this.temporary, this.symbol);
	}

	address() {
		// The address of a struct is a pointer TO that struct, not the struct itself
		return new RefPointerRet(this.glob, this.instructions, this.name,
			pointerTypeOf(this.glob, this.type), false);
	}
}

/* ------------------------------------------------------------------ *
 * Registry structs -- one register per member
 * ------------------------------------------------------------------ */

/**
 * A struct (or union) whose members are **not** laid out in memory but kept in
 * ordinary registers, one variable per member (`X.member`).
 *
 * This is the "registry struct" (`SymbolEntry.isRegistryStruct()` /
 * `TypeInfo.isRegStruct()`) representation:
 *  - `get()` is a no-op: the struct is already a value, and passing it around
 *    means passing every member;
 *  - `set()` copies member by member (there is nothing to memcpy);
 *  - `member(name)` gives the member as an ordinary value, so `a.b = 1` and
 *    `x = a.b` compile to a single `set`.
 *
 * Such a struct can never be turned into a pointer -- there is no address.
 */
export class RegistryStructRet extends StructRefRet {

	constructor(glob, instructions, name, type, temporary = false, symbol = null) {
		super(glob, instructions, name, type, temporary, symbol);
		this.setAttribute('isRegStruct', true);
		this.setAttribute('isPointer', false);
		this.setAttribute('isPointerAccess', false);
	}

	/**
	 * Look up a member of this registry struct.
	 * @param {string} memberName
	 * @param {TypeInfo?} memberType
	 * @returns {VariableRet}
	 */
	member(memberName, memberType = null) {
		const glob = this.glob;
		const type = memberType ?? memberTypeOf(glob, this.type, memberName);
		const name = `${this.instructionReturn}.${memberName}`;
		const member = valueClassOf(glob, type).create(glob, this.instructions, name, type, false);
		member.setAttribute('isFromRegStruct', true);
		member.setAttribute('isSymbolic', true);
		/*
		A member of a registry struct IS a register, so `&s.m` is simply its name
		-- which is exactly what a name pointer stores.
		*/
		if (glob.generator?.namePointerAllowed?.()) member.setAttribute('namePointersOnly', true);
		if (type && type.isTypeInfo) member.set_returns_type(type);
		return member;
	}

	/** The struct is a value already: reading it changes nothing. */
	get() {
		return this;
	}

	/**
	 * @param {VariableRet} other
	 * @returns {RegistryStructRet}
	 */
	set(other) {
		const gen = this.glob.generator;
		if (!gen) throw new InfrastructureNotImplementedExpection('struct copy without generator');
		const source = other.get();
		const copy = gen.copyStruct(this.instructionReturn, source.instructionReturn, this.type, source.type);
		return new RegistryStructRet(this.glob, [this.instructions, source.instructions, copy],
			this.instructionReturn, this.type, this.temporary, this.symbol);
	}

	address() {
		/*
		A registry struct is a group of registers, not a piece of memory, so it
		has no address in the usual sense. But a name-based pointer does not
		need one: it can carry the NAME of every member, which is exactly what
		`read @this <name>` consumes. So `&st` is allowed whenever that
		representation is permitted for this struct.
		*/
		const gen = this.glob.generator;
		if (gen?.structNamePointerAllowed?.(this.type)) {
			const ret = new NameStructPointerRet(this.glob, this.instructions,
				`${this.instructionReturn}__ptr`, this.type, true);
			ret.sourceBase = this.instructionReturn;
			return ret;
		}
		throw new InfrastructureNotImplementedExpection(
			`cannot take the address of '${this.instructionReturn}': a struct held in registers has no address (declare it 'volatile' or 'register', or take the address of a member instead)`);
	}
}

/**
 * The declared type of a struct member.
 * @param {Globals} glob
 * @param {TypeInfo?} structType
 * @param {string} memberName
 * @returns {TypeInfo?}
 */
export function memberTypeOf(glob, structType, memberName) {
	if (!structType || !structType.isTypeInfo) return null;
	const members = structType.members ?? [];
	for (const member of members) {
		if (member.name === memberName) {
			const inner = member.type ?? member.typeInfo ?? null;
			if (inner && inner.isTypeInfo) return inner;
			if (typeof inner === 'string') return glob.semantic.getTypeInfo(inner);
		}
	}
	return null;
}

/* ================================================================== *
 * Alternative representations
 *
 * Under `#pragma optimize(pointer)` (and version >= 150) a pointer does
 * not have to be a `X_block` / `X_pos` pair at all: it may store the
 * **name** of the register it points at, and reach it with
 *
 *      read  target @this <name>
 *      write value  @this <name>
 *
 * (`@this` is the processor itself, and `read` / `write` on it address
 * its own registers by name.)
 *
 * Arrays may be laid out in three further ways (see `selectArray
 * Implementation`), all of which avoid occupying memory:
 *   embedded.divided -- one register per element, picked with `select`;
 *   embedded.lookup  -- one register per element, named after a content
 *                       constant and found with `lookup` + `sensor @name`;
 *   encoded          -- a compile time constant table held in a string.
 * ================================================================== */

/* ------------------------------------------------------------------ *
 * Name pointers -- one register holding the target's NAME
 * ------------------------------------------------------------------ */

/**
 * A pointer that stores the *name* of the register it points at.
 *
 * Supported: taking the address of a register variable (`&x`), copying
 * another name pointer, dereferencing (`*p`, `*p = v`).
 *
 * NOT supported: pointer arithmetic (`p + 2`, `p[2]`) -- adding to a
 * name is meaningless -- and pointing at anything that is not a single
 * register (an array element, for instance).
 */
export class NamePointerRet extends PointerRet {

	constructor(glob, instructions, name, type, temporary = false, targetName = null) {
		super(glob, instructions, name, type, temporary);
		this.targetName = targetName;	// known at compile time, if at all
		this.setAttribute('isNamePointer', true);
	}

	/** The name itself is one word. */
	get width() {
		return 1;
	}

	/** The value is the address: nothing to read. */
	get() {
		return this;
	}

	address() {
		if (!this.getAttribute('isSymbolic')) return this;
		/*
		`&p` where p is itself a name pointer: the only thing one can do with
		that address is store it in another name pointer, and what it stores is
		the NAME of p.
		*/
		const glob = this.glob;
		const pointer = pointerTypeOf(glob, this.type);
		const target = glob.tmp(pointer, 'pointer');
		return new NamePointerRet(glob,
			[NamePointerRet.pointAt(glob, target, this.name)],
			target, pointer, true, this.name);
	}

	/** The type this pointer points at. */
	get pointee() {
		return (this.type && this.type.pointerTo) ? this.type.pointerTo : null;
	}

	/** The LValue reached through this pointer. */
	slot() {
		return new NameSlotRet(this.glob, this, this.pointee);
	}

	/**
	 * Materialize `&x`: store the name of `x` as a string.
	 * @param {Globals} glob
	 * @param {string} target Register receiving the name
	 * @param {string} assemblyName Name of the register being pointed at
	 * @returns {Instruction}
	 */
	static pointAt(glob, target, assemblyName) {
		return InstructionBuilder.set(target, `"${assemblyName}"`);
	}

	/**
	 * Read the pointed-to register.
	 * @param {string|SymbolEntry} target
	 * @returns {VariableRet}
	 */
	fetch(target) {
		const glob = this.glob;
		const inner = this.pointee;
		/*
		Only a single register can be named. A pointer counts as one here: when
		name pointers are in use, what `*pp` reads out of `p` is another NAME,
		so the result is a name pointer again (that is what makes `**pp` work).
		*/
		if (inner && widthOfType(inner) !== 1 && inner.kind !== 'pointer') {
			throw new InfrastructureNotImplementedExpection(
				'a name pointer cannot point at a value that is not a single register');
		}
		const name = nameOf(target);
		const source = this.targetName ? `"${this.targetName}"` : this.name;
		const instructions = [this.instructions, InstructionBuilder.read(name, '@this', source)];
		if (inner && inner.kind === 'pointer') {
			return new NamePointerRet(glob, instructions, name, inner, this.temporary, null);
		}
		return valueClassOf(glob, inner).create(glob, instructions, name, inner, this.temporary);
	}

	/**
	 * Write the pointed-to register.
	 * @param {VariableRet} other
	 * @returns {VariableRet}
	 */
	pointedSet(other) {
		const glob = this.glob;
		const inner = this.pointee;
		if (inner && widthOfType(inner) !== 1 && inner.kind !== 'pointer') {
			throw new InfrastructureNotImplementedExpection(
				'a name pointer cannot point at a value that is not a single register');
		}
		const value = other.get().convertTo(inner ?? other.type);
		const source = this.targetName ? `"${this.targetName}"` : this.name;
		return valueClassOf(glob, inner).create(glob,
			[this.instructions, value.instructions,
				InstructionBuilder.write(value.name, '@this', source)],
			value.name, inner, false);
	}

	deref() {
		const inner = (this.type && this.type.pointerTo) ? this.type.pointerTo : null;
		return this.fetch(this.glob.tmp(inner));
	}

	/**
	 * @param {VariableRet} other
	 * @returns {NamePointerRet}
	 */
	set(other) {
		const glob = this.glob;
		const value = other.get();
		if (value instanceof NamePointerRet) {
			return new NamePointerRet(glob, [this.instructions, value.instructions,
				InstructionBuilder.set(this.name, value.name)],
				this.name, this.type, this.temporary, value.targetName);
		}
		if (value instanceof ImmediateRet) {
			// `p = &x` written out as the plain name string
			return new NamePointerRet(glob, [this.instructions,
				InstructionBuilder.set(this.name, value.name)],
				this.name, this.type, this.temporary, null);
		}
		if (widthOfType(value.type) <= 1) {
			// Any other single register: it holds a name, so copy it
			return new NamePointerRet(glob, [this.instructions, value.instructions,
				InstructionBuilder.set(this.name, value.name)],
				this.name, this.type, this.temporary, null);
		}
		throw new InvalidCastException(other, this);
	}

	/** Not supported: adding to a name is meaningless. */
	offset(step, backward = false) {
		throw new InfrastructureNotImplementedExpection(
			'pointer arithmetic on a name pointer (point it at an array instead)');
	}
}

/**
 * A pointer only ever carries a name when the optimisation has been
 * asked for, the target version supports `read @this`, and static
 * analysis decided that this particular pointer is eligible (see
 * `analyzeNamePointers` in mindcOptimizer.js).
 *
 * @param {any} symbol The pointer's SymbolEntry
 * @param {{targetVersion: number, pointerOptimize: boolean}} options
 * @returns {boolean}
 */
export function usesNamePointer(symbol, options = {}) {
	if (!symbol) return false;
	// `read @this <name>` is what makes it possible at all
	if ((options.targetVersion ?? 0) < 150) return false;
	/*
	`byname(&c)` asks for it explicitly, so it does not need the pragma --
	only the version. Automatic name pointers still need both.
	*/
	if (symbol.forcedNamePointer === true) return true;
	if (options.pointerOptimize !== true) return false;
	return symbol.usesNamePointer === true;
}

/**
 * The slot a name pointer points at: reading emits `read @this <name>`,
 * writing emits `write @this <name>`.
 */
export class NameSlotRet extends VariableRet {

	/**
	 * @param {Globals} glob
	 * @param {NamePointerRet} pointer
	 * @param {TypeInfo?} type
	 */
	constructor(glob, pointer, type = null) {
		super(glob, pointer.instructions, pointer.name, type ?? pointer.pointee, pointer.temporary);
		this.pointer = pointer;
		this.setAttribute('isNameSlot', true);
	}

	get width() {
		return 1;
	}

	address() {
		return this.pointer;
	}

	get() {
		return this.pointer.fetch(this.glob.tmp(this.type));
	}

	/**
	 * @param {VariableRet} other
	 * @returns {VariableRet}
	 */
	set(other) {
		return this.pointer.pointedSet(other);
	}
}

/**
 * Which representation a pointer symbol should get.
 *
 * @param {any} symbol
 * @param {{targetVersion: number, pointerOptimize: boolean}} options
 * @returns {'name' | 'memory'}
 */
export function pointerStyleOf(symbol, options = {}) {
	return usesNamePointer(symbol, options) ? 'name' : 'memory';
}

/* ------------------------------------------------------------------ *
 * Arrays -- subscript access, several possible layouts
 * ------------------------------------------------------------------ */

/**
 * Turn an index (a `VariableRet`, a number or a name) into an operand.
 *
 * @param {Globals} glob
 * @param {VariableRet | number | string} index
 * @returns {{instructions: any[], name: string, constant: boolean, value: number?}}
 */
function operandOf(glob, index) {
	if (index instanceof VariableRet) {
		const value = index.get();
		return { instructions: [value.instructions], name: value.name, constant: false, value: null };
	}
	const text = String(index);
	const numeric = /^-?\d+$/.test(text) ? Number(text) : null;
	return { instructions: [], name: text, constant: numeric !== null, value: numeric };
}

/**
 * Base class of the array representations.
 *
 * Every representation supports `element(i)`, which returns an LValue
 * (`get()` / `set()`); only the traditional layout can be converted
 * into a pointer, since the others have no address.
 */
export class ArrayRet extends VariableRet {

	constructor(glob, instructions, name, type, temporary = false, length = 0) {
		super(glob, instructions, name, type, temporary);
		this.length = length;
		this.setAttribute('isArrayRet', true);
	}

	get elementTypeInfo() {
		return (this.type && this.type.pointerTo) ? this.type.pointerTo : null;
	}

	get elementSize() {
		return widthOfType(this.elementTypeInfo);
	}

	/**
	 * The LValue of `a[index]`.
	 * @param {VariableRet | number | string} index
	 * @returns {VariableRet}
	 */
	element(index) {
		throw new InfrastructureNotImplementedExpection(`subscript of ${this.constructor.name}`);
	}

	/** Arrays are never read as a whole. */
	get() {
		return this;
	}

	/** Whether this layout can be turned into a pointer. */
	get convertibleToPointer() {
		return false;
	}

	address() {
		throw new InfrastructureNotImplementedExpection(
			`${this.constructor.name} cannot be converted into a pointer`);
	}
}

/**
 * The ordinary layout: a contiguous block of memory reached through a
 * `X_block` / `X_pos` pointer. The only convertible one.
 */
export class TraditionalArrayRet extends ArrayRet {

	/**
	 * @param {Globals} glob
	 * @param {PointerRet} pointer Address of the first element
	 * @param {TypeInfo?} type
	 * @param {number?} length
	 */
	constructor(glob, pointer, type = null, length = 0) {
		super(glob, pointer.instructions, pointer.name, type ?? pointer.type, pointer.temporary, length);
		this.pointer = pointer;
	}

	get convertibleToPointer() {
		return true;
	}

	address() {
		return this.pointer;
	}

	/** `a[i]` is the memory slot at `base + i * size`. */
	element(index) {
		const glob = this.glob;
		const operand = operandOf(glob, index);
		const size = this.elementSize;
		const base = this.pointer.get();
		let step = operand.name, extra = [];
		if (size > 1) {
			if (operand.constant) {
				step = operand.value * size;
			} else {
				const scaled = glob.tmp(null);
				extra = [InstructionBuilder.op('mul', scaled, operand.name, size)];
				step = scaled;
			}
		}
		const address = (step === 0) ? base : base.offset(step);
		const slot = new MemoryRefRet(glob, [operand.instructions, extra, address.instructions],
			address.name, this.elementTypeInfo, true);
		// Keep where the block half comes from, so the read needs no copy
		return slot;
	}
}

/** One element of a `DividedArrayRet`. */
class DividedSlotRet extends VariableRet {

	/**
	 * @param {Globals} glob
	 * @param {DividedArrayRet} array
	 * @param {{instructions: any[], name: string, constant: boolean, value: number?}} operand
	 */
	constructor(glob, array, operand) {
		super(glob, operand.instructions, array.name, array.elementTypeInfo, false);
		this.array = array;
		this.operand = operand;
	}

	/**
	 * A constant index names one register outright: no code at all.
	 * Otherwise every element is tried in turn with `select`.
	 */
	get() {
		const glob = this.glob;
		if (this.operand.constant) {
			const index = this.operand.value;
			if (index < 0 || index >= this.array.length) {
				throw new InfrastructureNotImplementedExpection('subscript out of range');
			}
			return valueClassOf(glob, this.type).create(glob, this.instructions,
				this.array.elementName(index), this.type, false);
		}
		const helper = this.array.useAccessHelper
			? ensureArrayAccessHelper(glob, this.array, 'get') : null;
		if (helper) {
			const call = glob.funcReg.getFunctionCall(helper,
				new Map([['__arridx', this.operand.name]]), null);
			/*
			Copy out of `__arrret` into a real temporary. Handing `__arrret`
			itself to the caller loses the write: a shared result register is
			not something a value can own, and the assignment then emits
			nothing at all.
			*/
			const target = glob.tmp(this.type);
			return valueClassOf(glob, this.type).create(glob,
				[this.instructions, call, InstructionBuilder.set(target, '__arrret')],
				target, this.type, true);
		}
		const target = glob.tmp(this.type);
		const parts = [this.instructions,
			InstructionBuilder.set(target, this.array.elementName(0))];
		for (let i = 1; i < this.array.length; i++) {
			parts.push(InstructionBuilder.selectx(target, `equal ${this.operand.name} ${i}`,
				this.array.elementName(i), target));
		}
		return valueClassOf(glob, this.type).create(glob, parts, target, this.type, true);
	}

	/**
	 * Writing cannot be done with `select` (it picks a *value*, not a
	 * target), so the *name* of the element is selected and then used
	 * through `write @this <name>` -- exactly like a name pointer.
	 */
	set(other) {
		const glob = this.glob;
		const value = other.get().convertTo(this.type ?? other.type);
		if (this.operand.constant) {
			const index = this.operand.value;
			if (index < 0 || index >= this.array.length) {
				throw new InfrastructureNotImplementedExpection('subscript out of range');
			}
			return valueClassOf(glob, this.type).create(glob,
				[this.instructions, value.instructions,
					InstructionBuilder.set(this.array.elementName(index), value.name)],
				value.name, this.type, false);
		}
		const helper = this.array.useAccessHelper
			? ensureArrayAccessHelper(glob, this.array, 'set') : null;
		if (helper) {
			const call = glob.funcReg.getFunctionCall(helper,
				new Map([['__arridx', this.operand.name], ['__arrval', value.name]]), null);
			return valueClassOf(glob, this.type).create(glob,
				[this.instructions, value.instructions, call], value.name, this.type, false);
		}
		const chosen = glob.tmp(null);
		const parts = [this.instructions, value.instructions,
			InstructionBuilder.set(chosen, `"${this.array.elementName(0)}"`)];
		for (let i = 1; i < this.array.length; i++) {
			parts.push(InstructionBuilder.selectx(chosen, `equal ${this.operand.name} ${i}`,
				`"${this.array.elementName(i)}"`, chosen));
		}
		parts.push(InstructionBuilder.write(value.name, '@this', chosen));
		return valueClassOf(glob, this.type).create(glob, parts, value.name, this.type, false);
	}
}

/**
 * `embedded.divided`: every element is a register of its own, and the
 * subscript picks which one.
 *
 * A constant subscript costs nothing at all; a variable one costs
 * `length - 1` `select` instructions -- which is why this is only chosen
 * for short arrays, or when no memory is left.
 */
export class DividedArrayRet extends ArrayRet {

	/**
	 * @param {Globals} glob
	 * @param {string} baseName Elements are named `baseName_0`, ...
	 * @param {TypeInfo?} type
	 * @param {number} length
	 */
	constructor(glob, baseName, type, length) {
		super(glob, [], baseName, type, false, length);
	}

	elementName(index) {
		return `${this.name}_${index}`;
	}

	element(index) {
		return new DividedSlotRet(this.glob, this, operandOf(this.glob, index));
	}
}

/**
 * `embedded.lookup`: every element is a register named after a content
 * constant (`copper`, `water`, `@alpha`'s name, ...).
 *
 * The subscript is turned into the constant with `lookup`, its name is
 * read with `sensor @name`, and the register is then reached exactly
 * like a name pointer:
 *
 *      lookup item  T  i
 *      sensor       N  T @name
 *      read   target @this N
 *
 * This costs the same three instructions whatever the array is long,
 * but it only works for at most `list.length` elements.
 *
 * @see mindcBase.js -- objectList / liquidList / unitList / buildingList
 */
export class LookupArrayRet extends ArrayRet {

	/**
	 * @param {Globals} glob
	 * @param {string} category item | liquid | unit | building
	 * @param {string[]} names The register names, one per element
	 * @param {TypeInfo?} type
	 */
	constructor(glob, category, names, type, deviceReg = null) {
		super(glob, [], `__lookup_${category}`, type, false, names.length);
		this.category = category;
		this.names = names;
		/*
		`deviceReg === null` means THIS processor (`read @this <name>`), which
		is the ordinary case. A remote array passes the other processor here:
		everything up to the name is the same on both sides, since the lookup
		table (item / liquid / unit / building) is a property of the game itself.
		*/
		this.deviceReg = deviceReg;
	}

	/** How many elements this category can address. */
	get capacity() {
		return this.names.length;
	}

	/**
	 * @param {VariableRet | number | string} index
	 * @returns {VariableRet}
	 */
	element(index) {
		const glob = this.glob;
		const operand = operandOf(glob, index);
		if (operand.constant) {
			const at = this.names[operand.value];
			if (at === undefined) throw new InfrastructureNotImplementedExpection('subscript out of range');
			/*
			The name is known, so nothing has to be computed -- but if the
			element belongs to ANOTHER processor the name alone is not enough:
			it has to be read from there. Returning the bare name would read
			this processor's own register of the same name.
			*/
			if (this.deviceReg) {
				/*
				It has to be a SLOT, not the value read out of it: `ru[2] = 7`
				has to WRITE over there. Returning a read result here made the
				write land in a temporary and vanish.
				*/
				return new RemoteSlotRet(glob, this.deviceReg, `"${at}"`,
					this.elementTypeInfo);
			}
			return valueClassOf(glob, this.elementTypeInfo).create(glob, [],
				at, this.elementTypeInfo, false);
		}
		return new LookupSlotRet(glob, this, operand, this.elementTypeInfo, this.deviceReg);
	}
}

/** One element of a `LookupArrayRet`, addressed at run time. */
class LookupSlotRet extends VariableRet {

	constructor(glob, array, operand, type, deviceReg = null) {
		super(glob, operand.instructions, array.name, type, false);
		this.array = array;
		this.operand = operand;
		this.deviceReg = deviceReg ?? null;
	}

	/** Which processor holds the element: `@this`, or another one. */
	get device() {
		return this.deviceReg ?? '@this';
	}

	/**
	 * The register name of the element, in `target`.
	 * @param {Globals} glob
	 * @param {string} content Register receiving the looked-up constant
	 * @param {string} target Register receiving the name
	 * @returns {Instruction}
	 */
	resolve(glob, content, target) {
		return new Instruction([
			InstructionBuilder.lookup(this.array.category, content, this.operand.name),
			InstructionBuilder.sensor(target, content, '@name')
		]);
	}

	get() {
		const glob = this.glob;
		const content = glob.tmp(glob.semantic.getTypeInfo('content_t'));
		const nameHolder = glob.tmp(null);
		const target = glob.tmp(this.type);
		return valueClassOf(glob, this.type).create(glob, [
			this.instructions, this.resolve(glob, content, nameHolder),
			InstructionBuilder.read(target, this.device, nameHolder)
		], target, this.type, true);
	}

	set(other) {
		const glob = this.glob;
		const content = glob.tmp(glob.semantic.getTypeInfo('content_t'));
		const nameHolder = glob.tmp(null);
		const value = other.get().convertTo(this.type ?? other.type);
		return valueClassOf(glob, this.type).create(glob, [
			this.instructions, value.instructions, this.resolve(glob, content, nameHolder),
			InstructionBuilder.write(value.name, this.device, nameHolder)
		], value.name, this.type, false);
	}
}

/* ------------------------------------------------------------------ *
 * `encoded` -- a compile time constant table held in a string
 * ------------------------------------------------------------------ */

/**
 * The character codes that may appear in an encoded table.
 *
 * Everything from `\` (92) onwards is excluded -- and so is `"` (34) --
 * because those would need escaping inside a string literal.
 */
export const ENCODED_CODES = (() => {
	const codes = [];
	for (let code = 35; code <= 91; code++) codes.push(code);
	return codes;
})();

/**
 * `read target "<table>" i` yields the ASCII **code** of the i-th
 * character, so every digit comes back shifted by this amount.
 */
export const ENCODED_OFFSET = ENCODED_CODES[0];

/** How many distinct values one character can carry. */
export const ENCODED_RADIX = ENCODED_CODES.length;

/**
 * Turn digit values into the string that stores them.
 * @param {number[]} digits Every digit must be in `[0, ENCODED_RADIX)`.
 * @returns {string}
 */
export function encodeDigits(digits) {
	return digits.map(d => String.fromCharCode(ENCODED_CODES[d])).join('');
}

/**
 * Build the mapping `index -> value` of a constant array.
 *
 * A string is a read-only ROM: `read target "<table>" i` yields the
 * ASCII code of the i-th character, which is one instruction for the
 * whole lookup. Values that do not fit in a single character are split
 * into base-`ENCODED_RADIX` digits, one table per digit, and put back
 * together with Horner's rule (3 instructions per extra digit).
 *
 * The alternative -- a chain of `select` -- costs `n - 1` instructions,
 * so the cheaper of the two is returned.
 *
 * @param {number[]} values The constant contents, index by index
 * @returns {{tables: string[], radix: number, shift: number, digits: number,
 *            cost: number, kind: 'single' | 'digits' | 'chain'}}
 */
export function buildEncodedMapping(values, options = {}) {
	const radix = ENCODED_RADIX;
	/*
	When the program asked for `array("encoded", ...)` by name the string table
	is the whole point, so the `select` chain fallback is not allowed: the
	values must come out of a table even if a chain would be shorter.
	*/
	const forceTable = options.forceTable === true;
	const numbers = values.map(v => Number(v));
	if (numbers.some(v => !Number.isFinite(v))) {
		throw new InfrastructureNotImplementedExpection('encoded arrays must hold numbers');
	}
	const lowest = Math.min(...numbers);
	const shifts = (lowest < 0) ? [0, -lowest] : [0];
	let best = null;
	for (const shift of shifts) {
		const shifted = numbers.map(v => v + shift);
		if (shifted.some(v => v < 0)) continue;
		const highest = Math.max(...shifted);
		let digits = 1, span = radix;
		while (highest >= span) { digits++; span *= radix; }
		const tables = [];
		for (let k = 0; k < digits; k++) {
			const power = Math.pow(radix, k);
			tables.push(encodeDigits(shifted.map(v => Math.floor(v / power) % radix)));
		}
		/*
		Every character read comes back as `digit + ENCODED_OFFSET`, so all of
		them together add `OFFSET * (1 + R + ... + R^(K-1))` to the Horner sum.
		That whole quantity is a constant, so one `sub` at the end undoes it
		(together with the shift that made the values non-negative).
		*/
		let spanSum = 0;
		for (let k = 0; k < digits; k++) spanSum += Math.pow(radix, k);
		const correction = ENCODED_OFFSET * spanSum + shift;
		const cost = (digits === 1 ? 2 : 3 * digits - 1) - (correction !== 0 ? 0 : 1);
		if (!best || cost < best.cost) {
			best = { tables, radix, shift, digits, cost, correction, kind: digits === 1 ? 'single' : 'digits' };
		}
	}
	// The fallback: a plain chain of comparisons
	const chainCost = forceTable ? Infinity : Math.max(1, numbers.length - 1);
	if (!best || chainCost < best.cost) {
		best = { tables: [], radix, shift: 0, digits: 0, cost: chainCost, correction: 0, kind: 'chain' };
	}
	return best;
}

/**
 * A constant array held as one or more strings.
 *
 * Only reading makes sense -- the array is `const`, so `set()` refuses.
 */
export class EncodedArrayRet extends ArrayRet {

	/**
	 * @param {Globals} glob
	 * @param {number[]} values
	 * @param {TypeInfo?} type
	 * @param {{tables: string[], radix: number, shift: number, digits: number,
	 *          cost: number, kind: string}?} mapping From `buildEncodedMapping`
	 */
	constructor(glob, values, type, mapping = null, forceTable = false) {
		super(glob, [], '__encoded', type, false, values.length);
		this.values = values;
		this.mapping = mapping ?? buildEncodedMapping(values, { forceTable });
	}

	/** @param {VariableRet | number | string} index */
	element(index) {
		const glob = this.glob;
		const operand = operandOf(glob, index);
		// A constant index is known at compile time: emit the value itself
		if (operand.constant) {
			const value = this.values[operand.value];
			if (value === undefined) throw new InfrastructureNotImplementedExpection('subscript out of range');
			return new ImmediateRet(glob, String(value), this.elementTypeInfo);
		}
		return new EncodedSlotRet(glob, this, operand, this.elementTypeInfo);
	}
}

/** One element of an `EncodedArrayRet`, addressed at run time. */
class EncodedSlotRet extends VariableRet {

	constructor(glob, array, operand, type) {
		super(glob, operand.instructions, array.name, type, false);
		this.array = array;
		this.operand = operand;
	}

	get() {
		const glob = this.glob;
		const mapping = this.array.mapping;
		const target = glob.tmp(this.type);
		const parts = [this.instructions];
		if (mapping.kind === 'chain') {
			const valueAt = i => {
				const v = this.array.values[i];
				return (v === undefined || v === null || !Number.isFinite(Number(v))) ? 0 : Number(v);
			};
			parts.push(InstructionBuilder.set(target, String(valueAt(0))));
			for (let i = 1; i < this.array.values.length; i++) {
				parts.push(InstructionBuilder.selectx(target,
					`equal ${this.operand.name} ${i}`, String(valueAt(i)), target));
			}
			return valueClassOf(glob, this.type).create(glob, parts, target, this.type, true);
		}
		// Horner: start with the highest digit, then shift and add
		parts.push(InstructionBuilder.read(target, `"${mapping.tables[mapping.digits - 1]}"`,
			this.operand.name));
		for (let k = mapping.digits - 2; k >= 0; k--) {
			const digit = glob.tmp(null);
			parts.push(InstructionBuilder.read(digit, `"${mapping.tables[k]}"`, this.operand.name));
			parts.push(InstructionBuilder.op('mul', target, target, mapping.radix));
			parts.push(InstructionBuilder.op('add', target, target, digit));
		}
		if (mapping.correction !== 0) {
			parts.push(InstructionBuilder.op('sub', target, target, mapping.correction));
		}
		return valueClassOf(glob, this.type).create(glob, parts, target, this.type, true);
	}

	set(other) {
		throw new InfrastructureNotImplementedExpection(
			'an encoded array is constant and cannot be written to');
	}
}

/* ------------------------------------------------------------------ *
 * Choosing a representation
 * ------------------------------------------------------------------ */

/**
 * Rough instruction cost of one subscript access for each layout.
 *
 * The numbers are per-access estimates used only to *compare* layouts;
 * the traditional layout additionally needs `length * size` memory
 * cells, and everything but the traditional layout needs v150.
 */
export function accessCostOf(kind, length, digits = 1) {
	switch (kind) {
		case 'traditional': return 4;				// getlink + read/write + address
		case 'embedded.divided': return Math.max(1, length);	// a `select` per element
		case 'embedded.lookup': return 3;			// lookup + sensor + read/write
		case 'encoded': return digits <= 1 ? 2 : 3 * digits - 1;
		default: return 4;
	}
}

/**
 * Pick the layout of an array.
 *
 * The rules, in order:
 *  1. an explicitly requested layout (`array("...")`) always wins;
 *  2. otherwise memory is used while there is enough of it -- that is
 *     the layout with the cheapest access;
 *  3. when memory runs out, the layouts that need none are considered,
 *     and the one with the fewest instructions is taken. If even that
 *     does not fit the remaining instruction budget, the traditional
 *     layout is used anyway: running out of instructions is worse than
 *     running out of memory.
 *
 * @param {object} info
 * @param {number} info.length Number of elements
 * @param {number} [info.size] Words per element
 * @param {boolean} [info.isConst]
 * @param {number[]} [info.values] Contents, for constant arrays
 * @param {number} [info.availableMemory] Free memory cells
 * @param {number} [info.instructionBudget] Instructions still available
 * @param {number} [info.targetVersion]
 * @param {string?} [info.requested] Layout asked for by `array(...)`
 * @param {string[]?} [info.lookupNames] Names for the `lookup` layout
 * @returns {{type: string, implement: string?, reason: string}}
 */
/**
 * Build (once) a helper function that reads or writes one element of an
 * "embedded" array, and return the name to call it by.
 *
 * Why: `embedded.divided` picks an element with a `select` per element, and
 * that chain was emitted AT EVERY ACCESS. With `n` elements and `m` accesses
 * that is `m * n` instructions, so the array is effectively duplicated into
 * every call site -- which is the opposite of what "embedded" means (embedded
 * in the PROCESSOR, not in every use).
 *
 * With a helper each access costs a fixed handful of instructions and the
 * chain exists once:
 *
 *     set __arridx <i>
 *     op add __stackpos @counter 1
 *     set @counter _arrget_NAME
 *     set __stackpos __internal_stackpos
 *
 * @param {Globals} glob
 * @param {ArrayRet} array
 * @param {'get'|'set'} kind
 * @returns {string?} the function name, or null when there is no register to
 *          put it in
 */
export function ensureArrayAccessHelper(glob, array, kind) {
	const reg = glob?.funcReg;
	if (!reg) return null;
	const key = `__arr${kind}_${String(array.name).replace(/[^A-Za-z0-9_]/g, '_')}`;
	if (reg.functionCollection.has(key)) return key;
	const length = array.length;
	const body = new Instruction();
	if (kind === 'get') {
		body.concat(InstructionBuilder.set('__arrret', array.elementName(0)));
		for (let i = 1; i < length; i++) {
			body.concat(InstructionBuilder.selectx('__arrret', `equal __arridx ${i}`,
				array.elementName(i), '__arrret'));
		}
	} else {
		// `select` picks a VALUE, not a target, so pick the NAME and write
		// through `@this` -- the same trick a name pointer uses.
		body.concat(InstructionBuilder.set('__arrsel', `"${array.elementName(0)}"`));
		for (let i = 1; i < length; i++) {
			body.concat(InstructionBuilder.selectx('__arrsel', `equal __arridx ${i}`,
				`"${array.elementName(i)}"`, '__arrsel'));
		}
		body.concat(InstructionBuilder.write('__arrval', '@this', '__arrsel'));
	}
	reg.addFunction(key, body, null, [], true, false, true);
	return key;
}

/**
 * Build (once) a helper that turns a BUILDING into the NUMBER a pointer needs.
 *
 * A pointer is `X_block` / `X_pos`, and `_block` has to be a number: pointers
 * are stored in memory (that is how the block chain works) and memory only
 * holds numbers. `memsp` however is handed a building, so the link index has
 * to be looked up at run time -- by walking the links and comparing with
 * `strictEqual`, which is the only comparison that also looks at the type and
 * therefore tells `null` apart from `0`.
 *
 *     set __memsp_dev <building>
 *     op add __stackpos @counter 1
 *     set @counter ___memsp
 *
 * @param {Globals} glob
 * @returns {string} the function name
 */
export function ensureMemspHelper(glob) {
	const reg = glob?.funcReg;
	if (!reg) return null;
	const key = '__memsp';
	if (reg.functionCollection.has(key)) return key;
	const body = new Instruction();
	const loop = InstructionBuilder.jump('{loop}', 'lessThan', '__memsp_i', '@links');
	const found = InstructionBuilder.jump('{found}', 'notEqual', '__memsp_hit', 'false');
	body.concat(InstructionBuilder.set('__memsp_i', 0));
	body.concat(new InstructionReferrer(loop, 'loop'));
	body.concat(InstructionBuilder.getlink('__memsp_cur', '__memsp_i'));
	body.concat(InstructionBuilder.op('strictEqual', '__memsp_hit', '__memsp_cur', '__memsp_dev'));
	body.concat(found);
	body.concat(InstructionBuilder.op('add', '__memsp_i', '__memsp_i', 1));
	body.concat(loop);
	body.concat(new InstructionReferrer(found, 'found'));
	/*
	Whatever happens we land here: when the building was found `__memsp_i` is
	its index, and when it was not `__memsp_i` has run past the last link, so
	`__memsp_ret` is out of range -- the same thing `getlink` returns for a
	missing building.
	*/
	body.concat(InstructionBuilder.set('__memsp_ret', '__memsp_i'));
	reg.addFunction(key, body, null, [], true, false, true);
	return key;
}

/* ------------------------------------------------------------------ *
 * Remote access -- variables that live in ANOTHER processor
 * ------------------------------------------------------------------ */

/**
 * The two registers a remote binding needs: which processor, and which
 * variable in it.
 *
 * @param {string} assembly The symbol's assembly name
 * @returns {string}
 */
export function remoteDeviceRegOf(assembly) {
	return `${assembly}__rdev`;
}

/**
 * @param {string} assembly The symbol's assembly name
 * @returns {string}
 */
export function remoteNameRegOf(assembly) {
	return `${assembly}__rnm`;
}

/**
 * One variable of another processor.
 *
 * Everything is done with the ordinary `read` / `write`, only the building
 * operand is the other processor instead of `@this`:
 *
 *      read  tmp <devReg> <nameReg>
 *      write val <devReg> <nameReg>
 *
 * So `remote int rs` and `*p` for a `remote int *p` behave the same way.
 */
export class RemoteSlotRet extends VariableRet {

	/**
	 * @param {Globals} glob
	 * @param {string} deviceReg Register holding the other processor
	 * @param {string} nameReg Register holding the variable's name
	 * @param {TypeInfo?} type
	 * @param {any} [instructions=[]]
	 */
	constructor(glob, deviceReg, nameReg, type = null, instructions = []) {
		super(glob, instructions, deviceReg, type, false);
		this.deviceReg = deviceReg;
		this.nameReg = nameReg;
		this.setAttribute('isRemoteSlot', true);
		this.setAttribute('disallowReplacement', true);
	}

	/** The value read out of it is what the type says. */
	get width() {
		return widthOfType(this.type);
	}

	/** `&rs` -- the binding itself, so that it can be copied around. */
	address() {
		const ret = new RemotePointerRet(this.glob, this.deviceReg, this.nameReg,
			pointerTypeOf(this.glob, this.type), this.instructions);
		/*
		It is TWO registers (processor + name). `replace_variable` would
		rename the one it is told about and leave the other behind, so it has
		to be handed over as a value instead.
		*/
		ret.setAttribute('disallowReplacement', true);
		return ret;
	}

	get() {
		const inner = this.type;
		if (inner && widthOfType(inner) !== 1 && inner.kind !== 'pointer') {
			throw new InfrastructureNotImplementedExpection(
				'a remote variable cannot hold a value of more than one register');
		}
		const target = this.glob.tmp(inner);
		const read = [this.instructions,
			InstructionBuilder.read(nameOf(target), this.deviceReg, this.nameReg)];
		if (inner && inner.kind === 'pointer') {
			return new RemotePointerRet(this.glob, this.deviceReg, this.nameReg, inner, read);
		}
		return valueClassOf(this.glob, inner).create(this.glob, read, nameOf(target), inner, true);
	}

	/**
	 * @param {VariableRet} other
	 * @returns {VariableRet}
	 */
	set(other) {
		const value = other.get().convertTo(this.type ?? other.type);
		return valueClassOf(this.glob, this.type).create(this.glob,
			[this.instructions, value.instructions,
				InstructionBuilder.write(value.name, this.deviceReg, this.nameReg)],
			value.name, this.type, false);
	}
}

/**
 * A pointer to a variable of another processor: TWO registers (device, name).
 *
 * This is deliberately always two registers, even when the processor is known
 * at compile time -- one representation, so that a remote pointer can be
 * copied, passed around and stored without a second code path.
 */
export class RemotePointerRet extends PointerRet {

	/**
	 * @param {Globals} glob
	 * @param {string} deviceReg
	 * @param {string} nameReg
	 * @param {TypeInfo?} type
	 * @param {any} [instructions=[]]
	 */
	constructor(glob, deviceReg, nameReg, type = null, instructions = []) {
		super(glob, instructions, deviceReg, type, false);
		this.deviceReg = deviceReg;
		this.nameReg = nameReg;
		this.setAttribute('isRemotePointer', true);
		this.setAttribute('isPointer', true);
		this.setAttribute('disallowReplacement', true);
	}

	/** Device + name. */
	get width() {
		return 2;
	}

	get() {
		return this;
	}

	get pointee() {
		return (this.type && this.type.pointerTo) ? this.type.pointerTo : null;
	}

	/** The LValue reached through this pointer. */
	slot() {
		return new RemoteSlotRet(this.glob, this.deviceReg, this.nameReg, this.pointee, this.instructions);
	}

	/**
	 * Copy the binding (device, name) into another remote pointer.
	 * @param {VariableRet} other
	 * @returns {RemotePointerRet}
	 */
	set(other) {
		const value = other.get();
		if (value instanceof RemotePointerRet) {
			return new RemotePointerRet(this.glob, this.deviceReg, this.nameReg, this.type,
				[this.instructions, value.instructions,
					InstructionBuilder.set(this.deviceReg, value.deviceReg),
					InstructionBuilder.set(this.nameReg, value.nameReg)]);
		}
		if (value instanceof NamePointerRet) {
			/*
			`remote int *pself = ss;` -- a local name pointer becomes a remote
			one pointing at THIS processor.
			*/
			return new RemotePointerRet(this.glob, this.deviceReg, this.nameReg, this.type,
				[this.instructions, value.instructions,
					InstructionBuilder.set(this.deviceReg, '@this'),
					InstructionBuilder.set(this.nameReg, value.targetName
						? `"${value.targetName}"` : value.name)]);
		}
		throw new InvalidCastException(other, this);
	}

	/** Not supported: the name is a string, not an address. */
	offset(step, backward = false) {
		throw new InfrastructureNotImplementedExpection(
			'pointer arithmetic on a remote pointer');
	}

	deref() {
		return this.slot().get();
	}
}


/* ------------------------------------------------------------------ *
 * Remote aggregates -- arrays and stored pointers held by a processor
 * ------------------------------------------------------------------ */

/**
 * An array living in ANOTHER processor: every element is a register of that
 * processor, named `<array>_<index>`, exactly as an `embedded_divided` array
 * would be here.
 */
/**
 * One element of a `NameArrayRet`, reached through the name its register
 * holds.
 */
class NameArraySlotRet extends VariableRet {

	/**
	 * @param {Globals} glob
	 * @param {NameArrayRet} array
	 * @param {{instructions: any[], name: string, constant: boolean, value: number?}} operand
	 */
	constructor(glob, array, operand) {
		super(glob, operand.instructions, array.name, array.elementTypeInfo, false);
		this.array = array;
		this.operand = operand;
	}

	/**
	 * A constant subscript names the register that holds the element's NAME
	 * outright, so the access is a single `read <device> <that register>`.
	 */
	get() {
		const glob = this.glob;
		const target = glob.tmp(this.type);
		const tn = nameOf(target);
		if (this.operand.constant) {
			const i = this.operand.value;
			if (i < 0 || i >= this.array.length) {
				throw new InfrastructureNotImplementedExpection('subscript out of range');
			}
			return valueClassOf(glob, this.type).create(glob,
				[this.instructions,
					InstructionBuilder.read(tn, this.array.device, this.array.elementName(i))],
				tn, this.type, true);
		}
		const chosen = glob.tmp(null, 'nmarr');
		const cn = nameOf(chosen);
		const parts = [this.instructions,
			InstructionBuilder.set(cn, this.array.elementName(0))];
		for (let i = 1; i < this.array.length; i++) {
			parts.push(InstructionBuilder.selectx(cn, `equal ${this.operand.name} ${i}`,
				this.array.elementName(i), cn));
		}
		parts.push(InstructionBuilder.read(tn, this.array.device, cn));
		return valueClassOf(glob, this.type).create(glob, parts, tn, this.type, true);
	}

	/**
	 * Writing goes THROUGH the name: `write <value> <device> <name register>`.
	 * That is what makes the write land in the caller's array instead of in a
	 * copy -- a non-memory array is passed by reference, like every array in C.
	 */
	set(other) {
		const glob = this.glob;
		const value = other.get().convertTo(this.type ?? other.type);
		if (this.operand.constant) {
			const i = this.operand.value;
			if (i < 0 || i >= this.array.length) {
				throw new InfrastructureNotImplementedExpection('subscript out of range');
			}
			return valueClassOf(glob, this.type).create(glob,
				[this.instructions, value.instructions,
					InstructionBuilder.write(value.name, this.array.device, this.array.elementName(i))],
				value.name, this.type, false);
		}
		const chosen = glob.tmp(null, 'nmarr');
		const cn = nameOf(chosen);
		const parts = [this.instructions, value.instructions,
			InstructionBuilder.set(cn, this.array.elementName(0))];
		for (let i = 1; i < this.array.length; i++) {
			parts.push(InstructionBuilder.selectx(cn, `equal ${this.operand.name} ${i}`,
				this.array.elementName(i), cn));
		}
		parts.push(InstructionBuilder.write(value.name, this.array.device, cn));
		return valueClassOf(glob, this.type).create(glob, parts, value.name, this.type, false);
	}
}

/**
 * An array reached through the NAMES of its elements.
 *
 * This is how an array is handed to a FUNCTION. A non-memory array has no
 * address, so a pointer cannot be handed over -- but its elements do have
 * names, and a name is an ordinary value that fits in a register. So the
 * parameter becomes one register per element, each holding the NAME of the
 * caller's element:
 *
 *     set g:.f:test.s_0 "g:.r_0"      <- at the call site
 *     write 1 @this g:.f:test.s_0     <- inside the function
 *
 * Going through the name is what makes this pass BY REFERENCE: the write
 * really does change the caller's array. Copying the values in instead would
 * make `s[1] = 1` write into the parameter's own registers and lose it.
 *
 * `deviceReg` is null for a local array (accessed with `read/write @this`) and
 * the processor register for a `remote` array, whose elements live in another
 * processor.
 */
export class NameArrayRet extends ArrayRet {

	/**
	 * @param {Globals} glob
	 * @param {string} baseName Element `i`'s name lives in `baseName_i`
	 * @param {TypeInfo?} type
	 * @param {number} length
	 * @param {string?} [deviceReg] Register holding the other processor
	 * @param {any} [instructions=[]]
	 */
	constructor(glob, baseName, type, length, deviceReg = null, instructions = []) {
		super(glob, instructions, baseName, type, false, length);
		this.deviceReg = deviceReg;
		this.setAttribute('isNameArray', true);
		this.setAttribute('disallowReplacement', true);
	}

	/** Where element `i`'s NAME is stored. */
	elementName(index) {
		return `${this.name}_${index}`;
	}

	/** `@this`, or the register holding the other processor. */
	get device() {
		return this.deviceReg ?? '@this';
	}

	element(index) {
		return new NameArraySlotRet(this.glob, this, operandOf(this.glob, index));
	}
}

export class RemoteArrayRet extends VariableRet {

	/**
	 * @param {Globals} glob
	 * @param {string} deviceReg Register holding the other processor
	 * @param {string} targetAssembly Assembly name of the array over there
	 * @param {TypeInfo} type The array type
	 * @param {number} length
	 * @param {any} [instructions=[]]
	 */
	constructor(glob, deviceReg, targetAssembly, type, length, instructions = []) {
		super(glob, instructions, deviceReg, type, false);
		this.deviceReg = deviceReg;
		this.targetAssembly = targetAssembly;
		this.length = length;
		this.setAttribute('isRemoteArray', true);
		this.setAttribute('disallowReplacement', true);
	}

	get width() {
		return 1;
	}

	/**
	 * `ru[i]` -- the element is one register over there, so all that changes
	 * with the subscript is WHICH NAME is read. A constant subscript is just a
	 * string; a variable one picks the name with a chain of `select`.
	 *
	 * @param {number | VariableRet} index
	 * @returns {RemoteSlotRet}
	 */
	subscript(index) {
		const glob = this.glob;
		const inner = (this.type && this.type.pointerTo) ? this.type.pointerTo : null;
		const holder = glob.tmp(null, 'rname');
		const nm = nameOf(holder);
		const instructions = [...this.instructions];
		if (typeof index === 'number') {
			instructions.push(InstructionBuilder.set(nm, `"${this.targetAssembly}_${index}"`));
		} else {
			const idx = index.get ? index.get() : index;
			if (idx && idx.instructions) instructions.push(idx.instructions);
			instructions.push(InstructionBuilder.set(nm, `"${this.targetAssembly}_0"`));
			for (let i = 1; i < this.length; i++) {
				instructions.push(InstructionBuilder.select(nm, 'equal',
					idx.name ?? idx, String(i), `"${this.targetAssembly}_${i}"`, nm));
			}
		}
		return new RemoteSlotRet(glob, this.deviceReg, nm, inner, instructions);
	}

	/** `element(i)` is the same access as `subscript(i)`; both are `ru[i]`. */
	element(index) {
		return this.subscript(index);
	}
}

/**
 * `int *remote q`: the POINTER itself lives in another processor.
 *
 * A pointer over there is two registers (`q_block` / `q_pos`), so it is read
 * with two `read`s and then used here like any other memory pointer.
 */
/**
 * `remote struct pt *p`: a pointer whose target lives in another processor.
 *
 * A struct cannot be reached by one name, so the pointer carries the NAME of
 * every member plus the device register -- the remote counterpart of
 * `NameStructPointerRet`. Reading `p->x` is then one
 * `read out <device> p.x`, exactly like any other remote scalar.
 */
export class RemoteStructPointerRet extends PointerRet {

	/**
	 * @param {Globals} glob
	 * @param {string} deviceReg Register holding the other processor
	 * @param {string} name Base name; member `m` lives in `<name>.<m>`
	 * @param {TypeInfo?} type
	 * @param {any} [instructions=[]]
	 */
	constructor(glob, deviceReg, name, type = null, instructions = []) {
		super(glob, instructions, name, type, false);
		this.deviceReg = deviceReg;
		this.structType = (type && type.pointerTo) ? type.pointerTo : null;
		this.setAttribute('isRemoteStructPointer', true);
		this.setAttribute('isPointer', true);
		this.setAttribute('disallowReplacement', true);
	}

	/** The device, plus one register per member. */
	get width() {
		return 1 + componentCountOf(this.type);
	}

	get pointee() {
		return this.structType;
	}

	get() {
		return this;
	}

	/**
	 * `p->m` / `(*p).m`: the register `<name>.m` holds the name of the member
	 * over there, so it is a plain remote slot.
	 * @param {string} memberName
	 * @param {TypeInfo?} memberType
	 * @returns {VariableRet}
	 */
	member(memberName, memberType = null) {
		const glob = this.glob;
		const type = memberType ?? memberTypeOf(glob, this.structType, memberName);
		return new RemoteSlotRet(glob, this.deviceReg, `${this.name}.${memberName}`,
			type, this.instructions);
	}

	/** The whole struct is the members; nothing to read up front. */
	deref() {
		return this;
	}

	slot() {
		return this;
	}

	/**
	 * Store the packet: the member names, and the device they belong to.
	 * @param {VariableRet} other
	 * @returns {RemoteStructPointerRet}
	 */
	set(other) {
		const value = other.get();
		const members = (this.structType?.members ?? []).map(m => m.name ?? m.memberName);
		const instructions = [this.instructions, value.instructions];
		const fromAddress = value.sourceBase ?? null;
		if (fromAddress) {
			/*
			`&st` of a LOCAL struct: the names are constants, and the device is
			this very processor.
			*/
			instructions.push(InstructionBuilder.set(this.deviceReg, '@this'));
			instructions.push(...members.map(m =>
				InstructionBuilder.set(`${this.name}.${m}`, `"${fromAddress}.${m}"`)));
		} else if (value instanceof RemoteStructPointerRet || value instanceof NameStructPointerRet) {
			/*
			From another packet: copy the NAMES across (never the text
			"p.x" -- `q.a` has to hold what `p.a` holds).
			*/
			instructions.push(InstructionBuilder.set(this.deviceReg, value.deviceReg));
			instructions.push(...members.map(m =>
				InstructionBuilder.set(`${this.name}.${m}`, `${value.name}.${m}`)));
		} else {
			const base = value.instructionReturn ?? null;
			if (!base) throw new InvalidCastException(other, this);
			instructions.push(...members.map(m =>
				InstructionBuilder.set(`${this.name}.${m}`, `"${base}.${m}"`)));
		}
		return new RemoteStructPointerRet(this.glob, this.deviceReg, this.name, this.type, instructions);
	}

	offset(step, backward = false) {
		throw new InfrastructureNotImplementedExpection(
			'pointer arithmetic on a remote struct pointer');
	}
}

export class RemoteStoredPointerRet extends PointerRet {

	/**
	 * @param {Globals} glob
	 * @param {string} deviceReg
	 * @param {string} remoteAssembly Assembly name of the pointer over there
	 * @param {TypeInfo?} type
	 * @param {any} [instructions=[]]
	 */
	constructor(glob, deviceReg, remoteAssembly, type = null, instructions = [], byName = false) {
		super(glob, instructions, deviceReg, type, false);
		this.deviceReg = deviceReg;
		this.remoteAssembly = remoteAssembly;
		/*
		`int *byname remote q`: over there the pointer is stored as a single
		NAME register instead of the `q_block` / `q_pos` pair.
		*/
		this.byName = byName === true;
		this.setAttribute('isRemoteStoredPointer', true);
		this.setAttribute('isPointer', true);
		this.setAttribute('disallowReplacement', true);
	}

	get width() {
		return this.byName ? 1 : 2;
	}

	get pointee() {
		return (this.type && this.type.pointerTo) ? this.type.pointerTo : null;
	}

	/**
	 * Read the pointer out of the other processor: the pair `q_block`/`q_pos`,
	 * or the single name register when it is stored by name.
	 */
	get() {
		const glob = this.glob;
		const base = glob.tmp(this.type, 'rptr');
		const bn = nameOf(base);
		if (this.byName) {
			/*
			One register holding a NAME: that is exactly what a name pointer
			carries, so the result is used through `read @this <name>`.
			*/
			return new NamePointerRet(glob,
				[this.instructions,
					InstructionBuilder.read(bn, this.deviceReg, `"${this.remoteAssembly}"`)],
				bn, this.type, true);
		}
		return new RefPointerRet(glob,
			[this.instructions,
				InstructionBuilder.read(`${bn}_block`, this.deviceReg, `"${this.remoteAssembly}_block"`),
				InstructionBuilder.read(`${bn}_pos`, this.deviceReg, `"${this.remoteAssembly}_pos"`)],
			bn, this.type, true);
	}

	/** Write the pointer into the other processor. */
	set(other) {
		const value = other.get();
		if (this.byName) {
			/*
			What travels is a NAME. A `byname(&c)` pointer already holds one in
			its own register; anything else has to be turned into the name of
			the variable it points at.
			*/
			const carried = (value instanceof NamePointerRet)
				? value.name
				: (value.targetName ? null : null);
			const source = carried ?? (value instanceof NamePointerRet ? value.name : value.name);
			return new RemoteStoredPointerRet(this.glob, this.deviceReg, this.remoteAssembly,
				this.type,
				[this.instructions, value.instructions,
					InstructionBuilder.write(source, this.deviceReg, `"${this.remoteAssembly}"`)],
				true);
		}
		return new RemoteStoredPointerRet(this.glob, this.deviceReg, this.remoteAssembly,
			this.type,
			[this.instructions, value.instructions,
				InstructionBuilder.write(`${value.name}_block`, this.deviceReg, `"${this.remoteAssembly}_block"`),
				InstructionBuilder.write(`${value.name}_pos`, this.deviceReg, `"${this.remoteAssembly}_pos"`)]);
	}

	/**
	 * The variable this pointer reaches.
	 *
	 * For the by-name form the name that was read from the other processor
	 * names a variable OVER THERE, so access goes through the same device
	 * register -- not through `@this`.
	 */
	slot() {
		if (this.byName) {
			const bn = nameOf(this.glob.tmp(this.type, 'rptrn'));
			return new RemoteSlotRet(this.glob, this.deviceReg, bn, this.pointee,
				[this.instructions,
					InstructionBuilder.read(bn, this.deviceReg, `"${this.remoteAssembly}"`)]);
		}
		return this.get().slot();
	}

	deref() {
		return this.slot().get();
	}

	offset(step, backward = false) {
		throw new InfrastructureNotImplementedExpection(
			'pointer arithmetic on a remote stored pointer');
	}
}

/**
 * How many elements an array type holds in total (all dimensions multiplied).
 * @param {TypeInfo?} type
 * @returns {number}
 */
export function arrayLengthOfType(type) {
	if (!type || type.kind !== 'array') return 0;
	let sizes = type.arraySize ?? [];
	if (!Array.isArray(sizes)) sizes = [sizes];
	let total = 1;
	for (const dim of sizes) {
		const value = (typeof dim === 'number') ? dim
			: (dim && typeof dim === 'object' && typeof dim.value === 'number') ? dim.value : 1;
		total *= value;
	}
	return total;
}

export const CATEGORY_ALIASES = {
	item: 'item',
	liquid: 'liquid',
	unit: 'unit',
	block: 'building',
	building: 'building'
};


/**
 * How many registers a value of this type occupies when every part of it is
 * named separately -- the "component count".
 *
 * This is deliberately NOT `widthOfType`: that one answers 1 for a struct
 * (it is one value) and 2 for a pointer, whereas here we want to know how
 * many registers a name-based pointer has to hold.
 *
 * @param {any} type
 * @param {number} depth guards against recursive types
 * @returns {number}
 */
/**
 * Whether a struct is FLAT: every member is a single register, so a name
 * based pointer can carry one name per member. A member that is itself a
 * struct has no single name to carry, so such structs are excluded.
 *
 * @param {any} type
 * @returns {boolean}
 */
export function isFlatComponentType(type) {
	if (!type || !type.isTypeInfo) return true;
	return type.kind !== 'struct' && type.kind !== 'union';
}

export function componentCountOf(type, depth = 0) {
	if (!type || !type.isTypeInfo) return 1;
	if (depth > 8) return 1;
	switch (type.kind) {
		case 'pointer':
		case 'array':
			return 2;
		case 'union': {
			// Every member overlaps: only the widest one is needed
			const members = type.members ?? [];
			let widest = 1;
			for (const m of members) {
				widest = Math.max(widest, componentCountOf(m.type ?? m.memberType, depth + 1));
			}
			return widest;
		}
		case 'struct': {
			/*
			Only a struct that is really kept in registers can be named
			member by member; one living in memory is reached by a plain
			two-word pointer.
			*/
			if (type.isPointerImpl?.()) return 2;
			const members = type.members ?? [];
			let total = 0;
			for (const m of members) {
				total += componentCountOf(m.type ?? m.memberType, depth + 1);
			}
			return Math.max(total, 1);
		}
		default:
			return 1;
	}
}

/**
 * A pointer to a struct that is held in REGISTERS.
 *
 * Instead of an address it carries, one per member, the NAME of that member:
 * `p` occupies `p.a`, `p.b`, ... and each of those holds a string such as
 * `"g:.st.a"`. `p->a` is then simply `read out @this p.a` -- one instruction,
 * no memory, and no dependence on how buildings are linked.
 */
export class NameStructPointerRet extends PointerRet {

	/**
	 * @param {Globals} glob
	 * @param {any} instructions
	 * @param {string} name Register base name of the pointer itself
	 * @param {TypeInfo} type The struct type pointed at
	 * @param {boolean} temporary
	 */
	constructor(glob, instructions, name, type, temporary = false) {
		super(glob, instructions, name, type, temporary);
		this.setAttribute('isNameStructPointer', true);
		this.setAttribute('disallowReplacement', true);
		/*
		When this value came from `&st`, `sourceBase` is the base name of `st`
		itself: that is what the member names have to be built from.
		*/
		this.sourceBase = null;
		/* `this.type` is the POINTER type; the struct it points at is the one
		   whose members give the packet its shape. */
		this.structType = (type && type.pointerTo) ? type.pointerTo : null;
	}

	/** One register per member. */
	get width() {
		return componentCountOf(this.type);
	}

	get pointee() {
		return (this.type && this.type.pointerTo) ? this.type.pointerTo : null;
	}

	/** The value IS the set of names: nothing to read. */
	get() {
		return this;
	}

	/**
	 * `p->m`: the register `p.m` holds the name of the member, so reading it
	 * the usual dynamic-name way gives the member itself.
	 * @param {string} memberName
	 * @param {TypeInfo?} memberType
	 * @returns {VariableRet}
	 */
	member(memberName, memberType = null) {
		const glob = this.glob;
		const type = memberType ?? memberTypeOf(glob, this.structType ?? this.type, memberName);
		const holder = new NamePointerRet(glob, [], `${this.name}.${memberName}`, type, false);
		return new NameSlotRet(glob, holder, type);
	}

	/**
	 * `p = &st`: store every member name.
	 * @param {VariableRet} other
	 * @returns {NameStructPointerRet}
	 */
	set(other) {
		const glob = this.glob;
		const value = other.get();
		const members = (this.structType?.members ?? this.type?.members ?? []).map(m => m.name ?? m.memberName);
		const instructions = [this.instructions, value.instructions];
		/*
		`&st` first: it arrives as a packet too, but a freshly built one whose
		`sourceBase` says which struct the names come from. Those names are
		constants and can be written out directly.
		*/
		const fromAddress = value.sourceBase ?? null;
		if (fromAddress) {
			for (const m of members) {
				instructions.push(InstructionBuilder.set(`${this.name}.${m}`, `"${fromAddress}.${m}"`));
			}
			return new NameStructPointerRet(glob, instructions, this.name, this.type, this.temporary);
		}
		if (value instanceof NameStructPointerRet) {
			/*
			From another packet: copy each name ACROSS. It must not be rebuilt
			as a literal -- `q.a` has to hold what `p.a` holds, not the text
			"p.a".
			*/
			for (const m of members) {
				instructions.push(InstructionBuilder.set(`${this.name}.${m}`, `${value.name}.${m}`));
			}
			return new NameStructPointerRet(glob, instructions, this.name, this.type, this.temporary);
		}
		/*
		Fallback: the value still names the struct whose members are to be
		pointed at.
		*/
		const base = value.instructionReturn ?? null;
		if (!base) throw new InvalidCastException(other, this);
		for (const m of members) {
			instructions.push(InstructionBuilder.set(`${this.name}.${m}`, `"${base}.${m}"`));
		}
		return new NameStructPointerRet(glob, instructions, this.name, this.type, this.temporary);
	}

	/**
	 * The whole struct: `*p`.
	 *
	 * The members are not called `p.x` -- `p.x` HOLDS the name of the member,
	 * so the struct is the group of registers those names point at.
	 */
	deref() {
		return new NameStructDerefRet(this.glob, this.instructions, this.name, this.type, true);
	}

	slot() {
		return this.deref();
	}

	address() {
		return this;
	}

	offset(step, backward = false) {
		/*
		A step of 0 is what `(*p).member` costs: nothing moves, so there is
		nothing to recompute. Any real stepping is meaningless here -- the
		value carried is a set of names, not an address.
		*/
		if (!step || Number(step) === 0) return this;
		throw new InfrastructureNotImplementedExpection(
			'pointer arithmetic on a name-based struct pointer');
	}
}

/**
 * `*p` where `p` is a name based struct pointer.
 *
 * The struct is not a group of registers with a fixed name -- it is the group
 * NAMED by `p.x`, `p.y`, ... and those names are chosen at run time. So every
 * member is reached through its own dynamic name, exactly like `p->m`.
 *
 * This is what makes the WHOLE dereference usable (`(*p).x`, `*p = other`);
 * without it a whole dereference stepped the packet as if it were an address,
 * which read through a null block.
 */
export class NameStructDerefRet extends StructRefRet {

	constructor(glob, instructions, name, type, temporary = false) {
		super(glob, instructions, name, type, temporary);
		this.setAttribute('isNameStructDeref', true);
		this.setAttribute('isRegStruct', true);
		this.setAttribute('disallowReplacement', true);
	}

	/** `this.type` is the POINTER type; the struct is what it points at. */
	get structType() {
		return (this.type && this.type.pointerTo) ? this.type.pointerTo : this.type;
	}

	get width() {
		return componentCountOf(this.structType ?? this.type);
	}

	/**
	 * @param {string} memberName
	 * @param {TypeInfo?} memberType
	 * @returns {VariableRet}
	 */
	member(memberName, memberType = null) {
		const glob = this.glob;
		const type = memberType ?? memberTypeOf(glob, this.structType, memberName);
		const holder = new NamePointerRet(glob, [], `${this.name}.${memberName}`, type, false);
		const member = new NameSlotRet(glob, holder, type);
		member.setAttribute('isSymbolic', true);
		return member;
	}

	/** The members are the value; there is nothing to read up front. */
	get() {
		return this;
	}

	/**
	 * `*p = other`: copy member by member, because the two sides are reached
	 * through names that are only known at run time.
	 * @param {VariableRet} other
	 * @returns {NameStructDerefRet}
	 */
	set(other) {
		const glob = this.glob;
		const source = other.get();
		const instructions = [this.instructions, source.instructions];
		const members = (this.structType?.members ?? []);
		for (const m of members) {
			const name = m.name ?? m.memberName;
			const type = m.type ?? m.memberType;
			const dst = this.member(name, type);
			const src = (typeof source.member === 'function') ? source.member(name, type) : source;
			instructions.push(dst.set(src.get()));
		}
		return new NameStructDerefRet(glob, instructions, this.name, this.type, this.temporary);
	}
}

/* ------------------------------------------------------------------ *
 * Array handles -- how an array travels as a VALUE
 * ------------------------------------------------------------------ */

/**
 * The register that holds the entry point of a helper.
 *
 * `FunctionRegisterer` emits `op add _<name> @counter 1` at the head of every
 * function, so the address of `<name>` lives in `_<name>`.
 *
 * @param {string} key
 * @returns {string}
 */
function helperAddressOf(key) {
	return `_${key}`;
}

/**
 * How one element of an array is reached, in the shape a helper needs.
 *
 * @param {ArrayRet} array
 * @returns {{length: number, device: string, names: string[], dynamic: boolean}?}
 *          null when this array has no such description (so no handle exists)
 */
function handleShapeOf(array) {
	if (!array) return null;
	const device = array.deviceReg ?? '@this';
	// A remote array: its elements are registers of the other processor
	if (array instanceof RemoteArrayRet) {
		const names = [];
		for (let i = 0; i < array.length; i++) names.push(`${array.targetAssembly}_${i}`);
		return { length: array.length, device, names, dynamic: false };
	}
	if (array instanceof LookupArrayRet) {
		return { length: array.length, device, names: array.names ?? [], dynamic: false };
	}
	if (array instanceof NameArrayRet) {
		/*
		`s_i` does not hold the element -- it holds the NAME of the element, so
		every access is one more level of indirection.
		*/
		const names = [];
		for (let i = 0; i < array.length; i++) names.push(array.elementName(i));
		return { length: array.length, device, names, dynamic: true };
	}
	if (typeof array.elementName === 'function') {
		const names = [];
		for (let i = 0; i < array.length; i++) names.push(array.elementName(i));
		return { length: array.length, device: '@this', names, dynamic: false };
	}
	/*
	A MEMORY array has no element NAMES, but it does have an address -- and an
	address is enough: the helper computes `base + i * size`, turns the block
	number into the building and reads through it. No `select` chain is needed,
	which is what makes this cheap even for a long array.

	This is the one layout that can also travel as a plain pointer, so it keeps
	that ability; the handle is an additional representation, not a replacement.
	*/
	if (array instanceof TraditionalArrayRet) {
		const pointer = array.pointer ?? null;
		if (!pointer) return null;
		return {
			length: array.length,
			kind: 'memory',
			block: pointer.blockOperand?.() ?? `${pointer.name}_block`,
			pos: `${pointer.name}_pos`,
			size: array.elementSize ?? 1,
			names: [], dynamic: false
		};
	}
	return null;
}

/**
 * Build (once) the get / set helpers of an array and return their entry
 * points.
 *
 * Why this exists: an array that is not laid out in memory has NO ADDRESS, so
 * a pointer cannot be made of it -- and that is exactly what is needed when
 * the array is handed to a function, or to a function pointer (which is passed
 * through fixed `__param_N` slots and therefore cannot know anything about the
 * call site). A handle is two words that DO travel as a value:
 *
 *     set s_get ___arrget_NAME
 *     set s_set ___arrset_NAME
 *
 * After that, `s[i]` inside the function is
 *
 *     set __arridx i
 *     op add __stackpos @counter 1
 *     set @counter s_get          ; result in __arrret
 *     set __stackpos __internal_stackpos
 *
 * The helper is built for the CALLER's array, so the write really lands in the
 * caller's array -- the array stays passed by reference, as in C.
 *
 * @param {Globals} glob
 * @param {ArrayRet} array
 * @returns {{instructions: any[], get: string?, set: string?}?}
 *          null when this array cannot be described by a handle at all
 */
export function ensureArrayHandle(glob, array) {
	const shape = handleShapeOf(array);
	if (!shape || shape.length <= 0) return null;
	const reg = glob?.funcReg;
	if (!reg) return null;
	const keyOf = kind => `__arr${kind}_${String(array.name).replace(/[^A-Za-z0-9_]/g, '_')}`;
	const build = kind => {
		const key = keyOf(kind);
		if (reg.functionCollection.has(key)) {
			/*
			Taking the address does not count as a call, and a helper nobody
			"calls" would be dropped as dead code -- so count it by hand.
			*/
			reg.functionCollection.get(key).calls++;
			return key;
		}
		const body = new Instruction();
		/*
		A memory array: work out the address of element `__arridx` first, then
		read or write through it. `getlink` has to happen INSIDE the helper
		because only there is the offset known.
		*/
		if (shape.kind === 'memory') {
			if (shape.size > 1) {
				body.concat(InstructionBuilder.op('mul', '__arrsel', '__arridx', shape.size));
				body.concat(InstructionBuilder.op('add', '__arrsel', shape.pos, '__arrsel'));
			} else {
				body.concat(InstructionBuilder.op('add', '__arrsel', shape.pos, '__arridx'));
			}
			body.concat(InstructionBuilder.getlink('__arrblk', shape.block));
			if (kind === 'get') {
				body.concat(InstructionBuilder.read('__arrret', '__arrblk', '__arrsel'));
			} else {
				body.concat(InstructionBuilder.write('__arrval', '__arrblk', '__arrsel'));
			}
			reg.addFunction(key, body, null, [], true, false, true);
			reg.functionCollection.get(key).calls++;
			return key;
		}
		if (kind === 'get') {
			if (shape.dynamic) {
				body.concat(InstructionBuilder.read('__arrret', shape.device, shape.names[0]));
				for (let i = 1; i < shape.length; i++) {
					const got = nameOf(glob.tmp(null, 'arrd'));
					body.concat(InstructionBuilder.read(got, shape.device, shape.names[i]));
					body.concat(InstructionBuilder.selectx('__arrret',
						`equal __arridx ${i}`, got, '__arrret'));
				}
			} else {
				body.concat(InstructionBuilder.set('__arrret', shape.names[0]));
				for (let i = 1; i < shape.length; i++) {
					body.concat(InstructionBuilder.selectx('__arrret',
						`equal __arridx ${i}`, shape.names[i], '__arrret'));
				}
			}
		} else {
			/*
			`select` picks a VALUE, not a target, so the NAME is selected first
			and then written through -- the same trick a name pointer uses.
			*/
			const literal = i => (shape.dynamic ? shape.names[i] : `"${shape.names[i]}"`);
			body.concat(InstructionBuilder.set('__arrsel', literal(0)));
			for (let i = 1; i < shape.length; i++) {
				body.concat(InstructionBuilder.selectx('__arrsel',
					`equal __arridx ${i}`, literal(i), '__arrsel'));
			}
			body.concat(InstructionBuilder.write('__arrval', shape.device, '__arrsel'));
		}
		reg.addFunction(key, body, null, [], true, false, true);
		reg.functionCollection.get(key).calls++;
		return key;
	};
	return {
		instructions: array.instructions ?? [],
		get: helperAddressOf(build('get')),
		set: helperAddressOf(build('set'))
	};
}

/**
 * An array reached through a HANDLE: two registers holding the entry points of
 * its get / set helpers.
 *
 * @see ensureArrayHandle
 */
export class ArrayHandleRet extends ArrayRet {

	/**
	 * @param {Globals} glob
	 * @param {string} baseName Element access uses `<baseName>_get` / `_set`
	 * @param {TypeInfo?} type
	 * @param {number} length
	 * @param {any} [instructions=[]]
	 */
	constructor(glob, baseName, type, length, instructions = []) {
		super(glob, instructions, baseName, type, false, length);
		this.setAttribute('isArrayHandle', true);
		this.setAttribute('disallowReplacement', true);
	}

	get width() {
		return 2;
	}

	/** The address of the get helper. */
	get getReg() {
		return `${this.name}_get`;
	}

	/** The address of the set helper. */
	get setReg() {
		return `${this.name}_set`;
	}

	/**
	 * @param {VariableRet|number|string} index
	 * @returns {VariableRet}
	 */
	element(index) {
		return new ArrayHandleSlotRet(this.glob, this, operandOf(this.glob, index));
	}
}

/** One element of an `ArrayHandleRet`: a call to the handle's helper. */
class ArrayHandleSlotRet extends VariableRet {

	/**
	 * @param {Globals} glob
	 * @param {ArrayHandleRet} array
	 * @param {{instructions: any[], name: string, constant: boolean, value: number?}} operand
	 */
	constructor(glob, array, operand) {
		super(glob, operand.instructions, array.name, array.elementTypeInfo, false);
		this.array = array;
		this.operand = operand;
	}

	get() {
		const glob = this.glob;
		const target = glob.tmp(this.type);
		const tn = nameOf(target);
		/*
		An INDIRECT call: the target is a register holding a line number, not a
		name known at compile time. That is the whole point of a handle -- the
		function does not know which array it was handed.
		*/
		const call = glob.funcReg.getRawFunctionCall(this.array.getReg,
			new Map([['__arridx', this.operand.name]]), false, null);
		return valueClassOf(glob, this.type).create(glob,
			[this.instructions, call, InstructionBuilder.set(tn, '__arrret')],
			tn, this.type, true);
	}

	set(other) {
		const glob = this.glob;
		const value = other.get().convertTo(this.type ?? other.type);
		const call = glob.funcReg.getRawFunctionCall(this.array.setReg,
			new Map([['__arridx', this.operand.name], ['__arrval', value.name]]), false, null);
		return valueClassOf(glob, this.type).create(glob,
			[this.instructions, value.instructions, call], value.name, this.type, false);
	}
}

export function selectArrayImplementation(info) {
	const length = info.length ?? 0;
	const size = info.size ?? 1;
	const isConst = info.isConst === true;
	const version = info.targetVersion ?? 0;
	const memory = info.availableMemory ?? Infinity;
	const budget = info.instructionBudget ?? 1000;
	const requested = normalizeImplementation(info.requested);
	const lookupNames = info.lookupNames ?? null;

	if (requested) return requested;

	const cells = length * size;
	if (cells <= memory) {
		return { type: 'traditional', implement: null, reason: 'memory available' };
	}
	// No (or not enough) memory: consider the layouts that need none
	const candidates = [];
	if (version >= 150 && lookupNames && lookupNames.length >= length) {
		candidates.push({ type: 'embedded', implement: 'lookup',
			cost: accessCostOf('embedded.lookup', length) });
	}
	if (isConst) {
		const mapping = buildEncodedMapping(info.values ?? new Array(length).fill(0));
		candidates.push({ type: 'encoded', implement: null,
			cost: accessCostOf('encoded', length, mapping.digits) });
	}
	candidates.push({ type: 'embedded', implement: 'divided',
		cost: accessCostOf('embedded.divided', length) });
	// Fewest instructions first
	candidates.sort((x, y) => x.cost - y.cost);
	const affordable = candidates.find(c => c.cost <= budget);
	if (affordable) return { ...affordable, reason: 'out of memory' };
	// Nothing fits the budget either: instructions matter more than memory
	return { ...candidates[0], reason: 'budget exhausted, instructions take priority' };
}

/**
 * Turn the string given to `array(...)` into a style.
 *
 * @param {string?} text
 * @returns {{type: string, implement: string?, reason: string}?}
 */
export function normalizeImplementation(text) {
	if (!text) return null;
	const name = String(text).trim();
	/*
	`embedded_lookup_unit` (and _item / _liquid / _block) name the table
	outright, instead of letting the compiler guess. That matters across two
	processors: both sides have to agree on which table the names come from.
	*/
	if (name.startsWith('embedded_lookup_')) {
		const category = CATEGORY_ALIASES[name.slice('embedded_lookup_'.length)];
		if (category) return { type: 'embedded', implement: 'lookup', reason: 'requested', category };
	}
	switch (name) {
		case 'traditional': return { type: 'traditional', implement: null, reason: 'requested' };
		case 'embedded': return { type: 'embedded', implement: null, reason: 'requested' };
		case 'embedded_divided': return { type: 'embedded', implement: 'divided', reason: 'requested' };
		case 'embedded_lookup': return { type: 'embedded', implement: 'lookup', reason: 'requested' };
		case 'encoded': return { type: 'encoded', implement: null, reason: 'requested' };
		default: return null;
	}
}


/**
 * Build the array representation described by a style.
 *
 * @param {Globals} glob
 * @param {{type: string, implement: string?}} style
 * @param {object} options
 * @param {PointerRet?} [options.pointer] For the traditional layout
 * @param {TypeInfo?} [options.type] The array type
 * @param {number} [options.length]
 * @param {string} [options.baseName] Element register prefix
 * @param {string} [options.category] item | liquid | unit | building
 * @param {string[]} [options.names] Register names for `lookup`
 * @param {number[]} [options.values] Contents for `encoded`
 * @returns {ArrayRet}
 */
export function arrayRetOf(glob, style, options = {}) {
	const type = options.type ?? null;
	const length = options.length ?? 0;
	if (!style || style.type === 'traditional') {
		if (!options.pointer) {
			throw new InfrastructureNotImplementedExpection('traditional array without a pointer');
		}
		return new TraditionalArrayRet(glob, options.pointer, type, length);
	}
	if (style.type === 'encoded') {
		// `reason === 'requested'`: the program named this implementation, so the
		// string table may not be replaced by a `select` chain
		return new EncodedArrayRet(glob, options.values ?? new Array(length).fill(0), type,
			null, style.reason === 'requested');
	}
	// style.type === 'embedded'
	if (style.implement === 'lookup') {
		const names = options.names ?? [];
		const category = style.category ?? options.category ?? 'item';
		return new LookupArrayRet(glob, category, names, type, options.deviceReg ?? null);
	}
	// 'divided', or 'embedded' left unspecified
	return new DividedArrayRet(glob, options.baseName ?? '__divided', type, length);
}

/* ------------------------------------------------------------------ *
 * Factories & bridge with the legacy attribute-based Instruction
 * ------------------------------------------------------------------ */

/**
 * Which value class should be used for a given type.
 * @param {Globals} glob
 * @param {TypeInfo?} type
 */
export function valueClassOf(glob, type) {
	if (!type || !type.isTypeInfo) return BasicRet;
	if (type.kind === 'pointer' || type.kind === 'array') {
		return RefPointerRet;
	}
	if (type.kind === 'struct' || type.kind === 'union') {
		// Registry structs live in registers (one variable per member); the others
		// are laid out in memory and referred to through a pointer.
		return type.isRegStruct() ? RegistryStructRet : StructRefRet;
	}
	if (glob.semantic.isLogicalType(type.name)) {
		if (type.name === 'bool') return BoolRet;
		if (['float', 'double'].includes(type.name)) return FloatRet;
		return IntRet;
	}
	switch (type.name) {
		case 'content_t': return ContentRet;
		case 'device': return DeviceRet;
		case 'null_t': return NullRet;
		case 'char': return BasicRet;
		default: return BasicRet;
	}
}

/**
 * Build the reference (LValue) of a symbol: something you can get() and set().
 * This centralizes what used to be `generateSymbolRead` / `generateSymbolWrite`.
 * @param {Globals} glob
 * @param {SymbolEntry} symbol
 * @returns {VariableRet}
 */
export function symbolRef(glob, symbol) {
	const assembly = symbol.getAssemblySymbol();
	const type = symbol.myType();

	/*
	A `remote` variable: the value lives in another processor, so the symbol
	itself is only a binding (device + name) and reading it emits a `read`.
	*/
	if (symbol.isRemote) {
		const devReg = remoteDeviceRegOf(assembly);
		const nmReg = remoteNameRegOf(assembly);
		/*
		`int *remote q`: the pointer is stored over there, so what is reached
		by name is the pair `q_block` / `q_pos`.
		*/
		if (symbol.remoteStored === true && (symbol.remoteName ?? symbol.remoteTarget)) {
			const ret = new RemoteStoredPointerRet(glob, devReg,
				symbol.remoteName ?? symbol.remoteTarget.getAssemblySymbol(), type, [],
				symbol.remoteStoredByName === true);
			ret.setAttribute('relevantSymbol', symbol);
			ret.setAttribute('relevantSymbolRead', symbol);
			ret.setAttribute('isSymbolic', true);
			return ret;
		}
		/*
		A remote ARRAY: the elements are registers of the other processor.
		*/
		if (type && type.kind === 'array' && (symbol.remoteName ?? symbol.remoteTarget)) {
			/*
			A remote array whose elements are named after a lookup table: the
			names are the same on both sides (they come from the game), so all
			that changes is the final `read` / `write`.
			*/
			const targetImpl = symbol.requestedArrayImplementation
				?? symbol.remoteTarget?.requestedArrayImplementation ?? null;
			if (targetImpl && targetImpl.startsWith('embedded_lookup_')) {
				const wanted = CATEGORY_ALIASES[targetImpl.slice('embedded_lookup_'.length)] ?? 'item';
				/*
				Only the first `length` names belong to this array: the rest
				would make it look longer than it is. (The elements are named
				after CONTENTS -- `copper`, `lead`, ... -- which are bare names,
				not `g:.` variables, so no declaration of ours has to exist on
				the other side for them.)
				*/
				const all = (glob.semantic?.contentTable?.namesOf?.(
					wanted === 'building' ? 'block' : wanted) ?? []).map(n => n.replace(/^@/, ''));
				const length = arrayLengthOfType(type);
				const names = (length > 0 && length < all.length) ? all.slice(0, length) : all;
				const ret = new LookupArrayRet(glob, wanted, names, type, devReg);
				ret.setAttribute('relevantSymbol', symbol);
				ret.setAttribute('relevantSymbolRead', symbol);
				ret.setAttribute('isSymbolic', true);
				return ret;
			}
			const ret = new RemoteArrayRet(glob, devReg,
				symbol.remoteName ?? symbol.remoteTarget.getAssemblySymbol(), type,
				arrayLengthOfType(type));
			ret.setAttribute('relevantSymbol', symbol);
			ret.setAttribute('relevantSymbolRead', symbol);
			ret.setAttribute('isSymbolic', true);
			return ret;
		}
		/*
		`remote struct pt *p`: the target is a struct, so the pointer carries
		the name of every member (see RemoteStructPointerRet).
		*/
		if (type && type.kind === 'pointer' && (type.pointerTo?.kind === 'struct'
			|| type.pointerTo?.kind === 'union')
			&& glob.generator?.structNamePointerAllowed?.(type.pointerTo)) {
			const ret = new RemoteStructPointerRet(glob, devReg, assembly, type);
			ret.setAttribute('relevantSymbol', symbol);
			ret.setAttribute('relevantSymbolRead', symbol);
			ret.setAttribute('isSymbolic', true);
			ret.setAttribute('disallowReplacement', true);
			return ret;
		}
		const value = (type && type.kind === 'pointer')
			? new RemotePointerRet(glob, devReg, nmReg, type)
			: new RemoteSlotRet(glob, devReg, nmReg, type);
		value.setAttribute('relevantSymbol', symbol);
		value.setAttribute('relevantSymbolRead', symbol);
		value.setAttribute('isSymbolic', true);
		return value;
	}

	if (symbol.isAutoDevice) {
		return new DeviceRet(glob, [], assembly, type, false);
	}

	// Structs and unions
	if (type && type.isTypeInfo && ['struct', 'union'].includes(type.kind)) {
		if (type.isPointerImpl()) {
			return new StructRefRet(glob, [], symbol.accessThroughPointer ? `${assembly}.__pointer` : assembly,
				type, false, symbol);
		}
		// Registry struct: the members are `X.member` registers
		const reg = new RegistryStructRet(glob, [], assembly, type, false, symbol);
		reg.setAttribute('isSymbolic', true);
		reg.setAttribute('relevantSymbol', symbol);
		reg.setAttribute('relevantSymbolRead', symbol);
		reg.setAttribute('disallowReplacement', true);
		reg.setAttribute('isTemporary', symbol.isVirtualSymbol);
		return reg;
	}

	/*
	Pointers that store the NAME of what they point at. `forcedNamePointer`
	is `byname(&c)`: the same representation, asked for explicitly.
	*/
	/*
	A pointer to a struct may carry the NAME of every member: one register per
	member instead of a block/offset pair.
	*/
	/*
	`remote int *p` -- the POINTEE is remote, so the pointer is the pair
	(processor, name) rather than (block, offset). Reading it as the latter is
	what made `remote int *p2 = &rs;` emit `p2_block`, a register that was never
	assigned.
	*/
	if (!symbol.isRemote && type && type.kind === 'pointer'
		&& (type.pointerTo?.qualifiers ?? []).includes('remote')
		&& !symbol.accessThroughPointer && !symbol.needMemoryAllocation) {
		const ret = new RemotePointerRet(glob, remoteDeviceRegOf(assembly),
			remoteNameRegOf(assembly), type, []);
		ret.setAttribute('relevantSymbol', symbol);
		ret.setAttribute('relevantSymbolRead', symbol);
		ret.setAttribute('isSymbolic', true);
		ret.setAttribute('disallowReplacement', true);
		return ret;
	}

	if (type && type.kind === 'pointer' && glob.generator?.structNamePointerAllowed?.(type.pointerTo)
		&& (symbol.usesNamePointer === true || symbol.forcedNamePointer === true
			|| symbol.addressedByName === true)) {
		const ret = new NameStructPointerRet(glob, [], assembly, type, symbol.isVirtualSymbol);
		ret.setAttribute('isSymbolic', true);
		ret.setAttribute('relevantSymbol', symbol);
		ret.setAttribute('relevantSymbolRead', symbol);
		ret.setAttribute('disallowReplacement', true);
		ret.setAttribute('isTemporary', symbol.isVirtualSymbol);
		return ret;
	}

	if (symbol.usesNamePointer === true || symbol.forcedNamePointer === true) {
		const ret = new NamePointerRet(glob, [], assembly, type, symbol.isVirtualSymbol, null);
		ret.setAttribute('isSymbolic', true);
		ret.setAttribute('relevantSymbol', symbol);
		ret.setAttribute('relevantSymbolRead', symbol);
		ret.setAttribute('disallowReplacement', true);
		ret.setAttribute('isTemporary', symbol.isVirtualSymbol);
		return ret;
	}

	// Pointer-like values (pointer / array): the value *is* a pointer
	if (symbol.implementAsPointer) {
		/*
		An array is delivered BY REFERENCE: its value IS the address held in
		`X_block` / `X_pos`. Reading it out of `X.__pointer` instead treats the
		array's own data as a pointer, which is what broke `&a[1]`.
		*/
		if (symbol.accessThroughPointer && !(type && type.kind === 'array')) {
			const fixed = symbol.memoryRef();
			return new MemPointerRet(glob, [], `${assembly}.__pointer`, type, false, fixed, symbol);
		}
		const ret = new RefPointerRet(glob, [], assembly, type, symbol.isVirtualSymbol, symbol);
		ret.setAttribute('isSymbolic', true);
		ret.setAttribute('relevantSymbol', symbol);
		ret.setAttribute('relevantSymbolRead', symbol);
		ret.setAttribute('isNearPointer', symbol.isNearPointer);
		ret.setAttribute('disallowReplacement', true);
		ret.setAttribute('isTemporary', symbol.isVirtualSymbol);
		return ret;
	}

	// Ordinary values stored in memory
	if (symbol.accessThroughPointer) {
		const fixed = symbol.memoryLocation;
		return new MemoryRefRet(glob, [], `${assembly}.__pointer`, type, false, fixed, symbol);
	}

	// Ordinary values in registers
	const ret = valueClassOf(glob, type).create
		? valueClassOf(glob, type).create(glob, [], assembly, type, symbol.isVirtualSymbol, symbol)
		: new (valueClassOf(glob, type))(glob, [], assembly, type, symbol.isVirtualSymbol);
	ret.setAttribute('isSymbolic', true);
	ret.setAttribute('relevantSymbol', symbol);
	ret.setAttribute('relevantSymbolRead', symbol);
	ret.setAttribute('disallowReplacement', true);
	ret.setAttribute('isTemporary', symbol.isVirtualSymbol);
	ret.setAttribute('isNearPointer', symbol.isNearPointer);
	if (symbol.namePointersOnly === true) ret.setAttribute('namePointersOnly', true);
	return ret;
}

/**
 * Wrap a legacy `Instruction` (attribute based) into the object oriented world.
 * Used while the refactoring is in progress, and for every non-refactored visitor.
 * @param {Globals} glob
 * @param {Instruction} inst
 * @param {TypeInfo?} type
 * @returns {VariableRet}
 */
/**
 * Is this mlog operand an immediate constant rather than a name?
 * @param {string?} text
 * @returns {boolean}
 */
export function isImmediateName(text) {
	if (text === null || text === undefined || text === '') return true;
	if (/^-?[0-9]+(\.[0-9]+)?$/.test(text)) return true;	// numeric literal
	if (text[0] === '@') return true;						// @copper & friends
	if (text[0] === '"') return true;						// string literal
	return ['null', 'true', 'false'].includes(text);
}

/**
 * Build the Ret described by a legacy instruction. Shared by `ExportRet` and by the
 * "protected name" path.
 * @param {Globals} glob
 * @param {Instruction} inst
 * @param {TypeInfo?} type
 * @param {number} width
 * @param {boolean} throughPointer
 * @param {boolean} temporary
 * @param {SymbolEntry?} relevant
 * @returns {VariableRet}
 */
function buildRet(glob, inst, type, width, throughPointer, temporary, relevant) {
	const fixed = relevant ? relevant.memoryLocation : null;
	if (throughPointer) {
		if (width >= 2) {
			return new MemPointerRet(glob, inst.instructions, inst.instructionReturn, type,
				temporary, fixed, relevant ?? null);
		}
		return new MemoryRefRet(glob, inst.instructions, inst.instructionReturn, type,
			temporary, fixed, relevant ?? null);
	}
	if (width >= 2) {
		return new RefPointerRet(glob, inst.instructions, inst.instructionReturn, type, temporary);
	}
	if (type && type.isTypeInfo && ['struct', 'union'].includes(type.kind)) {
		return new StructRefRet(glob, inst.instructions, inst.instructionReturn, type,
			temporary, relevant ?? null);
	}
	return valueClassOf(glob, type).create(glob, inst.instructions, inst.instructionReturn, type, temporary);
}

export function ExportRet(glob, inst, type = null) {
	if (!inst) return new ImmediateRet(glob, 'null', type);
	if (inst instanceof VariableRet) return inst;

	const attrs = name => inst.getAttribute(name);
	const relevant = attrs('relevantSymbol') ?? attrs('relevantSymbolRead');
	/*
	`volatile` means "this variable MUST live in memory", so a volatile value is
	always talked about THROUGH a pointer -- but a volatile *scalar* still occupies
	a single word. Legacy attributes:
	  isPointerAccess && width 1 : value in memory, one word   -> MemoryRefRet
	  isPointerAccess && width 2 : pointer value itself in mem -> MemPointerRet
	  isPointer       && width 2 : a pointer value             -> RefPointerRet
	*/
	const width = widthOfType(type);
	/*
	Whether the value is *really* reached through a pointer is decided by the SYMBOL,
	not by the type: a type qualifier (`volatile`) is inherited by intermediate
	results (e.g. `vg * 2` is still `volatile int`), but a temporary symbol holding
	that result is a plain register.
	*/
	const throughPointer = relevant
		? (relevant.accessThroughPointer === true)
		: (attrs('isPointerAccess') === true);
	// `isTemporary` gets lost through Instruction.concat_returns (it is not part of its
	// transfer list), so fall back to the symbol itself.
	const temporary = attrs('isTemporary') === true
		|| (relevant ? relevant.isVirtualSymbol === true : false);

	/*
	`disallowReplacement` in the legacy system only means "my name must not be
	substituted by a template". A constant therefore really is an immediate value,
	but a *variable* carrying that flag is still a perfectly ordinary reference --
	it just may not be renamed.
	*/
	if (attrs('disallowReplacement')) {
		if (isImmediateName(inst.instructionReturn)) {
			return new ImmediateRet(glob, inst.instructionReturn, type ?? inst.type, attrs('builtinConstant') === true);
		}
		const protectedValue = buildRet(glob, inst, type, width, throughPointer, temporary, relevant);
		protectedValue.setAttribute('disallowRename', true);
		protectedValue.setAttribute('isSymbolic', true);
		return protectedValue;
	}
	return buildRet(glob, inst, type, width, throughPointer, temporary, relevant);
}



/*
MinCCompiler on JS -- 编译 Worker

WHY THIS FILE EXISTS
--------------------
`Compiler.compile()` 是一条完整的流水线：词法 -> 语法 -> 语义 -> 优化（最多
10 轮不动点迭代）-> 代码生成。大一点的程序要跑几百毫秒甚至几秒，而且过程中
会 `console.log` 大量调试输出（优化器每轮都会打印，作用域优化还会 dump 整个
AST 节点对象）。放在主线程意味着：

  - 页面在编译期间完全不响应输入；
  - 成千上万条 console 输出把 DevTools / 主线程一起拖垮。

放进 Worker 后主线程始终空闲可输入、可滚动；编译也能被取消（terminate + 重建）。

协议
----
  主线程 -> worker : { id: number, code: string, memory: [{name, size}] }
  worker -> 主线程 : { id, ok, success, code, fatalError, errors, warnings }
                   | { id, ok: false, error, stack }

`Compiler` 的返回值里含 Instruction / SymbolEntry 等带循环引用的对象，无法
结构化克隆，所以 errors / warnings 在 worker 内先净化成纯数据。
*/

import { Compiler } from './mindc.js';
import { MemoryBlock } from './mindcGeneratorBase.js';

/*
必须关：编译期 console.log 的量非常大（见 mindcOptimizer.js 的每轮日志和
[ScopeOp Stage] dump），在 Worker 里每条日志都要跨线程传回主线程。
*/
const noop = () => {};
console.log = noop;
console.debug = noop;
console.info = noop;
console.warn = noop;

/**
 * 把 errors / warnings 压成可克隆的 {message, line, column}。
 * 有些错误对象携带 AST 节点（含循环引用），直接 String() 或 postMessage 都会炸。
 *
 * @param {any[]} list
 * @returns {{message: string, line: number?, column: number?}[]}
 */
function sanitizeDiagnostics(list) {
	if (!Array.isArray(list)) return [];
	const result = [];
	for (const item of list) {
		if (!item) {
			result.push({ message: `${item}` });
			continue;
		}
		if (typeof item === 'string') {
			result.push({ message: item });
			continue;
		}
		const entry = {};
		try {
			entry.message = (item.message === undefined || item.message === null)
				? String(item) : String(item.message);
		} catch (error) {
			entry.message = '（错误信息无法序列化）';
		}
		if (item.line !== undefined) entry.line = item.line;
		if (item.column !== undefined) entry.column = item.column;
		result.push(entry);
	}
	return result;
}

self.onmessage = async event => {
	const { id, code, memory } = event.data ?? {};
	try {
		const blocks = Array.isArray(memory)
			? memory.map(block => new MemoryBlock(String(block?.name), Number(block?.size) || 64))
			: [];

		const compiler = new Compiler(code ?? '', blocks, null);
		const result = await compiler.compile();

		self.postMessage({
			id,
			ok: true,
			success: !!result.success,
			code: result.code ?? '',
			fatalError: result.fatalError ?? null,
			errors: sanitizeDiagnostics(result.errors),
			warnings: sanitizeDiagnostics(result.warnings)
		});
	} catch (error) {
		self.postMessage({
			id,
			ok: false,
			success: false,
			code: '',
			fatalError: ((error && error.message) || `${error}`) + '\n' + ((error && error.stack) || ''),
			errors: [],
			warnings: []
		});
	}
};

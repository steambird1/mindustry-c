/*
MinCCompiler on JS -- 语法高亮 Worker

WHY THIS FILE EXISTS
--------------------
main.html 原来在每次按键时都于主线程跑一遍 `new Lexer(code).tokenize()`：
扫描整份源码 + 构造上千个 Token 对象 + 预处理指令求值。用户逐字符敲入
`#pragma optimize(array)` 这类内容时，这个代价要付二十几次，而中途的源码
始终是"半截"的非法文本，纯粹是浪费。

词法分析本身是纯计算，放进 Worker 后主线程只剩"拼一段 HTML 字符串 + 一次
innerHTML 赋值"。

协议
----
  主线程 -> worker : { id: number, code: string }
  worker -> 主线程 : { id, ok: true, n, idx, len, codes, codebook }
                   | { id, ok: false, error: string }

大文件（10 万字符以上）会有几万个 token。若逐个回传 Token 对象，结构化克隆的
反序列化要在主线程花掉几十毫秒，所以这里回传三个 Int32Array + 一张类型编码表：

  idx   : 每个 token 在源码中的起始下标
  len   : 该 token 实际显示的字符数（字符串/字符字面量取 raw，其余取 value）
  codes : token 类型在 codebook 中的下标
  codebook : 类型名数组（真实 TokenType + 两个高亮伪类型，两端顺序一致）

三个 TypedArray 通过 transfer list 零拷贝移交，主线程不再为几万个小对象买单。

注释与预处理指令由 mindcHighlightScan.js 单独扫描（Lexer 的 token 流要喂给
Parser，不能塞 COMMENT；PREPROCESSOR_* 也在 tokenize() 末尾被过滤掉了），
合并后一并编码。
*/

import { Lexer, TokenType } from './mindcLexer.js';
import {
	HIGHLIGHT_CODEBOOK, codeOf, scanDecorations, mergeTokensWithDecorations
} from './mindcHighlightScan.js';

/*
词法分析阶段不需要日志；关掉可以避免任何 console 输出跨线程同步回主线程
（DevTools 开着时，每条日志都要走一次 postMessage，量一大就会拖慢页面）。
*/
const noop = () => {};
console.log = noop;
console.debug = noop;
console.info = noop;

/*
主线程的 token id 是单调递增的。若处理期间又来了新请求，说明这次的结果已经
过期，直接丢弃 —— 不再 postMessage，省掉主线程那边的反序列化开销。
（Worker 内部无法抢占正在跑的 tokenize，但至少不会把废结果送回去。）
*/
let latestId = -1;

self.onmessage = async event => {
	const { id, code } = event.data ?? {};
	latestId = Math.max(latestId, id ?? 0);

	try {
		const source = code ?? '';
		const lexer = new Lexer(source, {}, true);
		const result = await lexer.tokenize();
		if (id !== latestId) return;            // 已有更新的请求，本次作废

		const list = result.tokens;
		const base = new Array(list.length);
		for (let k = 0; k < list.length; k++) {
			const token = list[k];
			/*
			与渲染端保持一致：字符串/字符字面量用 raw（带引号的原文），
			其余用 value。
			*/
			const isLiteral = token.type === TokenType.STRING || token.type === TokenType.CHARACTER;
			const text = isLiteral ? `${token.raw ?? token.value ?? ''}` : `${token.value ?? ''}`;
			base[k] = {
				index: token.location ? token.location.index : 0,
				length: text.length,
				type: token.type
			};
		}

		// 注释 / 预处理指令：单独扫描后合并进同一条流
		const merged = mergeTokensWithDecorations(base, scanDecorations(source));

		const n = merged.length;
		const idx = new Int32Array(n);
		const len = new Int32Array(n);
		const codes = new Int32Array(n);
		for (let k = 0; k < n; k++) {
			const item = merged[k];
			idx[k] = item.index;
			len[k] = item.length;
			codes[k] = codeOf(item.type);
		}

		self.postMessage(
			{ id, ok: true, n, idx, len, codes, codebook: HIGHLIGHT_CODEBOOK },
			[idx.buffer, len.buffer, codes.buffer]
		);
	} catch (error) {
		if (id !== latestId) return;
		self.postMessage({ id, ok: false, error: (error && error.message) || `${error}` });
	}
};

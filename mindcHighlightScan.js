/*
MinCCompiler on JS -- 高亮专用：注释与预处理指令识别

WHY THIS FILE EXISTS
--------------------
用户希望注释（`//`、`/* * /`）和预处理指令（`#pragma` 等）有自己的配色。
但它们都不能直接从 Lexer 的 token 流里拿：

  - 注释：Lexer 的 skipSingleLineComment / skipMultiLineComment 里，push
    COMMENT token 的那两行是被注释掉的 —— 因为这套 token 流会原样喂给
    Parser，多出来的 token 会让解析器报错。
  - 预处理指令：tokenize() 末尾有一句
        this.tokens = this.tokens.filter(t => !t.type.startsWith("PREPROCESSOR_"))
    指令 token 在返回前就被清掉了。

所以这里只对"高亮"这条链路做一遍轻量扫描，编译器流水线完全不受影响。
扫描结果是"源码区间"，之后与词法 token 合并、按起点排序，交给渲染端上色。

两个区间与词法 token 天然不重叠：Lexer 会跳过注释、也会把整条指令行读掉，
因此合并时不会撞车（下面的 merge 仍做了防御性丢弃）。
*/

import { TokenType } from './mindcLexer.js';

/** 伪 token 类型：注释 */
export const DECOR_COMMENT = '__COMMENT__';
/** 伪 token 类型：预处理指令（整条逻辑行） */
export const DECOR_DIRECTIVE = '__DIRECTIVE__';

/**
 * 高亮用的类型编码表：真实 TokenType + 两个伪类型。
 * Worker 与主线程共用同一份，保证 code -> 类型 的下标一致。
 */
export const HIGHLIGHT_CODEBOOK = [...Object.values(TokenType), DECOR_COMMENT, DECOR_DIRECTIVE];

const CODE_INDEX = new Map(HIGHLIGHT_CODEBOOK.map((type, i) => [type, i]));

/** @param {string} type @returns {number} */
export function codeOf(type) {
	return CODE_INDEX.has(type) ? CODE_INDEX.get(type) : 0;
}

/**
 * 扫描源码里的注释与预处理指令。
 *
 * 会正确跳过字符串/字符字面量，因此 `"http://x"` 里的 `//` 不会被当成注释。
 *
 * @param {string} code
 * @returns {{start: number, end: number, type: string}[]} 按起点升序、互不重叠
 */
export function scanDecorations(code) {
	const decorations = [];
	const n = code.length;
	let i = 0;
	// 本行到目前为止只见过空白（指令必须是一行的第一个非空白字符）
	let atLineStart = true;

	while (i < n) {
		const ch = code[i];

		if (ch === '\n') {
			i++;
			atLineStart = true;
			continue;
		}
		if (ch === ' ' || ch === '\t' || ch === '\r') {
			i++;                    // 空白不改变"是否行首"
			continue;
		}

		/* ---- 预处理指令：# 到本逻辑行结束（支持反斜杠续行） ---- */
		if (atLineStart && ch === '#') {
			const start = i;
			let j = i;
			while (j < n) {
				const c = code[j];
				if (c === '\\') {
					const nx = code[j + 1];
					if (nx === '\n') { j += 2; continue; }
					if (nx === '\r' && code[j + 2] === '\n') { j += 3; continue; }
				}
				if (c === '\n') break;
				j++;
			}
			decorations.push({ start, end: j, type: DECOR_DIRECTIVE });
			i = j;                  // 停在换行处（或末尾），交给上面的分支重置 atLineStart
			continue;
		}

		atLineStart = false;

		/* ---- 行注释 ---- */
		if (ch === '/' && code[i + 1] === '/') {
			const start = i;
			let j = i + 2;
			while (j < n && code[j] !== '\n') j++;
			decorations.push({ start, end: j, type: DECOR_COMMENT });
			i = j;
			continue;
		}

		/* ---- 块注释（未闭合则延伸到文件末尾） ---- */
		if (ch === '/' && code[i + 1] === '*') {
			const start = i;
			let j = i + 2;
			while (j < n && !(code[j] === '*' && code[j + 1] === '/')) j++;
			j = (j < n) ? j + 2 : n;    // 吃掉结尾的 */
			decorations.push({ start, end: j, type: DECOR_COMMENT });
			i = j;
			continue;
		}

		/* ---- 跳过字面量，避免把里面的 // # 误判 ---- */
		if (ch === '"' || ch === "'") {
			const quote = ch;
			i++;
			while (i < n) {
				if (code[i] === '\\') { i += 2; continue; }
				if (code[i] === quote) { i++; break; }
				if (code[i] === '\n') break;        // 未闭合，到此为止
				i++;
			}
			continue;
		}

		i++;
	}

	return decorations;
}

/**
 * 把词法 token 与注释/指令区间合并成一条按起点升序的流。
 *
 * @param {{index: number, length: number, type: string}[]} tokens 按 index 升序
 * @param {{start: number, end: number, type: string}[]} decorations 按 start 升序
 * @returns {{index: number, length: number, type: string}[]}
 */
export function mergeTokensWithDecorations(tokens, decorations) {
	const result = [];
	let ti = 0, di = 0;

	while (ti < tokens.length || di < decorations.length) {
		const token = ti < tokens.length ? tokens[ti] : null;
		const dec = di < decorations.length ? decorations[di] : null;

		if (dec && (!token || dec.start <= token.index)) {
			result.push({ index: dec.start, length: dec.end - dec.start, type: dec.type });
			const end = dec.end;
			di++;
			// 防御：理论上不会有 token 落在注释/指令里，万一有就丢掉
			while (ti < tokens.length && tokens[ti].index < end) ti++;
		} else if (token) {
			result.push({ index: token.index, length: token.length, type: token.type });
			ti++;
		}
	}

	return result;
}

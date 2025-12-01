# mindustry-c: A JavaScript program compiling C code (under C89) to Mindustry Assembly

## What is Mindustry-C?

**NOTICE: Currently, works are still in progress.**

Above all, please notice that: Mindustry-C is an **almost 100% AI** and partially-experimental project. The AI tool used in this project is DeepSeek (for individuals).

> I have no time to code, though. I've written only prompts and a few lines of code for it. (Yet for sure, I have to do all bug-fixing.)
> (AI is just no so realiable as I've expected)

Mindustry-C aims to compile C code to Mindustry Assembly, a brain-racking language for most of us.

There have been many similar softwares aiming at using languages like C/C++ (alike grammar) or Python to code in Mindustry, but they aren't so effective. So here we are.

## Features about Mindustry-C (Envisioned)

- Reserving most of C89 features
  - Yes, you'll have struct, pointers, unions, and typedef!
  - But you won't have `#pragma`

- Adding special types for Mindustry:
  - `device` for device (conveyors, scatters, etc.)
  - `null_t` for `null` in Mindustry (notice that it is **not** like `nullptr` or `(void*)0`)

- Memory mapping (adjusted outside the code)

- Function call stack (whose size can be adjusted)

- Mindustry "system calls"

- Direct "assembly" insertion

- Compiler's constant optimizing

## Notice

Following functions **rely on** memory allocation:

- Any variable that has address
- Any `struct`
- Any array

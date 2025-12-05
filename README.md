# mindustry-c: A JavaScript program compiling C code (under C89) to Mindustry Assembly

## What is Mindustry-C?

**NOTICE: Currently, works are still in progress.**

Above all, please notice that: Mindustry-C is an **mainly AI-generated** and partially-experimental project. The AI tool used in this project is DeepSeek (for individuals).

> I have no time to code, though. I've written only prompts and a few lines of code for it. (Yet for sure, I have to do all bug-fixing.)
> (AI is just no so realiable as I've expected)

Mindustry-C aims to compile C code to Mindustry Assembly, a brain-racking language for most of us.

There have been many similar softwares aiming at using languages like C/C++ (alike grammar) or Python to code in Mindustry, but they aren't so effective. So here we are.

## Features about Mindustry-C (Envisioned)

- Reserving most of C89 features
  - Yes, you'll have structs, pointers, unions, and `typedef`!
  - But you won't have `#pragma`
  - Also, support for function pointers might be limited

- Adding special types for Mindustry:
  - `device` for device (conveyors, scatters, etc.)
  - `null_t` for `null` in Mindustry (notice that it might **not** like `nullptr` or `(void*)0`)

- Memory mapping (adjusted outside the code)

- Function call stack (whose size can be adjusted)

- Mindustry "system calls"

- Direct "assembly" insertion

- Compiler's optimizing
  - Compile-time constant evaluation
    - Including those in `for` or `while` loop!
  - Removal of unused code/variable
    - Including unused `if` branches or `while` loops
  - Inlining functions
  - ~~Loop unrolling~~ (because instructions are expensive in Mindustry!)

## Notice

Following functions **rely on** memory allocation:

- Any variable that has address (`&var`)
- Any `struct` or `union`
- Any array

A pointer takes up 2 units of spaces, respectively storing the memory cell and address of the element it points to.

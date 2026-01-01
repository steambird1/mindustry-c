# mindustry-c: A JavaScript program compiling C code to Mindustry Assembly

## What is Mindustry-C?

**NOTICE: Currently, works are still in progress.**

## Current Plan

Incomplete parts:

- Testing
- `asm`
- Unit-relevant instructions

----

Above all, please notice that: Mindustry-C is an **mainly AI-generated** and partially-experimental project. The AI tool used in this project is DeepSeek (for individuals).

> I have no time to code, though. I've written only prompts and a few lines of code for it. (Yet for sure, I have to do all bug-fixing.)
> (AI is just no so realiable as I've expected)

Mindustry-C aims to compile C code to Mindustry Assembly, a brain-racking language for most of us.

There have been many similar softwares aiming at using languages like C/C++ (alike grammar) or Python to code in Mindustry, but they aren't so effective. So here we are.

## Features about Mindustry-C (Envisioned)

- Reserving most of C89 features
  - Yes, you'll have structs, pointers, unions, and `typedef`!
  - But you won't have `#pragma` or `#include`
  - Also, support for function pointers might be limited

- Some C/C++ features:
  - Partial support for `const`

- Adding special types for Mindustry:
  - `device` for device (conveyors, scatters, etc.)
  - `null_t` for `null` in Mindustry (notice that it might **not** like `nullptr` or `(void*)0`)

- Memory mapping (adjusted outside the code)

- Function call stack (whose size can be adjusted)

- Mindustry "system calls"
  - Building control (interfaces reserved)
  - Unit control (to be done)

- Direct "assembly" insertion

- Compiler's optimizing
  - Compile-time constant evaluation
    - Including those in `for` or `while` loop (they won't be so efficient, though)!
  - Removal of unused code/variable
    - Including unused `if` branches or `while` loops
  - Inlining functions
  - **There's no** ~~Loop unrolling~~ (because instructions are expensive in Mindustry!)

## Notice

Following functions **rely on** memory allocation:

- Any `volatile` variable
- Any variable that has address (`&var`)
- Any `struct` or `union`
- Any array

A pointer takes up 2 units of spaces, respectively storing the memory cell and address of the element it points to.

Using heap memory might be costly. Avoid declaring memory-demanding variables (listed above) in non-main functions (place them in global scope instead).

Arrays might also take up additional spaces to store its previous/next cell of storage. Designing memory structure for this strange "cyber-" hardware is hard.

## Special Grammar

- Use `auto device conveyor1;` to register a connected device (**`auto` is essential**).
- Use `(volatile int*)` (or similar) to force to transmit registry variables to some functions (reserved for builtin calls).
- Function pointers must have a `typedef` declared.

# mindustry-c: A JavaScript program compiling C code to Mindustry Assembly

[中文 README](https://github.com/steambird1/mindustry-c/blob/main/docs/LiyueSprache/README.md)

## What is Mindustry-C?

## Current Plan

Incomplete parts:

- Comprehensive testing (will be done as I occasionally use it)

----

Please notice that: Mindustry-C has AI-generated code. The AI tool used in this project is DeepSeek (for individuals).

> I have no time to code, though. I've written only prompts and some parts of code (including **100% part of final code generation**) for it. (Yet for sure, I also have to do all bug-fixing.)
> (AI is just no so realiable as I've expected)

Mindustry-C aims to compile C code to Mindustry Assembly, a brain-racking language for most of us.

There have been many similar softwares aiming at using languages like C/C++ (alike grammar) or Python to code in Mindustry, but they aren't so effective. So here we are.

## Installation

Installation is not needed. Just download `main.html` and `mindc.js` and ensure that you have Internet connection (UI requires Bootstrap from CDN; or the display might be a little bit weird). Then, input your code and start compiling!

For grammar introduction (about the features unique to Mindustry-C) and examples, refer to docs (might be in progress).

## Features about Mindustry-C

- Most of C89 features
  - Yes, you'll have structs, pointers, unions, and `typedef`!
  - But you won't have `#pragma` or `#include`
  - Also, support for function pointers might be limited

- Some C/C++ features:
  - Partial support for `const` and `auto` (used to describe automatically-connected devices)

- Adding special types for Mindustry:
  - `device` for device (conveyors, scatters, etc.)
  - `null_t` for `null` in Mindustry (notice that it might **not** like `nullptr` or `(void*)0`)

- Memory mapping (adjusted outside the code)

- Function call stack

- Mindustry "system calls"
  - Building control
  - Unit control

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
- Any array (If you don't want to use memory but still want arrays, refer to *pseudo list's* documents to look for possible alternatives.)
- Any recursive function

If functions aren't called recursively, they does not take up memory space.

A pointer takes up 2 units of spaces, respectively storing the memory cell and address of the element it points to.

Using heap memory might be costly. Avoid declaring memory-demanding variables (listed above) in non-main functions (place them in global scope instead). It is also **strongly** recommended to prevent using recursive functions. They are supported but unstable and costly as the compiler is unsure about every variable's state.

Arrays might also take up additional spaces to store its previous/next cell of storage. Designing memory structure for this strange "cyber-" hardware is hard.

## An Overview of Special Grammar

### Specialized for Mindustry

- `device` type is used to describe buildings/units in Mindustry while `content_t` is used to describe objects.
- Use `auto device conveyor1;` to register a connected device (**`auto` is essential**).
- Use `(volatile int*)` (or similar) to force to transmit registry variables to some functions (for builtin calls, especially `ulocate`).
- Function pointers must have a `typedef` to declare its type before being used.

### Different from C/C++

- Initializer lists are recommended to be unwrapped and must be **non-empty (or the compiler will crash!)**.

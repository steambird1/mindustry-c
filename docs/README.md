# Mindustry-C Guide

Mindustry-C is just like normal C, with a few special features listed below to make it suitable for Mindustry. This document helps you understand Mindustry-C quickly.

## First of All: Why Mindustry-C?

### Why is Mindustry-C Created?

Personally, I am a Mindustry player and have been looking for ways to simplify logic programming. Here are a few alternatives available on Github which I found not so useful:

- `mlogjs`: Easy to use (online compiler) but has bad support on recursive functions and memory operations or list (they have macros)
- `pyndustric`: Few array or complex structure supports

So here it is.

### Who is the Most Suitable for Mindustry-C?

If your logic is becoming sufficiently complex or needs dynamic adjustment but does not care much about speed, then Mindustry-C might be your choice, as it provides almost 100% C features including free functions, structures and arrays' use.

## Introduction

Mindustry-C is just like normal C, as I've said before. However, the standard library function differs (the only thing with the same name and the same behavior is `memcpy`). For example:

```
auto device message1;  // Declare message1 device useable in the program

void main() {          // The return type does not matter, but 'void' is recommended.
  print("Hello world!");  // There's no 'printf'.
  printflush(message1);  // Like original mindustry logic, printflush must be called.
}
```

In examples of **`testers` directory**, you can see more practical examples.

Also, the content like `@copper` and `@time` are of type `content_t` (in particular, `@copper` is of type `item_t` which can be converted into `content_t` for further use) which can be then converted.

We also have `null_t` which can refer to any type of data (by default used to contain `null`).

Here provides a short reference table for some of the useful functions:

| Function | Usage |
| :-: | :-: |
| `null_t sensor(device target, content_t data)` | Sensor data from building |
| `void ubind(content_t type)` | Bind a unit |
| ... | |

(Refer to [this](https://github.com/steambird1/mindustry-c/blob/main/docs/TYPES_AND_FUNCTIONS.md) to learn more.)

## Major Differences

The following codes are **invalid**:

- `int a[5] = {};` (Empty initializer list **(This will currently result in compiler crash!)**)
- `int a[] = {1,2,3};` (Unclear length of array)
- `struct a { struct b {} c; };` (Embedded struct or union structure)
- There's no `do-while` loop so far

There are some additional features:

- `(volatile type)` type cast will present everything as its raw form (mainly used for builtin calls, especially `ulocate`)

## Coding Style

Mindustry-C compiler itself provides very limited optimize methods. Therefore, following coding styles are recommended to help speed up your program and reduce memory usage, as processors are slow and memory blocks are expensive and small.

### 1. Prevent using recursion

Recursion functions in Mindustry-C will be automatically labelled and compiled specially, resulting in the following consequences:

- All symbols (including temporary symbols) of relevant functions will be stored in memory instead of registers, slowing down the execution of program and increasing the stress of memory use.
- Stack override is **silent**.
- Some code might result in unexpected results as recursion is not well supported currently.

*Note:* it is also recommended to prevent using function calls unless necessary because automatic inline is limited so far, and function call results in complexity.

### 2. Use global variables (especially for arrays and structures)

Memory of global variables will be assigned exactly once; however, the variables in the functions will be assigned more than once, resulting in time complexity increment.

*Note:* It is recommended not to use as few arrays and structures as you can to speed up execution. (~~But in this case, why don't you use other providers?~~)

### 3. Control the Control Flow

Although Mindustry logic will executed from the beginning after termination, it is recommended to use a `while(true)` loop to permanently hold the control flow to prevent initial initializations.

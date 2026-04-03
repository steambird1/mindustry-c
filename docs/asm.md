# ASM Statement

ASM Statement has the following special instructions:

| Statement | Meaning |
| :-: | :-: |
| `:varStorage[Cp] <target> <symbol>` | Read the symbol to the target raw variable |
| `:varRead <target>` | Representing `<target>` variable literal |
| `:label <name>` | Representing a label that will be correctly added as `<name>:` in final generation |
| `:copyObject[Cp] <target>:<type>,<source>:<type>` | Perform a copy from source object to target object. The type should be C type description (e.g. `int`, `const int`) |

(Note: `Cp` means deep copy variants)

Also, you can use Javascript-like `${}` grammar, for example:

```c
void main() {
    extern int x;
    asm("print ${x}");
}
```

This is an equivalent to the following code in the main program:

```mlog
print g:.f:main.b:1.x
```
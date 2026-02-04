# Types And Functions of Mindustry-C

Types and functions similar to C are not listed below.

## Types

Types with “*” symbol **cannot** be put in `struct` or as array type. (**It is possible that no warning or error is generated and they will be implicitly be converted to boolean values!**)

| Type | Size | Description |
| :-: | :-: | :-: |
| `null_t` | 1 | Type of `null` and can contain any value in fact |
| `device` | 1* | Handle of buildings or units |
| `content_t` | 1* | Handle of contents |
| `item_t`, `liquid_t`, `object_t`, `block_t` | 1 | Representative of item/liquid/object/block(building)s |
| `char` | 1 | Char or **Mindustry string** (String literals are ***not*** `char*`) |
| Any pointer | 2 | |

## Functions

C functions supported: `asm`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `ceil`, `floor`, `sqrt`, `abs`, `memcpy`.
Deprecated functions are not listed below.

### Mindustry System Calls

The order of these functions are defined as follows:

- If the original instruction in Mindustry has return values placed as their first parameter, then this parameter is omitted and replaced by the function's return values.
- Otherwise (for `ulocate`), they should be given as pointers.

`...` means variant parameters: In order of Mindustry code text instructions lays all **necessary** parameters.

| Function | Remarks |
| :-: | :-: |
| `void draw(const char action, ...)` | **`action` must be a string literal.** |
| `void drawflush(device target)` | |
| `void printflush(device target)` | |
| `device getlink(int id)` | |
| `void control(const char action, device target, ...)` | **`action` must be a string literal.** |
| `device radar(const char condit1, const char condit2, const char condit3, const char sorting, device from, int order)` | Relevant strings **must be string literal.** |
| `null_t sensor(device source, content_t data)` | The return value might be of any type, so cast it on your own. |
| `content_t lookup(const char category, int id)` | **`category` must be a string literal.** |
| `void wait(float time)` | |
| `void stop()` | |
| `void end()` | |
| `void ubind(content_t unit)` | |
| `void ucontrol(const char action, device target, ...)` | **`action` must be a string literal.** |
| `void ulocate(const char action, ...)` | **`action` must be a string literal.** (See remarks and examples to learn more) |
| `device uradar(const char condit1, const char condit2, const char condit3, const char sorting,int reserved, int order)` | `reserved` should always be 0. |
| `float adiff(float a, float b)` | Act as "angle diff". |
| `void* memsp(device cell, int location)` | Return a pointer pointing to a particular memory space. |

* `ulocate`: The last 4 parameters represent the output of resultX, resultY, found and building device handle. They should
  - either be a pointer (**Note: `device` type does not support pointer**),
  - or be a `(volatile type*)(object)` ("MindustryC's volatile-cast objects") which will make them compiled as they are.
  See examples to learn more.

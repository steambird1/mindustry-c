# Pseudo List

Pseudo list is an alternative when you face one of the following circumstances:

- You want dynamic access of `device` / `content_t` type (usually the former, as the latter has alternative types)
- You have no memory space but want to implement arrays

It uses registers to substitute memory spaces at O(log n) time cost.

## Example

```c
pdlist_t ls;

void main() {
   ls = pdcreate(10);
   pdwrite(ls, 3, 114514);
   print(pdread(ls, 3));
}
```

## Interfaces

### Types

| Type | Size | Description |
| :-: | :-: | :-: |
| `pdlist_t` | 1 | Handle of pseudo list. |

### Functions

| Function | Description |
| :-: | :-: |
| `pdlist_t pdcreate(int size)` | Create a pseudo list. |
| `null_t pdread(pdlist_t list, int pos)` | Read from a pseudo list. |
| `void pdwrite(pdlist_t list, int pos, null_t data)` | Write to a pseudo list. |

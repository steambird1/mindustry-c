# 开始使用 Mindustry-C

大部分时候，你可以像写 C 语言一样写 Mindustry-C。当然，你要记住这些事情：

- Mindustry 的单个内存元容量很小，所以在定义数组和指针时有 `near`（没有 `far`）。
- 大部分变量都会直接存储在寄存器中。
- 只有数值可以放到内存中（游戏限制）。Mindustry-C 对此进行了一定的扩展，表示类型的枚举（如 `@mono`、`@conveyor`）也可以放入内存。
- 不支持 do-while 循环与 switch-case。
- 尽量不要使用递归。

## Hello World

Mindustry-C 的 Hello World 略复杂一些，因为我们需要向信息板输出。

```c
auto device message1; // 声明信息板 message1。

void main() {       // 主函数的返回类型没有限制，但最好为 int 或 void。
    print("Hello world!");  // 输出字符串。Mindustry-C 没有 printf。
    printflush(message1); // 把输出刷新到信息板。
}
```

复制到编辑器后点击【编译代码】即可得到程序。

## 内存管理

你可能会注意到，这样生成的程序非常长，有很多内存处理用的代码。

如果你的程序不需要访问内存，可以在【内存块配置】中删除预设的内存块。

------

可以参阅其它部分文档以了解 Mindustry-C 的具体用法。

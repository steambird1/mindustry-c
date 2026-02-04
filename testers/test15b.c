auto device message1;

int foo(int x) {
    if (x <= 0) return 1;
    return foo(x-1) + foo(x-1);
}

void main() {
    print(foo(3));
    printflush(message1);
}
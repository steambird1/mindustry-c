auto device message1;

void inner2() {
    print("Test3");
    printflush(message1);
    wait(2);
}

void inner() {
    print("Test2");
    inner2();
    printflush(message1);
    wait(2);
}

void outer() {
    inner();
    print("Test1");
    printflush(message1);
}

void main() {
    outer();
    inner();
}
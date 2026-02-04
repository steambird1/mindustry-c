void foo(int n) {
    print("Pre",n);
    if (n <= 0) return;
    foo(n-1);
print("Post",n);
}

void main() {
    foo(2);
print("End");
}
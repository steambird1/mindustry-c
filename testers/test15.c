int fib(int n) {
    if (n <= 2) return 1;
    else return fib(n-1) + fib(n-2);
}

void main() {
    print(fib(5));
}
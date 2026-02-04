
void main() {
    volatile int a = 3, b = 4, c = 0;
    int i = 0;
    print(a,b);
    for (i = 0; i < a+b; i++) {
        if (c % 2 == 0) {
            c += i;
        } else if (c % 3 == 0) {
            a++;
        } else {
            c -= i;
        }
    }
}
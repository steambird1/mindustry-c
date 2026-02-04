
void main() {
    volatile int a = 3, b = 4, c = 0;
    int i = 0;
    print(a,b);
    while (i < a+b) {
        if (c % 2 == 0) {
            c += i;
        } else if (c == -1) {
            a++;
        } else {
            c -= i;
        }
        i++;
    }
}
void oddproc(int x) {
    print("奇数");
}

void evenproc(int x) {
    print("偶数");
}

typedef void(*fptr)(int);

void main() {
    int i = 0;
    fptr fs;
    for (i = 0; i < 10; i++) {
        if (i % 2 == 0) {
            fs = oddproc;
        } else {
            fs = evenproc;
        }
        (fs)(i);
    }
}
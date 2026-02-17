int sid = 0;

void foo() {
    sid++;
}

void main() {
    int i = 0;
    while (i < 10) {
        foo(); i++;
    }
print(sid);
}
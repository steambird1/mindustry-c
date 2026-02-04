int data[6];

void visit(int n) {
    print(n);
    if (n < 0) return;
    data[n] = n*2;
    visit(n-1);
}

void main() {
    visit(5);
    int i;
    for (i = 0; i < 6; i++) print(data[i],";");
}
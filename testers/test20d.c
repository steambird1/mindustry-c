void main() {
    int a = 3;
    int b = 2;
    int sum = a + b, total = 1, i, j;
    for (i = 0; i < sum; i++) {
    for (j = 0; j < sum; j++) total += i + j;
}
    print(total);
    
}
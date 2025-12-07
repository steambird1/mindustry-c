int* pts(int w) {
    return (int*)(w);
}

void main() {
    int *ref = pts(114);
}
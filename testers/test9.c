volatile void fcall(int *arg) {
   arg += 10;
}

void main() {
    int t = 5;
    fcall((int*)t);
}
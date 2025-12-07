void main2() {
  int a, i, j; a = 0; j = 20;
  for (i = 0; i < 10; i++) {
     a += i * j; j--;
}
}

void main() {
  const int a; int i; a = 0;
  for (i = 0; i < 10; i++) {
if (i % 2 == 0) a *= i;
else a += i;
}
}





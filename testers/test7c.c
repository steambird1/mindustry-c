int foo(int x, int y) {
   return x+y;
}

void main() {
   int test = 0, i;
   for (i = 0; i < foo(11,45); i++) {
      test++;
   }
}
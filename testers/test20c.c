int foo(int x, int y) {
   return x+y;
}

void main() {
   int res = foo(3,4), test = 0, i;
   for (i = 0; i < foo(11,45); i++) {
      test += res + i + 10;
   }
   test += foo(5,6);
print(test);
}
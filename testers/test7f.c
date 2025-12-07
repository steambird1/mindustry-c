int foo(int x, int y) {
   return x+y;
}

void main() {
   int res = foo(3,4), test = 0, i = 0;
   while (i < foo(11,45)) {
      test += res + i;
	  if (test > 200) break;
     i++;
   }
   test += foo(5,6);
}
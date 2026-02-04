int bar(int x, int y) {
   return x*y;
}

int foo(int x, int y) {
   return bar(x,1)+bar(y,1);
}

int sub(int x, int y) {
   return foo(bar(x,x),foo(bar(y,y),bar(2*x,y)));
}

void main() {
   print(sub(6,5));
   int res = foo(3,4), test = 0, i, *w;
   w = &test;
   for (i = 0; i < foo(11,45); i++) {
      test += res + i;
	  (*w) += 10;
   }
   test += foo(5,6);
}
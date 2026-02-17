int bar(int x, int y) {
   return x*y;
}

int foo(int x, int y) {
   return bar(x,y)+x;
}

int test(int x, int y) {
   int a = foo(x+1,y+1);
   int b = foo(x+3,y+4);
   return a+b;
}

void main() {
   print(test(5,14));
} 
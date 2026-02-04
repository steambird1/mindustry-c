int bar(int x, int y) {
   return x*y;
}

int foo(int x, int y) {
   return bar(x,y)+x;
}

void main() {
   print(foo(5,14));
}
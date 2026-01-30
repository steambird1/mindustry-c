void main() {
   int i, *ptr = (int*)50;
   for (i = 0; i < 40; i++) {
       ptr -= 1; wait(0.1);
   }
}
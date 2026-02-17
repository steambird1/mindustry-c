void main() {
   int sum = 0, i = 0, j = 0;
   while (i < 10) {
       j = 0;
       while (j < 10) {
sum += i+j; j++;
       } sum += i;
       i++;
   }
print(sum);
}
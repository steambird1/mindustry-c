int arr[4][4];

void main() {
   int i,j;
   for (i = 0; i < 4; i++) {
       for (j = 0; j < 4; j++) {
int *refs = arr[i];
refs[j] = (i*i) + (j*j);
       }
   }
for (i = 0; i < 4; i++) {
       for (j = 0; j < 4; j++) {
print(arr[i][j]);
       }
   }
}
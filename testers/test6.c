int temp(int x) {
  return 123;
}

typedef int(*tptr)(int);

int main2() {

}

void main() {
   tptr s;
   s = temp;
}
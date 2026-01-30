int a[5] = {1,2,3};

struct stru {
   int a; int b;
};

stru sts[6];

void main() {
   //stru w;
   //w = {4+6, 5};
   //print(w.a+w.b);
   sts[3] = {222, 333};
   print(sts[3].a);
}
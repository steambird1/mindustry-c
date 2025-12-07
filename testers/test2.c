struct ws {
   int a; char b;
};

union uns {
  int a;
  ws t;
};

struct qs {
  int r;
  uns w;
};

void main() {
  qs t, *s;
  void *r;
  int cs;
}
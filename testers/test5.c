int g(int w) {
	return 114 + w;
}

int* pts(int w) {
    return (int*)(w);
}

struct test {
	int q; int *r;
}

void main() {
	int h = g(10) + 20;
    int *ref = pts(114);
	test *ptr;
	ptr = (test*) ref;
}
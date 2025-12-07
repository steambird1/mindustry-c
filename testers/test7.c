void main() {
	int n = 10, w = 0, t = 0, q = 0, i, j;
	for (i = 1; i < n; i++) {
		w += i;
		for (j = 0; j < 5; j++) {
			t += w;
			t -= j;
			q++;
		}
		t++;
	}		
}

void main() {
	int n = 10, w = 0, t = 0, q = 0, i, j;
	for (i = 1; i < n; i++) {
		for (j = 0; j < 5; j++) {
			w++;
		}
	}		
}
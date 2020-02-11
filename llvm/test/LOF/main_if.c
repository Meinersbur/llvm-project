// RUN:
// REQUIRES: fixing

void test(long long i);

int main(int argc, char *argv[]) {
#pragma clang loop vectorize(assume_safety)
	for (long long i = 0ll; i < 32ll; i += 1ll) {
		if (i != 16)
			test(i);
	}
	return 0;
}

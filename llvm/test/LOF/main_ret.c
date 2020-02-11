// RUN:
// REQUIRES: fixing

void test(long long i);

int main(int argc, char *argv[]) {
#pragma clang loop vectorize(assume_safety)
	for (long long i = 0ll; i < 32ll; i += 1ll)
		for (long long j = 0ll; j < 32ll; j += 1ll) {
			if (j == 16)
				return 1;
			test(i);
		}
	return 0;
}

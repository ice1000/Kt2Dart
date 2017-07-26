#include <stdio.h>
#include <stdlib.h>

int main(const int argc, const char *argv[]) {
	if (argc <= 1) {
		printf("Please specify an input file.\n");
		return 0;
	}
	char *s = argv[1];
	return 0;
}

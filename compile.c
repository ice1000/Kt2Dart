#include <stdio.h>
#include <stdlib.h>
#include <memory.h>

int main(const int argc, const char *argv[]) {
	if (argc <= 1) printf("Please specify an input file.\n");
	else {
		size_t size = 5000 * sizeof(char);
		char *buffer = (char *) malloc(size);
		int ret = fread(buffer, 1, size, fopen(argv[1], "r"));
		char *command = (char *) malloc(size + 10);
		memset(command, '\0', size);
		sprintf(command, "echo \"%s\" | ./Main", buffer);
		puts(buffer);
		ret = system(command);
		free(buffer);
		free(command);
		return ret;
	}
}

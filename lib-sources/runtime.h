#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>

void *malloc_and_return_address(size_t size);

char *concat_strings(const char *str1, const char *str2);

void printInt(int i);

void printString(char *s);

void error();

char *readString();

int readInt();

bool compare_strings(const char *str1, const char *str2);
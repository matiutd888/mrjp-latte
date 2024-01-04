#include "runtime.h"
#include <stdint.h>

void *malloc_and_return_address(size_t size) {
    void *res = malloc(size);
    if (!res) {
        perror("malloc failed");
        exit(EXIT_FAILURE);
    }
    return res;
}

char *concat_strings(const char *str1, const char *str2) {
    size_t len1 = strlen(str1);
    size_t len2 = strlen(str2);
    size_t total_len = len1 + len2 + 1;  // Add 1 for the null terminator

    char *result = (char *)malloc_and_return_address(total_len);

    strcpy(result, str1);
    strcat(result, str2);

    return result;
}

void printInt(int32_t i) {
    printf("%d\n", i);
}

void printString(char *s) {
    printf("%s\n", s);
}

void error() {
    fprintf(stderr, "runtime error");
    exit(EXIT_FAILURE);
}

char *readString() {
    char *inputString = NULL;
    size_t bufferSize = 0;

    getline(&inputString, &bufferSize, stdin);
    
    // Remove the newline character, if present
    size_t length = strlen(inputString);
    if (length > 0 && inputString[length - 1] == '\n') {
        inputString[length - 1] = '\0';
    }
    return inputString;
}

int32_t readInt() {
    int n;
    scanf("%d\n", &n);
    return n;
}

int32_t compare_strings(const char *str1, const char *str2) {
    int32_t res = (int32_t)strcmp(str1, str2);
    return (int)(res == 0);
}
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

inline void *malloc_and_return_address(size_t size) {
    void *res = malloc(size);
    if (!res) {
        perror("malloc failed");
        exit(1);
        exit(EXIT_FAILURE);
    }
    return res;
}

char *concat_strings(const char *str1, const char *str2) {
    // Calculate the total length needed for the concatenated string
    size_t len1 = strlen(str1);
    size_t len2 = strlen(str2);
    size_t total_len = len1 + len2 + 1;  // Add 1 for the null terminator

    // Allocate memory for the concatenated string
    char *result = (char *)malloc_and_return_address(total_len);

    strcpy(result, str1);
    strcat(result, str2);

    return result;
}

bool compare_strings(const char *str1, const char *str2);
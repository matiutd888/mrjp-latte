// malloc_util.c
#include <stdlib.h>

inline void *malloc_and_return_address(size_t size) {
    void *res = malloc(size);
    if (!res) {
        perror("malloc failed");
        exit(1);
        exit(EXIT_FAILURE);
    }
    return res;
}


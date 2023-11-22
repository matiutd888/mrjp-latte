#include <stdio.h>

struct c {
    int x;
    void siema(int val) {
        int x = val;
        printf("%d\n", x);
    }
};

struct c retC() {
    struct c c;
    return c;
}


int main() {
    int x = 0;
    // {
    // 	int c = 10;
    //     int x = x + 2, y = x;
    //     printf("c=%d\n", c);
    //     printf("x=%d\n", x);
    //     printf("y=%d\n", y);
    // }
    struct c c;
    c.x = 1;
    c.siema(2);
    retC().x = 7;
}

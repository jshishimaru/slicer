/* example2.c — Another test program with reducible code */

#include <stdio.h>

/* Unused globals */
static int g_unused_x = 100;
static int g_unused_y = 200;

/* Unused function */
static int unused_add(int a, int b) {
    return a + b;
}

/* Used globals */
static int g_total = 0;

/* Used function */
static int multiply(int a, int b) {
    /* Constant expression */
    int factor = (2 << 3) / 4;
    return a * b * factor;
}

int main(void) {
    int i;
    int result = 0;

    for (i = 1; i <= 5; i++) {
        result += multiply(i, i + 1);
        g_total++;
    }

    printf("result = %d, total = %d\n", result, g_total);
    return 0;
}

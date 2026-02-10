/* Example C program — mimics Csmith output structure.
   Has unused globals, unused functions, and reducible code. */

#include <stdio.h>
#include <stdlib.h>

/* --- Unused global variables --- */
static int g_unused_1 = 42;
static long g_unused_2 = 99999L;
static int g_unused_arr[10] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};

/* --- Used global variables --- */
static int g_counter = 0;
static int g_result = 0;

/* --- Unused helper function --- */
static int func_unused_helper(int a, int b) {
    return a + b * 2;
}

/* --- Another unused function --- */
static void func_dead_code(void) {
    int x = 10;
    int y = 20;
    g_unused_1 = x + y;
}

/* --- Used helper function --- */
static int func_compute(int x) {
    int tmp = x * 3 + 1;
    /* Constant expression that can be folded */
    int mask = (1 << 4) - 1;
    return tmp & mask;
}

/* --- Another used function with reducible expressions --- */
static int func_accumulate(int start, int count) {
    int i;
    int sum = start;
    for (i = 0; i < count; i++) {
        sum = sum + func_compute(i);
        g_counter++;
    }
    return sum;
}

/* --- Unused struct type --- */
struct unused_struct {
    int field_a;
    long field_b;
    char field_c[32];
};

/* --- Used struct --- */
struct result_info {
    int value;
    int iterations;
};

/* --- Empty function (no-op) --- */
static void func_noop(void) {
    /* intentionally empty */
}

int main(void) {
    struct result_info res;
    int a = 5;
    int b = 10;
    int c;

    /* Call used functions */
    c = func_accumulate(a, b);

    /* Store results */
    res.value = c + g_counter;
    res.iterations = b;
    g_result = res.value;

    /* Produce deterministic output */
    printf("checksum = %X\n", (unsigned int)(g_result ^ res.iterations));
    return 0;
}

#include <stdio.h>
#include <stdlib.h>

// Define our simple standard library functions

// Naming is tricky on macOS - functions get an underscore prefix automatically
// So we'll define the bare names and let the assembler add the underscore

void errorlevel(int exit_code) {
    exit(exit_code);
}

void print(int value) {
    printf("%d", value);
    fflush(stdout);
}

void println(int value) {
    printf("%d\n", value);
}
#include <stdio.h>
int main() {
    int a = 1;
    int b = 99;
    int c = b;
    int d = 0;
    int e = 0;
    int f = 0;
    int g = 0;
    int h = 0;
    if (a != 0) {
        b *= 100;
        b += 100000;
        c = b;
        c += 17000;
        // c == 126900
    }
    // b == 109900
    for (; b += 17; b != c) { // 1000 times
        f = 1;
        d = 2;
        do {
            e = 2;
            do {
                if (d * e == b) {
                    f = 0;
                }
                e++;
            } while (b != e);
            // e == b
            d++;
        } while (b != d);
        // d == b
        if (f == 0) {
            h++;
        }
        b -= 17;
    }
    printf("h: %d\n", h);
    return 0;
}

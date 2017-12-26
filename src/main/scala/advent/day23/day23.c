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
    }
    for (; b != c; b += 17) {
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
            d++;
        } while (b != d);
        if (f == 0) {
            h++;
        }
    }
    printf("h: %d\n", h);
    return 0;
}

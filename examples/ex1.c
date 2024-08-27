int lol;
int lel[12];

/* void halt_wrapper(int code) { */
/*     if (code) { */
/*         // faster than using !code */
/*     } */
/*     else { */
/*         code = 42; */
/*     } */
/*     asm ("hlt l.code"); */
/* } */

/* int main() { */
/*     int lal = 29; */
/*     lal++; */
/*     lol = lal+5; */
/*     return 34+lol; */
/* } */

void _start() {
    char* hi = "hello magalads";
    int code = 33;
    code++;
    code+=35;
    asm ("hlt *l.code");
}

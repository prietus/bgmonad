/* Abstract wallpapers in the Catppuccin Mocha palette: long curved lines
 * and soft gradients, which is what a slow flow shows best.
 *
 *   gcc -O3 -ffast-math -fopenmp -o gen-wallpapers tools/gen-wallpapers.c -lm
 *   ./gen-wallpapers STYLE WIDTH HEIGHT SEED > out.ppm
 *
 * STYLE: 0 ribbons, 1 contours, 2 silk, 3 aurora, 4 ember, and the minimal
 * ones: 5 line, 6 strands, 7 arcs, 8 dunes, 9 ring. The ones in wallpapers/
 * are 3840x2160 with seeds 3, 3, 11, 3, 5 and 3 for all the minimal ones; another seed gives
 * other shapes in the same style. */
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct { double r, g, b; } col;
#define HEX(h) ((col){ ((h >> 16) & 255) / 255.0, ((h >> 8) & 255) / 255.0, (h & 255) / 255.0 })
static col CRUST, MANTLE, BASE, SURF0, SURF1, OVER0, MAUVE, PINK, LAVENDER, BLUE, SAPPHIRE, SKY, TEAL,
           GREEN, YELLOW, PEACH, MAROON, RED, FLAMINGO, ROSEWATER, TEXT;
static void palette(void)
{
    CRUST = HEX(0x11111b); MANTLE = HEX(0x181825); BASE = HEX(0x1e1e2e); SURF0 = HEX(0x313244);
    SURF1 = HEX(0x45475a); OVER0 = HEX(0x6c7086); MAUVE = HEX(0xcba6f7); PINK = HEX(0xf5c2e7);
    LAVENDER = HEX(0xb4befe); BLUE = HEX(0x89b4fa); SAPPHIRE = HEX(0x74c7ec); SKY = HEX(0x89dceb);
    TEAL = HEX(0x94e2d5); GREEN = HEX(0xa6e3a1); YELLOW = HEX(0xf9e2af); PEACH = HEX(0xfab387);
    MAROON = HEX(0xeba0ac); RED = HEX(0xf38ba8); FLAMINGO = HEX(0xf2cdcd); ROSEWATER = HEX(0xf5e0dc);
    TEXT = HEX(0xcdd6f4);
}

static double clamp01(double x) { return x < 0 ? 0 : x > 1 ? 1 : x; }
static double sstep(double a, double b, double x) { double t = clamp01((x - a) / (b - a)); return t * t * (3 - 2 * t); }
static col mix(col a, col b, double t) { return (col){ a.r + (b.r - a.r) * t, a.g + (b.g - a.g) * t, a.b + (b.b - a.b) * t }; }
static col scale(col a, double k) { return (col){ a.r * k, a.g * k, a.b * k }; }
static col add(col a, col b, double k) { return (col){ a.r + b.r * k, a.g + b.g * k, a.b + b.b * k }; }
/* a colour ramp through n stops */
static col ramp(const col *c, int n, double t)
{
    t = clamp01(t) * (n - 1);
    int i = (int)t;
    if (i >= n - 1) return c[n - 1];
    return mix(c[i], c[i + 1], sstep(0, 1, t - i));
}

/* smooth noise: a few sine waves in seeded random directions */
typedef struct { double kx[8], ky[8], ph[8], amp[8]; int n; } field;
static uint64_t rng;
static double rnd(void)
{
    rng ^= rng << 13; rng ^= rng >> 7; rng ^= rng << 17;
    return (rng >> 11) / 9007199254740992.0;
}
static field mkfield(int n, double freq, double gain, double lac)
{
    field f; f.n = n;
    double a = 1, tot = 0;
    for (int i = 0; i < n; i++) {
        double th = rnd() * 2 * M_PI, k = freq * pow(lac, i) * (0.8 + 0.4 * rnd());
        f.kx[i] = k * cos(th); f.ky[i] = k * sin(th); f.ph[i] = rnd() * 2 * M_PI; f.amp[i] = a;
        tot += a; a *= gain;
    }
    for (int i = 0; i < n; i++) f.amp[i] /= tot;
    return f;
}
static double ev(const field *f, double x, double y)
{
    double s = 0;
    for (int i = 0; i < f->n; i++) s += f->amp[i] * sin(f->kx[i] * x + f->ky[i] * y + f->ph[i]);
    return s;
}

static field F[8];
static int style;
static double px; /* one pixel, in screen heights */
static double aspect;

/* the scalar field whose level lines are the curves of the picture */
static double height(double x, double y)
{
    double wx = x + 0.35 * ev(&F[1], x, y), wy = y + 0.35 * ev(&F[2], x, y);
    switch (style) {
    case 0: /* ribbons: diagonal bands, bent */
    case 4:
        return 7.0 * (0.62 * wy + 0.38 * wx) + 2.2 * ev(&F[0], wx, wy);
    case 1: /* contours */
        return 9.0 * ev(&F[0], wx, wy);
    case 2: /* silk */
        return 5.5 * (0.75 * wx - 0.5 * wy) + 1.6 * ev(&F[0], wx, wy);
    case 3: /* aurora */
        return 5.0 * (wy - 0.25 * wx) + 3.0 * ev(&F[0], wx, wy);
    /* the minimal ones: one slow field, hardly bent */
    case 5: /* line */
        return y + 0.17 * ev(&F[6], x, y) + 0.05 * x;
    case 6: /* strands */
        return 0.8 * y + 0.25 * x + 0.15 * ev(&F[6], x, y);
    case 7: /* arcs, around a point beyond the lower right corner */
        return hypot(x - aspect * 0.94, y - 1.18) + 0.025 * ev(&F[6], x, y);
    case 8: /* dunes */
        return y + 0.13 * ev(&F[6], x, y) + 0.10 * x / aspect;
    default: /* ring */
        return hypot(x - aspect * 0.64, y - 0.5);
    }
}

static col shade(double x, double y)
{
    double h = height(x, y);
    double e = px * 0.5;
    double gx = (height(x + e, y) - height(x - e, y)) / (2 * e), gy = (height(x, y + e) - height(x, y - e)) / (2 * e);
    double g = sqrt(gx * gx + gy * gy) + 1e-9;   /* field units per screen height */
    double pxf = g * px;                          /* field units per pixel */
    double big = ev(&F[3], x, y);                 /* a slow variation over the screen */

    if (style == 0 || style == 4) {
        static col cool[] = { {0}, {0}, {0}, {0}, {0}, {0} }, warm[6];
        cool[0] = CRUST; cool[1] = BASE; cool[2] = mix(SURF0, MAUVE, 0.25); cool[3] = mix(SURF1, MAUVE, 0.55);
        cool[4] = mix(MAUVE, BASE, 0.25); cool[5] = mix(PINK, BASE, 0.2);
        warm[0] = CRUST; warm[1] = BASE; warm[2] = mix(SURF0, RED, 0.30); warm[3] = mix(RED, BASE, 0.50);
        warm[4] = mix(RED, MAROON, 0.5); warm[4] = mix(warm[4], BASE, 0.22); warm[5] = mix(PEACH, BASE, 0.15);
        const col *rp = style == 0 ? cool : warm;
        double i = floor(h), t = h - i;
        /* every band has its own tone, wandering up and down the ramp */
        double tone = 0.5 + 0.5 * sin(i * 1.7 + 0.6 * sin(i * 0.37)) ;
        tone = 0.12 + 0.76 * tone + 0.12 * big;
        col c = ramp(rp, 6, tone);
        /* lit from the edge it starts at, darker towards the next band */
        c = scale(c, 1.08 - 0.42 * pow(t, 0.8));
        /* the shadow the previous band casts, and its bright rim */
        c = scale(c, 1 - 0.35 * exp(-t / 0.07));
        col rim = style == 0 ? mix(LAVENDER, ROSEWATER, 0.35) : mix(YELLOW, ROSEWATER, 0.4);
        double d = (1 - t) / pxf;                 /* pixels to the end of the band */
        c = add(c, rim, 0.55 * exp(-d / 2.2) + 0.10 * exp(-d / 14.0));
        /* antialias the jump to the next band */
        if (t / pxf < 1.0) {
            double tone0 = 0.5 + 0.5 * sin((i - 1) * 1.7 + 0.6 * sin((i - 1) * 0.37));
            col prev = add(scale(ramp(rp, 6, 0.12 + 0.76 * tone0 + 0.12 * big), 0.66), rim, 0.65);
            c = mix(prev, c, clamp01(t / pxf));
        }
        return c;
    }
    if (style == 1) {
        col bg = mix(CRUST, BASE, 0.5 + 0.5 * big);
        bg = add(bg, MAUVE, 0.05 * sstep(-0.2, 0.9, ev(&F[4], x, y)));
        bg = add(bg, BLUE, 0.05 * sstep(-0.2, 0.9, -ev(&F[4], x, y)));
        /* faintly terraced, so the lines read as a relief */
        double i = floor(h + 0.5);
        bg = scale(bg, 1 + 0.035 * sin(i * 2.1));
        double d = fabs(h - i) / pxf;             /* pixels to the nearest level line */
        int major = ((int)i % 5 + 5) % 5 == 0;
        double w = major ? 1.6 : 0.9;
        double a = 1 - sstep(w - 0.6, w + 0.6, d);
        static col lr[4];
        lr[0] = MAUVE; lr[1] = LAVENDER; lr[2] = BLUE; lr[3] = TEAL;
        col lc = ramp(lr, 4, 0.5 + 0.5 * ev(&F[5], x, y) * 1.4);
        col c = mix(bg, lc, a * (major ? 0.95 : 0.5));
        return add(c, lc, (major ? 0.10 : 0.04) * exp(-d / 9.0));
    }
    if (style == 2) {
        static col sr[6];
        sr[0] = CRUST; sr[1] = MANTLE; sr[2] = mix(SURF0, BLUE, 0.2); sr[3] = mix(SURF1, SAPPHIRE, 0.45);
        sr[4] = mix(BLUE, BASE, 0.3); sr[5] = mix(SKY, LAVENDER, 0.5);
        double s = sin(2 * M_PI * h), s2 = sin(2 * M_PI * h * 0.5 + 1.0);
        double v = 0.5 + 0.5 * s;
        col c = ramp(sr, 6, 0.06 + 0.66 * pow(v, 2.2) * (0.7 + 0.3 * s2) + 0.10 * big);
        /* the sheen along the crest of every fold: a hairline with a halo */
        double crest = h - floor(h) - 0.25;       /* sin peaks at 0.25 */
        double d = fabs(crest) / pxf;
        col sh = mix(ROSEWATER, LAVENDER, 0.5);
        c = add(c, sh, (0.45 * exp(-d * d / 2.5) + 0.12 * exp(-d / 18.0)) * (0.55 + 0.45 * s2));
        return c;
    }
    if (style >= 5) {
        /* Minimal: nearly all of it is the dark base. Distances are in
         * pixels of a 1080 screen, so it looks the same at any size. */
        #define DIST(c) (fabs(h - (c)) / g * 1080.0)
        #define LINE(d, w) (1 - sstep((w) - 0.55, (w) + 0.55, (d)))
        col bg = mix(CRUST, BASE, 0.60 + 0.30 * big);
        if (style == 5) {
            static col lr[3];
            lr[0] = MAUVE; lr[1] = LAVENDER; lr[2] = BLUE;
            col lc = ramp(lr, 3, x / aspect);
            double c0 = 0.60, d = DIST(c0);
            /* a breath of colour under the line, like a horizon */
            if (h > c0) bg = add(bg, lc, 0.055 * exp(-(h - c0) / 0.22));
            col c = mix(bg, lc, 0.95 * LINE(d, 0.95));
            return add(c, lc, 0.11 * exp(-d / 40.0) + 0.03 * exp(-d / 220.0));
        }
        if (style == 6) {
            col lc[3] = { PINK, MAUVE, BLUE };
            col c = bg;
            for (int k = 0; k < 3; k++) {
                /* they run together and drift apart */
                double d = DIST(0.66 + 0.035 * (k - 1) + 0.030 * ev(&F[3 + k], x * 0.8, y * 0.8));
                c = mix(c, lc[k], 0.9 * LINE(d, 0.8));
                c = add(c, lc[k], 0.07 * exp(-d / 32.0));
            }
            return c;
        }
        if (style == 7) {
            double r0 = 0.56, bright[4] = { 0.95, 0.60, 0.38, 0.22 };
            col lc = mix(LAVENDER, MAUVE, 0.5);
            /* the disc inside the first arc is a shade lighter */
            col c = mix(bg, mix(BASE, SURF0, 0.55), 1 - sstep(-0.7, 0.7, (h - r0) / g * 1080.0));
            for (int k = 0; k < 4; k++) {
                double d = DIST(r0 + 0.15 * k);
                c = mix(c, lc, bright[k] * LINE(d, 0.9));
                c = add(c, lc, 0.08 * bright[k] * exp(-d / 36.0));
            }
            return c;
        }
        if (style == 8) {
            double edge[3] = { 0.40, 0.64, 0.88 };
            col tone[4];
            tone[0] = mix(CRUST, BASE, 0.5); tone[1] = BASE; tone[2] = mix(BASE, SURF0, 0.5); tone[3] = mix(BASE, SURF0, 0.95);
            col c = scale(tone[0], 0.92 + 0.10 * big);
            for (int k = 0; k < 3; k++) {
                double sd = (h - edge[k]) / g * 1080.0;     /* signed: positive below the edge */
                col t = scale(tone[k + 1], 0.92 + 0.10 * big);
                /* each dune is lit at its crest and falls into shade */
                t = scale(t, 1.06 - 0.20 * clamp01((h - edge[k]) / 0.24));
                c = mix(c, t, sstep(-0.7, 0.7, sd));
                /* and casts a soft shadow on the one behind */
                if (sd < 0) c = scale(c, 1 - 0.22 * exp(sd / 26.0));
                col rim = k == 0 ? mix(PEACH, ROSEWATER, 0.4) : ROSEWATER;
                double d = fabs(sd);
                c = add(c, rim, (k == 0 ? 0.75 : 0.22) * LINE(d, 0.8) + (k == 0 ? 0.07 : 0.02) * exp(-d / 30.0));
            }
            return c;
        }
        {
            double R = 0.30, d = DIST(R);
            static col rr[4];
            rr[0] = PINK; rr[1] = MAUVE; rr[2] = BLUE; rr[3] = TEAL;
            double ang = atan2(y - 0.5, x - aspect * 0.64);
            col lc = ramp(rr, 4, 0.5 + 0.5 * sin(ang + 0.9));
            col c = bg;
            if (h < R) c = add(c, lc, 0.075 * exp(-(R - h) / 0.11));
            c = mix(c, lc, 0.95 * LINE(d, 1.0));
            return add(c, lc, 0.12 * exp(-d / 42.0) + 0.03 * exp(-d / 200.0));
        }
    }
    /* aurora: curtains of colour over the dark base, with hairlines */
    {
        col c = mix(CRUST, BASE, 0.5 + 0.4 * big);
        static col ar[5];
        ar[0] = MAUVE; ar[1] = BLUE; ar[2] = TEAL; ar[3] = GREEN; ar[4] = PINK;
        double t = h - floor(h);
        double i = floor(h);
        col cc = ramp(ar, 5, 0.5 + 0.5 * sin(i * 1.3 + 2.0 * ev(&F[5], x, y)));
        /* each curtain glows from its upper edge and fades out */
        double glow = exp(-t / 0.33) * (0.30 + 0.20 * ev(&F[4], x * 1.0, y));
        c = add(c, cc, clamp01(glow));
        double d = t / pxf;
        c = add(c, mix(cc, ROSEWATER, 0.5), 0.5 * exp(-d * d / 2.0) + 0.10 * exp(-d / 10.0));
        if ((1 - t) / pxf < 1.0) {                /* antialias the next curtain's edge */
            col nx = ramp(ar, 5, 0.5 + 0.5 * sin((i + 1) * 1.3 + 2.0 * ev(&F[5], x, y)));
            col top = add(add(mix(CRUST, BASE, 0.5 + 0.4 * big), nx, clamp01(0.30 + 0.20 * ev(&F[4], x, y))), mix(nx, ROSEWATER, 0.5), 0.6);
            c = mix(c, top, 1 - clamp01((1 - t) / pxf));
        }
        return c;
    }
}

int main(int argc, char **argv)
{
    if (argc < 5) return 1;
    style = atoi(argv[1]);
    int W = atoi(argv[2]), H = atoi(argv[3]);
    rng = 0x9e3779b97f4a7c15ull * (uint64_t)(atoi(argv[4]) + 1);
    for (int i = 0; i < 20; i++) rnd();
    palette();
    px = 1.0 / H;
    aspect = (double)W / H;
    F[0] = mkfield(5, 2.2, 0.55, 1.7);   /* the shape of the curves */
    F[1] = mkfield(3, 1.3, 0.5, 1.8);    /* domain warp */
    F[2] = mkfield(3, 1.3, 0.5, 1.8);
    F[3] = mkfield(2, 0.9, 0.5, 1.5);    /* slow light */
    F[4] = mkfield(3, 1.6, 0.6, 1.6);
    F[5] = mkfield(2, 1.1, 0.6, 1.6);
    F[6] = mkfield(2, 1.5, 0.45, 1.7);  /* the minimal ones */
    uint8_t *out = malloc((size_t)W * H * 3);
    const int SS = 3;
    #pragma omp parallel for schedule(dynamic, 8)
    for (int y = 0; y < H; y++) {
        uint64_t r = 0x2545F4914F6CDD1Dull * (uint64_t)(y + 7);
        for (int x = 0; x < W; x++) {
            col a = { 0, 0, 0 };
            for (int sy = 0; sy < SS; sy++)
                for (int sx = 0; sx < SS; sx++) {
                    col c = shade((x + (sx + 0.5) / SS) / H, (y + (sy + 0.5) / SS) / H);
                    a.r += c.r; a.g += c.g; a.b += c.b;
                }
            double v[3] = { a.r / (SS * SS), a.g / (SS * SS), a.b / (SS * SS) };
            for (int k = 0; k < 3; k++) {
                /* triangular dither: no banding in the dark gradients */
                r ^= r << 13; r ^= r >> 7; r ^= r << 17; double n1 = (r >> 11) / 9007199254740992.0;
                r ^= r << 13; r ^= r >> 7; r ^= r << 17; double n2 = (r >> 11) / 9007199254740992.0;
                double q = clamp01(v[k]) * 255.0 + (n1 + n2 - 1.0) * 0.5 + 0.5;
                out[((size_t)y * W + x) * 3 + k] = (uint8_t)(q < 0 ? 0 : q > 255 ? 255 : q);
            }
        }
    }
    printf("P6\n%d %d\n255\n", W, H);
    fwrite(out, 3, (size_t)W * H, stdout);
    return 0;
}

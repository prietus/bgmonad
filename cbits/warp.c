/* The renderer: every pixel of the output is a blend of up to a few layers
 * (the wallpaper, its blurred copy, the wallpaper it is replacing...), all
 * sampled at a position displaced by a smooth field. The field comes as a
 * coarse grid of source coordinates, computed in Haskell from the effects
 * of the configuration, and is interpolated linearly inside every cell. */
#include <pthread.h>
#include <stdint.h>
#include <string.h>

#define LANES 0x00FF00FF00FF00FFull
#define HALF  0x0080008000800080ull

typedef struct {
    uint32_t *dst;
    int dw, dh;
    const float *grid;            /* gw * gh pairs (x, y), in source pixels */
    int gw, cell;
    const uint32_t *const *layers; /* NULL = a plain colour */
    const uint32_t *colors;
    const uint32_t *weights;       /* out of 256 */
    int nlayers;
    int sw, sh;
    int y0, y1;
} job;

/* 0xAARRGGBB -> the four channels in 16 bit lanes, and back. */
static inline uint64_t spread(uint32_t p)
{
    return ((uint64_t)p | ((uint64_t)p << 24)) & LANES;
}

static inline uint32_t pack(uint64_t x)
{
    return (uint32_t)(x | (x >> 24));
}

static inline uint64_t sample(const uint32_t *px, int sw, int32_t x, int32_t y)
{
    const uint32_t *p = px + (size_t)(y >> 16) * sw + (x >> 16);
    uint64_t fx = (x >> 8) & 0xff, fy = (y >> 8) & 0xff;
    uint64_t top = ((spread(p[0]) * (256 - fx) + spread(p[1]) * fx + HALF) >> 8) & LANES;
    uint64_t bot = ((spread(p[sw]) * (256 - fx) + spread(p[sw + 1]) * fx + HALF) >> 8) & LANES;
    return ((top * (256 - fy) + bot * fy + HALF) >> 8) & LANES;
}

static inline int32_t clampi(int32_t v, int32_t hi)
{
    return v < 0 ? 0 : v > hi ? hi : v;
}

#if defined(__aarch64__)
#include <arm_neon.h>
#define HAVE_SIMD 1

/* a + (b - a) * w / 256 for sixteen channels, rounded like sample() does. */
static inline uint8x16_t lerp16(uint8x16_t a, uint8x16_t b, uint8x16_t w)
{
    uint16x8_t lo = vmlaq_u16(vshll_n_u8(vget_low_u8(a), 8),
                              vsubl_u8(vget_low_u8(b), vget_low_u8(a)), vmovl_u8(vget_low_u8(w)));
    uint16x8_t hi = vmlaq_u16(vshll_high_n_u8(a, 8), vsubl_high_u8(b, a), vmovl_high_u8(w));
    return vrshrn_high_n_u16(vrshrn_n_u16(lo, 8), hi, 8);
}

/* The fractions (bits 8..15) of four coordinates, one per channel. */
static inline uint8x16_t fractions(int32_t s, int32_t d)
{
    static const int32_t steps[4] = { 0, 1, 2, 3 };
    int32x4_t v = vmlaq_n_s32(vdupq_n_s32(s), vld1q_s32(steps), d);
    uint32x4_t f = vandq_u32(vshrq_n_u32(vreinterpretq_u32_s32(v), 8), vdupq_n_u32(0xff));
    return vreinterpretq_u8_u32(vmulq_n_u32(f, 0x01010101u));
}

/* Four output pixels that read five consecutive pixels of two rows: this
 * is nearly always the case, because the field is smooth. */
static inline uint8x16_t sample4(const uint32_t *p, int sw, uint8x16_t wx, uint8x16_t wy)
{
    const uint8_t *t = (const uint8_t *)p, *b = (const uint8_t *)(p + sw);
    uint8x16_t top = lerp16(vld1q_u8(t), vld1q_u8(t + 4), wx);
    uint8x16_t bot = lerp16(vld1q_u8(b), vld1q_u8(b + 4), wx);
    return lerp16(top, bot, wy);
}
#endif

static inline uint32_t pixel(const job *j, int32_t sx, int32_t sy, int32_t maxx, int32_t maxy)
{
    int32_t ux = clampi(sx, maxx), uy = clampi(sy, maxy);
    if (j->nlayers == 1 && j->layers[0] && j->weights[0] == 256)
        return pack(sample(j->layers[0], j->sw, ux, uy));
    uint64_t acc = HALF;
    for (int l = 0; l < j->nlayers; l++) {
        uint64_t s = j->layers[l] ? sample(j->layers[l], j->sw, ux, uy) : spread(j->colors[l]);
        acc += s * j->weights[l];
    }
    return pack((acc >> 8) & LANES);
}

static void *band(void *arg)
{
    const job *j = arg;
    const int cell = j->cell, gw = j->gw;
    const int32_t maxx = ((j->sw - 1) << 16) - 1, maxy = ((j->sh - 1) << 16) - 1;
    const float inv = 1.0f / cell;
    const int plain = j->nlayers == 1 && j->layers[0] && j->weights[0] == 256;
    (void)plain;

    for (int y = j->y0; y < j->y1; y++) {
        const float *g0 = j->grid + (size_t)(y / cell) * gw * 2;
        const float *g1 = g0 + (size_t)gw * 2;
        const float fy = (y % cell) * inv;
        uint32_t *out = j->dst + (size_t)y * j->dw;

        for (int cx = 0, x = 0; x < j->dw; cx++) {
            /* the row crosses this cell from (lx, ly) to (rx, ry) */
            float lx = g0[2 * cx] + (g1[2 * cx] - g0[2 * cx]) * fy;
            float ly = g0[2 * cx + 1] + (g1[2 * cx + 1] - g0[2 * cx + 1]) * fy;
            float rx = g0[2 * cx + 2] + (g1[2 * cx + 2] - g0[2 * cx + 2]) * fy;
            float ry = g0[2 * cx + 3] + (g1[2 * cx + 3] - g0[2 * cx + 3]) * fy;
            int32_t sx = (int32_t)(lx * 65536.0f), sy = (int32_t)(ly * 65536.0f);
            int32_t dx = (int32_t)((rx - lx) * inv * 65536.0f), dy = (int32_t)((ry - ly) * inv * 65536.0f);
            int end = x + cell < j->dw ? x + cell : j->dw;

#ifdef HAVE_SIMD
            for (; x + 4 <= end; x += 4, sx += 4 * dx, sy += 4 * dy) {
                int32_t ex = sx + 3 * dx, ey = sy + 3 * dy;
                /* in range, one pixel to the right each time, on one row */
                if (sx < 0 || ex > maxx || sy < 0 || sy > maxy || ey < 0 || ey > maxy
                    || (ex >> 16) != (sx >> 16) + 3 || (ey >> 16) != (sy >> 16)) {
                    for (int k = 0; k < 4; k++)
                        out[x + k] = pixel(j, sx + k * dx, sy + k * dy, maxx, maxy);
                    continue;
                }
                size_t at = (size_t)(sy >> 16) * j->sw + (sx >> 16);
                uint8x16_t wx = fractions(sx, dx), wy = fractions(sy, dy), r;
                if (plain) {
                    r = sample4(j->layers[0] + at, j->sw, wx, wy);
                } else {
                    uint16x8_t lo = vdupq_n_u16(0), hi = vdupq_n_u16(0);
                    for (int l = 0; l < j->nlayers; l++) {
                        uint8x16_t s = j->layers[l] ? sample4(j->layers[l] + at, j->sw, wx, wy)
                                                    : vreinterpretq_u8_u32(vdupq_n_u32(j->colors[l]));
                        lo = vmlaq_n_u16(lo, vmovl_u8(vget_low_u8(s)), (uint16_t)j->weights[l]);
                        hi = vmlaq_n_u16(hi, vmovl_high_u8(s), (uint16_t)j->weights[l]);
                    }
                    r = vrshrn_high_n_u16(vrshrn_n_u16(lo, 8), hi, 8);
                }
                vst1q_u8((uint8_t *)(out + x), r);
            }
#endif
            for (; x < end; x++, sx += dx, sy += dy)
                out[x] = pixel(j, sx, sy, maxx, maxy);
        }
    }
    return NULL;
}

#define MAX_THREADS 16

/* Draw a frame of dw x dh pixels. The grid has gw x gh nodes, one every
 * `cell` pixels (so gw > dw / cell and gh > dh / cell). Layers are all
 * sw x sh pixels (at least 2 x 2); weights should add up to 256 or less
 * (less darkens the picture). */
void bgmonad_warp(uint32_t *dst, int dw, int dh, const float *grid, int gw, int cell,
                  const uint32_t *const *layers, const uint32_t *colors, const uint32_t *weights,
                  int nlayers, int sw, int sh, int threads)
{
    job jobs[MAX_THREADS];
    pthread_t tids[MAX_THREADS];
    int started[MAX_THREADS];
    if (threads < 1) threads = 1;
    if (threads > MAX_THREADS) threads = MAX_THREADS;
    if (threads > dh) threads = dh;
    for (int t = 0; t < threads; t++) {
        jobs[t] = (job){ dst, dw, dh, grid, gw, cell, layers, colors, weights, nlayers, sw, sh,
                         (int)((int64_t)dh * t / threads), (int)((int64_t)dh * (t + 1) / threads) };
        started[t] = t > 0 && pthread_create(&tids[t], NULL, band, &jobs[t]) == 0;
    }
    for (int t = 0; t < threads; t++)
        if (!started[t])
            band(&jobs[t]);
    for (int t = 1; t < threads; t++)
        if (started[t])
            pthread_join(tids[t], NULL);
}

/* The same without displacement or blending: copy the dw x dh rectangle
 * at (ox, oy) of a layer that is sw pixels wide. */
void bgmonad_blit(uint32_t *dst, int dw, int dh, const uint32_t *src, int sw, int ox, int oy)
{
    for (int y = 0; y < dh; y++)
        memcpy(dst + (size_t)y * dw, src + (size_t)(y + oy) * sw + ox, (size_t)dw * 4);
}

void bgmonad_fill(uint32_t *dst, size_t n, uint32_t color)
{
    for (size_t i = 0; i < n; i++)
        dst[i] = color;
}

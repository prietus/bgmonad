/* Loading wallpapers with gdk-pixbuf into XRGB8888 layers of exactly the
 * size the renderer wants, and blurring them. */
#include <gdk-pixbuf/gdk-pixbuf.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

enum { FIT_COVER = 0, FIT_CONTAIN = 1, FIT_STRETCH = 2, FIT_CENTER = 3 };

/* Load an image scaled to w x h pixels (see the Fit type), over the colour
 * bg (0xRRGGBB) where it is transparent or does not reach. unit is the size
 * of one image pixel in layer pixels for FIT_CENTER. Returns a malloc'ed
 * array of w*h pixels, or NULL with *err set to a malloc'ed message. */
uint32_t *bgmonad_load(const char *path, int w, int h, int fit, uint32_t bg, double unit, char **err)
{
    GError *gerr = NULL;
    GdkPixbuf *raw = gdk_pixbuf_new_from_file(path, &gerr);
    if (!raw) {
        *err = strdup(gerr ? gerr->message : "cannot load the image");
        g_clear_error(&gerr);
        return NULL;
    }
    GdkPixbuf *src = gdk_pixbuf_apply_embedded_orientation(raw);
    g_object_unref(raw);
    GdkPixbuf *dst = src ? gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE, 8, w, h) : NULL;
    uint32_t *out = dst ? malloc((size_t)w * h * 4) : NULL;
    if (!out) {
        *err = strdup("out of memory");
        if (src) g_object_unref(src);
        if (dst) g_object_unref(dst);
        return NULL;
    }
    gdk_pixbuf_fill(dst, (bg << 8) | 0xff);

    double iw = gdk_pixbuf_get_width(src), ih = gdk_pixbuf_get_height(src);
    double sx = w / iw, sy = h / ih;
    switch (fit) {
    case FIT_COVER:   sx = sy = sx > sy ? sx : sy; break;
    case FIT_CONTAIN: sx = sy = sx < sy ? sx : sy; break;
    case FIT_CENTER:  sx = sy = unit; break;
    default: break;
    }
    double ox = (w - iw * sx) / 2, oy = (h - ih * sy) / 2;
    /* the part of the layer the scaled image covers */
    int x0 = ox > 0 ? (int)ox : 0, y0 = oy > 0 ? (int)oy : 0;
    int x1 = (int)(ox + iw * sx + 0.999), y1 = (int)(oy + ih * sy + 0.999);
    if (x1 > w) x1 = w;
    if (y1 > h) y1 = h;
    if (x1 > x0 && y1 > y0)
        gdk_pixbuf_composite(src, dst, x0, y0, x1 - x0, y1 - y0, ox, oy, sx, sy,
                             (sx < 1 || sy < 1) ? GDK_INTERP_BILINEAR : GDK_INTERP_HYPER, 255);
    g_object_unref(src);

    const guchar *pix = gdk_pixbuf_read_pixels(dst);
    int stride = gdk_pixbuf_get_rowstride(dst);
    for (int y = 0; y < h; y++) {
        const guchar *p = pix + (size_t)y * stride;
        uint32_t *q = out + (size_t)y * w;
        for (int x = 0; x < w; x++, p += 3)
            q[x] = 0xff000000u | ((uint32_t)p[0] << 16) | ((uint32_t)p[1] << 8) | p[2];
    }
    g_object_unref(dst);
    return out;
}

/* One box blur pass along rows (in -> out), clamping at the edges. */
static void box_h(const uint32_t *in, uint32_t *out, int w, int h, int r)
{
    uint32_t inv = (1u << 24) / (2 * r + 1) + 1;
    for (int y = 0; y < h; y++) {
        const uint8_t *s = (const uint8_t *)(in + (size_t)y * w);
        uint8_t *d = (uint8_t *)(out + (size_t)y * w);
        uint32_t acc[3];
        for (int c = 0; c < 3; c++) {
            acc[c] = (r + 1) * s[c];
            for (int x = 1; x <= r; x++)
                acc[c] += s[4 * (x < w ? x : w - 1) + c];
        }
        for (int x = 0; x < w; x++) {
            int add = x + r + 1 < w ? x + r + 1 : w - 1;
            int sub = x - r > 0 ? x - r : 0;
            for (int c = 0; c < 3; c++) {
                d[4 * x + c] = (uint8_t)(((uint64_t)acc[c] * inv) >> 24);
                acc[c] += s[4 * add + c] - s[4 * sub + c];
            }
            d[4 * x + 3] = 0xff;
        }
    }
}

/* One box blur pass along columns, a whole row at a time. */
static int box_v(const uint32_t *in, uint32_t *out, int w, int h, int r)
{
    uint32_t inv = (1u << 24) / (2 * r + 1) + 1;
    size_t n = (size_t)w * 4;
    uint32_t *acc = malloc(n * sizeof *acc);
    if (!acc)
        return -1;
    const uint8_t *s = (const uint8_t *)in;
#define ROW(y) (s + (size_t)((y) < 0 ? 0 : (y) >= h ? h - 1 : (y)) * n)
    for (size_t i = 0; i < n; i++)
        acc[i] = (r + 1) * ROW(0)[i];
    for (int y = 1; y <= r; y++) {
        const uint8_t *p = ROW(y);
        for (size_t i = 0; i < n; i++)
            acc[i] += p[i];
    }
    for (int y = 0; y < h; y++) {
        uint8_t *d = (uint8_t *)out + (size_t)y * n;
        const uint8_t *add = ROW(y + r + 1), *sub = ROW(y - r);
        for (size_t i = 0; i < n; i++) {
            d[i] = (uint8_t)(((uint64_t)acc[i] * inv) >> 24);
            acc[i] += add[i] - sub[i];
        }
    }
#undef ROW
    free(acc);
    return 0;
}

/* A blurred copy of a layer: three box passes in each direction, which is
 * very close to a gaussian of the given radius. NULL if out of memory. */
uint32_t *bgmonad_blur(const uint32_t *src, int w, int h, int radius)
{
    size_t bytes = (size_t)w * h * 4;
    uint32_t *a = malloc(bytes), *b = malloc(bytes);
    if (!a || !b) {
        free(a);
        free(b);
        return NULL;
    }
    int r = radius / 2;
    if (r < 1) {
        memcpy(a, src, bytes);
        free(b);
        return a;
    }
    if (r > w / 2) r = w / 2;
    if (r > h / 2) r = h / 2;
    const uint32_t *cur = src;
    for (int i = 0; i < 3; i++) {
        box_h(cur, b, w, h, r);
        if (box_v(b, a, w, h, r) < 0) {
            free(a);
            free(b);
            return NULL;
        }
        cur = a;
    }
    free(b);
    return a;
}

void bgmonad_free(void *p)
{
    free(p);
}

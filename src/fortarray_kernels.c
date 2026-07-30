#include <stdint.h>

#if defined(_MSC_VER)
#define FORTARRAY_RESTRICT __restrict
#else
#define FORTARRAY_RESTRICT restrict
#endif

void fortarray_add_r64(
    int64_t count,
    const double *left,
    const double *right,
    double *FORTARRAY_RESTRICT output)
{
    int64_t i;

    for (i = 0; i < count; ++i) {
        output[i] = left[i] + right[i];
    }
}

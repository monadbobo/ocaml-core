/* Common binary protocol definitions */

#ifndef COMMON_STUBS_H
#define COMMON_STUBS_H

#include <string.h>
#include <arpa/inet.h>

#include <caml/config.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/signals.h>


/* Portable byte swapping */

#ifndef bswap_16
#define bswap_16(value) \
  (((uint16_t) ((value) & 0xff) << 8) | ((uint16_t) (value) >> 8))
#endif

#ifndef bswap_32
#define bswap_32(value) \
  (((uint32_t) bswap_16(((value) & 0xffff)) << 16) | \
    (uint32_t) bswap_16(((value) >> 16)))
#endif

#ifndef bswap_64
#define bswap_64(value) \
  (((uint64_t) bswap_32(((value) & 0xffffffff)) << 32) | \
    (uint64_t) bswap_32(((value) >> 32)))
#endif


/* Bin-prot integer codes */

#define CODE_NEG_INT8 (char) -1
#define CODE_INT16 (char) -2
#define CODE_INT32 (char) -3
#define CODE_INT64 (char) -4


/* Buffer short exception */

extern value *v_bin_prot_exc_Buffer_short;


/* GNU compiler pragmas */

#if __GNUC__ >= 3
# ifndef inline
#   define inline inline __attribute__ ((always_inline))
# endif
# ifndef __pure
#   define __pure __attribute__ ((pure))
# endif
# ifndef __const
#   define __const __attribute__ ((const))
# endif
# ifndef __malloc
#   define __malloc __attribute__ ((malloc))
# endif
# ifndef __unused
#   define __unused __attribute__ ((unused))
# endif
# ifndef __likely
#   define likely(x) __builtin_expect (!!(x), 1)
# endif
# ifndef __unlikely
#   define unlikely(x) __builtin_expect (!!(x), 0)
# endif
#else
# ifndef inline
#   define inline
# endif
# ifndef __pure
#   define __pure
# endif
# ifndef  __const
#   define __const
# endif
# ifndef  __malloc
#   define __malloc
# endif
# ifndef  __unused
#   define __unused
# endif
# ifndef  __likely
#   define likely(x) (x)
# endif
# ifndef  __unlikely
#   define unlikely(x) (x)
# endif
#endif

#endif /* COMMON_STUBS_H */

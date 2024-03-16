#include <byteswap.h>
#include <time.h>

#define SUCCESS 0

int timer() {
    // Do nothing
}

long platformLongToBigEndian(long value) {
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    return bswap_64(value);
#else
    return value;
#endif
}

int getCurrentTimeMs(long* timeMs) {    
    struct timespec time;
        
    clock_gettime(CLOCK_MONOTONIC, &time);
    
    *timeMs = platformLongToBigEndian((long) (time.tv_sec * 1000) + (time.tv_nsec / 10000000));
    return SUCCESS;
}

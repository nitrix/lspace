#ifndef SIZE_H
#define SIZE_H

#include <stdint.h>

/* Type big enough to contain the size of the world in chunk units */
typedef int32_t TYPE_SIZE_WORLD;

/* Type big enough to contain the size of a chunk in cell units */
typedef uint8_t TYPE_SIZE_CHUNK;
#define SIZE_CHUNK 16

/* Type big enough to contain the size of a hashmap */
typedef uint8_t TYPE_SIZE_HASHMAP;

#endif /* SIZE_H */

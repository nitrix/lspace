CMAKE_MINIMUM_REQUIRED(VERSION 2.6)
PROJECT(client)

ADD_DEFINITIONS(
    -std=c89
    -pedantic
    -Wall
    -Wextra
    -Wno-attributes
    -Wno-unused-variable
    -Wno-unused-function
)

INCLUDE_DIRECTORIES(
    ../common/lib/SDL/include
    ../common/lib/SDL_image
    ../common/include
    include
)

ADD_EXECUTABLE(
    client
    src/camera.c
    src/cell.c
    src/coord.c
    src/gfx.c
    src/hashmap.c
    src/main.c    
    src/renderer.c
    src/world.c
)

TARGET_LINK_LIBRARIES(
    client
    #common
    SDL2-static
    SDL2_image
)

ADD_SUBDIRECTORY(tests)

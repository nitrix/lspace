CMAKE_MINIMUM_REQUIRED(VERSION 2.6)
PROJECT(client)

#INCLUDE(FindPkgConfig)
#PKG_SEARCH_MODULE(SDL2 REQUIRED sdl2)

INCLUDE_DIRECTORIES(
    include
    #common
    #${SDL2_INCLUDE_DIRS}
)

ADD_EXECUTABLE(
    client
    src/main.c    
    src/renderer.c
)

TARGET_LINK_LIBRARIES(
    client
    #common
    #${SDL2_LIBRARIES}
    SDL2
    GL
)

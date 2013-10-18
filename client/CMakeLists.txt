CMAKE_MINIMUM_REQUIRED(VERSION 2.6)
PROJECT(client)

INCLUDE_DIRECTORIES(
    include
    #common
)

ADD_EXECUTABLE(
    client
    src/main.c    
    src/renderer.c
)

TARGET_LINK_LIBRARIES(
    client
    #common
    SDL2
    SDL2_image
)

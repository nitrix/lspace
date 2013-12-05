CMAKE_MINIMUM_REQUIRED(VERSION 2.6)
PROJECT(launcher)

ADD_DEFINITIONS(
    -O3
)

INCLUDE_DIRECTORIES(
    include
)

ADD_EXECUTABLE(
    launcher
    src/main.c
    src/list.c
)

TARGET_LINK_LIBRARIES(
    launcher
    SDL2
    SDL2_ttf
)

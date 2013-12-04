CMAKE_MINIMUM_REQUIRED(VERSION 2.6)
PROJECT(launcher)

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
    enet
    SDL2
)

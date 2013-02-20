CMAKE_MINIMUM_REQUIRED(VERSION 2.6)
PROJECT(client)

INCLUDE_DIRECTORIES(
    "../common/include"
    "include"
)

ADD_EXECUTABLE(
    client
    src/main.c
    src/renderer.c
    src/window.c
)

TARGET_LINK_LIBRARIES(
    client
    common    
)

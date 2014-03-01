CMAKE_MINIMUM_REQUIRED(VERSION 2.6)
PROJECT(client)

INCLUDE_DIRECTORIES(
    ../common/include
    include
)

ADD_EXECUTABLE(
    client
    src/main.cpp
    src/window.cpp
)

TARGET_LINK_LIBRARIES(
    client
    common
    sfml-system
    sfml-window
    sfml-graphics
)

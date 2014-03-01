CMAKE_MINIMUM_REQUIRED(VERSION 2.6)
PROJECT(server)

INCLUDE_DIRECTORIES(
    ../common/include
    include
)

ADD_EXECUTABLE(
    server
    src/main.c
)

TARGET_LINK_LIBRARIES(
    server
    common
)

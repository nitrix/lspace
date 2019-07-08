CMAKE_MINIMUM_REQUIRED(VERSION 3.0)
PROJECT(lspace)

ADD_SUBDIRECTORY(lib/SDL)

ADD_DEFINITIONS(
    -std=c11
    -pedantic
    -Wall
    -Werror
)

ADD_EXECUTABLE(lspace src/main.c)

TARGET_INCLUDE_DIRECTORIES(
    lspace
    PRIVATE
    include
    lib/SDL/include
)

IF (WIN32 OR MINGW)
    TARGET_LINK_LIBRARIES(
        lspace
        mingw32
        SDL2main
    )
ENDIF()

TARGET_LINK_LIBRARIES(
    lspace
    SDL2-static
)
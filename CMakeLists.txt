CMAKE_MINIMUM_REQUIRED(VERSION 3.0)
PROJECT(lspace)

ADD_SUBDIRECTORY(lib/SDL)
ADD_SUBDIRECTORY(lib/toolkit)

ADD_EXECUTABLE(lspace src/main.c)
TARGET_INCLUDE_DIRECTORIES(lspace PRIVATE include)

TARGET_COMPILE_OPTIONS(
    lspace PRIVATE

    # Self-contained executable.
    -static

    # Ensure compliance with the C11 standard.
    -std=c11
    -pedantic
    -Wpedantic

    # Enable as many warnings as possible.
    -Wall
    -Wextra
)

IF (MINGW)
    TARGET_LINK_LIBRARIES(lspace PRIVATE mingw32)
ENDIF()

IF (WIN32)
    TARGET_LINK_LIBRARIES(lspace PRIVATE SDL2main)
ENDIF()

TARGET_LINK_LIBRARIES(lspace PRIVATE toolkit SDL2-static)

IF(CMAKE_BUILD_TYPE MATCHES Release)
    TARGET_LINK_OPTIONS(lspace PRIVATE -flto -O3)
ENDIF()

IF(CMAKE_BUILD_TYPE MATCHES Debug)
    TARGET_LINK_OPTIONS(lspace PRIVATE -Werror)
ENDIF()
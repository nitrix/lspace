CMAKE_MINIMUM_REQUIRED(VERSION 2.6)
PROJECT(common)

INCLUDE_DIRECTORIES(
    "include"
)

ADD_LIBRARY(
    common STATIC
    src/entity.c
)

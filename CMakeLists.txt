CMAKE_MINIMUM_REQUIRED(VERSION 2.6)
PROJECT(lspace)

INCLUDE(CTest)
ENABLE_TESTING()

# Optimise and strip binaries
IF(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
SET(CMAKE_C_FLAGS "-O2 -s")
ENDIF()

# ------------------- COMMON --------------------
ADD_SUBDIRECTORY(common)

# ------------------- CLIENT --------------------
ADD_SUBDIRECTORY(client)
ADD_SUBDIRECTORY(launcher)

# ------------------- SERVER --------------------
#ADD_SUBDIRECTORY(server)

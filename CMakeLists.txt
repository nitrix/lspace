CMAKE_MINIMUM_REQUIRED(VERSION 2.6)
PROJECT(lspace)

ADD_DEFINITIONS(
    -std=c99                 # Only /partial/ C99 support.
    -O2                      # Optimize for performance.
    -Wall                    # Misnamed, enables a list of warning options, but not all of them.
    -Wextra                  # Print extra warning information.
    -Werror                  # Warnings are errors.
    -Wmissing-include-dirs   # Warn if a user-supplied include directory does not exist.
    -Wunused-parameter       # Warn whenever a function parameter is unused aside from its declaration, unless the "unused" attribute is used.
    -Wmissing-prototypes     # Warn for missing prototypes
    -Wmissing-declarations   # Global functions defined without a previous prototype
    -Wstrict-prototypes      # Warn if a function is declared or defined w/o specifying the argument types.
    -Wold-style-definition   # Warn if an old-style function definition is used, even with a valid prototype.
    -Wmissing-noreturn       # Warn for functions with no returns.
    -Winit-self              # Warn about uninitialized variables that are initialized with themselves.
    -Wunreachable-code       # Warn about unreachable code
    -Wpointer-arith          # Warn about anything that depends on the size of a function type or a "void".
)

ADD_SUBDIRECTORY(common)
ADD_SUBDIRECTORY(client)
ADD_SUBDIRECTORY(server)

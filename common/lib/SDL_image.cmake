CMAKE_MINIMUM_REQUIRED(VERSION 2.6)
PROJECT(SDL2_image)

ADD_DEFINITIONS(
    -Wno-attributes
    -DLOAD_PNG
    -DLOAD_BMP
    -DLOAD_GIF
    -DLOAD_JPG
    -DLOAD_LBM
    -DLOAD_PCX
    -DLOAD_PNG
    -DLOAD_PNM
    -DLOAD_TGA
    -DLOAD_TIF
    -DLOAD_WEBP
    -DLOAD_XCF
    -DLOAD_XPM
    -DLOAD_XV
    -DLOAD_XXX
)

ADD_LIBRARY(
    SDL2_image
    SDL_image/IMG.c
    SDL_image/IMG_bmp.c
    SDL_image/IMG_gif.c
    SDL_image/IMG_jpg.c
    SDL_image/IMG_lbm.c
    SDL_image/IMG_pcx.c
    SDL_image/IMG_png.c
    SDL_image/IMG_pnm.c
    SDL_image/IMG_tga.c
    SDL_image/IMG_tif.c
    SDL_image/IMG_webp.c
    SDL_image/IMG_xcf.c
    SDL_image/IMG_xpm.c
    SDL_image/IMG_xv.c
    SDL_image/IMG_xxx.c
)

TARGET_LINK_LIBRARIES(
    SDL2_image
    png
    jpeg
    tiff
    webp
)

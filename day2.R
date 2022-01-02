
library(imager)


dither_image <- function(im, pre_blur = 0,
                         noise_mean = 0.5, noise_sd = 0.1, noise_blur = 1,
                         dim_x = dim(im)[1], dim_y = dim(im)[2]) {
  if(dim(im)[4] != 1) {
    im <- grayscale(im)
  }
  if (dim_x != dim(im)[1] || dim_y != dim(im)[2]) {
    im_scaled <- resize(im, dim_x, dim_y, interpolation_type = 5)
  } else {
    im_scaled <- im
  }
  im_blur <- isoblur(im_scaled, pre_blur)
  im_noise <- isoblur(imnoise(dim_x, dim_y,
                              mean = noise_mean, sd = noise_sd),
                      sigma = noise_blur)
  as.cimg(imeval(im_blur, ~ . > im_noise))
}

im <- load.image("day2_youyangs1.jpg")

set.seed(2022)
z <- dither_image(im, 0.2, 0.55, 0.1, 0.2, 640, 320)
zz <- imresize(z, scale = 8, interpolation = 1)

plot(zz)
save.image(zz, "day2.png")


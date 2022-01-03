
library(imager)

set.seed(2022)

# add high frequency noise and low frequency noise
# warp LF noise to give perspective
lfnoise <- imnoise(64, 18, mean = 0.53, sd = 0.03)
lfnoise_large <- resize(lfnoise, 640, 180, interpolation_type = 5)
#plot(lfnoise_large)

hfnoise <- imnoise(640, 180, mean = 1, sd = 0.15)
#plot(hfnoise)

combined_noise <- cimg(lfnoise_large * hfnoise)
combined_noise_large <- resize(combined_noise, 6400, 1800, interpolation_type = 5)
#plot(combined_noise_large)

cnl_warped <- imwarp(
  combined_noise_large,
  function(x, y) list(x = (1 - 0.4/1800*y)*x + (0.2*6400/1800*y),
                      y = y),
  direction = "backward",
  interpolation = "cubic"
)
#plot(cnl_warped)

cnl_hsv <- RGBtoHSL(add.colour(cnl_warped, FALSE))
cnl_hsv[, , 1] <- 48
cnl_hsv[, , 2] <- 0.75
cnl_col <- HSLtoRGB(cnl_hsv)
#plot(cnl_col)

sky_gradient <- resize(HSLtoRGB(cimg(
  array(
    c(seq(222, 222, length.out = 1800),
      seq(0.72, 0.65, length.out = 1800),
      seq(0.49, 0.91, length.out = 1800)),
    dim = c(1, 1800, 1, 3)
  )
)), 6400, 1800, interpolation_type = 5)
#plot(sky_gradient)

img <- imappend(list(sky_gradient, cnl_col), axis = "y")
#plot(img)

save.image(img, "day3.png")

# Genuary 2022 day 1: draw 10,000 of something
# Cameron Patrick <cameron@largestprime.net>

library(tidyverse)

set.seed(2022)

hex_points <- tribble(
  ~x, ~y,
  0, -1,
  sqrt(3)/2, -0.5,
  sqrt(3)/2, 0.5,
  0, 1,
  -sqrt(3)/2, 0.5,
  -sqrt(3)/2, -0.5,
)

giant_hex_scale <- 60
aspect <- 1.25
density <- 16 / 9

n_points <- giant_hex_scale^2 * aspect^2 * density

giant_hex <- mutate(
  hex_points,
  x = x * giant_hex_scale,
  y = y * giant_hex_scale
)

hexes <- 
  tibble(
    cx = runif(n_points, -giant_hex_scale, giant_hex_scale),
    cy = runif(n_points, -aspect * giant_hex_scale, aspect * giant_hex_scale),
    id = 1:n_points
  ) %>%
  expand_grid(hex_points) %>%
  mutate(x = x + cx,
         y = y + cy)

ggplot(hexes, aes(x = x, y = y, group = id, fill = id)) +
  geom_polygon(alpha = 0.5, colour = NA) +
  geom_polygon(data = giant_hex,
               aes(x = x, y = y),
               inherit.aes = FALSE,
               alpha = 0.8,
               fill = "black") +
  coord_fixed() +
  scale_fill_distiller(palette = "PuOr") +
  theme_void() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "black"))

ggsave("day1.png", width = 8, height = 10, units = "in", dpi = 600)

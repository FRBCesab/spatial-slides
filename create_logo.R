library(tmap)
library(sf)
data(World) # The dataset is stored as an sf object
st_crs(World)
#fmt: skip
World2 <- World[!World$continent %in% c("Seven seas (open ocean)", "Antarctica"),]

tm_shape(World) + tm_fill()

# Bonne projection
World_54024 <- st_transform(World, crs = "ESRI:54024")
tm_shape(World_54024) + tm_fill()

#Lambert Equalarea
World_3035 <- st_transform(World, crs = "EPSG:3035")
tm_shape(World_3035) + tm_fill()

#Sinusoidal
World_54008 <- st_transform(World, crs = "ESRI:54008")
tm_shape(World_54008) + tm_graticules() + tm_fill()

# Polyconic
World_54021 <- st_transform(World, crs = "ESRI:54021")
tm_shape(World_54021) + tm_graticules() + tm_fill()

# Peirce quincuncial North Pole in a square - ESRI:54091
World_54090 <- st_transform(World2, crs = "ESRI:54090")
tm_shape(World_54090) + tm_graticules(n.x = 20) + tm_fill()
# in a diamond
World_54091 <- st_transform(World2, crs = "ESRI:54091")
tm_shape(World_54091) + tm_graticules(n.x = 20) + tm_fill()

# 2154 / 3812
World_2154 <- st_transform(World2, crs = "EPSG:2154")
tm <- tm_shape(World_2154) +
  tm_graticules(
    n.x = 20,
    n.y = 20,
    col = "grey",
    lwd = 0.5,
    labels.show = FALSE
  ) +
  tm_fill("grey30") +
  tm_logo("img/Rlogo.png", position = c("right", "bottom")) +
  tm_layout(legend.outside = TRUE, frame = FALSE, inner.margins = 0)
tm
tmap_save(tm, "img/spatialr_logo02.png", width = 1000, height = 1000, dpi = 300)

# Eckert II
World_54014 <- st_transform(World, crs = "ESRI:54014")
tm <- tm_shape(World_54014) +
  tm_graticules(
    n.x = 20,
    n.y = 20,
    col = "grey",
    lwd = 0.5,
    labels.show = FALSE
  ) +
  tm_fill("grey30") +
  tm_logo("img/Rlogo.png", position = c("center", "bottom"), height = 2) +
  tm_layout(legend.outside = TRUE, frame = FALSE, inner.margins = 0)
tm
tmap_save(tm, "img/spatialr_logo01.png", width = 1000, height = 750, dpi = 300)

# install.packages("hexSticker")
library(hexSticker)

tm <- tm_shape(World_54090) +
  tm_graticules(
    n.x = 20,
    n.y = 20,
    col = "grey",
    lwd = 0.5,
    labels.show = FALSE
  ) +
  tm_fill("grey30") +
  tm_logo("img/Rlogo.png", position = c(0.44, 0.43), height = 2) +
  tm_layout(legend.outside = TRUE, frame = FALSE, inner.margins = 0)

library(ggplot2)


s <- sticker(
  tm,
  package = "spatial-r",
  p_size = 0,
  s_x = 1,
  s_y = 1,
  s_width = 3,
  s_height = 3,
  h_size = 1,
  h_fill = "white",
  h_color = "black",
  filename = "img/spatialr_logo03.png",
  white_around_sticker = TRUE
)
theme_sticker(theme(plot.margin = margin(b = -.2, l = -.2, unit = "lines")))
imgurl <- system.file("figures/cat.png", package = "hexSticker")
sticker(
  imgurl,
  package = "hexSticker",
  p_size = 20,
  s_x = 1,
  s_y = .75,
  s_width = .6,
  filename = "inst/figures/imgfile.jpg",
)

theme_sticker <- function(size = 1.2, ...) {
  center <- 1
  radius <- 1
  h <- radius
  w <- sqrt(3) / 2 * radius
  m <- 1.05
  list(
    theme_transparent() +
      theme(
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "lines"),
        strip.text = element_blank(),
        line = element_blank(),
        text = element_blank(),
        title = element_blank(),
        ...
      ),
    coord_fixed(),
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(center - h * m, center + h * m)
    ),
    scale_x_continuous(
      expand = c(0, 0),
      limits = c(center - w * m, center + w * m)
    )
  )
}

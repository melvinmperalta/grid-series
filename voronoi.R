library(tidyverse)
library(ggplot2)
library(purrr)
library(ambient)
library(sf)
library(deldir)
library(colorspace)

#initialize grid with DC points
load(file.choose())

min_lon <- -77.120498; max_lon <- -76.909698
min_lat <- 38.802760; max_lat <- 38.996739

dc_cropped <- st_crop(dc_points, xmin = min_lon, xmax = max_lon, ymin = min_lat, ymax = max_lat)
dt <- st_coordinates(dc_cropped) %>% as_tibble()
dt <- dt %>% rename(x = X, y = Y)
dt$x <- normalise(dt$x, to = c(1, 50))
dt$y <- normalise(dt$y, to = c(1, 50))

ggplot() +
  geom_point(data = dt, aes(x = x, y = y)) + 
  coord_equal()

#voronoi tessalation
tessalation <- deldir(dt$x, dt$y)
tiles <- tile.list(tessalation)

#remove border points
tiles_nb <- Filter(function(x) !(TRUE %in% x$bp), tiles)

#extract polygon coordinates
coord <- tiles_nb %>% map(~ data.frame(x = .x$x, 
                                       y = .x$y, 
                                       id = .x$ptNum,
                                       v = sample(1:6, 1)))
df <- do.call("rbind", coord)

#create color gradient
cPalette <- c("#ffffff", "#a02040", "#b34c66", "#d9a5b2", "#ecd2d8", "#f5e8eb")

ggplot() +
  geom_polygon(data = df, aes(x = x, y = y, group = id, fill = v),
               colour = "white", size = 0.1) + 
  scale_fill_gradientn(colours = cPalette) +
  coord_equal() +
  theme_void() +
  theme(legend.position = "none")

library(tidyverse)
library(ggplot2)
library(purrr)
library(Rcpp)
library(deldir)

# Import C++ code
sourceCpp('select_points.cpp')

#initialize grid and function
xdim <- 100
ydim <- 100
data <- list(x = seq(-1.8, 1.8, length.out = xdim), y = seq(-1.8, 1.8, length.out = ydim))
dt <- data %>% cross_df() %>% mutate(
  x = x + rnorm(xdim*ydim, 0, 0.1),
  y = y + rnorm(xdim*ydim, 0, 0.1)
)

func <- data.frame(t = seq(0, 2*pi, length.out = 400)) %>% mutate(
  x = cos(t) + cos(6*t)/2 + sin(14*t)/3, 
  y = sin(t) + sin(6*t)/2 + cos(14*t)/3
) %>% select(2:3)

#voronoi tessalation
tessalation <- deldir(dt$x, dt$y)
tiles <- tile.list(tessalation)

#filter points
s <- design(dt, func)
tiles_f <- Filter(function(i) { i$ptNum %in% s }, tiles)

#extract polygon coordinates
coord <- tiles_f %>% map(~ data.frame(x = .x$x, 
                                       y = .x$y, 
                                       id = .x$ptNum))

df <- do.call("rbind", coord)

#create color gradient
cPalette <- c("#ffffff", "#fff0f5", "#ffd7e4", "#ffbdd3", "#ffa4c2")

ggplot() +
  geom_polygon(data = df, aes(x = x, y = y, group = id, fill = id),
               color = "black", size = 2) + 
  scale_fill_gradientn(colours = cPalette) +
  coord_equal() +
  theme_void() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "black"))

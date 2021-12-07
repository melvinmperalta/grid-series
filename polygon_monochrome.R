library(tidyverse)
library(ggplot2)
library(purrr)
library(ambient)

#create grid of points
ratio <- 1.2
data <- list(
  x = seq(0, 10*ratio, by = ratio),
  y = seq(0, 10)
)

#create polygon functions
poly <- function(l) {
  xmin = l[[1]]
  ymin = l[[2]]
  r = normalize(ymin, from = range(data[[2]]), to = c(0, 0.13))
  dt <- data.frame(
    x = c(xmin + rnorm(1, 0, r), xmin + ratio + rnorm(1, 0, r), 
          xmin + ratio + rnorm(1, 0, r), xmin + rnorm(1, 0, r)),
    y = c(ymin + rnorm(1, 0, r), ymin + rnorm(1, 0, r), 
          ymin + 1 + rnorm(1, 0, r), ymin + 1 + rnorm(1, 0, r)),
    v = sample(1:5, 1)
  )
  return(dt)
}

poly2 <- function (l) {
  xmin = l[[1]] + 0.33
  ymin = l[[2]] + 0.33
  r = 0.01
  dt <- data.frame(
    x = c(xmin + rnorm(1, 0, r), xmin + ratio*0.4 + rnorm(1, 0, r), 
          xmin + ratio*0.4 + rnorm(1, 0, r), xmin + rnorm(1, 0, r)),
    y = c(ymin + rnorm(1, 0, r), ymin + rnorm(1, 0, r), 
          ymin + 0.4 + rnorm(1, 0, r), ymin + 0.4 + rnorm(1, 0, r)),
    v = sample(1:5, 1)
  )
  return(dt)
}

#create cross, map poly across cross
df <- do.call("rbind", data %>% cross() %>% map(poly))
df$g <- rep(1:(nrow(df)/4), each = 4)
df2 <- do.call("rbind", data %>% cross() %>% map(poly2))
df2$g <- rep(1:(nrow(df)/4), each = 4)

#create color palette
cpalette <- c("#A58105", "#BD9B15", "#DDB639", "#FFDF01", "#FFCC01")

#plot rectangles
ggplot() +
  geom_polygon(data = df, aes(x = x, y = y, fill = v, group = g),
               color = "black", size = 1.2) +
  geom_polygon(data = df2, aes(x = x, y = y, fill = v, group = g),
               color = "black", size = 0.1) +
  scale_fill_gradientn(colors = cpalette) + coord_equal() +theme_void() +
  theme(legend.position = "none")

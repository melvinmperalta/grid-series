library(tidyverse)
library(ggplot2)
library(ggforce)
library(emojifont)
library(ggimage)
library(purrr)
library(ambient)
library(deldir)
library(poissoned)

#create grid
grid <- data.frame(x = runif(500, 0, 10),
                   y = runif(500, 0, 10))

#voronoi tessalation
tessalation <- deldir(grid$x, grid$y)
tiles <- tile.list(tessalation)

#remove borders
tiles <- Filter(function(x) !(TRUE %in% x$bp), tiles)

#get centroids
centroids <- tile.centroids(tiles)
centroids_list <- split(centroids, seq(nrow(centroids)))

#inner circle radius function
#adapted from George Francis: https://georgefrancis.dev/writing/crafting-organic-patterns-with-voronoi-tessellations/
dist2 <- function(v, w) {
 return((v$x - w$x)^2 + (v$y - w$y)^2)
}

distToSegment <- function(p, v, w) {
  l2 = dist2(v, w)
  if (l2 == 0) { return(sqrt(dist2(p, v))) }
  t = ((p$x - v$x)*(w$x - v$x) + (p$y - v$y)*(w$y - v$y))/l2
  t = max(0, min(1, t))
  q = data.frame(x = v$x + t*(w$x - v$x),
                 y = v$y + t*(w$y - v$y))
  return(sqrt(dist2(p, q)))
}

innerCircleRadius <- function(center, vertices) {
  dtCenter = data.frame(x = center$x,
                         y = center$y)
  dtVertices = data.frame(x = vertices$x,
                           y = vertices$y)
  dtVertices[nrow(dtVertices) + 1, ] <- c(dtVertices$x[1], dtVertices$y[1])
  closest = distToSegment(dtCenter, dtVertices[1, ], dtVertices[2, ])
  for(i in 2:(nrow(dtVertices)-1)) {
    if(!is.na(dtVertices[i+1, ]$x)) {
      dist = distToSegment(dtCenter, dtVertices[i, ], dtVertices[i+1, ])
      if(dist < closest) { closest = dist }
    }
  }
  return(closest)
}

#map innerCircleRadius function across centroids and tiles
rVector <- map2_dbl(centroids_list, tiles, innerCircleRadius)

#create data frame of centroids and radii
df <- centroids %>% mutate(
  r = round(rVector, 2)
)

#emojis and colors
search_emoji("cat")
emoji_list <- c(
  emoji("cat2"),
  emoji("scream_cat"),
  emoji("smirk_cat")
)
df$emoji <- sample(emoji_list, nrow(df), replace = TRUE)

color_list <- c(
  "darkgoldenrod3",
  "black",
  "darkgrey"
)
df$color <- sample(color_list, nrow(df), replace = TRUE)

#pokemon
df$poke <- sample(
  c("squirtle", "charmander", "bulbasaur", "pikachu", "mew"), 
  nrow(df),
  replace = TRUE
)

#create texture
#courtest of Will Chase: https://www.williamrchase.com/post/textures-and-geometric-shapes-12-months-of-art-july/
pts <- poisson_disc(ncols = 100, nrows = 100, cell_size = 1, xinit = 50, yinit = 0, keep_idx = TRUE) %>%
  arrange(idx)
cPalette <- c("seagreen4", "springgreen4", "palegreen3", "olivedrab3", "green4", "darkgreen")
tiff("grass.tiff", height = 1000, width = 1000, units = "px", pointsize = 12, res = 300, compression = 'lzw')
ggplot() +
  geom_point(data = pts, aes(x = x, y = y, color = idx), size = 5, alpha = 0.6) +
  scale_color_gradientn(colors = cPalette) +
  coord_equal() +
  theme_void() +
  theme(legend.position = "none")
dev.off()

#plot cats
tiff("cats.tiff", height = 1000, width = 1000, units = "px", pointsize = 12, res = 300, compression = 'lzw')
ggplot() +
  geom_text(data = df, 
            family = "EmojiOne",
            aes(x = x, y = y, size = 1.3*r, label = emoji, color = color)) +
  scale_size_identity() +
  scale_color_identity() +
  theme_void()
dev.off()

#plot pokemon
tiff("pokemon.tiff", height = 1000, width = 1000, units = "px", pointsize = 12, res = 300, compression = 'lzw')
ggplot(data = df, aes(x = x, y = y)) +
  geom_pokemon(aes(image = poke, size = r/6)) +
  scale_size_identity() +
  coord_equal() +
  theme_void()
dev.off()

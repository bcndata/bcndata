library(rgdal)
library(dplyr)
library(rgeos)
library(extrafont)
# font_import()
loadfonts(device="postscript")
fonts()

# Directories
root <- getwd()

# Read map
mapa <- readOGR('data/geo/barris', 'BCN_Barri_SHP')

plot(mapa, col = cols)
mapa2 <- gSimplify(mapa, tol = 300)

tri <- c('red', 'yellow', 'darkblue')
tri <- adjustcolor(tri, alpha.f = 0.6)
cols <- sample(tri, nrow(mapa), replace = TRUE)


# plot(mapa2, col = cols, border = FALSE, family = 'Gentium')
# 
# text(mean(coordinates(mapa2)[,1]), 
#      mean(coordinates(mapa2)[,2]),
#      'BCN\nDATA', cex = 8, 
#      col = adjustcolor('black', alpha.f = 1),
#      family = 'Gentium')
# 
# my_fonts <- fonts()

good_fonts <- 'Sawasdee'
par(mar = c(0,0,0,0))
for (i in 1:length(good_fonts)){
  try({
    plot(mapa2, 
         col = cols, 
         border = FALSE, bg = adjustcolor('white', alpha.f = 0.8))
    
    text(quantile(coordinates(mapa2)[,1], 0.3), 
         quantile(coordinates(mapa2)[,2], 0.35),
         'BCN\nDATA', cex = 12, 
         col = adjustcolor('black', alpha.f = 1),
         family = good_fonts[i])
    print(i)
    # title(main = good_fonts[i])
  })
}



plot(mapa)
for (i in seq(0.4, 3, 0.6)){
  points(coordinates(mapa), cex = i, 
         col = adjustcolor('black', alpha.f = 0.6))
}


districtes <- readOGR('data/geo/districtes', 'BCN_Districte_SHP')
plot(districtes)
for (i in seq(1, 60, 5)){
  points(coordinates(districtes), cex = i, 
         col = adjustcolor('black', alpha.f = 0.3))
}

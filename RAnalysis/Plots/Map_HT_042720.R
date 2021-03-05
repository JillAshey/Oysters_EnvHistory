



#install maps
install.packages("maps", "map_data")

library(maps)
library(mapdata)
map(col="grey80", border = "grey40", fill = TRUE,
    xlim = c(10, 36), ylim = c(50, 68), mar = rep(0.1, 4))
box()
points(25.615672,58.977768,col=2,pch=18)

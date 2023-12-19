library(marmap)
chile <- getNOAA.bathy(lon1=-100, lon2=-60, lat1=-60, lat2=-18, resolution=10, keep=TRUE)
summary(chile)

blues <- c("lightsteelblue4", "lightsteelblue3", "lightsteelblue2", "lightsteelblue1")
greys <- c(grey(0.6), grey(0.93), grey(0.99))
           
plot(chile, image = TRUE, land = TRUE, lwd = 0.1, bpal = blues(100), deep = 0, shallow = 0, step = 0, lwd = 0.4, add = TRUE)

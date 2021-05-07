library(raster)
library(ggplot2)
library(dplyr)
library(proxy)

#This script was used to create centroids and calculate displacement from homerange centroids during fire
centroids <- shapefile(here::here("Publication_Data",'Collars','MCP_Centroid_Shapefiles',"lab.shp"))
a <- coordinates(centroids)


cent$x<- as.numeric(cent$coords.x1)
cent$y <- as.numeric(cent$coords.x2)
far <- read.csv(here::here('Publication_Data','Collars','Far.csv'))
far$x <- as.numeric(far$x)
far$y <- as.numeric(far$y)

cent1 <- cent[c(2,3)]
far1 <- far[c(2,3)]

a <-dist(cent1, far1, method = "euclidean")
write.csv(a, "DuringFireDisplacement.csv")



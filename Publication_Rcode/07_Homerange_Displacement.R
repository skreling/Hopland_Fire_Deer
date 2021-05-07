library(rgdal)
library(rgeos) #get centroid
library(sp)
library(raster)
library(dplyr)
library(proxy)

#This script is used to create distance matrices between homeranges centroids pre and post fire



#read in mcp shapefiles
predoe <- readOGR(here::here("Publication_Data","Collars","MCPs","mcppredoe.shp"))
postdoe <- readOGR(here::here("Publication_Data","Collars","MCPs","mcppostdoe1.shp"))
prebuck <- readOGR(here::here("Publication_Data","Collars","MCPs","prebuckmcp.shp"))
postbuck <- readOGR(here::here("Publication_Data","Collars","MCPs","postbuckmcp.shp"))

#Get a centroid for each shapefile layer, byid will take a centroid for each animalID 
Cpredoe <- gCentroid(predoe, byid=TRUE)
Cpostdoe <- gCentroid(postdoe, byid=TRUE)
Cprebuck <- gCentroid(prebuck, byid=TRUE)
Cpostbuck <- gCentroid(postbuck, byid=TRUE)

#Turn centrids into list of coords dataframe
Cpredoe1 <- as.data.frame(Cpredoe)
Cpostdoe1 <- as.data.frame(Cpostdoe)
Cprebuck1 <- as.data.frame(Cprebuck)
Cpostbuck1 <- as.data.frame(Cpostbuck)

#creates distance between centroid matrix - we only want diagnol 
distmatrixdoe <- proxy::dist(Cpredoe1,Cpostdoe1 , method="euclidean")
distmatrixbuck <- proxy::dist(Cprebuck1, Cpostbuck1, method="euclidean")

#extract just diagnol from matrix
doedist <- diag(distmatrixdoe)
buckdist <- diag(distmatrixbuck)


#ttests and averages
mean(doedist)#140.12
mean(buckdist)#33.118
sd(doedist)#67.442
sd(buckdist)#4.22 


#################################################
#for deer outside the burn perimeter
#vectronic collars - a5b, j5, h4, h5
library(adehabitatHR) #mcp generation
collars <- read.csv(here::here("Publication_Data","Collars","Vectronic","VectronicPreFire.csv"))
head(collars)
collars$TimeStamp <-collars$Date_Time
collars$TimeStamp <- as.POSIXct((collars$Time), format = "%Y-%m-%d %H:%M:%S", origin = '1970-01-01', tz = "America/Los_Angeles")
##Extract XY coordinates, then tell system original CRS and transform into UTM for meteres and convert to spatial points, then convert spatial points back into dataframe
collars.spdf<- SpatialPointsDataFrame(coords=as.data.frame(cbind(collars$Longitude, collars$Latitude)), data=collars, proj4string=CRS("+proj=longlat +datum=WGS84"))
collars.spdf<-spTransform(collars.spdf,CRS("+proj=utm +zone=10 ellps=WGS84") )
plot(collars.spdf)

collars.mcppost <- mcp(collars.spdf[,"AnimalID"], percent=95, unin="m", unout="km2")
plot(collars.mcppost, col=c(1:13))
collarsOut <- collars.mcppost[collars.mcppost$id=="A5b"|collars.mcppost$id=="J5"|collars.mcppost$id=="H5"|collars.mcppost$id=="H4",]
trueCentroids2 <- gCentroid(collarsOut, byid=TRUE)
plot(trueCentroids2)
VectronicDoesPre<-as.data.frame(trueCentroids2) #pre fire - outside vectronic does




#post fire vectronic does outside
collars <- read.csv(here::here("Publication_Data","Collars","Vectronic","VectronicPostFire.csv"))
head(collars)
collars$TimeStamp <-collars$Date_Time
collars$TimeStamp <- as.POSIXct((collars$Time), format = "%Y-%m-%d %H:%M:%S", origin = '1970-01-01', tz = "America/Los_Angeles")
##Extract XY coordinates, then tell system original CRS and transform into UTM for meteres and convert to spatial points, then convert spatial points back into dataframe
collars.spdf<- SpatialPointsDataFrame(coords=as.data.frame(cbind(collars$Longitude, collars$Latitude)), data=collars, proj4string=CRS("+proj=longlat +datum=WGS84"))
collars.spdf<-spTransform(collars.spdf,CRS("+proj=utm +zone=10 ellps=WGS84") )
plot(collars.spdf)

collars.mcppost <- mcp(collars.spdf[,"AnimalID"], percent=95, unin="m", unout="km2")
plot(collars.mcppost, col=c(1:13))
collarsOut <- collars.mcppost[collars.mcppost$id=="A5b"|collars.mcppost$id=="J5"|collars.mcppost$id=="H5"|collars.mcppost$id=="H4",]
trueCentroids2 <- gCentroid(collarsOut, byid=TRUE)
plot(trueCentroids2)
VectronicDoesPost<-as.data.frame(trueCentroids2) #pre fire - outside vectronic does




#lotek pre fire does
collars <- read.csv(here::here("Publication_Data","Collars","Lotek","LotekPreFire-fix.csv"))
head(collars)
collars$TimeStamp <-collars$Date_Time
collars$TimeStamp <- as.POSIXct((collars$Time), format = "%Y-%m-%d %H:%M:%S", origin = '1970-01-01', tz = "America/Los_Angeles")
##Extract XY coordinates, then tell system original CRS and transform into UTM for meteres and convert to spatial points, then convert spatial points back into dataframe
collars.spdf<- SpatialPointsDataFrame(coords=as.data.frame(cbind(collars$Longitude, collars$Latitude)), data=collars, proj4string=CRS("+proj=longlat +datum=WGS84"))
collars.spdf<-spTransform(collars.spdf,CRS("+proj=utm +zone=10 ellps=WGS84") )
plot(collars.spdf)

collars.mcppost <- mcp(collars.spdf[,"AnimalID"], percent=95, unin="m", unout="km2")
plot(collars.mcppost, col=c(1:13))
trueCentroids2 <- gCentroid(collars.mcppost, byid=TRUE)
plot(trueCentroids2)
LotekDoesPre<-as.data.frame(trueCentroids2) #pre fire - outside vectronic does


#lotek pre fire does
collars <- read.csv(here::here("Publication_Data","Collars","Lotek","LotekPostFire-fix.csv"))
head(collars)
collars$TimeStamp <-collars$Date_Time
collars$TimeStamp <- as.POSIXct((collars$Time), format = "%Y-%m-%d %H:%M:%S", origin = '1970-01-01', tz = "America/Los_Angeles")
##Extract XY coordinates, then tell system original CRS and transform into UTM for meteres and convert to spatial points, then convert spatial points back into dataframe
collars.spdf<- SpatialPointsDataFrame(coords=as.data.frame(cbind(collars$Longitude, collars$Latitude)), data=collars, proj4string=CRS("+proj=longlat +datum=WGS84"))
collars.spdf<-spTransform(collars.spdf,CRS("+proj=utm +zone=10 ellps=WGS84") )
plot(collars.spdf)

collars.mcppost <- mcp(collars.spdf[,"AnimalID"], percent=95, unin="m", unout="km2")
plot(collars.mcppost, col=c(1:13))
trueCentroids2 <- gCentroid(collars.mcppost, byid=TRUE)
plot(trueCentroids2)
LotekDoesPost<-as.data.frame(trueCentroids2) #pre fire - outside vectronic does

#combine the dataframes

OutPre <- rbind(LotekDoesPre, VectronicDoesPre)
OutPre <- OutPre[-c(1:3),]

OutPost <- rbind(LotekDoesPost, VectronicDoesPost)
OutPost <- OutPost[-c(1:3),]

########################################
#now we want to create distance matrices for outside does

#creates distance between centroid matrix - we only want diagnol 
distmatrixdoeOut <- proxy::dist(OutPre,OutPost , method="euclidean")

#extract just diagnol from matrix
doeOUTdist <- diag(distmatrixdoeOut)

#ttests and averages
mean(doeOUTdist)#119.22
sd(doeOUTdist)#108.37
t.test(doedist, doeOUTdist, paired=F)

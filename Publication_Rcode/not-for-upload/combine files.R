packages <- c('adehabitatHR','adehabitatLT','lubridate','plyr','rgeos','here','rgdal')

install_load <- function(packages){
  for (p in packages) {
    if (p %in% rownames(installed.packages())) {
      library(p, character.only=TRUE)
    } else {
      install.packages(p)
      library(p,character.only = TRUE)
    }
  }
}

install_load(packages)

library(tidyverse)

# vetronic pre ------------------------------------------------------------

vectronicpre <- read.csv("Publication_Data_orig/Collars/Vectronic/VectronicPreFire.csv") %>% 
  mutate(TimeStamp = as.POSIXct((Time), format = "%Y-%m-%d %H:%M:%S", origin = '1970-01-01', tz = "UTC")) %>% 
  dplyr::rename("Longitude" = "Longitude....",
         "Latitude" = "Latitude....") %>% 
  dplyr::select(AnimalID, Latitude, Longitude, TimeStamp)

##Extract XY coordinates, then tell system original CRS and transform into UTM for meteres and convert to spatial points, then convert spatial points back into dataframe
xy <- dplyr::select(vectronicpre, Longitude, Latitude)  ## get lat/long from data frame
coordinates(xy) <- c("Longitude", "Latitude")
proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")
res <- spTransform(xy, CRS("+proj=utm +zone=10 ellps=WGS84"))
res2 <- as.data.frame(res) %>% 
  rename(Easting = Longitude,
         Northing = Latitude)

vectronicpre <- cbind(vectronicpre, res2) %>% 
  mutate(Sex = "Doe")

head(vectronicpre)


# lotek pre ---------------------------------------------------------------

lotekpre <- read.csv("Publication_Data_orig/Collars/Lotek/LotekPreFire.csv") %>% 
  mutate(TimeStamp = as.POSIXct((TimeStamp), format = "%Y-%m-%d %H:%M:%S", origin = '1970-01-01', tz = "UTC")) %>% 
  dplyr::select(AnimalID, Latitude, Longitude, TimeStamp)

# remove the NAs
lotekpre <- lotekpre[complete.cases(lotekpre),]

##Extract XY coordinates, then tell system original CRS and transform into UTM for meteres and convert to spatial points, then convert spatial points back into dataframe
xy <- dplyr::select(lotekpre, Longitude, Latitude)  ## get lat/long from data frame
coordinates(xy) <- c("Longitude", "Latitude")
proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")
res <- spTransform(xy, CRS("+proj=utm +zone=10 ellps=WGS84"))
res2 <- as.data.frame(res) %>% 
  dplyr::rename(Easting = Longitude,
         Northing = Latitude)

lotekpre <- cbind(lotekpre, res2) %>% 
  mutate(Sex = "Doe")

head(lotekpre)



# ATS pre ------------------------------------------------------------

atspre <- read.csv("Publication_Data_orig/Collars/ATS/ATSPreFire.csv") %>% 
  mutate(TimeStamp = as.POSIXct((Time), format = "%m/%d/%Y %H:%M", origin = '1970-01-01', tz = "UTC")) %>% 
  dplyr::select(AnimalID, Latitude, Longitude, TimeStamp)

# remove the NAs
atspre <- atspre[complete.cases(atspre),]

##Extract XY coordinates, then tell system original CRS and transform into UTM for meteres and convert to spatial points, then convert spatial points back into dataframe
xy <- dplyr::select(atspre, Longitude, Latitude)  ## get lat/long from data frame
coordinates(xy) <- c("Longitude", "Latitude")
proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")
res <- spTransform(xy, CRS("+proj=utm +zone=10 ellps=WGS84"))
res2 <- as.data.frame(res) %>% 
  dplyr::rename(Easting = Longitude,
         Northing = Latitude)

atspre <- cbind(atspre, res2) %>% 
  mutate(Sex = "Buck")

head(atspre)


# vetronic post ------------------------------------------------------------

vectronicpost <- read.csv("Publication_Data_orig/Collars/Vectronic/VectronicPostFire.csv") %>% 
  mutate(TimeStamp = as.POSIXct((Time), format = "%Y-%m-%d %H:%M:%S", origin = '1970-01-01', tz = "UTC")) %>% 
  dplyr::rename("Longitude" = "Longitude....",
         "Latitude" = "Latitude....") %>% 
  dplyr::select(AnimalID, Latitude, Longitude, TimeStamp)

##Extract XY coordinates, then tell system original CRS and transform into UTM for meteres and convert to spatial points, then convert spatial points back into dataframe
xy <- dplyr::select(vectronicpost, Longitude, Latitude)  ## get lat/long from data frame
coordinates(xy) <- c("Longitude", "Latitude")
proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")
res <- spTransform(xy, CRS("+proj=utm +zone=10 ellps=WGS84"))
res2 <- as.data.frame(res) %>% 
  dplyr::rename(Easting = Longitude,
         Northing = Latitude)

vectronicpost <- cbind(vectronicpost, res2) %>% 
  mutate(Sex = "Doe")

head(vectronicpost)

# lotek post ---------------------------------------------------------------

lotekpost <- read.csv("Publication_Data_orig/Collars/Lotek/LotekPostFire.csv") %>% 
  mutate(TimeStamp = as.POSIXct((TimeStamp), format = "%Y-%m-%d %H:%M", origin = '1970-01-01', tz = "UTC")) %>% 
  dplyr::select(AnimalID, Latitude, Longitude, TimeStamp)

# remove the NAs
lotekpost <- lotekpost[complete.cases(lotekpost),]

##Extract XY coordinates, then tell system original CRS and transform into UTM for meteres and convert to spatial points, then convert spatial points back into dataframe
xy <- dplyr::select(lotekpost, Longitude, Latitude)  ## get lat/long from data frame
coordinates(xy) <- c("Longitude", "Latitude")
proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")
res <- spTransform(xy, CRS("+proj=utm +zone=10 ellps=WGS84"))
res2 <- as.data.frame(res) %>% 
  dplyr::rename(Easting = Longitude,
         Northing = Latitude)

lotekpost <- cbind(lotekpost, res2) %>% 
  mutate(Sex = "Doe")

head(lotekpost)


# ATS post ------------------------------------------------------------

atspost <- read.csv("Publication_Data_orig/Collars/ATS/ATSPostFire.csv") %>% 
  mutate(TimeStamp = as.POSIXct((Time), format = "%Y-%m-%d %H:%M:%S", origin = '1970-01-01', tz = "UTC")) %>% 
  dplyr::select(AnimalID, Latitude, Longitude, TimeStamp)

# remove the NAs
atspost <- atspre[complete.cases(atspost),]

##Extract XY coordinates, then tell system original CRS and transform into UTM for meteres and convert to spatial points, then convert spatial points back into dataframe
xy <- dplyr::select(atspost, Longitude, Latitude)  ## get lat/long from data frame
coordinates(xy) <- c("Longitude", "Latitude")
proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")
res <- spTransform(xy, CRS("+proj=utm +zone=10 ellps=WGS84"))
res2 <- as.data.frame(res) %>% 
  dplyr::rename(Easting = Longitude,
         Northing = Latitude)

atspost <- cbind(atspost, res2) %>% 
  mutate(Sex = "Buck")

head(atspost)


# combine and export ------------------------------------------------------

pre <- bind_rows(atspre, vectronicpre, lotekpre)
post <- bind_rows(atspost, vectronicpost, lotekpost)

write.csv(pre, "Publication_Data/deer-gps-prefire.csv", row.names = F)
write.csv(post, "Publication_Data/deer-gps-postfire.csv", row.names = F)

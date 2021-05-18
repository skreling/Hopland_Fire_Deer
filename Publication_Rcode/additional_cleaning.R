# conduct additional data cleaning on files before analysis

library(tidyverse)


# Clean collar data -------------------------------------------------------

pre <- read.csv(here::here('Publication_Data','Collars','deer-gps-prefire.csv')) %>% 
  select(c("AnimalID", "Latitude", "Longitude", "TimeStamp", "Easting", "Northing", "Sex")) %>% 
  filter(Latitude < 39.2) # removes that one erroneous point

post <- read.csv(here::here('Publication_Data','Collars','deer-gps-postfire.csv')) %>% 
  select(c("AnimalID", "Latitude", "Longitude", "TimeStamp", "Easting", "Northing", "Sex"))

# confirmed that all are complete cases

# rewrite files
write.csv(pre, "Publication_Data/Collars/deer-gps-prefire.csv", row.names = F)
write.csv(post, "Publication_Data/Collars/deer-gps-postfire.csv", row.names = F)


# Clean BCI data ----------------------------------------------------------

# Import data
bci.all <- read.csv(here::here("Publication_Data","Camera","BCI_All.csv"))

# Calculate a "days since fire" variable to include in the body condition modeling
fire.date <- as.Date("7/27/2018", format="%m/%d/%Y")
bci.all$date2 <- as.Date(bci.all$Date, format="%m/%d/%Y")
bci.all$time.since.fire <- as.numeric((bci.all$date2-fire.date))

# make version where negative values are 0
bci.all$time.since.fire.noneg <- NA
for(i in 1:nrow(bci.all)) {
  if(bci.all$time.since.fire[i] < 0) {
    bci.all$time.since.fire.noneg[i] <- 0
  } else {
    bci.all$time.since.fire.noneg[i] <- bci.all$time.since.fire[i]
  }
}

bci <- dplyr::select(bci.all, Station, BCI, Species, TimePeriod, Area, time.since.fire, time.since.fire.noneg)

write.csv(bci, "Publication_Data/Camera/BCI_All_cleaned.csv", row.names = F)


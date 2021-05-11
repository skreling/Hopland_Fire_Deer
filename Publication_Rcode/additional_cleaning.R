# conduct additional data cleaning on files before analysis

library(tidyverse)

predoe <- read.csv(here::here('Publication_Data','Collars','deer-gps-prefiredoe.csv')) %>% 
  filter(AnimalID != "H1") %>% 
  select(-c("X", "X.1"))

postdoe <- read.csv(here::here('Publication_Data','Collars','deer-gps-postfiredoe.csv')) %>% 
  filter(AnimalID != "H1") %>% 
  select(-c("X", "X.1"))

prebuck <- read.csv(here::here('Publication_Data','Collars','deer-gps-prefirebuck.csv')) %>% 
  select(-c("X", "X.1"))

postbuck <- read.csv(here::here('Publication_Data','Collars','deer-gps-postfirebuck.csv')) %>% 
  select(-c("X", "X.1"))

# confirmed that all are complete cases

# rewrite files
write.csv(predoe, "Publication_Data/Collars/deer-gps-prefiredoe.csv", row.names = F)
write.csv(postdoe, "Publication_Data/Collars/deer-gps-postfiredoe.csv", row.names = F)
write.csv(prebuck, "Publication_Data/Collars/deer-gps-prefirebuck.csv", row.names = F)
write.csv(prebuck, "Publication_Data/Collars/deer-gps-postfirebuck.csv", row.names = F)

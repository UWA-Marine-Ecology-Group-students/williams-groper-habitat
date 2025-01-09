## blue groper abundance data for qgis mapping ##
rm(list=ls())

library(dplyr)
library(tidyverse)

blueys <- read.csv("./data/raw/blueys.csv")

blueys <- blueys %>%
  dplyr::filter(location %in% c("Arid", "Duke", "Eastern Recherche", "Esperance", "Middle Island")) %>%
  dplyr::select(count, sample, longitude_dd, latitude_dd, location)%>%
  dplyr::mutate(source = "globalarch")%>%                
  glimpse()


dat <- read.csv("./data/raw/blue_groper_abund.csv")                

dat <- dat %>%
  dplyr::rename(latitude_dd = lat)%>%
  dplyr::rename(longitude_dd = long)%>%
  dplyr::rename(count = abund)%>%
  dplyr::select(-date)%>%
  glimpse()

gropies <- dplyr::union(blueys, dat)

write_csv(gropies, "./data/raw/gropies.csv")



##########################################################################
### Merge EventMeasure database output tables into maxn 

rm(list=ls()) # Clear memory

## Load Libraries ----

#install.packages('remotes')
library('remotes')
options(timeout=9999999)
#remotes::install_github("GlobalArchiveManual/CheckEM")
library(CheckEM)
library(tidyverse)
library(googlesheets4)
library(sf)
library(terra)
library(here)
library(dplyr)
#install_github("UWAMEGFisheries/GlobalArchive") 
library(GlobalArchive)
library(lubridate)

#set study name
name <- "2024_Wudjari_bait_comp"


#metadata (labsheet)
metadata <- read_metadata(here::here("./data/raw/bait_comp/em export"), 
                          method = "BRUVs") %>%
  dplyr::select(opcode, bait, longitude_dd, latitude_dd, date_time, location,
                depth_m, successful_count, successful_habitat_forward, 
                maxn_by_size, behaviour_success, approach_success, site) %>%
  dplyr::mutate(date_time = mdy_hm(date_time, tz = "GMT")) %>% 
  dplyr::mutate(date_time = with_tz(date_time, tzone = "Asia/Singapore"))%>%
  dplyr::mutate(date_time = format(date_time, "%Y/%m/%dT%H:%M:%S")) %>%
  glimpse()

#saving metadata as RDS
saveRDS(metadata, file = here::here(paste0("./data/tidy/",
                                           name, "_Metadata.rds")))

################################
######### COUNT DATA ###########
################################

############################
## points -- MaxN ##########
############################

metadata <- readRDS("./data/tidy/2024_Wudjari_bait_comp_Metadata.rds")

maxn.all <- read_points(here::here("./data/raw/bait_comp/em export")) %>%
  dplyr::filter(genus %in% "Achoerodus") %>%
  dplyr::group_by(filename,opcode,frame,family,genus,species, periodtime)%>%
  ## frame is very important
  dplyr::mutate(number=as.numeric(number))%>%
  dplyr::summarise(maxn=sum(number))%>%
  dplyr::group_by(opcode,family,genus,species)%>%
  dplyr::slice(which.max(maxn))%>%
  dplyr::ungroup()%>%#we don't care about grouping anymore
  dplyr::select(-c(frame, filename))%>% #remove frame wth -
  full_join(metadata)%>%
  mutate(family = ifelse(is.na(family), 'Labridae', family))%>%
  mutate(genus = ifelse(is.na(genus), 'Achoerodus', genus))%>%
  mutate(species = ifelse(is.na(species), 'gouldii', species))%>%
  tidyr::replace_na(list(maxn=0))%>%
  glimpse()


count.maxn.all <- maxn.all%>%
  dplyr::filter(successful_count == "Yes")%>%
  glimpse()

saveRDS(count.maxn.all, file = here::here(paste0("./data/tidy/",
                                           name, "_count.maxn.all.rds")))


###############################################
## points -- maxn by stage BY OPCODE ##########
###############################################

#by stage
metadata <- readRDS("./data/tidy/2024_Wudjari_bait_comp_Metadata.rds") ##remove sample & period here?

maxn.stage <- read_points(here::here("./data/raw/bait_comp/em export")) %>%
  dplyr::filter(genus %in% "Achoerodus") %>%
  dplyr::group_by(filename,opcode,frame,family,genus,species,stage,periodtime)%>% 
  ## frame is very important
  dplyr::mutate(number=as.numeric(number))%>%
  dplyr::summarise(maxn=sum(number))%>%
  dplyr::group_by(opcode, family, genus,species, stage)%>% 
  dplyr::slice(which.max(maxn))%>%
  dplyr::ungroup()%>%#we don't care about grouping anymore
  dplyr::select(-c(frame, filename))%>% #remove frame wth - 
  glimpse()

#adding zeros for all size classes

all_size_classes <- c("0300-0499 mm", "0500-0699 mm", "0700-0899 mm", 
                      "0900-1099 mm", "1100-1299mm", "M", "F", "AD")

count.maxn.stage <- maxn.stage%>%
  full_join(metadata)%>%
  mutate(family = ifelse(is.na(family), 'Labridae', family))%>%
  mutate(genus = ifelse(is.na(genus), 'Achoerodus', genus))%>%
  mutate(species = ifelse(is.na(species), 'gouldii', species))%>%
  mutate(stage = ifelse(is.na(stage), "AD", stage))%>%
  tidyr::replace_na(list(maxn=0))%>%
  dplyr::filter(successful_count == "Yes")%>%
  tidyr::complete(nesting(opcode), stage = all_size_classes, fill = list(maxn = 0))%>%
  mutate(family = ifelse(is.na(family), 'Labridae', family))%>%
  mutate(genus = ifelse(is.na(genus), 'Achoerodus', genus))%>%
  mutate(species = ifelse(is.na(species), 'gouldii', species))%>%
  #left_join(metadata)%>% ######## FIGURE OUT THE JOIN
  glimpse()


saveRDS(count.maxn.stage, file = here::here(paste0("./data/tidy/",
                                                 name, "_count.maxn.stage.rds")))


#########################################################################
######## SUM MaxN by Stage

maxn.sum.stage <- maxn.stage %>%
  dplyr::group_by(sample,opcode, period, family,genus,species)%>%
  dplyr::summarise(maxn_period = sum(maxn))%>%
  dplyr::ungroup()%>%
  glimpse()

count.sum.stage <- maxn.sum.stage%>%
  full_join(metadata)%>%
  dplyr::filter(successful_count == "Yes")%>%
  glimpse()

saveRDS(count.sum.stage, file = here::here(paste0("./data/tidy/",
                                                   name, "_count.sum.stage.rds")))








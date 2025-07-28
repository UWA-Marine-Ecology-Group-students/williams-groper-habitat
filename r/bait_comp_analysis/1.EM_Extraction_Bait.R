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
library(GlobalArchive) #do I need this?
# library(lubridate) #in tidyverse

#set study name
name <- "2024_Wudjari_bait_comp"


#metadata (labsheet)
metadata <- read_metadata(here::here("./data/raw/bait_comp/em export"), 
                          method = "BRUVs") %>%
  dplyr::select(opcode, bait, longitude_dd, latitude_dd, date_time, location,
                depth_m, successful_count, successful_habitat_forward, 
                maxn_by_size, behaviour_success, approach_success, site, successful_length) %>%
  dplyr::mutate(date_time = mdy_hm(date_time, tz = "GMT")) %>% 
  dplyr::mutate(date_time = with_tz(date_time, tzone = "Asia/Singapore"))%>%
  dplyr::mutate(date_time = format(date_time, "%Y/%m/%dT%H:%M:%S")) %>%
  dplyr::mutate(date = substr(date_time, 1, 10))%>%
  dplyr::mutate(time = substr(date_time, 12, 19))%>%
  dplyr::mutate(time_of_day = as.POSIXct(time, format = "%H:%M:%S"))%>%
  dplyr::mutate(time_sec = as.numeric(format(time_of_day, "%H")) * 3600 +
                  as.numeric(format(time_of_day, "%M")) * 60 +
                  as.numeric(format(time_of_day, "%S")))%>%
  dplyr::mutate(time_hr = as.numeric(time_sec/3600))%>%
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

rm(list=ls()) # Clear memory

name <- "2024_Wudjari_bait_comp" #set study name

metadata <- readRDS("./data/tidy/2024_Wudjari_bait_comp_Metadata.rds")%>%
  glimpse()

maxn.all <- read_points(here::here("./data/raw/bait_comp/em export")) %>%
  dplyr::mutate(periodtime = as.numeric(periodtime),
                periodtime = case_when(
                  period == "B" ~ periodtime + 15,
                  period == "C" ~ periodtime + 30,
                  period == "D" ~ periodtime + 45,
                  TRUE ~ periodtime))%>%
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
  dplyr::mutate(presence = ifelse(maxn > 0, 1, 0))%>%
  dplyr::mutate(titomaxn_s = periodtime * 60)%>% #creating covariate of time to maxn in seconds only
  dplyr::mutate(titomaxn_m = periodtime)%>% #creating covariate of titomaxn in mins (same as periodtime)
  dplyr::mutate(bait = as.factor(bait), location = as.factor(location), 
                site = as.factor(site), date = as.factor(date))%>% #removed mutate(species = 'gouldii')
  dplyr::mutate(depth_m = as.numeric(depth_m), 
                longitude_dd = as.numeric(longitude_dd),
                latitude_dd = as.numeric(latitude_dd))%>%
  clean_names()%>%
  glimpse()


count.maxn.all <- maxn.all%>%
  dplyr::filter(successful_count == "Yes")%>%
  glimpse()

saveRDS(count.maxn.all, file = here::here(paste0("./data/tidy/",
                                           name, "_count.maxn.all.rds")))


###############################################
## points -- maxn by stage BY OPCODE ##########
###############################################

rm(list=ls()) # Clear memory

name <- "2024_Wudjari_bait_comp" #set study name

#by stage
metadata <- readRDS("./data/tidy/2024_Wudjari_bait_comp_Metadata.rds") %>%
  glimpse()

maxn.stage <- read_points(here::here("./data/raw/bait_comp/em export")) %>%
  dplyr::mutate(periodtime = as.numeric(periodtime),
                periodtime = case_when(
                  period == "B" ~ periodtime + 15,
                  period == "C" ~ periodtime + 30,
                  period == "D" ~ periodtime + 45,
                  TRUE ~ periodtime))%>%
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
  dplyr::select(opcode, family, genus,species, stage, maxn, periodtime)%>% 
  left_join(metadata)%>% ######## FIGURE OUT THE JOIN
  dplyr::filter(maxn_by_size == "Yes")%>%
  dplyr::mutate(titomaxn_s = periodtime * 60)%>% #creating covariate of time to maxn in seconds only
  dplyr::mutate(titomaxn_m = periodtime)%>% #creating covariate of titomaxn in mins (same as periodtime)
  dplyr::mutate(bait = as.factor(bait), location = as.factor(location), 
                site = as.factor(site), stage = as.factor(stage), date = as.factor(date))%>%
  dplyr::mutate(depth_m = as.numeric(depth_m), 
                longitude_dd = as.numeric(longitude_dd),
                latitude_dd = as.numeric(latitude_dd))%>%
  glimpse()

length(unique(count.maxn.stage$opcode)) #should be 99 *8 = 792

saveRDS(count.maxn.stage, 
             file = here::here(paste0("./data/tidy/",
                     name, "_count.maxn.stage.rds")))


#########################################################################
######## SUM MaxN by Stage (removing the individual stages and summing them)

rm(list=ls()) # Clear memory

name <- "2024_Wudjari_bait_comp" #set study name

metadata <- readRDS("./data/tidy/2024_Wudjari_bait_comp_Metadata.rds") %>%
  glimpse()

count.maxn.stage <- readRDS("./data/tidy/2024_Wudjari_bait_comp_count.maxn.stage.rds") %>%
  glimpse()

maxn.sum.stage <- count.maxn.stage %>%
  dplyr::group_by(opcode, family, genus, species)%>%
  dplyr::summarise(maxn_sum = sum(maxn))%>%
  dplyr::ungroup()%>%
  glimpse()

count.sum.stage <- maxn.sum.stage%>%
  full_join(metadata)%>%
  dplyr::filter(maxn_by_size == "Yes")%>%
  glimpse()

saveRDS(count.sum.stage, file = here::here(paste0("./data/tidy/",
                                                   name, "_count.sum.stage.rds")))



#########################################################################
######## TIME OF ARRIVAL

rm(list=ls()) # Clear memory

name <- "2024_Wudjari_bait_comp" #set study name

metadata <- readRDS("./data/tidy/2024_Wudjari_bait_comp_Metadata.rds") %>%
  glimpse()

time.of.arrival <- read_points(here::here("./data/raw/bait_comp/em export")) %>%
    dplyr::mutate(periodtime = as.numeric(periodtime),
                  periodtime = case_when(
                    period == "B" ~ periodtime + 15,
                    period == "C" ~ periodtime + 30,
                    period == "D" ~ periodtime + 45,
                    TRUE ~ periodtime))%>%
    dplyr::filter(genus %in% "Achoerodus") %>%
    dplyr::group_by(opcode, stage)%>%
    dplyr::slice_min(periodtime, with_ties = F)%>%
    dplyr::ungroup()%>%
    dplyr::select(opcode, periodtime, family, genus, species, stage, number, 
                frame)%>%
    dplyr::rename(time_of_arrival = periodtime)%>%
    left_join(metadata)%>% ##rightjoin here instead
    dplyr::filter(successful_count == "Yes")%>%
  dplyr::select(-c(behaviour_success, approach_success))%>%
  dplyr::mutate(number = ifelse(is.na(number), 1, number))%>% #one emob with no. missing from point - manually resetting
  # dplyr::mutate(bait = as.factor(bait), location = as.factor(location), 
  #               site = as.factor(site), stage = as.factor(stage), date = as.factor(date))%>%
  # dplyr::mutate(depth_m = as.numeric(depth_m), 
  #               longitude_dd = as.numeric(longitude_dd),
  #               latitude_dd = as.numeric(latitude_dd))%>%  
  glimpse()

length(unique(time.of.arrival$opcode)) #83 - no. opcodes w. blue groper - see below

# maxn.all <- readRDS("./data/tidy/2024_Wudjari_bait_comp_count.maxn.all.RDS")
# sum(maxn.all$presence)

anyNA(time.of.arrival)
sum(is.na(time.of.arrival))
time.of.arrival[!complete.cases(time.of.arrival), ] #accidentally removed number in emob

#no zeros
saveRDS(time.of.arrival, file = here::here(paste0("./data/tidy/",
                                                  name, "_time.of.arrival.rds")))

##TODO -- time of arrival needs zeros for all size classes for all emobs

all_size_classes <- c("0300-0499 mm", "0500-0699 mm", "0700-0899 mm", 
                      "0900-1099 mm", "1100-1299mm", "M", "F", "AD")

toa.with.zeros <- time.of.arrival %>%
  right_join(metadata)%>%
  mutate(family = ifelse(is.na(family), 'Labridae', family))%>%
  mutate(genus = ifelse(is.na(genus), 'Achoerodus', genus))%>%
  mutate(species = ifelse(is.na(species), 'gouldii', species))%>%
  tidyr::complete(nesting(opcode), stage = all_size_classes, fill = list(maxn = 0))%>%
  glimpse()

length(unique(toa.with.zeros$opcode))
108*8



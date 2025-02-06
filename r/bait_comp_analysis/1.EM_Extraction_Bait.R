##########################################################################
### Merge EventMeasure database output tables into maxn and length files

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

## periods

period <- read_periods(here::here("./data/raw/bait_comp/em export")) %>%
  dplyr::mutate(sample = paste(opcode, period, sep = "_"))%>%
  dplyr::select(opcode, period, sample)%>%
  glimpse()  

unique(period$period)
unique(period$opcode)

which(is.na(period$period))

## adding sites to METADATA
site <- data.frame(
  opcode = sprintf("%03d", 001:108),
  stringsAsFactors = FALSE) %>%
  dplyr::mutate(site= case_when(
    between(as.numeric(opcode), 1, 18)  ~ "middle",
    between(as.numeric(opcode), 19, 30) ~ "arid",
    between(as.numeric(opcode), 31, 36) ~ "ruby",
    between(as.numeric(opcode), 37, 48 ) ~ "ct",
    between(as.numeric(opcode), 49,54 ) ~ "twin",
    between(as.numeric(opcode), 55,66 ) ~ "mart",
    between(as.numeric(opcode), 67,72 ) ~ "york",
    between(as.numeric(opcode), 73,78 ) ~ "finger",
    between(as.numeric(opcode), 79, 90 ) ~ "mondrain",
    between(as.numeric(opcode), 91, 93 ) ~ "miss",
    between(as.numeric(opcode), 94,102 ) ~ "lucky",
    between(as.numeric(opcode), 103, 108) ~ "ram"))%>%
  dplyr::mutate(opcode = as.character(opcode))%>%
  glimpse()

#metadata (labsheet)
metadata <- read_metadata(here::here("./data/raw/bait_comp/em export"), method = "BRUVs") %>%
  dplyr::select(opcode, bait, longitude_dd, latitude_dd, date_time, location,
                depth_m, successful_count, successful_habitat_forward, 
                maxn_by_size, behaviour_success, approach_success) %>%
  dplyr::mutate(date_time = mdy_hm(date_time, tz = "GMT")) %>% 
  dplyr::mutate(date_time = with_tz(date_time, tzone = "Asia/Singapore"))%>%
  dplyr::mutate(date_time = format(date_time, "%Y/%m/%dT%H:%M:%S")) %>%
  left_join(site)%>%
  glimpse()



##create 'samples' in metadata
metadata <- period %>%
  dplyr::full_join(metadata, by="opcode")%>%
  glimpse()

unique(metadata$opcode)

#saving metadata as RDS
saveRDS(metadata, file = here::here(paste0("./data/tidy/",
                                           name, "_Metadata.rds")))

################################
######### COUNT DATA ###########
################################

###############################
## points -- maxn for all classes per period ##########
#################################

metadata <- readRDS("./data/tidy/2024_Wudjari_bait_comp_Metadata.rds")

maxn.all <- read_points(here::here("./data/raw/bait_comp/em export")) %>%
  dplyr::mutate(sample = paste(opcode, period, sep = "_"))%>%
  dplyr::filter(genus %in% "Achoerodus") %>%
  dplyr::group_by(sample,filename,opcode,period,frame,family,genus,species, periodtime)%>%
  ## frame is very important
  dplyr::mutate(number=as.numeric(number))%>%
  dplyr::summarise(maxn=sum(number))%>%
  dplyr::group_by(sample,opcode, period, family,genus,species)%>%
  # don't care about frame here anymore, just want to group by period and opcode
  dplyr::slice(which.max(maxn))%>%
  dplyr::ungroup()%>%#we don't care about grouping anymore
  dplyr::full_join(period)%>%
  dplyr::select(-c(frame, filename))%>% #remove frame wth -
  tidyr::replace_na(list(maxn=0))%>% #change any NAs to 0
  dplyr::mutate(maxn=as.numeric(maxn))%>%
  glimpse()


maxn.all <- maxn.all%>%
  mutate(family = ifelse(is.na(family), 'Labridae', family))%>%
  mutate(genus = ifelse(is.na(genus), 'Achoerodus', genus))%>%
  mutate(species = ifelse(is.na(species), 'gouldii', species))
 

count.maxn.all <- maxn.all%>%
  full_join(metadata)%>%
  dplyr::filter(successful_count == "Yes")%>%
  glimpse()

unique(count.maxn.all$opcode)

saveRDS(count.maxn.all, file = here::here(paste0("./data/tidy/",
                                           name, "_count.maxn.all.rds")))


################################################
## points -- maxn by stage per period ##########
################################################

#by stage
metadata <- readRDS("./data/tidy/2024_Wudjari_bait_comp_Metadata.rds")

maxn.stage <- read_points(here::here("./data/raw/bait_comp/em export")) %>%
  dplyr::mutate(sample = paste(opcode, period, sep = "_"))%>%
  dplyr::filter(genus %in% "Achoerodus") %>%
  dplyr::group_by(sample,filename,opcode,period,frame,family,genus,species,stage, periodtime)%>%
  ## frame is very important
  dplyr::mutate(number=as.numeric(number))%>%
  dplyr::summarise(maxn=sum(number))%>%
  dplyr::group_by(sample,opcode, period, family,genus,species, stage)%>%
  # don't care about frame here anymore, just want to group by period and opcode
  dplyr::slice(which.max(maxn))%>%
  dplyr::ungroup()%>%#we don't care about grouping anymore
  dplyr::full_join(period)%>%
  dplyr::select(-c(frame, filename))%>% #remove frame wth -
  tidyr::replace_na(list(maxn=0))%>% #change any NAs to 0
  dplyr::mutate(maxn=as.numeric(maxn))%>%
  dplyr::mutate(family = ifelse(is.na(family), 'Labridae', family))%>%
  dplyr::mutate(genus = ifelse(is.na(genus), 'Achoerodus', genus))%>%
  dplyr::mutate(species = ifelse(is.na(species), 'gouldii', species))%>%
  glimpse()


count.maxn.stage <- maxn.stage%>%
  full_join(metadata)%>%
  dplyr::filter(successful_count == "Yes")%>%
  glimpse()

unique(count.maxn.stage$opcode)

saveRDS(count.maxn.stage, file = here::here(paste0("./data/tidy/",
                                                 name, "_count.maxn.stage.rds")))


######## SUM MaxN by Stage by Period

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








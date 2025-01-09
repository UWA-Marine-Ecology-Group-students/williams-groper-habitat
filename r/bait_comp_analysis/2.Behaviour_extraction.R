################################################################################
#############         BEHAVIOUR EXTRACTIONS                 ####################
################################################################################

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

em_length3dpoints <- read_em_length(here::here("./data/raw/bait_comp/em export")) %>%
  dplyr::mutate(sample = paste(opcode, period, sep = "_"))%>%
  dplyr::filter(comment != "sync"| is.na (comment))%>%
  dplyr::select(-c(comment))%>% # there is a comment column in metadata, so you will need to remove this column from EM data
  dplyr::inner_join(metadata, by = join_by(sample)) %>%
  dplyr::filter(successful_length %in% "Yes") %>%
  glimpse() 

unique(em_length3dpoints$period)

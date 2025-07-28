#######################################
## 1.5 -- Length data extraction ######
#######################################

rm(list=ls()) # Clear memory

# install.packages('remotes')
library('remotes')
options(timeout=9999999)
# remotes::install_github("GlobalArchiveManual/CheckEM")
library(CheckEM)
library(tidyverse)
library(googlesheets4)
library(sf)
library(terra)
library(here)


name <- "2024_Wudjari_bait_comp" #set study name

metadata <- readRDS("./data/tidy/2024_Wudjari_bait_comp_Metadata.rds") %>%
  glimpse()




em_length3dpoints <- read_em_length(here::here("r-workflows/data/raw/")) %>%                   
  dplyr::select(-c(comment))%>% # there is a comment column in metadata, so you will need to remove this column from EM data
  dplyr::inner_join(metadata, by = join_by(sample, campaignid)) %>%
  dplyr::filter(successful_length %in% "Yes") %>%
  # dplyr::rename(length_mm = length) %>%
  glimpse() 
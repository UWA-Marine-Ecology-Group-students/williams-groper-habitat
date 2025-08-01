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

length <- read_em_length(here::here("./data/raw/bait_comp/em export/")) %>%  
  dplyr::inner_join(metadata, by = join_by(opcode)) %>%
  dplyr::filter(successful_length %in% "Yes") %>%
  glimpse() 

#remove sync points
unique(length$comment) # 'sync', 'SYNC'
test <- length %>%
  dplyr::filter(!comment %in% c("sync", "SYNC"))

unique(test$comment)




######## TODO: Think about maxn.all data frame & maxn.stage (all)

#format & add zeros
tidy_lengths <- length %>%
  dplyr::mutate(family = ifelse(is.na(family), 'Labridae', family))%>%
  dplyr::mutate(genus = ifelse(is.na(genus), 'Achoerodus', genus))%>%
  dplyr::mutate(species = ifelse(is.na(species), 'gouldii', species))%>%
  dplyr::mutate(number = as.numeric(number)) %>%
  tidyr::uncount(number) %>%
  dplyr::mutate(number = 1) %>% 
  # Add in missing samples
  dplyr::right_join(metadata) %>%
  dplyr::filter(successful_length %in% "Yes") %>%
  # Complete the data (add in zeros for every species)
  dplyr::select(campaignid, sample, family, genus, species, length_mm, number, any_of(c("range", "rms", "precision"))) %>% # this will keep EM only columns
  tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species)) %>%
  tidyr::replace_na(list(number = 0)) %>%
  ungroup() %>%
  dplyr::filter(!is.na(number)) %>%
  dplyr::mutate(length_mm = as.numeric(length_mm)) %>%
  left_join(., metadata) %>%
  glimpse()




#maxn.stage_lengths
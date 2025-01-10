############################################################################
### 4.0 Format & Visualise habitat data
######

rm(list=ls()) # Clear memory

library('remotes')
options(timeout=9999999)
# remotes::install_github("GlobalArchiveManual/CheckEM")
library(CheckEM)
library(tidyverse)
library(ggbeeswarm)
library(RColorBrewer)
library(leaflet)
library(leaflet.minicharts)
library(here)
library(ggplot2)


#set stuggplot2#set study name
name <- "2024_Wudjari_bait_comp"

# Load metadata with bathymetry added 
#(see CheckEM workflow ' Generate spacial layers for modelling')
#I'm not sure I'm interested in the bathymetry stuff at this point


# metadata.bathy.derivatives <- readRDS(here::here(paste0(
#   "r-workflows/data/tidy/", name, "_metadata-bathymetry-derivatives.rds"))) %>%
#   dplyr::mutate(sample = as.character(sample)) %>%
#   glimpse()

#read in metadata 
metadata <- readRDS("./data/tidy/2024_Wudjari_bait_comp_Metadata.rds")

#Load the habitat data and format it into ‘broad’ classes for modelling. 

habitat <- readRDS("./data/tidy/2024_Wudjari_bait_comp_habitat.rds") 

unique(habitat$level_2)
unique(habitat$level_3)
unique(habitat$genus)

habitat <- habitat%>%
  dplyr::mutate(
    habitat = case_when(level_2 %in% "Macroalgae" & !level_3 %in% "Large canopy-forming" ~ level_2, 
                        level_2 %in% "Macroalgae" & genus %in% "Scytothalia" ~ "Scytothalia",
                        level_2 %in% "Macroalgae" & genus %in% "Ecklonia" ~ "Ecklonia",
                        level_2 %in% "Macroalgae" & genus %in% "Sargassum"  ~ "Sargassum",
                        #level_2 %in% "Seagrasses" ~ level_2, 
                        level_2 %in% "Substrate" & level_3 %in% "Consolidated (hard)" ~ level_3, 
                        level_2 %in% "Substrate" & level_3 %in% "Unconsolidated (soft)" ~ level_3, 
                        level_2 %in% "Sponges" ~ "Sessile invertebrates", 
                        level_2 %in% "Sessile invertebrates" ~ level_2, 
                        #level_2 %in% "Bryozoa" ~ "Sessile invertebrates", 
                        level_2 %in% "Cnidaria" ~ "Sessile invertebrates")) %>% 
  dplyr::select(campaignid, opcode, habitat, number) %>%
  group_by(campaignid, opcode, habitat) %>% 
  dplyr::tally(number, name = "number") %>% dplyr::mutate(total_points_annotated = sum(number)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "habitat", values_from = "number", values_fill = 0) %>%
  dplyr::mutate(reef = Macroalgae + `Sessile invertebrates` + 
                  `Consolidated (hard)` + Scytothalia + Ecklonia + Sargassum) %>%
  pivot_longer(cols = c("Macroalgae", 
                        "Scytothalia",
                        "Ecklonia",
                        "Sargassum",
                        #"Seagrasses", 
                        "Sessile invertebrates", 
                        "Consolidated (hard)", 
                        "Unconsolidated (soft)", 
                        "reef"), 
               names_to = "habitat", values_to = "number") %>%
  glimpse()

unique(habitat$habitat)

# Load the relief data and summarise this into mean and standard deviation relief.

tidy.relief <- readRDS("./data/tidy/2024_Wudjari_bait_comp_relief.rds") %>%
  uncount(number) %>%
  group_by(campaignid, opcode) %>%
  dplyr::summarise(mean.relief = mean(as.numeric(level_5)), sd.relief = sd(as.numeric(level_5), na.rm = T)) %>%
  ungroup() %>%
  glimpse()

# Join the habitat data with relief, metadata and bathymetry derivatives.
tidy.habitat <- metadata %>%
  left_join(habitat) %>%
  left_join(tidy.relief) %>%
  dplyr::mutate(longitude_dd = as.numeric(longitude_dd),
                latitude_dd = as.numeric(latitude_dd)) %>%
  clean_names() %>%
  glimpse()


# Format the relief into a format suitable for exploratory plotting.

plot.relief <- readRDS("./data/tidy/2024_Wudjari_bait_comp_relief.rds") %>%
  group_by(campaignid, opcode, level_5) %>%
  dplyr::summarise(number = sum(number)) %>%
  ungroup() %>%
  dplyr::mutate(class.relief = as.factor(level_5)) %>%
  glimpse()

# Plot the occurence data per habitat class. Each data point represents a unique sample.
## TODO check this with Brooke/Claude because looks wrong
ggplot() +
  geom_quasirandom(data = tidy.habitat, 
                   aes(x = total_points_annotated, y = habitat), 
                   groupOnX = F, method = "quasirandom", 
                   alpha = 0.25, size = 1.8, width = 0.2) +
  labs(x = "Number of points", y = "") +
  theme_classic()

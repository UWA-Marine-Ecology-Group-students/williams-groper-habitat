############################################################################
### 4.0 Format & Visualise habitat data
######

rm(list=ls()) # Clear memory

library('remotes')
options(timeout=9999999)
# remotes::install_github("GlobalArchiveManual/CheckEM")
library(CheckEM)
library(tidyverse)
#library(ggbeeswarm)
library(RColorBrewer)
#library(leaflet)
#library(leaflet.minicharts)
library(here)
library(ggplot2)

#set study name
name <- "2024_Wudjari_bait_comp"

# Load metadata with bathymetry added 
#(see CheckEM workflow ' Generate spacial layers for modelling')
#I'm not sure I'm interested in the bathymetry stuff at this point

# metadata.bathy.derivatives <- readRDS(here::here(paste0(
#   "r-workflows/data/tidy/", name, "_metadata-bathymetry-derivatives.rds"))) %>%
#   dplyr::mutate(sample = as.character(sample)) %>%
#   glimpse()

#read in metadata -- with period and sample removed
metadata <- readRDS("./data/tidy/2024_Wudjari_bait_comp_Metadata.rds")%>%
  dplyr::select(-c("period", "sample"))%>% # don't need period or sample for this
  dplyr::distinct(across(everything()))%>% # removing duplicates of opcodes 
  glimpse()

#Load the habitat data and format it into ‘broad’ classes for modelling. 

habitat <- readRDS("./data/tidy/2024_Wudjari_bait_comp_habitat.rds")%>%
    dplyr::mutate(
    habitat = case_when(level_2 %in% "Macroalgae" & !level_3 %in% "Large canopy-forming" ~ level_2, 
                        level_2 %in% "Macroalgae" & genus %in% "Scytothalia" ~ "Scytothalia",
                        level_2 %in% "Macroalgae" & genus %in% "Ecklonia" ~ "Ecklonia",
                        level_2 %in% "Macroalgae" & genus %in% "Sargassum"  ~ "Sargassum",
                        level_2 %in% "Substrate" & level_3 %in% "Consolidated (hard)" ~ level_3, 
                        level_2 %in% "Substrate" & level_3 %in% "Unconsolidated (soft)" ~ level_3, 
                        level_2 %in% "Sponges" ~ "Sessile invertebrates", 
                        level_2 %in% "Sessile invertebrates" ~ level_2, 
                        level_2 %in% "Ascidians" ~ "Sessile invertebrates", 
                        level_2 %in% "Cnidaria" ~ "Sessile invertebrates",
                        level_2 %in% "Fishes" ~ level_2,
                        level_2 %in% "Seagrasses" & genus %in% "Posidonia" ~ "Posidonia")) %>% 
  dplyr::mutate(habitat = ifelse(level_2 == "Macroalgae" & 
                                 level_3 == "Large canopy-forming" &
                                 is.na(habitat), "Canopy", habitat))%>%
  dplyr::select(campaignid, opcode, habitat, number) %>%
  group_by(campaignid, opcode, habitat) %>% 
  dplyr::tally(number, name = "number") %>% 
  dplyr::mutate(total_points_annotated = sum(number)) %>% 
  ungroup()%>% 
  pivot_wider(names_from = "habitat", values_from = "number", values_fill = 0) %>%
  dplyr::mutate(reef = Macroalgae + `Sessile invertebrates` + 
                  `Consolidated (hard)` + Canopy + 
                  Scytothalia + Ecklonia + Sargassum) %>%
  pivot_longer(cols = c("Macroalgae", 
                        "Scytothalia",
                        "Ecklonia",
                        "Sargassum",
                        "Canopy", 
                        "Sessile invertebrates", 
                        "Consolidated (hard)", 
                        "Unconsolidated (soft)", 
                        "reef",
                        "Fishes",
                        "Posidonia"), 
               names_to = "habitat", values_to = "number") %>%
  glimpse()

unique(habitat$habitat)

# Load the relief data and summarise this into mean and standard deviation relief.

tidy.relief <- readRDS("./data/tidy/2024_Wudjari_bait_comp_relief.rds") %>%
  uncount(number) %>%
  group_by(campaignid, opcode) %>%
  dplyr::summarise(mean.relief = mean(as.numeric(level_5)), 
                   sd.relief = sd(as.numeric(level_5), na.rm = T)) %>%
  ungroup() %>%
  glimpse()


# Join the habitat data with relief, & metadata
tidy.habitat <- metadata %>%
  left_join(habitat) %>% 
  left_join(tidy.relief) %>%
  dplyr::mutate(longitude_dd = as.numeric(longitude_dd),
                latitude_dd = as.numeric(latitude_dd)) %>%
  clean_names() %>%
  dplyr::select(-c(campaignid))%>%
  dplyr::filter(successful_habitat_forward == "Yes")%>% #046 all unscorable = NAs here
  glimpse()


unique(tidy.habitat$habitat)

# Plot the occurence data per habitat class. Each data point represents a unique sample.

plot.habitat<- tidy.habitat %>%
  group_by(opcode, habitat) %>%
  dplyr::summarise(number = sum(number)) %>%
  ungroup() %>%
  #dplyr::mutate(class.relief = as.factor(level_5)) %>%
  glimpse()


ggplot() +
  geom_quasirandom(data = plot.habitat, 
                   aes(x = number, y = habitat), #changed x from total_annotated_points
                   groupOnX = F, method = "quasirandom", 
                   alpha = 0.25, size = 1.8, width = 0.2) +
  labs(x = "Number of points", y = "") +
  theme_classic()


# Format the relief into a format suitable for exploratory plotting.

plot.relief <- readRDS("./data/tidy/2024_Wudjari_bait_comp_relief.rds") %>%
  group_by(campaignid, opcode, level_5) %>%
  dplyr::summarise(number = sum(number)) %>%
  ungroup() %>%
  dplyr::mutate(class.relief = as.factor(level_5)) %>%
  glimpse()

# plotting relief

ggplot() +
  geom_quasirandom(data = plot.relief, 
                   aes(x = number, y = class.relief), 
                   groupOnX = F, 
                   method = "quasirandom", 
                   alpha = 0.25, size = 1.8, width = 0.05) +
  labs(x = "Number of points", y = "Relief (0-5)") + 
  theme_classic()


###### FANCY CLAUDE PLOTS
# create colour palette for plotting

#cols <- colorRampPalette(brewer.pal(12, "Paired"))(length(unique(tidy.habitat$habitat))) 

#format habitat into wide format suitable for plotting

# plot.habitat <- tidy.habitat %>%
#   pivot_wider(names_from = "habitat", values_from = "number", names_prefix = "broad.") %>%
#   glimpse()
# 


#Visualise the habitat classes as spatial pie charts.
# leaflet() %>%                                          
#   addTiles(group = "Open Street Map") %>%                           
#   addProviderTiles('Esri.WorldImagery', group = "World Imagery") %>%
#   addLayersControl(baseGroups = c("World Imagery", "Open Street Map"),
#                    options = layersControlOptions(collapsed = FALSE)) %>%
#   addMinicharts(plot.habitat$longitude_dd, plot.habitat$latitude_dd, 
#                 type = "pie", colorPalette = cols, 
#                 chartdata = plot.habitat[grep("broad", names(plot.habitat))], 
#                 width = 20, transitionTime = 0) %>%                 
#   setView(mean(as.numeric(plot.habitat$longitude_dd)), 
#           mean(as.numeric(plot.habitat$latitude_dd)), zoom = 12)




saveRDS(tidy.habitat, file = here::here(paste0("./data/tidy/", name, "_tidier.habitat.rds")))

###################################################
# adding percentage cover for each habitat class

habitat <- readRDS("./data/tidy/2024_Wudjari_bait_comp_tidier.habitat.rds")%>%
  dplyr::select(opcode, total_points_annotated, habitat, number, 
                mean_relief, sd_relief)%>%
  dplyr::group_by(opcode, habitat, total_points_annotated) %>%
  dplyr::mutate(percentage = (number / total_points_annotated) * 100
  ) %>%
  ungroup()%>%
  dplyr::select(opcode, habitat, percentage)%>%
  pivot_wider(names_from = "habitat", values_from = "percentage", values_fill = 0)%>%
  left_join(tidy.relief)%>%
  glimpse()


saveRDS(habitat, file = here::here(paste0("./data/tidy/", name, "_full.habitat.rds")))


     
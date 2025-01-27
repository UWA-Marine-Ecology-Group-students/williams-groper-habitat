#########################################################
###
### Final Plots for Substantial piece of writing
###
#########################################################


rm(list=ls())

# libraries----

library(tidyverse)
library(ggplot2)
#install.packages("ggforce")
library(ggforce)

#install.packages('scatterpie')
library(scatterpie)
library(cowplot)
library(RColorBrewer)
library(paletteer)
library(sf)

name <- "2024_Wudjari_bait_comp"

folder_path <- "./plots/baitcomp/"

#######################################
######      MaxN by Bait ##############
#######################################
habitat <- readRDS("./data/tidy/2024_Wudjari_bait_comp_full.habitat.rds")%>%
  dplyr::rename(inverts = "Sessile invertebrates", rock = "Consolidated (hard)", 
                sand = "Unconsolidated (soft)")%>%
  glimpse()

#creating a df for adding more specific site names
site <- data.frame(
  opcode = sprintf("%03d", 001:108), 
  stringsAsFactors = FALSE) %>%
  dplyr::mutate(site = case_when(
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

#read in maxn data and joining

maxn.all <- readRDS("./data/tidy/2024_Wudjari_bait_comp_count.maxn.all.RDS") %>%
  dplyr::mutate(species = "gouldii", bait = as.factor(bait), location = as.factor(location))%>%
  dplyr::mutate(depth_m = as.numeric(depth_m))%>%
  dplyr::mutate(period = as.factor(period))%>%
  dplyr::mutate(date = substr(date_time, 1, 10))%>%
  dplyr::mutate(time = substr(date_time, 12, 19))%>%
  dplyr::mutate(date = as.factor(date))%>%
  dplyr::group_by(opcode)%>%
  dplyr::slice_max(order_by = maxn, n=1, with_ties = FALSE)%>% # sliced the highest maxN by opcode 
  dplyr::ungroup()%>%
  dplyr::mutate(location = factor(location, levels = c("mart", "twin", "arid", "middle")))%>% #reordering
  left_join(habitat)%>%
  left_join(site)%>%
  dplyr::mutate(longitude_dd = as.numeric(longitude_dd), 
                latitude_dd = as.numeric(latitude_dd))%>%
  dplyr::mutate(site = as.factor(site))%>%
  dplyr::mutate(site = factor(site, levels = c("mart", "twin", "ct", "ruby", "arid", "middle")))%>% 
  glimpse()

## MaxN(stage) dataframe

maxn.stage <- readRDS("./data/tidy/2024_Wudjari_bait_comp_count.maxn.stage.RDS") %>%
  dplyr::mutate(species = "gouldii", bait = as.factor(bait), location = as.factor(location))%>%
  dplyr::mutate(depth_m = as.numeric(depth_m))%>%
  dplyr::mutate(period = as.factor(period))%>%
  dplyr::mutate(date = substr(date_time, 1, 10))%>%
  dplyr::mutate(time = substr(date_time, 12, 19))%>%
  dplyr::mutate(date = as.factor(date))%>%
  dplyr::group_by(opcode, stage)%>%
  dplyr::slice_max(order_by = maxn, n=1, with_ties = FALSE)%>%
  dplyr::ungroup()%>%
  dplyr::mutate(location = factor(location, levels = c("mart", "twin", "arid", "middle")))%>% #reordering
  left_join(habitat)%>%
  left_join(site)%>%
  dplyr::mutate(longitude_dd = as.numeric(longitude_dd), 
                latitude_dd = as.numeric(latitude_dd))%>%
  dplyr::mutate(site = as.factor(site))%>%
  dplyr::mutate(site = factor(site, levels = c("mart", "twin", "ct", "ruby", "arid", "middle")))%>% 
  glimpse()

### Filtering MaxNstage DF
dat <- maxn.stage %>%
  dplyr::filter(maxn != 0)%>% #filtering out zeros
  dplyr::mutate(stage = as.factor(stage))%>%
  dplyr::filter(!stage %in% c("AD", "M", "F"))%>% #filtering out these
  dplyr::mutate(location = factor(location, levels = c("mart", "twin", "arid", "middle")))%>% #reordering
  glimpse()


##DF with the MaxN per Stage summed for each opcode

sum.stage <- maxn.stage %>% ##DF with the MaxN per Stage summed for each opcode
  dplyr::group_by(opcode, family, genus, species, bait, 
                  longitude_dd, latitude_dd, date_time, 
                  location, depth_m, date, time) %>%
  dplyr::summarise(maxn=sum(maxn))%>%
  dplyr::ungroup()%>%
  left_join(habitat)%>%
  left_join(site)%>%
  dplyr::mutate(longitude_dd = as.numeric(longitude_dd), 
                latitude_dd = as.numeric(latitude_dd))%>%
  dplyr::mutate(site = as.factor(site))%>%
  dplyr::mutate(site = factor(site, 
                              levels = c("mart", "twin", "ct", "ruby", "arid", "middle")))%>% 
  glimpse()

############################################################
####      Colour schemes
##

bg_col <- c("0300-0499 mm" = "#2ecc71", "0500-0699 mm" = "#52be80", 
            "0700-0899 mm" = "#45b39d", "0900-1099 mm" = "#148f77",
            "1100-1299mm" = "#2471a3")

## Bait Types
bait_col <- c("abalone" = "#27ae60", 
              "octopus" = "#f39c12" , 
              "pilchard" = "#CC79A7")




#####################
### MEAN MAXN by bait type with double bars

# # have to combine data frames (bind rows)
maxn.all$group <- "Group1" #creating group for maxn
sum.stage$group <- "Group2" #creating group for maxnstage
merge <- bind_rows(maxn.all, sum.stage) # Combine data frames

summary_df <- merge %>% ## getting the means etc. 
  group_by(bait, group) %>%
  summarise(
    mean_abundance = mean(maxn),
    se_abundance = sd(maxn) / sqrt(n()),
    .groups = "drop"
  )

## dynamite (bar plot with standard error bars)
abund <-
ggplot(summary_df, aes(x= bait, y = mean_abundance, fill = group))+
  geom_bar(stat = "identity", position = position_dodge(width = 0.8))+ 
  geom_errorbar(
    aes(ymin = mean_abundance - se_abundance, ymax = mean_abundance + se_abundance),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
 labs(x = "Bait Treatment", y = "Mean Abundance of WBG")+
  #scale_fill_manual(values = )+                          #add colours here
  scale_x_discrete(labels = c("Abalone", "Octopus", "Pilchard"))+
  theme_cowplot()+
  theme(legend.position = "none")

### plot saving
# png(file.path(folder_path, "abund.png"), width = 600, height = 400)
# abund # plot code
# dev.off() # Close the PNG device

########################################
#### mean MaxN by bait, facetted by size class
## TODO remove AD, F, M & NAs


maxnstage.bait <-
  ggplot(dat, aes(x = bait, y = maxn, fill = bait)) +
  stat_summary(fun = mean, geom = "bar", width = 0.6) +  # Bar plot with mean
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5, color = "black") +  # SE as error bars
  labs(x = "Bait Type", y = "mean Abundance +/- se") +
  scale_x_discrete(labels = c("Abalone", "Octopus", "Pilchard"))+
  scale_fill_manual(values = bait_col)+
  facet_wrap(.~stage, ncol = 2)+
  theme_cowplot()+
  theme(legend.position = "none")

######################
sum(sum.stage$maxn)
sum(maxn.all$maxn)

ggplot(maxn.stage, aes(x = stage, y = maxn, fill = bait)) + 
  geom_jitter()


#################
## plot saving ##
#################
# 
# folder_path <- "./plots/"
# 
# # change title
# png(file.path(folder_path, "poster_pred.png"), width = 600, height = 400)
# 
# # plot code
# 
# ggmod.pred1
# 
# # Close the PNG device
# dev.off()


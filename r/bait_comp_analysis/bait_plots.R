##############################################################################
#####         PLOTTING        ####################
####################################################

rm(list=ls())

# libraries----

library(tidyverse)
library(ggplot2)
library(cowplot)

name <- "2024_Wudjari_bait_comp"

### MaxN (all) by period 

maxn.all <- readRDS("./data/tidy/2024_Wudjari_bait_comp_count.maxn.all.RDS") %>%
  dplyr::mutate(species = "gouldii", bait = as.factor(bait), location = as.factor(location))%>%
  dplyr::mutate(depth_m = as.numeric(depth_m))%>%
  dplyr::mutate(period = as.factor(period))%>%
  dplyr::mutate(date = substr(date_time, 1, 10))%>%
  dplyr::mutate(time = substr(date_time, 12, 19))%>%
  dplyr::mutate(date = as.factor(date))%>%
  #dplyr::mutate(date = as.numeric(julian(date)))%>%
  glimpse()


# sliced the highest maxN by opcode first and run

test <- maxn.all %>% 
  dplyr::group_by(opcode)%>%
  dplyr::slice_max(order_by = maxn, n=1, with_ties = FALSE)%>%
  dplyr::ungroup()%>%
  glimpse()


maxn.all <- test

###

ggplot(maxn.all, aes(x = bait, y = maxn, fill = bait)) +
  geom_boxplot() +
  labs(x = "Bait", y = "MaxN", title = "MaxN by Bait")+
  scale_y_continuous(
    breaks = c(0, 5, 10, 15), 
    limits = c(0,15)) +
  theme_cowplot()

ggplot(maxn.all, aes(x = depth_m, y = maxn)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red")+
  labs(x = "Depth (m)", y = "MaxN", title = "MaxN by Depth")+
  scale_y_continuous(
    breaks = c(0, 5, 10, 15), 
    limits = c(0,15)) +
  theme_cowplot()

ggplot(maxn.all, aes(x = location, y = maxn)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  labs(x = "Location", y = "MaxN", title = "MaxN by Location")+
  scale_y_continuous(
    breaks = c(0, 5, 10, 15), 
    limits = c(0,15)) +
  theme_cowplot()

ggplot(maxn.all, aes(x = date, y = maxn)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  labs(x = "Date", y = "MaxN", title = "MaxN by Date")+
  scale_y_continuous(
    breaks = c(0, 5, 10, 15), 
    limits = c(0,15)) +
  theme_cowplot()

#################################################
## MaxN by STAGE per OPCODE

maxn.stage <- readRDS("./data/tidy/2024_Wudjari_bait_comp_count.maxn.stage.RDS") %>%
  dplyr::mutate(species = "gouldii", bait = as.factor(bait), location = as.factor(location))%>%
  dplyr::mutate(depth_m = as.numeric(depth_m))%>%
  dplyr::mutate(period = as.factor(period))%>%
  dplyr::mutate(date = substr(date_time, 1, 10))%>%
  dplyr::mutate(time = substr(date_time, 12, 19))%>%
  dplyr::mutate(date = as.factor(date))%>%
  glimpse()

test <- maxn.stage %>%
  dplyr::group_by(opcode, stage)%>%
  dplyr::slice_max(order_by = maxn, n=1, with_ties = FALSE)%>%
  dplyr::ungroup()%>%
  glimpse()

maxn.stage <- test

maxn.stage <- maxn.stage %>%
  dplyr::group_by(opcode, family, genus, species, bait, longitude_dd, latitude_dd,date_time, location, depth_m, date, time) %>%
  dplyr::summarise(maxn=sum(maxn))%>%
  dplyr::ungroup()%>%
  glimpse()


ggplot(maxn.stage, aes(x = bait, y = maxn, fill = bait)) +
  geom_boxplot() +
  labs(x = "Bait", y = "MaxN", title = "MaxN by Bait with Size Class")+
  scale_y_continuous(
    breaks = c(0, 5, 10, 15), 
    limits = c(0,15)) +
  theme_cowplot()


ggplot(maxn.stage, aes(x = depth_m, y = maxn)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red")+
  labs(x = "Depth (m)", y = "MaxN", title = "MaxN with Size Class by Depth")+
  scale_y_continuous(
    breaks = c(0, 5, 10, 15), 
    limits = c(0,15)) +
  theme_cowplot()

## location plot
ggplot(maxn.stage, aes(x = location, y = maxn)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  labs(x = "Location", y = "MaxN", title = "MaxN with Size Class by Location")+
  scale_y_continuous(
    breaks = c(0, 5, 10, 15), 
    limits = c(0,15)) +
  theme_cowplot()

## by date

ggplot(maxn.stage, aes(x = date, y = maxn)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  labs(x = "Date", y = "MaxN", title = "MaxN with Size Class by Date")+
  scale_y_continuous(
    breaks = c(0, 5, 10, 15), 
    limits = c(0,15)) +
  theme_cowplot()



maxn.stage <- readRDS("./data/tidy/2024_Wudjari_bait_comp_count.maxn.stage.rds") %>%
  glimpse()

dat <- maxn.stage %>%
  dplyr::filter(maxn != 0)%>%
  dplyr::mutate(stage = as.factor(stage))%>%
  dplyr::filter(!stage %in% c("AD", "F", "M"))%>%
  dplyr::mutate(location = factor(location, levels = c("mart", "twin", "arid", "middle")))%>%
  glimpse()

summary(dat$maxn)
unique(dat$stage)

## jitter plot for the maxn by size class, faceted by location

ggplot(dat, aes(x = stage, y = maxn))+
  geom_jitter(alpha = 0.5)+
  facet_grid(.~location,
             labeller = labeller(location = c("mart" = "Mart & York Islands", 
                                              "twin" = "Twin Peak Islands", 
                                              "arid" = "Cape Arid",
                                              "middle" = "Middle Island")))
## bar plot for maxn by size class, faceted by location
color_palette <- c("0300-0499 mm" = "#2ecc71", "0500-0699 mm" = "#52be80", 
                   "0700-0899 mm" = "#45b39d", "0900-1099 mm" = "#148f77",
                   "1100-1299mm" = "#2471a3")


ggplot(dat, aes(x = stage, y = maxn, fill = stage)) +
  stat_summary(fun = mean, geom = "bar", width = 0.6) +  # Bar plot with mean
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5, color = "black") +  # SE as error bars
  labs(x = "Size Class", y = "mean MaxN +/- se") +
  facet_wrap(.~location, nrow = 2, ncol = 2,
             labeller = labeller(location = c("mart" = "Mart & York Islands", 
                                              "twin" = "Twin Peak Islands", 
                                              "arid" = "Cape Arid",
                                              "middle" = "Middle Island")))+
  scale_fill_manual(values = color_palette)+
  theme_cowplot()+
  theme(legend.position = "none")


maxn.stage <- readRDS("./data/tidy/2024_Wudjari_bait_comp_count.maxn.stage.rds") %>%
  dplyr::filter(maxn != 0)%>%
  dplyr::mutate(stage = as.factor(stage))%>%
  dplyr::mutate(species = "gouldii", bait = as.factor(bait), location = as.factor(location))%>%
  dplyr::mutate(depth_m = as.numeric(depth_m))%>%
  dplyr::mutate(period = as.factor(period))%>%
  dplyr::mutate(date = substr(date_time, 1, 10))%>%
  dplyr::mutate(time = substr(date_time, 12, 19))%>%
  dplyr::mutate(date = as.factor(date))%>%
  dplyr::mutate(periodtime = as.numeric(periodtime))%>%
  dplyr::mutate(periodtime = if_else(period == "B", periodtime + 15, 
                                     periodtime))%>%
  dplyr::mutate(periodtime = if_else(period == "C", periodtime + 30,  
                                     periodtime))%>%
  dplyr::mutate(periodtime = if_else(period == "D", periodtime + 45,  
                                     periodtime))%>%
  dplyr::group_by(opcode, stage)%>% #just want to group by opcode to get maxn per opcode
  dplyr::slice(which.max(maxn))%>%
  dplyr::ungroup()%>%
  dplyr::filter(!stage %in% c("AD"))%>%
  glimpse()

unique(maxn.stage$stage)

## exploratory plots
#colour scheme
bg_col <- c("0300-0499 mm" = "#2ecc71", "0500-0699 mm" = "#52be80", 
            "0700-0899 mm" = "#45b39d", "0900-1099 mm" = "#148f77",
            "1100-1299mm" = "#2471a3")

# distribution

ggplot(data = maxn.stage, aes(x = periodtime, y = maxn))+
  geom_jitter(alpha = 0.5)+
  labs(x = "Time to MaxN", y = "MaxN", 
       title = "Distribution of Time to MaxN with Size Class")+
  theme_cowplot()

#periodtime ~ bait
ggplot(data = maxn.stage, aes(x = maxn, y = periodtime))+
  geom_jitter(alpha = 0.5)+
  labs(x = "Bait Type", y = "Time to MaxN", 
       title = "Bait affect on Time to MaxN with Size Class")+
  theme_cowplot()+
  stat_summary(
    geom = "point",fun.y = "mean", col = "black", size = 3, shape = 24,
    fill = "red" )+
  facet_wrap(.~stage)

bait_col <- c("abalone" = "#27ae60", "octopus" = "#f39c12", 
              "pilchard" = "#1a5276")

ggplot(data = maxn.stage, aes(x = bait, y = periodtime, colour = bait))+
  geom_jitter(alpha = 0.5)+
  theme_cowplot()+
  stat_summary( geom = "point", fun.y = "mean", col = "black", size = 3, shape = 24, fill = "red" )+
  scale_colour_manual(values = bait_col)+
  facet_wrap(.~stage)


############


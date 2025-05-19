########################################
##
##    MAXN subsetted  = individual size classes

rm(list=ls())

# libraries----
#library(devtools)
library(tidyverse)
library(CheckEM)
#library(mgcv)
# library(MuMIn)
library(car)
#library(doBy)
#library(doSNOW)
library(ggplot2)
#library(corrr)
library(lme4)
library(cowplot)
library(multcomp)
library(emmeans)
#install.packages("glmmTMB")
library(glmmTMB)
#install.packages("DHARMa")
library(DHARMa)
#library(performance)

name <- "2024_Wudjari_bait_comp"

# Read in the formatted data


# read in habitat data
habitat <- readRDS("./data/tidy/2024_Wudjari_bait_comp_full.habitat.rds")%>%
  glimpse()

## MaxN by STAGE

maxn.stage <- readRDS("./data/tidy/2024_Wudjari_bait_comp_count.maxn.stage.RDS") %>%
  dplyr::mutate(bait = as.factor(bait), location = as.factor(location), 
                site = as.factor(site), stage = as.factor(stage))%>%
  dplyr::mutate(depth_m = as.numeric(depth_m), 
                longitude_dd = as.numeric(longitude_dd),
                latitude_dd = as.numeric(latitude_dd))%>%
  dplyr::mutate(date = substr(date_time, 1, 10))%>%
  dplyr::mutate(time = substr(date_time, 12, 19))%>%
  dplyr::mutate(date = as.factor(date))%>%
  left_join(habitat)%>%
  dplyr::filter(opcode != "046")%>%
  dplyr::filter(opcode != "078")%>% #remove drops only M F and AD recorded
  dplyr::filter(opcode != "082")%>% #remove drops only M F and AD recorded
  dplyr::filter(!stage %in% c("AD", "M", "F"))%>% #filtering out these
  dplyr::mutate(presence = ifelse(maxn > 0, 1, 0))%>%
  dplyr::mutate(titomaxn_s = periodtime * 60)%>% #creating covariate of time to maxn in seconds only
  dplyr::mutate(titomaxn_m = periodtime)%>% #creating covariate of titomaxn in mins (same as periodtime)
  clean_names() %>% 
  glimpse()


length(unique(maxn.stage$opcode))
length(unique(maxn.stage$stage))
#96*8 = 768
#96*5 = 480 #after removing AD, F, M


##TODO
## sussing Time of day -- move up when done

maxn.stage$time_of_day <- as.POSIXct(maxn.stage$time, format = "%H:%M:%S")
# Convert to seconds since midnight

maxn.stage$time_sec <- as.numeric(format(maxn.stage$time_of_day, "%H")) * 3600 +
  as.numeric(format(maxn.stage$time_of_day, "%M")) * 60 +
  as.numeric(format(maxn.stage$time_of_day, "%S"))
maxn.stage$time_hr <- maxn.stage$time_sec / 3600

#with time as factor
maxn.stage$time_block <- cut(maxn.stage$time_hr,
                             breaks = c(0, 6, 12, 18, 24),
                             labels = c("Night", "Morning", "Afternoon", "Evening"),
                             right = FALSE)

####################
####### SUBSETTING BY SIZE CLASSES
### Juveniles 
juvies <- maxn.stage%>%
  filter(stage == "0300-0499 mm") %>%
  glimpse()


# Histogram of distribution
ggplot(juvies, aes(x = maxn)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Maxn Values",
       x = "Maxn Value",
       y = "Count") +
  # scale_y_continuous(
  #   breaks = c(0, 5, 10, 15), 
  #   limits = c(0, 15)) +
  # scale_x_continuous(
  #   breaks = c(0:8))+
  facet_wrap(.~stage, ncol = 2)+
  theme_cowplot()



## maxn.stage dynamite plot by bait
ggplot(juvies, aes(x = bait, y = maxn, fill = bait)) +
  stat_summary(fun = mean, geom = "bar", width = 0.6) +  # Bar plot with mean
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5, color = "black") +  # SE as error bars
  labs(x = "Bait", y = "mean MaxN +/- se") +
  theme_cowplot()+
  theme(legend.position = "none")


## maxn by depth_m
ggplot(juvies, aes(x = depth_m, y = maxn))+
  geom_jitter(alpha = 0.5)+
  geom_smooth(method = 'lm', se = T)+
  labs(x = "Depth (m)", y = "MaxN") + 
  theme_cowplot()+
  theme(legend.position = "none")

#
jv1 <- glmer(maxn ~ bait + depth_m + (1|location),
             data = juvies,
             family = "poisson")
summary(jv1)
Anova(jv1)


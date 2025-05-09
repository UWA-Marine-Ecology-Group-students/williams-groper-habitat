###################################################################################
####    HYPOTHESIS 1.2 - MAXN BY SIZE CLASSWILL BE GREATER WITH AB BAIT      ######
###################################################################################

rm(list=ls())

# libraries----
#library(devtools)
library(tidyverse)
#library(mgcv)
library(MuMIn)
library(car)
#library(doBy)
#library(doSNOW)
library(ggplot2)
#library(corrr)
library(lme4)
library(cowplot)
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

#ctrl - shift - m is %>% 
# alt - is <- 

## MaxN by STAGE

maxn.stage <- readRDS("./data/tidy/2024_Wudjari_bait_comp_count.maxn.stage.RDS") %>%
  dplyr::mutate(bait = as.factor(bait), location = as.factor(location), 
                site = as.factor(site), stage = as.factor(stage))%>%
  dplyr::mutate(depth_m = as.numeric(depth_m), 
                longitude_dd = as.numeric(longitude_dd),
                latitude_dd = as.numeric(latitude_dd))%>%
  dplyr::mutate(date = substr(date_time, 1, 10))%>%
  dplyr::mutate(time = substr(date_time, 12, 19))%>%
  # dplyr::mutate(date = as.factor(date))%>%
  # dplyr::group_by(opcode, stage)%>%
  # dplyr::slice_max(order_by = maxn, n=1, with_ties = FALSE)%>%
  # dplyr::ungroup()%>%
  left_join(habitat)%>%
  #dplyr::filter(!stage %in% c("AD", "M", "F"))%>% #filtering out these
  dplyr::filter(opcode != "046")%>%
  clean_names() %>% 
  glimpse()


#####################################################
####################################################
####### shits and giggles - analysing the stages
##TODOs -- need rows for the zeros for each size class missing from the samples

stmod <- glmmTMB(maxn~bait + stage + (1|site),
                 data = maxn.stage,
                 family = "poisson")

summary(stmod)


stmod2 <- glmmTMB(maxn~bait*stage + (1|site),
                  data = maxn.stage,
                  family = 'poisson')
summary(stmod2)

unique(maxn.stage$stage)

juvies <- maxn.stage %>%
  dplyr::filter(stage %in% c("0300-0499 mm"))%>%
  glimpse()

# ggplot(juvies, aes(x = maxn)) +
#   geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
#   labs(title = "Histogram of Maxn Values",
#        x = "Maxn Value",
#        y = "Count")

j1 <- glmmTMB

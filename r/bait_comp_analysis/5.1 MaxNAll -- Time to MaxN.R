###########################################
####    TIME TO MAXN -- MAXN.ALL     ######
####        SURVIVAL ANALYSIS        ######
####        TIME TO EVENT DATA       ######
###########################################

rm(list=ls())

library(tidyverse)
# install.packages("survival")
#library(survival)
# install.packages("coxme")
library(coxme)

name <- "2024_Wudjari_bait_comp"


# read in habitat data
habitat <- readRDS("./data/tidy/2024_Wudjari_bait_comp_full.habitat.rds")%>%
  glimpse()


### MaxN (all) 

maxn.all <- readRDS("./data/tidy/2024_Wudjari_bait_comp_count.maxn.all.RDS") %>%
  dplyr::select(-c(behaviour_success, approach_success))%>%
  # dplyr::select(-c(titomaxn_s, titomaxn_m, periodtime))%>% NAs only exist in these columns
  left_join(habitat)%>% #joining to habitat 
  dplyr::filter(opcode != "046")%>% #no habitat data for 046
  dplyr::mutate(event = ifelse(is.na(titomaxn_m), 0, 1))%>% #need event column
  dplyr::mutate(titomaxn_m = if_else(is.na(titomaxn_m), 60, titomaxn_m))%>% #setting videos with no BG to 60mins
  clean_names()%>%
  glimpse()

which(is.na(maxn.all$titomaxn_m)) #should be no nas now


mod <- coxme(Surv(titomaxn_m, event) ~ bait + (1 | location/site), 
             data = maxn.all)

summary(mod)

##TODO run from here 
library(survival) #for coxph
library(survminer)

# You’ll need to use coxph() for plotting survival curves
mod_plot <- coxph(Surv(titomaxn_m, event) ~ bait, data = maxn.all)

fit <- survfit(mod_plot)

ggsurvplot(fit, data = maxn.all, pval = TRUE, risk.table = TRUE)


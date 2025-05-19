####################################################
##
##    TIME OF ARRIVAL MODELLING
##
####################################################

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
#library(dplyr)
library(lme4)
library(cowplot)
library(emmeans)
#install.packages("performance")
library(glmmTMB)
#install.packages("glmm")
library(glmm)
#(performance)
#citation("emmeans")
#RStudio.Version()
library(leaflet)

name <- "2024_Wudjari_bait_comp"


# read in habitat data
habitat <- readRDS("./data/tidy/2024_Wudjari_bait_comp_full.habitat.rds")%>%
  glimpse()


# read in TOA


time.of.arrival <- readRDS("./data/tidy/2024_Wudjari_bait_comp_time.of.arrival.rds") %>%
  dplyr::mutate(bait = as.factor(bait), location = as.factor(location),
                site = as.factor(site))%>%
  dplyr::mutate(depth_m = as.numeric(depth_m),
                longitude_dd = as.numeric(longitude_dd),
                latitude_dd = as.numeric(latitude_dd))%>%
  dplyr::mutate(date = substr(date_time, 1, 10))%>%
  dplyr::mutate(time = substr(date_time, 12, 19))%>%
  dplyr::mutate(date = as.factor(date))%>%
  left_join(habitat)%>% #joining to habitat
  dplyr::filter(opcode != "046")%>% #no habitat data for 046
  dplyr::mutate(toa_s = periodtime * 60)%>% #creating covariate of toa in seconds only
  dplyr::mutate(toa_m = periodtime)%>% #creating covariate of toa in mins (same as periodtime)
  dplyr::mutate(time_of_day = as.POSIXct(time, format = "%H:%M:%S"))%>%
  dplyr::mutate(time_secs = as.numeric(format(time_of_day, "%H")) * 3600 + 
                  as.numeric(format(time_of_day, "%M")) * 60 +
                  as.numeric(format(time_of_day, "%S")))%>%
  dplyr::mutate(time_hr = time_secs / 3600)%>%
  clean_names()%>%
  glimpse()


toa.wo.stage <- time.of.arrival %>%
  dplyr::group_by(opcode) %>%
  dplyr::slice_min(periodtime, with_ties = FALSE) %>%
  dplyr::ungroup()%>%
  glimpse()


#### modelling earliest time of arrival without stage
# distribution family checking
#visualsing to select family

#density plot
ggplot(toa.wo.stage, aes(x = toa_s)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  theme_minimal() +
  labs(title = "Density Plot of Time of arrival in seconds",
       x = "Time (seconds)", y = "Density") +
  facet_wrap(.~bait, ncol = 3)


#modelling
#library(glmmTMB) because glmer() in lme4 package doesn't support
#gamma distribution

model <- glmmTMB(toa_s ~ bait + (1 | location/site),
                 data = toa.wo.stage,
                 family = Gamma(link = "log"))

summary(model)
Anova(model)

model2 <- glmmTMB(toa_s ~ bait + time_hr + (1 | location),
                  data = toa.wo.stage,
                  family = Gamma(link = "log"))

AIC(model, model2)

model3 <- glmmTMB(toa_s ~ bait + date + (1 | location),
                  data = toa.wo.stage,
                  family = Gamma(link = "log"))

AIC(model, model3)

model4 <- glmmTMB(toa_s ~ bait + depth_m + (1 | location/site),
                  data = toa.wo.stage,
                  family = Gamma(link = "log"))

AIC(model, model4)

model5 <- glmmTMB(toa_s ~ bait + ecklonia + (1 | location),
                  data = toa.wo.stage,
                  family = Gamma(link = "log"))
AIC(model4, model5)

model6 <- glmmTMB(toa_s ~ bait + location + (1 | date),
                  data = toa.wo.stage,
                  family = Gamma(link = "log"))
AIC(model4, model6)

model7 <- glmmTMB(toa_s ~ bait +  (1 | site),
                  data = toa.wo.stage,
                  family = Gamma(link = "log"))
AIC(model4, model7)

model8 <- glmmTMB(toa_s ~ bait +  (1 | location/site),
                  data = toa.wo.stage,
                  family = Gamma(link = "log"))
AIC(model4, model8)

model9 <- glmmTMB(toa_s ~ bait + mean_relief +  (1 | location),
                 data = toa.wo.stage,
                 family = Gamma(link = "log"))

AIC(model4, model9)

model10 <- glmmTMB(toa_s ~ bait + macroalgae +  (1 | location),
                  data = toa.wo.stage,
                  family = Gamma(link = "log"))

AIC(model4, model10)

#best model posthocs
post <- emmeans(model4, ~ bait)
pairs(post)


ggplot(toa.wo.stage, aes(x = bait, y = toa_s)) +
  geom_boxplot(fill = "lightgreen") +
  theme_minimal() +
  labs(title = "Time of Arrival by Bait Type",
       x = "Bait", y = "Time (seconds)")



#### modelling earliest time of arrival with stage
# distribution family checking
#visualsing to select family

#density plot
ggplot(time.of.arrival, aes(x = toa_s)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  theme_minimal() +
  labs(title = "Density Plot of Time of arrival in seconds",
       x = "Time (seconds)", y = "Density") +
  facet_wrap(.~bait, ncol = 3)


#modelling
#library(glmmTMB) because glmer() in lme4 package doesn't support
#gamma distribution

model <- glmmTMB(toa_s ~ bait + stage + (1 | opcode/location),
                 data = time.of.arrival,
                 family = Gamma(link = "log"))

summary(model)
Anova(model)

model2 <- glmmTMB(toa_s ~ bait + stage + (1 | opcode),
                 data = time.of.arrival,
                 family = Gamma(link = "log"))
 
AIC(model, model2) #model2 better

model3 <- glmmTMB(toa_s ~ bait + stage + depth_m + (1 | opcode),
                  data = time.of.arrival,
                  family = Gamma(link = "log"))

AIC(model2, model3)

model4 <- glmmTMB(toa_s ~ bait + stage + date + (1 | opcode),
                  data = time.of.arrival,
                  family = Gamma(link = "log"))

AIC(model2, model4)

model5 <- glmmTMB(toa_s ~ bait + stage + time_hr + (1 | opcode),
                  data = time.of.arrival,
                  family = Gamma(link = "log"))
AIC(model2, model5)

# model6 <- glmmTMB(toa_s ~ bait + stage + time_secs + (1 | opcode),
#                   data = time.of.arrival,
#                   family = Gamma(link = "log")) -- didn't converge

model6 <- glmmTMB(toa_s ~ bait + stage + site + (1 | opcode),
                  data = time.of.arrival,
                  family = Gamma(link = "log"))
AIC(model2, model6)

#actual best model is model7 but I need to include stage somewhere otherwise not good
model7 <- glmmTMB(toa_s ~ bait + (1 | opcode),
                  data = time.of.arrival,
                  family = Gamma(link = "log"))

AIC(model2, model7) 

model8  <- glmmTMB(toa_s ~ bait + (1 | opcode) + (1|stage),
                         data = time.of.arrival,
                         family = Gamma(link = "log"))
AIC(model2, model8)
AIC(model7, model8)
########
# BEST MDOEL

best <- glmmTMB(toa_s ~ bait + stage + (1 | opcode),
                  data = time.of.arrival,
                  family = Gamma(link = "log"))

summary(best) #model2 better



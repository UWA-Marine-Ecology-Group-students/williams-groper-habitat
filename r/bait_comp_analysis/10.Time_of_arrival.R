####################################################
##
##    TIME OF ARRIVAL MODELLING
##
####################################################

rm(list=ls())

# libraries----
#library(devtools)
library(CheckEM)
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
  left_join(habitat)%>% #joining to habitat
  dplyr::filter(opcode != "046")%>% #no habitat data for 046
  dplyr::mutate(toa_s = time_of_arrival * 60)%>% #creating covariate of toa in seconds only
  dplyr::mutate(toa_m = time_of_arrival)%>%
  clean_names()%>%
  glimpse() #dataframe only includes videos where blue groper were seen 

##TODO -- go back to extraction code

test <- time.of.arrival

## removing stage from the data
# toa.wo.stage <- time.of.arrival %>%
#   dplyr::group_by(opcode) %>%
#   dplyr::slice_min(time_of_arrival, with_ties = FALSE) %>%
#   dplyr::ungroup()%>%
#   glimpse()


is.na(time.of.arrival$toa_s)
length(unique(time.of.arrival$opcode))

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

###  checking model fit before comparing with other models

library(DHARMa)

# Simulate residuals from the model
simres_toa <- simulateResiduals(fittedModel = model, plot = TRUE)

# Dispersion test
testDispersion(simres_toa)

# Optionally: test for outliers, uniformity, and other assumptions
testOutliers(simres_toa)
testUniformity(simres_toa)
testZeroInflation(simres_toa)  # Not crucial for Gamma models, but can be run


model2 <- glmmTMB(toa_s ~ bait + time_hr + (1 | location/site),
                  data = toa.wo.stage,
                  family = Gamma(link = "log"))

AIC(model, model2)

model3 <- glmmTMB(toa_s ~ bait + date + (1 | location/site),
                  data = toa.wo.stage,
                  family = Gamma(link = "log"))

AIC(model, model3)

model4 <- glmmTMB(toa_s ~ bait + depth_m + (1 | location/site),
                  data = toa.wo.stage,
                  family = Gamma(link = "log"))

AIC(model, model4)

model5 <- glmmTMB(toa_s ~ bait + ecklonia + (1 | location/site),
                  data = toa.wo.stage,
                  family = Gamma(link = "log"))
AIC(model, model5)


model9 <- glmmTMB(toa_s ~ bait + mean_relief +  (1 | location/site),
                 data = toa.wo.stage,
                 family = Gamma(link = "log"))

AIC(model, model9)

model10 <- glmmTMB(toa_s ~ bait + macroalgae +  (1 | location/site),
                  data = toa.wo.stage,
                  family = Gamma(link = "log"))

AIC(model, model10)

model10 <- glmmTMB(toa_s ~ bait + time_block +  (1 | location/site),
                   data = toa.wo.stage,
                   family = Gamma(link = "log"))

AIC(model, model10)


#best model posthocs
post <- emmeans(model, ~ bait)
pairs(post)


ggplot(toa.wo.stage, aes(x = bait, y = toa_s)) +
  geom_boxplot(fill = "lightgreen") +
  theme_minimal() +
  labs(title = "Time of Arrival by Bait Type",
       x = "Bait", y = "Time (seconds)")

# pretty much indicating that they are already in the area where we are dropping because we are biasing towards their habitat 

#### modelling earliest time of arrival with stage
# distribution family checking
#visualsing to select family

#density plot
ggplot(time.of.arrival, aes(x = toa_s, color = bait)) +
  # geom_density(fill = "skyblue", alpha = 0.5) +
  geom_density(alpha = 0.5) + 
  theme_minimal() +
  labs(title = "Density Plot of Time of arrival in seconds",
       x = "Time (seconds)", y = "Density") +
  facet_wrap(.~stage, ncol = 3)


#modelling
#library(glmmTMB) because glmer() in lme4 package doesn't support
#gamma distribution

# model <- glmmTMB(toa_s ~ bait + stage + 
#                 (1 | opcode) + (1 |location/site), ## I don't think it makes sense to have both
#                  data = time.of.arrival,
#                  family = Gamma(link = "log"))
# 
# summary(model)
# Anova(model)

model2 <- glmmTMB(toa_s ~ bait + stage + (1 | opcode),
                 data = time.of.arrival,
                 family = Gamma(link = "log"))
 
summary(model2)

model3 <- glmmTMB(toa_s ~ bait + stage + depth_m + (1 | opcode),
                  data = time.of.arrival,
                  family = Gamma(link = "log"))

AIC(model2, model3)

# model4 <- glmmTMB(toa_s ~ bait + stage + date + (1 | opcode),
#                   data = time.of.arrival,
#                   family = Gamma(link = "log"))

# AIC(model2, model4)

model5 <- glmmTMB(toa_s ~ bait + stage + time_hr + (1 | opcode),
                  data = time.of.arrival,
                  family = Gamma(link = "log"))
AIC(model2, model5)

# model6 <- glmmTMB(toa_s ~ bait + stage + time_secs + (1 | opcode),
#                   data = time.of.arrival,
#                   family = Gamma(link = "log")) #-- didn't converge


# model6 <- glmmTMB(toa_s ~ bait + stage + site + (1 | opcode),
#                   data = time.of.arrival,
#                   family = Gamma(link = "log"))
# AIC(model2, model6)

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

model9  <- glmmTMB(toa_s ~ bait  + (1|stage) + (1 | opcode),
                   data = time.of.arrival,
                   family = Gamma(link = "log"))

AIC(model8, model9) # no difference

########
# BEST MDOEL

best <- glmmTMB(toa_s ~ bait + (1 | opcode) + (1|stage),
                data = time.of.arrival,
                family = Gamma(link = "log"))

summary(best) 


#best model posthocs
post <- emmeans(best, ~ bait)
pairs(post)


ggplot(time.of.arrival, aes(x = bait, y = toa_s)) +
  geom_boxplot(fill = "lightgreen") +
  theme_minimal() +
  labs(title = "Time of Arrival by Bait Type",
       x = "Bait", y = "Time (seconds)")

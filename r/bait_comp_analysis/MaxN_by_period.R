#############################################
####### Hypothesis - Time - BG will appear earlier in the video

rm(list=ls())

# libraries----
library(devtools)
library(tidyverse)
library(mgcv)
library(MuMIn)
library(car)
library(doBy)
library(doSNOW)
library(ggplot2)
library(corrr)
library(dplyr)
library(lme4)
library(ggplot2)
library(cowplot)
#install.packages("emmeans")
library(emmeans)

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


#visualise data


ggplot(maxn.all, aes(x = period, y = maxn)) +
  geom_boxplot() +
  labs(x = "Period", y = "MaxN", title = "MaxN by Period")+
  scale_y_continuous(
    breaks = c(0, 5, 10, 15), 
    limits = c(0,15)) +
  theme_cowplot()+
  facet_wrap(~bait)


ggplot(maxn.all, aes())

mod1 <- aov(maxn~period, data = maxn.all)
summary(mod1)
anova(mod1)
TukeyHSD(mod1)

mod2 <-aov(maxn~period*bait, data = maxn.all)
summary(mod2)

mod3 <- aov(maxn~period + bait, data = maxn.all)
anova(mod3)

mod4 <- lmer(maxn~period + (1|location), data = maxn.all)
anova(mod4)
summary(mod4)

post4 <- emmeans(mod4, pairwise~period) #posthoc analysis of lmer

mod5 <- lmer(maxn~period + (1|location) + (1|date), data = maxn.all)
anova(mod5)
summary(mod5)

emmeans(mod5, pairwise~period) #posthoc analysis of lmer
anova(mod4, mod5)

mod6 <- lmer(maxn~period + bait + (1|location), data = maxn.all)
anova(mod6)
summary(mod6)
anova(mod4, mod6)

plot(post4)

############################################################
### MaxN by stage and period - summed

sum.stage <- readRDS("./data/tidy/2024_Wudjari_bait_comp_count.sum.stage.RDS") %>%
  dplyr::mutate(species = "gouldii", bait = as.factor(bait), location = as.factor(location))%>%
  dplyr::mutate(depth_m = as.numeric(depth_m))%>%
  dplyr::mutate(period = as.factor(period))%>%
  dplyr::mutate(date = substr(date_time, 1, 10))%>%
  dplyr::mutate(time = substr(date_time, 12, 19))%>%
  dplyr::mutate(date = as.factor(date))%>%
  dplyr::rename(maxn = maxn_period)%>%
  #dplyr::mutate(date = as.numeric(julian(date)))%>%
  glimpse()


## visualise

ggplot(sum.stage, aes(x = period, y = maxn)) +
  geom_boxplot() +
  labs(x = "Period", y = "MaxN", title = "MaxN by Period with size classes")+
  scale_y_continuous(
    breaks = c(0, 5, 10, 15), 
    limits = c(0,15)) +
  theme_cowplot()+
  facet_wrap(~bait)


## models

ms1 <- aov(maxn~period, data = sum.stage)
summary(ms1)
anova(ms1)
TukeyHSD(ms1)

ms2 <- aov(maxn~period*bait, data = sum.stage)
anova(ms2)
TukeyHSD(ms2)
anova(ms1, ms2)

ms3 <- aov(maxn~period + bait, data = sum.stage)
anova(ms3)
TukeyHSD(ms3)

ms4 <- lmer(maxn~period + bait + (1|location), data = sum.stage)
summary(ms4)

ms5 <- lmer(maxn~period + bait + (1|location) + (1|date), data = sum.stage)
summary(ms5)

anova(ms4, ms5)

ph4 <- emmeans(ms4, pairwise~period)
print(ph4)
plot(ph4)

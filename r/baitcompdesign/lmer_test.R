#################################
###                           ###
### LMER for Blue Gropers     ###
###                           ###
#################################
rm(list=ls())

library(dplyr)
library(plyr)

library(lmerTest)
library(ggplot2)
library(MuMIn)
library(multcomp)

grops = read.csv("./data/raw/test_permanova.csv", stringsAsFactors = T)

str(grops)
summary(grops)

#################################
####LMER 
#################################

### abundance by bait treatment - location as random effect
m1 = lmer(maxn~treatment + (1|location),
                  data = grops)

summary(m1)
anova(m1)
step(m1)
r.squaredGLMM(m1)


#### abundance by bait treatment - location as interaction

lm2 = lm(maxn~treatment*location,
                data = grops)

anova(lm2)
step(lm2)


lm3 = lm(maxn~treatment+location,
           data = grops)

anova(lm2, lm3)
anova(lm3)


##############
##### GLM test -- matt said to use this
##############

library(lme4)
plot(maxn~treatment, data = grops)

glm1 <- lmer(maxn ~ treatment + (1|location), data = grops)
summary(glm1)


## with only 3 locations

df2 <- df %>%
  filter(location != "clg")%>%
  glimpse()

glm2 <- lmer(maxn ~ treatment + (1|location), data = df2)

summary(glm2)

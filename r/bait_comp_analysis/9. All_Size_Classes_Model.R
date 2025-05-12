###################################################################################
####    HYPOTHESIS 1.2 - MAXN BY SIZE CLASSWILL BE GREATER WITH AB BAIT      ######
###################################################################################

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
  clean_names() %>% 
  glimpse()

length(unique(maxn.stage$opcode))
length(unique(maxn.stage$stage))
#96*8 = 768
#96*5 = 480 #after removing AD, F, M

# #checking for drops with AD, M, F recorded - then removed above
# sub <- maxn.stage %>%
#   group_by(opcode) %>%
#   filter(sum(stage %in% c("AD", "F", "M") & maxn > 0) >1) %>%
#   ungroup()


## maxn.stage dynamite plot by bait
ggplot(maxn.stage, aes(x = bait, y = maxn, fill = bait)) +
  stat_summary(fun = mean, geom = "bar", width = 0.6) +  # Bar plot with mean
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5, color = "black") +  # SE as error bars
  labs(x = "Bait", y = "mean MaxN +/- se") +
  facet_wrap(.~stage, ncol = 2)+
  theme_cowplot()+
  theme(legend.position = "none")

# maxn.stage maxn by other variables - remove after this
# ggplot(maxn.stage, aes(x = reef, y = maxn)) +
#   geom_jitter(alpha = 0.5) +
#   geom_smooth(method = "lm", colour = "darkgreen", se = TRUE)+
#   theme_cowplot()+
#   facet_wrap(.~stage, ncol = 2)+
#   theme(legend.position = "none")


maxn.stage %>%
  group_by(bait) %>%
  summarise(
    mean_maxn = mean(maxn, na.rm = TRUE),
    median_maxn = median(maxn, na.rm = TRUE),
    min_maxn = min(maxn, na.rm = TRUE),
    max_maxn = max(maxn, na.rm = TRUE),
    sd_maxn = sd(maxn, na.rm = TRUE),
    range_maxn = max_maxn - min_maxn)


#plot freq
ggplot(maxn.stage, aes(x = maxn)) +
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

##################################
## Stepwise modelling approach

sc1 <- glmer(maxn ~ bait + stage + (1|location), data = maxn.stage,
            family = "poisson")

summary(sc1)
Anova(sc1)
#r2_nakagawa(p1) #r2 using performance package

## CHECKING OVERDISPERSION OF POISSON DISTRIBUTION
#deviance / residual degrees of freedom

deviance(sc1)/df.residual(sc1)

# Compare the mean and variance of response
mean(maxn.stage$maxn)/var(maxn.stage$maxn)


#plotting residuals

r <- residuals(sc1)

# Plot residuals - if systematic patterns (ie funnel shape) 
# indicates heteroscedasticity
# also look for large residuals not explained by the model
plot(r, main = "Residuals from Poisson MaxN(stage)", 
     xlab = "Index", ylab = "Residuals")

pears.sc1 <- residuals(sc1, type = "pearson")
var(pears.sc1) #looks like there are some patterns

# running some posthocs
post <- emmeans(sc1, ~ bait)  # Specify the fixed factor of interest

# Perform pairwise comparisons
pairs(post)

## visualising post-hoc tests

pairwise_results <- contrast(post, method = "pairwise")

# Convert pairwise results to a data frame
pairwise_df <- as.data.frame(pairwise_results)

#plot
ggplot(pairwise_df, aes(x = contrast, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = estimate - SE, ymax = estimate + SE), width = 0.2) +
  labs(x = "Pairwise Comparisons", y = "Estimate", title = "MaxN(stage) ~ bait + stage") +
  theme_minimal() +
  coord_flip()

#posthoc on stage
post2 <- emmeans(sc1, ~ stage)  # Specify the fixed factor of interest

# Perform pairwise comparisons
pairs(post2)

## visualising post-hoc tests

pairwise_results <- contrast(post2, method = "pairwise")

# Convert pairwise results to a data frame
pairwise_df <- as.data.frame(pairwise_results)

#plot
ggplot(pairwise_df, aes(x = contrast, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = estimate - SE, ymax = estimate + SE), width = 0.2) +
  labs(x = "Pairwise Comparisons", y = "Estimate", title = "MaxN(stage) ~ bait + stage") +
  theme_minimal() +
  coord_flip()


## comparing with site
sc2 <- glmer(maxn ~ bait + stage + (1|site), data = maxn.stage,
             family = "poisson")
summary(sc2)
Anova(sc2)

deviance(sc2)/df.residual(sc2)

#plotting residuals

r <- residuals(sc2)
plot(r, main = "Residuals from Poisson MaxN(stage)", 
     xlab = "Index", ylab = "Residuals")
#same residual patterns 

pears.sc2 <- residuals(sc2, type = "pearson")
var(pears.sc2) #looks like there are some patterns

## interaction between bait and stage
sc3 <- glmer(maxn ~ bait*stage + (1|location), data = maxn.stage,
             family = "poisson")
summary(sc3)
Anova(sc3) #no interaction effect

## depth
sc4 <- glmer(maxn ~ bait +stage + depth_m +(1|location), data = maxn.stage,
             family = "poisson")
summary(sc4)
Anova(sc4)

deviance(sc4)/df.residual(sc4)

#plotting residuals
r <- residuals(sc4)
plot(r, main = "Residuals from Poisson MaxN(stage)", 
     xlab = "Index", ylab = "Residuals")
#same weird shape

pears.sc4 <- residuals(sc4, type = "pearson")
var(pears.sc4) 

## mean relief
sc5 <- glmer(maxn ~ bait + stage + mean_relief +(1|location), data = maxn.stage,
             family = "poisson")
summary(sc5)
Anova(sc5)

deviance(sc5)/df.residual(sc5)

#plotting residuals
r <- residuals(sc5)
plot(r, main = "Residuals from Poisson MaxN(stage)", 
     xlab = "Index", ylab = "Residuals")
#same weird shape

pears <- residuals(sc5, type = "pearson")
var(pears) 

## ecklonia
sc6 <- glmer(maxn ~ bait + stage + ecklonia +(1|location), data = maxn.stage,
             family = "poisson")
summary(sc6)
Anova(sc6)

## scytothalia
sc7 <- glmer(maxn ~ bait + stage + scytothalia +(1|location), data = maxn.stage,
             family = "poisson")
summary(sc7)
Anova(sc7)

## macroalgae
sc8 <- glmer(maxn ~ bait + stage + macroalgae +(1|location), data = maxn.stage,
             family = "poisson")
summary(sc8)
Anova(sc8)

## depth_m no bait
sc9 <- glmer(maxn ~ stage + depth_m + (1|location), data = maxn.stage,
             family = "poisson")
summary(sc9)
Anova(sc9)

############################################################################
############################################################################
## SUBSETTING SIZE CLASSES

unique(maxn.stage$stage)


elders <- maxn.stage%>%
  filter(stage == "1100-1299mm") %>%
  glimpse()

eld1 <- glmer(maxn ~ bait + (1|location), data = elders,
              family = "poisson")
summary(eld1)
deviance(eld1)/df.residual(eld1)
pears <- residuals(eld1, type = "pearson")
var(pears) 
#plotting residuals
r <- residuals(eld1)
plot(r,  
     xlab = "Index", ylab = "Residuals")

##refit with NB
eldnb <- glmmTMB(maxn ~ bait + (1|location), 
               data = elders, 
               family = "nbinom2")

summary(eldnb)
Anova(eldnb)
deviance(eldnb)/df.residual(eldnb)
pears <- residuals(eldnb, type = "pearson")
var(pears) #0.94 indicates should be poisson?

#with DHARMa package - check residuals
sim_res <- simulateResiduals(fittedModel = eldnb)
plot(sim_res)

eld2 <- glmer(maxn~bait + depth_m + (1|location),
              data = elders,
              family = "poisson")
summary(eld2)
Anova(eld2)

### Juveniles 
juvies <- maxn.stage%>%
  filter(stage == "0300-0499 mm") %>%
  glimpse()

jv1 <- glmer(maxn ~ bait + depth_m + (1|location),
             data = juvies,
             family = "poisson")
summary(jv1)
Anova(jv1)

### Running as presence/absence
test <- maxn.stage
test$presence <- ifelse(test$maxn > 0, 1, 0)

mod <- glmmTMB(presence ~ bait + stage + depth_m + (1 | location),
  family = binomial,
  data = test)

summary(mod)
Anova(mod)

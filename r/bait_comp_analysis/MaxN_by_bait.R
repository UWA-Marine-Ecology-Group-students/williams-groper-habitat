####################################################################
####    HYPOTHESIS 1 - MAXN WILL BE GREATER WITH AB BAIT      ######
####################################################################

rm(list=ls())

# libraries----
#library(devtools)
library(tidyverse)
#library(mgcv)
#library(MuMIn)
#library(car)
#library(doBy)
#library(doSNOW)
library(ggplot2)
#library(corrr)
#library(dplyr)
library(lme4)
library(cowplot)
library(emmeans)
#install.packages("glmmTMB")
library(glmmTMB)

name <- "2024_Wudjari_bait_comp"

# Read in the formatted data

############################################################
######        HYPOTHESIS 1 = MaxN will be higher with abalone
####################

# read in habitat data
habitat <- readRDS("./data/tidy/2024_Wudjari_bait_comp_full.habitat.rds")%>%
  dplyr::rename(inverts = "Sessile invertebrates", rock = "Consolidated (hard)", 
                sand = "Unconsolidated (soft)")%>%
  glimpse()

#creating a df for adding more specific site names
site <- data.frame(
  opcode = sprintf("%03d", 001:108), 
  stringsAsFactors = FALSE) %>%
  dplyr::mutate(site= case_when(
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

### MaxN (all) 

maxn.all <- readRDS("./data/tidy/2024_Wudjari_bait_comp_count.maxn.all.RDS") %>%
  dplyr::mutate(species = "gouldii", bait = as.factor(bait), location = as.factor(location))%>%
  dplyr::mutate(depth_m = as.numeric(depth_m))%>%
  dplyr::mutate(period = as.factor(period))%>%
  dplyr::mutate(date = substr(date_time, 1, 10))%>%
  dplyr::mutate(time = substr(date_time, 12, 19))%>%
  dplyr::mutate(date = as.factor(date))%>%
  dplyr::mutate(longitude_dd = as.numeric(longitude_dd),
                latitude_dd = as.numeric(latitude_dd)) %>%
  dplyr::group_by(opcode)%>%
  dplyr::slice_max(order_by = maxn, n=1, with_ties = FALSE)%>% # sliced the highest maxN by opcode 
  dplyr::ungroup()%>%
  left_join(habitat)%>% #joining to habitat 
  left_join(site)%>%
  dplyr::mutate(site = as.factor(site))%>%
  glimpse()

summary(maxn.all)

## summary stas for bait

aggregate(maxn ~ bait, data = maxn.all, FUN = mean)
aggregate(maxn ~ bait, data = maxn.all, FUN = median)
aggregate(maxn ~ bait, data = maxn.all, FUN = min)
aggregate(maxn ~ bait, data = maxn.all, FUN = max)

## summary stas for location

aggregate(maxn ~ location, data = maxn.all, FUN = mean)
aggregate(maxn ~ location, data = maxn.all, FUN = median)
aggregate(maxn ~ location, data = maxn.all, FUN = min)
aggregate(maxn ~ location, data = maxn.all, FUN = max)

## summary stas for date

aggregate(maxn ~ date, data = maxn.all, FUN = mean)
aggregate(maxn ~ date, data = maxn.all, FUN = median)
aggregate(maxn ~ date, data = maxn.all, FUN = min)
aggregate(maxn ~ date, data = maxn.all, FUN = max)
## plot Freq. distribution of MaxNs ## plot Frmin()eq. distribution of MaxNs 
hist(maxn.all$maxn)

#sexier
ggplot(maxn.all, aes(x = maxn)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Maxn Values",
       x = "Maxn Value",
       y = "Count") +
  scale_y_continuous(
    breaks = c(0, 10, 20, 30), 
    limits = c(0, 30)) +
  scale_x_continuous(
  breaks = c(0, 1, 2, 3, 4, 5))+
  theme_cowplot()

##### plot of mean maxns per bait type

ggplot(maxn.all, aes(x = bait, y = maxn, fill = bait)) +
  geom_jitter(alpha = 0.5) +
  labs(x = "Bait", y = "MaxN", title = "MaxN by Bait")+
  scale_y_continuous(
    breaks = c(0, 5, 10, 15), 
    limits = c(0,15)) +
  theme_cowplot() +
stat_summary( geom = "point", fun.y = "mean", col = "black", size = 3, shape = 24, fill = "red" )

################################################################################
################################################################################ 
## Running with habitat in it

p1 <- glmer(maxn ~ bait + (1|site), data = maxn.all,
            family = "poisson")

summary(p1)
anova(p1)

#r.squaredGLMM(p1)

## CHECKING OVERDISPERSION OF POISSON DISTRIBUTION
#deviance / residual degrees of freedom

deviance(p1)/df.residual(p1)

# Compare the mean and variance of response
mean(maxn.all$maxn)
var(maxn.all$maxn)


#plotting residuals

r <- residuals(p1)

# Plot residuals - if systematic patterns (ie funnel shape) indicates heteroscedasticity
# also look for large residuals not explained by the model
plot(r, main = "Residuals from Poisson MaxN", 
     xlab = "Index", ylab = "Residuals")


### with mean.relief
p2 <- glmer(maxn ~ bait + mean.relief + (1|site),
            data = maxn.all,
            family = "poisson")
summary(p2)
anova(p2)

deviance(p2)/df.residual(p2)

r2 <- residuals(p2)
plot(r2, main = "MaxN~Bait + Mean.relief", 
     xlab = "Index", ylab = "Residuals")

### with mean.relief & Scytothalia

p3 <- glmer(maxn ~ bait + mean.relief + Scytothalia + (1|site),
            data = maxn.all,
            family = "poisson")
summary(p3)

deviance(r3)/df.residual(r3)

r3 <- residuals(p3)
plot(r3, main = "MaxN~Bait + Mean.relief + Scytothalia", 
     xlab = "Index", ylab = "Residuals")

### with Scytothalia

p4 <- glmer(maxn ~ bait +  Scytothalia + (1|site),
            data = maxn.all,
            family = "poisson")
summary(p4)

deviance(p4)/df.residual(p4)

r4 <- residuals(p4)
plot(r4, main = "MaxN~Bait + Scytothalia", 
     xlab = "Index", ylab = "Residuals")

## Post - hoc for location

post4 <- emmeans(p4, ~ bait)  # Specify the fixed factor of interest

# Perform pairwise comparisons
pairs(post4)

## visualising post-hoc tests

pairwise_results <- contrast(post4, method = "pairwise")

# Convert pairwise results to a data frame
pairwise_df <- as.data.frame(pairwise_results)

#plot
ggplot(pairwise_df, aes(x = contrast, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = estimate - SE, ymax = estimate + SE), width = 0.2) +
  labs(x = "Pairwise Comparisons", y = "Estimate", title = "MaxN ~ bait + Scytothalia") +
  theme_minimal() +
  coord_flip()

###
# with Ecklonia

p5 <- glmer(maxn ~ bait +  Ecklonia + (1|site),
            data = maxn.all,
            family = "poisson")
summary(p5)

deviance(p5)/df.residual(p5)


### with site nested in location for completeness?

p6 <-glmer(maxn~bait + Scytothalia + (site|location),
           data = maxn.all,
           family = "poisson")

summary(p6)
deviance(p6)/df.residual(p6)

#############################################################################
############################################################################
###   before habitat added

## GLMER using LME4 package

mod1 <- glmer(maxn ~ bait + (1|location), data = maxn.all,
             family = "poisson")

summary(mod1)
anova(mod1)

#r.squaredGLMM(mod1)

## CHECKING OVERDISPERSION OF POISSON DISTRIBUTION

# Alternatively, calculate deviance and residual degrees of freedom
deviance_value <- deviance(mod1)
df_residual <- df.residual(mod1)

# Calculate the dispersion statistic
dispersion_statistic <- deviance_value / df_residual
dispersion_statistic

# Mean and variance of the response variable
mean_response <- mean(maxn.all$maxn)
var_response <- var(maxn.all$maxn)

# Compare the mean and variance
mean_response
var_response

## Post - hoc (even tho not sig -- just looking)

postmod1 <- emmeans(mod1, ~ bait)  # Specify the fixed factor of interest

# Perform pairwise comparisons
pairs(postmod1)

#plotting residuals

r <- residuals(mod1)

# Plot residuals - if systematic patterns (ie funnel shape) indicates heteroscedasticity
# also look for large residuals not explained by the model
plot(r, main = "Residuals from Poisson MaxN no SC", 
     xlab = "Index", ylab = "Residuals")


##############
### Running with Location as fixed effect


mod2 <- glm(maxn ~ bait + location, data = maxn.all,
              family = "poisson")

summary(mod2)
anova(mod2)
logLik(mod2)
#r.squaredGLMM(mod1)

## CHECKING OVERDISPERSION OF POISSON DISTRIBUTION

# Alternatively, calculate deviance and residual degrees of freedom
# dispersion statistic
deviance(mod2)/df.residual(mod2)

# Mean and variance of the response variable
mean_response <- mean(maxn.all$maxn)
var_response <- var(maxn.all$maxn)

# Compare the mean and variance
mean_response
var_response

## Post - hoc for location

postmod2 <- emmeans(mod2, ~ location)  # Specify the fixed factor of interest

# Perform pairwise comparisons
pairs(postmod2)

## visualising post-hoc tests

pairwise_results <- contrast(postmod2, method = "pairwise")
summary(pairwise_results)

# Convert pairwise results to a data frame
pairwise_df <- as.data.frame(pairwise_results)

#plot
ggplot(pairwise_df, aes(x = contrast, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = estimate - SE, ymax = estimate + SE), width = 0.2) +
  labs(x = "Pairwise Comparisons", y = "Estimate", title = "Post Hocs from Poisson with No RE") +
  theme_minimal() +
  coord_flip()

#plotting residuals

r <- residuals(mod2)

# Plot residuals - if systematic patterns (ie funnel shape) indicates heteroscedasticity
# also look for large residuals not explained by the model
plot(r, main = "Residuals from Poisson MaxN no RE", 
     xlab = "Index", ylab = "Residuals")


###########################################
### RUNNING WITH NEGATIVE BINOMIAL JUST TO SEE

nb1 <- glmmTMB(maxn~bait + (1|location), 
        data = maxn.all, 
        family = "nbinom2")  # Negative Binomial with two parameters


summary(nb1)
### warning message - didn't like it ###

## checking for overdispersion
# dispersion statistic 

deviance(nb1)/df.residual(nb1)

#plotting residuals

r <- residuals(nb1)

# Plot residuals - if systematic patterns (ie funnel shape) indicates heteroscedasticity
# also look for large residuals not explained by the model
plot(r, main = "Residuals from Negative Binomial MaxN with RE(location)", 
     xlab = "Index", ylab = "Residuals")

## Pearson residuals - 
# checking to see if variance of residuals is larger than expected

pearson_residuals <- residuals(nb1, type = "pearson")

#Calculate variance of the pearson residuals - if around 1 indicates that poisson assumption
# of constant variance is actually reasonable - but as this is a negative binomial would expect
# it to be larger than 1
var(pearson_residuals)

# Plot Pearson residuals
plot(pearson_residuals, main = "Pearson Residuals - Negative Binomial - MaxN with RE(location)")


### Negative binomial with Location as fixed effect 

nb2 <- glmmTMB(maxn~bait + location, 
               data = maxn.all, 
               family = "nbinom2")  # Negative Binomial with two parameters


summary(nb2)
### warning message - didn't like it ###

## checking for overdispersion
# dispersion statistic 

deviance(nb2)/df.residual(nb2)

#plotting residuals

r <- residuals(nb2)

# Plot residuals - if systematic patterns (ie funnel shape) indicates heteroscedasticity
# also look for large residuals not explained by the model
plot(r, main = "Residuals from Negative Binomial MaxN no RE", 
     xlab = "Index", ylab = "Residuals")

## Pearson residuals - 
# checking to see if variance of residuals is larger than expected

pearson_residuals <- residuals(nb1, type = "pearson")

#Calculate variance of the pearson residuals - if around 1 indicates that poisson assumption
# of constant variance is actually reasonable - but as this is a negative binomial would expect
# it to be larger than 1
var(pearson_residuals)

# Plot Pearson residuals
plot(pearson_residuals, main = "Pearson Residuals - Negative Binomial - MaxN no RE")



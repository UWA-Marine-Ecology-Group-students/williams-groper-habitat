####################################################################
####    HYPOTHESIS 1 - MAXN WILL BE GREATER WITH AB BAIT      ######
####################################################################

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
#(performance)
#citation("emmeans")
#RStudio.Version()

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

# summary(maxn.all)

## summary stas for bait

# aggregate(maxn ~ bait, data = maxn.all, FUN = mean)

## plot Freq. distribution of MaxNs ## plot Frmin()eq. distribution of MaxNs 
#
# ggplot(maxn.all, aes(x = maxn)) +
#   geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
#   labs(title = "Histogram of Maxn Values",
#        x = "Maxn Value",
#        y = "Count") +
#   scale_y_continuous(
#     breaks = c(0, 10, 20, 30), 
#     limits = c(0, 30)) +
#   scale_x_continuous(
#   breaks = c(0, 1, 2, 3, 4, 5))+
#   theme_cowplot()
# 


##########################################################################
### BEST MODEl
####

best <- glmer(maxn ~ bait + Scytothalia +(1|site),
              data = maxn.all,
              family = "poisson")


summary(best)
Anova(best, type = "III")


# post <- emmeans(best, ~ bait)
# pairs(post)

## MODEL PLOTS
bait_col <- c("abalone" = "#27ae60", 
              "octopus" = "#f39c12" , 
              "pilchard" = "#CC79A7" )

## 'Dynamite' Plot of MaxN by Bait with post hocs

  
maxn.w.post <-
ggplot(maxn.all, aes(x= bait, y = maxn, fill = bait))+
  stat_summary(fun = mean, geom = "bar", color = "black", width = 0.6) +  # Bars
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +  # Error bars with standard error
  labs(x = "Bait Type", y = "Mean Abundance")+
  scale_fill_manual(values = bait_col)+
  scale_x_discrete(labels = c("Abalone", "Octopus", "Pilchard"))+
  theme_cowplot()+
  theme(legend.position = "none")+
  # # Abalone vs Octopus
  geom_segment(aes(x = 1, xend = 2, y = 1.7, yend = 1.7), linewidth = 0.6) +  
  geom_segment(aes(x = 1, xend = 1, y = 1.7, yend = 1.675), linewidth = 0.6) +  
  geom_segment(aes(x = 2, xend = 2, y = 1.7, yend = 1.675), linewidth = 0.6) +
  annotate("text", x = 1.5, y = 1.75, label = "p = 0.939", size = 3) +
  #Ab - Pilchard Line and Ticks
  geom_segment(aes(x = 1, xend = 3, y = 1.8, yend = 1.8), linewidth = 0.6) +  
  geom_segment(aes(x = 1, xend = 1, y = 1.8, yend = 1.775), linewidth = 0.6) +
  geom_segment(aes(x = 3, xend = 3, y = 1.8, yend = 1.775), linewidth = 0.6) +
  annotate("text", x = 2, y = 1.85, label = "p = 0.896", size = 3)+
  ## Octopus vs Pilchard
  geom_segment(aes(x = 2, xend = 3, y = 1.9, yend = 1.9), linewidth = 0.6) +  
  geom_segment(aes(x = 2, xend = 2, y = 1.9, yend = 1.875), linewidth = 0.6) +
  geom_segment(aes(x = 3, xend = 3, y = 1.9, yend = 1.875), linewidth = 0.6) +
  annotate("text", x = 2.5, y = 1.95, label = "p = 0.998", size = 3)

maxn.w.post
#################
## plot saving ##
#################
# 
folder_path <- "./plots/baitcomp/"

# change title
png(file.path(folder_path, "maxn.w.post.png"), width = 600, height = 400)

# plot code

maxn.w.post

# Close the PNG device
dev.off()

### MaxN by scytothalia with the actual model line
new_data <- expand.grid(
  Scytothalia = seq(min(maxn.all$Scytothalia), max(maxn.all$Scytothalia), length.out = 100),
  bait = unique(maxn.all$bait)[1]  # Choose first level if categorical
)

# Get predictions (fixed effects only)
new_data$predicted <- predict(best, newdata = new_data, re.form = NA, type = "response")

# Get predictions and standard errors
pred <- predict(best, newdata = new_data, re.form = NA, type = "link", se.fit = TRUE)

# Convert to response scale (Poisson uses log link)
new_data$predicted <- exp(pred$fit)
new_data$lower <- exp(pred$fit - 1.96 * pred$se.fit)  # 95% CI lower bound
new_data$upper <- exp(pred$fit + 1.96 * pred$se.fit)  # 95% CI upper bound

# Plot data with model predictions
scy<-
ggplot(maxn.all, aes(x = Scytothalia, y = maxn)) +
  geom_jitter(alpha = 0.5) +
  labs(x = "% cover of Scytothalia", y = "WBG Abundance") +
  geom_line(data = new_data, aes(x = Scytothalia, y = predicted), 
            colour = "darkgreen", linewidth = 1, inherit.aes = F) +
  geom_ribbon(data = new_data, aes(x = Scytothalia, ymin = lower, ymax = upper), 
              fill = "darkgreen", alpha = 0.2, inherit.aes = F) +  # Shaded confidence band
  theme_cowplot() +
  theme(legend.position = "none")

scy

#################
## plot saving ##
#################
# 
folder_path <- "./plots/baitcomp/"

# change title
png(file.path(folder_path, "scy.png"), width = 600, height = 400)

# plot code

scy

# Close the PNG device
dev.off()


################################################################################
################################################################################ 
## Running with habitat in it

p1 <- glmer(maxn ~ bait + (1|site), data = maxn.all,
            family = "poisson")

summary(p1)
anova(p1)
r.squaredGLMM(p1)

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
r.squaredGLMM(p2)

deviance(p2)/df.residual(p2)

r2 <- residuals(p2)
plot(r2, main = "MaxN~Bait + Mean.relief", 
     xlab = "Index", ylab = "Residuals")

### with mean.relief & Scytothalia

p3 <- glmer(maxn ~ bait + mean.relief + Scytothalia + (1|site),
            data = maxn.all,
            family = "poisson")
summary(p3)
r.squaredGLMM(p3)

deviance(r3)/df.residual(r3)

r3 <- residuals(p3)
plot(r3, main = "MaxN~Bait + Mean.relief + Scytothalia", 
     xlab = "Index", ylab = "Residuals")

### with Scytothalia

p4 <- glmer(maxn ~ bait +  Scytothalia + (1|site),
            data = maxn.all,
            family = "poisson")
summary(p4)
r.squaredGLMM(p4)
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

### with depth
p7 <-glmer(maxn~bait + depth_m + (1|site),
           data = maxn.all,
           family = "poisson")

summary(p7)

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

###########################################
## Likelihood ratio tests on best model

lrt1.both <- glmer(maxn ~ bait + Scytothalia + (1|location) + (1|site),
                      data = maxn.all,
                      family = "poisson")
lrt2.loc <- glmer(maxn ~ bait + Scytothalia + (1|location),
                  data = maxn.all,
                  family = "poisson")

lrt3.site <- glmer(maxn ~ bait + Scytothalia + (1|site),
                   data = maxn.all,
                   family = "poisson")

lrt4.nest <- glmer(maxn ~ bait + Scytothalia + (site|location),
                   data = maxn.all,
                   family = "poisson")

anova(lrt2.loc, lrt1.both)

anova(lrt3.site, lrt1.both)

anova(lrt2.loc, lrt3.site)

anova(lrt3.site, lrt4.nest)


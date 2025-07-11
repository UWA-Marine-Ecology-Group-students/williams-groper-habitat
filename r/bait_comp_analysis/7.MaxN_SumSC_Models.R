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

sum.stage <- readRDS("./data/tidy/2024_Wudjari_bait_comp_count.sum.stage.rds")%>%
  dplyr::mutate(bait = as.factor(bait), location = as.factor(location), 
                site = as.factor(site))%>%
  dplyr::mutate(depth_m = as.numeric(depth_m), 
                longitude_dd = as.numeric(longitude_dd),
                latitude_dd = as.numeric(latitude_dd))%>%
  dplyr::mutate(date = substr(date_time, 1, 10))%>%
  dplyr::mutate(time = substr(date_time, 12, 19))%>%
  left_join(habitat)%>%
  dplyr::filter(opcode != "046")%>% #no habitat data for 046
  dplyr::mutate(presence = ifelse(maxn_sum > 0, 1, 0))%>%
  clean_names()%>%
  glimpse()

# summary(sum.stage$presence) 
#   
## SUMMARY STATS
# MaxN summary per bait type
sum.stage %>%
  group_by(bait) %>%
  summarise(
    mean_maxn = mean(maxn_sum, na.rm = TRUE),
    median_maxn = median(maxn_sum, na.rm = TRUE),
    min_maxn = min(maxn_sum, na.rm = TRUE),
    max_maxn = max(maxn_sum, na.rm = TRUE),
    range_maxn = max_maxn - min_maxn)


summary(sum.stage$bait) #number of drops per bait type
summary(sum.stage$location)

 

## plot Freq. distribution of MaxNs 

ggplot(sum.stage, aes(x = maxn_sum)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Maxn Values",
       x = "Maxn Value",
       y = "Count") +
  # scale_y_continuous(
  #   breaks = c(0, 5, 10, 15), 
  #   limits = c(0, 15)) +
  scale_x_continuous(
    breaks = c(0:8))+
  theme_cowplot()


### Visualise MaxN by bait type
## Bait Types
bait_col <- c("abalone" = "#27ae60", 
              "octopus" = "#f39c12" , 
              "pilchard" = "#CC79A7")

#plot
ggplot(sum.stage, aes(x= bait, y = maxn_sum, fill = bait))+
  stat_summary(fun = mean, geom = "bar", color = "black", width = 0.6) +  # Bars
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +  # Error bars with standard error
  labs(x = "Bait Type", y = "Mean MaxN", title = "Dynamite Plot MaxN(Stage) by Bait Type")+
  scale_fill_manual(values = bait_col)+
  scale_x_discrete(labels = c("Abalone", "Octopus", "Pilchard"))+
  theme_cowplot()+
  theme(legend.position = "none")


# Checking predictor variables and running a Generalized Linear Model (GLM)
## Check correlation of co-variates
ggplot() +
  geom_point(data = sum.stage, aes(x = macroalgae, y = scytothalia))

cor(sum.stage$macroalgae, sum.stage$scytothalia)

ggplot() +
  geom_point(data = sum.stage, aes(x = macroalgae, y = ecklonia))

cor(sum.stage$macroalgae, sum.stage$ecklonia)

##############################
## visualising relationships between covariates and response

ggplot() +
  geom_point(data = sum.stage, aes(x = canopy, y = maxn_sum))

#doesn't look like there's much of a relationship between any

##################################
## Stepwise modelling approach

p1 <- glmer(maxn_sum ~ bait + (1|location/site), data = sum.stage,
            family = "poisson")

summary(p1)
Anova(p1)

#r2_nakagawa(p1) #r2 using performance package

## CHECKING OVERDISPERSION OF POISSON DISTRIBUTION
#deviance / residual degrees of freedom

deviance(p1)/df.residual(p1)

# Compare the mean and variance of response
mean(sum.stage$maxn_sum)/var(sum.stage$maxn_sum)


#plotting residuals

r <- residuals(p1)

# Plot residuals - if systematic patterns (ie funnel shape) indicates heteroscedasticity
# also look for large residuals not explained by the model
plot(r, main = "Residuals from Poisson MaxN(stage)", 
     xlab = "Index", ylab = "Residuals")

pears.p1 <- residuals(p1, type = "pearson")
var(pears.p1)


## Not Overdispersed but checking with Negative Binomial just in case 

## NEGATIVE BINOMIAL MODEl

nb1 <- glmmTMB(maxn_sum~bait + (1|location/site), 
               data = sum.stage, 
               family = "nbinom2")  # Negative Binomial with two parameters

summary(nb1)
Anova(nb1)
#r.squaredGLMM(nb1) #mumin package

## checking for overdispersion
# dispersion statistic 

deviance(nb1)/df.residual(nb1)

#plotting residuals

nbr1 <- residuals(nb1)

# Plot residuals - if systematic patterns (ie funnel shape) indicates heteroscedasticity
# also look for large residuals not explained by the model
plot(nbr1, main = "NB: MaxN(stage)~bait", 
     xlab = "Index", ylab = "Residuals")

## Pearson residuals - 
# checking to see if variance of residuals is larger than expected

pears1 <- residuals(nb1, type = "pearson")

#Calculate variance of the pearson residuals - 
#if around 1 indicates that poisson assumption
# of constant variance is actually reasonable - 
# but as this is a negative binomial would expect
# it to be larger than 1
var(pears1)

# Plot Pearson residuals
plot(pears1, main = "Pearson - MaxN(stage)~bait")

#with DHARMa package - check residuals
sim_res <- simulateResiduals(fittedModel = nb1)
plot(sim_res)


#bait + depth_m 

p2 <- glmer(maxn_sum ~ bait + depth_m + (1|location/site), data = sum.stage,
            family = "poisson")
summary(p2)
Anova(p2)

## checking for overdispersion
# dispersion statistic 

deviance(p2)/df.residual(p2)

#plotting residuals

nbr1 <- residuals(p2)

# Plot residuals - if systematic patterns (ie funnel shape) 
# indicates heteroscedasticity
# also look for large residuals not explained by the model
plot(nbr1, xlab = "Index", ylab = "Residuals")

## Pearson residuals - 
# checking to see if variance of residuals is larger than expected

pears <- residuals(p2, type = "pearson")
var(pears)

# Plot Pearson residuals
plot(pears)

#with DHARMa package - check residuals
# sim_res <- simulateResiduals(fittedModel = p2)
# plot(sim_res)
# 
# ##refit with NB
# nb2 <- glmmTMB(maxn_sum~bait + depth_m + (1|location), 
#         data = sum.stage, 
#         family = "nbinom2")
# 
# summary(nb2)
# Anova(nb2)

## checking for overdispersion
# dispersion statistic 

# deviance(nb2)/df.residual(nb2)
# 
# #plotting residuals
# 
# nbr1 <- residuals(nb2)
# 
# # Plot residuals - if systematic patterns (ie funnel shape) indicates heteroscedasticity
# # also look for large residuals not explained by the model
# plot(nbr1, xlab = "Index", ylab = "Residuals")
# 
# ## Pearson residuals - 
# # checking to see if variance of residuals is larger than expected
# 
# pears1 <- residuals(nb2, type = "pearson")
# 
# #Calculate variance of the pearson residuals - 
# #if around 1 indicates that poisson assumption
# # of constant variance is actually reasonable - 
# # but as this is a negative binomial would expect
# # it to be larger than 1
# var(pears1)
# 
# # Plot Pearson residuals
# plot(pears1, main = "Pearson - MaxN(stage)~bait")
# 
# #with DHARMa package - check residuals
# sim_res <- simulateResiduals(fittedModel = nb2)
# plot(sim_res)
# 
# # Post - hoc for best model
# 
# post <- emmeans(p2, ~ bait)  # Specify the fixed factor of interest
# 
# # Perform pairwise comparisons
# pairs(post)
# 
# ## visualising post-hoc tests
# 
# pairwise_results <- contrast(post, method = "pairwise")
# 
# # Convert pairwise results to a data frame
# pairwise_df <- as.data.frame(pairwise_results)
# 
# #plot
# ggplot(pairwise_df, aes(x = contrast, y = estimate)) +
#   geom_point(size = 3) +
#   geom_errorbar(aes(ymin = estimate - SE, ymax = estimate + SE), width = 0.2) +
#   labs(x = "Pairwise Comparisons", y = "Estimate", title = "MaxN(stage) ~ bait + depth_m") +
#   theme_minimal() +
#   coord_flip()
# 


###########################################################################
#depth + reef

p3 <- glmer(maxn_sum ~ bait + depth_m + reef + (1|location/site), data = sum.stage,
            family = "poisson")
summary(p3)
Anova(p3)

#depth + ecklonia
p4 <- glmer(maxn_sum ~ bait + depth_m + ecklonia + (1|location/site), data = sum.stage,
            family = "poisson")
summary(p4)
Anova(p4)

#ecklonia
p5 <- glmer(maxn_sum ~ bait + ecklonia + (1|location/site), data = sum.stage,
            family = "poisson")
summary(p5)
Anova(p5)

#scyto
p6 <- glmer(maxn_sum ~ bait + scytothalia + (1|location/site), data = sum.stage,
            family = "poisson")
summary(p6)
Anova(p6)

#reef
p7 <- glmer(maxn_sum ~ bait + reef + (1|location/site), data = sum.stage,
            family = "poisson")
summary(p7)


#mean_relief
p8 <- glmer(maxn_sum ~ bait + mean_relief + (1|location/site), data = sum.stage,
            family = "poisson")
summary(p8)


#macroalgae
p9 <- glmer(maxn_sum ~ bait + macroalgae + (1|location/site), data = sum.stage,
            family = "poisson")
summary(p9)

#canopy
p10 <- glmer(maxn_sum ~ bait + canopy + (1|location/site), data = sum.stage,
            family = "poisson")
summary(p10)


ggplot() +
  geom_point(data = sum.stage, aes(x = maxn_sum, y = canopy))

#############################################################################
### Running as presence/absence

mod <- glmer(presence ~ bait + (1 | location/site),
             family = binomial, #if overdispersed use family = quasibinomial
             data = sum.stage)

summary(mod)
Anova(mod)

#testing for overdispersion
#
overdispersion_test <- function(model) {
  # Calculate Pearson residuals
  res <- residuals(model, type = "pearson")
  
  # Degrees of freedom = number of observations - number of parameters
  df_resid <- df.residual(model)
  
  # Dispersion ratio
  dispersion <- sum(res^2) / df_resid
  
  cat("Dispersion ratio:", round(dispersion, 3), "\n")
  cat("Degrees of freedom:", df_resid, "\n")
  
  # Optional: test if dispersion is significantly greater than 1 (chi-squared test)
  p_val <- pchisq(sum(res^2), df = df_resid, lower.tail = FALSE)
  cat("Chi-squared p-value:", round(p_val, 4), "\n")
  
  invisible(dispersion)
}

# Run the test
overdispersion_test(mod)
# Dispersion ratio ≈ 1: No overdispersion — your model fits well under binomial assumptions.
# 
# Dispersion ratio > 1.2 (rule of thumb): Overdispersion may be present.
# 
# p-value < 0.05: Evidence that overdispersion is significant.

#posthoc on stage

# Run the pairwise comparisons for the 'stage' variable
emm_stage <- emmeans(mod, ~ stage, type = "response")
pairwise_stage <- pairs(emm_stage, adjust = "tukey")
pairwise_stage

# Convert to data frame
pairwise_df <- as.data.frame(pairwise_stage)
pairwise_df


###################################################################################
####    HYPOTHESIS 1.2 - MAXN BY SIZE CLASSWILL BE GREATER WITH AB BAIT      ######
###################################################################################

rm(list=ls())

# libraries----
#library(devtools)
library(tidyverse)
#library(mgcv)
library(MuMIn)
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
#install.packages("DHARMa")
library(DHARMa)
#library(performance)

name <- "2024_Wudjari_bait_comp"

# Read in the formatted data


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

## MaxN by STAGE per OPCODE

maxn.stage <- readRDS("./data/tidy/2024_Wudjari_bait_comp_count.maxn.stage.RDS") %>%
  dplyr::mutate(species = "gouldii", bait = as.factor(bait), location = as.factor(location))%>%
  dplyr::mutate(depth_m = as.numeric(depth_m))%>%
  dplyr::mutate(period = as.factor(period))%>%
  dplyr::mutate(date = substr(date_time, 1, 10))%>%
  dplyr::mutate(time = substr(date_time, 12, 19))%>%
  dplyr::mutate(date = as.factor(date))%>%
  dplyr::group_by(opcode, stage)%>%
  dplyr::slice_max(order_by = maxn, n=1, with_ties = FALSE)%>%
  dplyr::ungroup()%>%
  glimpse()


sum.stage <- maxn.stage %>% ##DF with the MaxN per Stage summed for each opcode
  dplyr::group_by(opcode, family, genus, species, bait, longitude_dd, latitude_dd,date_time, location, depth_m, date, time) %>%
  dplyr::summarise(maxn=sum(maxn))%>%
  dplyr::ungroup()%>%
  left_join(habitat)%>% #joining to habitat 
  left_join(site)%>%
  dplyr::mutate(site = as.factor(site))%>%
  glimpse()

#summary(sum.stage)

## summary details for bait type
aggregate(maxn ~ bait, data = sum.stage, FUN = mean)
aggregate(maxn ~ bait, data = sum.stage, FUN = median)
aggregate(maxn ~ bait, data = sum.stage, FUN = min)
aggregate(maxn ~ bait, data = sum.stage, FUN = max)

## summary details for location
aggregate(maxn ~ location, data = sum.stage, FUN = mean)
aggregate(maxn ~ location, data = sum.stage, FUN = median)
aggregate(maxn ~ location, data = sum.stage, FUN = min)
aggregate(maxn ~ location, data = sum.stage, FUN = max)

## summary details for date
aggregate(maxn ~ date, data = sum.stage, FUN = mean)
aggregate(maxn ~ date, data = sum.stage, FUN = median)
aggregate(maxn ~ date, data = sum.stage, FUN = min)
aggregate(maxn ~ date, data = sum.stage, FUN = max)

## plot Freq. distribution of MaxNs 

ggplot(sum.stage, aes(x = maxn)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Maxn Values",
       x = "Maxn Value",
       y = "Count") +
  scale_y_continuous(
    breaks = c(0, 5, 10, 15), 
    limits = c(0, 15)) +
  scale_x_continuous(
    breaks = c(0:12))+
  theme_cowplot()



#####
ggplot(sum.stage, aes(x = bait, y = maxn, fill = bait)) +
  geom_jitter(alpha = 0.5) +
  labs(x = "Bait", y = "MaxN", title = "MaxN by Bait with Size Class")+
  scale_y_continuous(
    breaks = c(0, 5, 10, 15), 
    limits = c(0,15)) +
  theme_cowplot() + 
  stat_summary( geom = "point", fun.y = "mean", col = "black", size = 3, shape = 24, fill = "red" )

################################################################################
################################################################################ 
## BEST MODEl

best <- glmmTMB(maxn~bait + mean.relief +  depth_m + (1|site), 
               data = sum.stage, 
               family = "nbinom2")


summary(best)

################################################################################
################################################################################ 
## Running with habitat in it

p1 <- glmer(maxn ~ bait + (1|site), data = sum.stage,
            family = "poisson")

summary(p1)
anova(p1)
r2_nakagawa(p1) #r2 using performance package

## CHECKING OVERDISPERSION OF POISSON DISTRIBUTION
#deviance / residual degrees of freedom

deviance(p1)/df.residual(p1)

# Compare the mean and variance of response
mean(sum.stage$maxn)
var(sum.stage$maxn)


#plotting residuals

r <- residuals(p1)

# Plot residuals - if systematic patterns (ie funnel shape) indicates heteroscedasticity
# also look for large residuals not explained by the model
plot(r, main = "Residuals from Poisson MaxN(stage)", 
     xlab = "Index", ylab = "Residuals")

pears.p1 <- residuals(p1, type = "pearson")
var(pears.p1)

### with mean.relief
p2 <- glmer(maxn ~ bait + mean.relief + (1|site),
            data = sum.stage,
            family = "poisson")
summary(p2)
anova(p2)

deviance(p2)/df.residual(p2)

r2 <- residuals(p2)
plot(r2, main = "MaxN(stage)~Bait + Mean.relief", 
     xlab = "Index", ylab = "Residuals")

pears.p2 <- residuals(p2, type = "pearson")

### with mean.relief & Scytothalia

p3 <- glmer(maxn ~ bait + mean.relief + Scytothalia + (1|site),
            data = sum.stage,
            family = "poisson")
summary(p3)

deviance(p3)/df.residual(p3)

r3 <- residuals(p3)
plot(r3, main = "MaxN(stage)~Bait + Mean.relief + Scytothalia", 
     xlab = "Index", ylab = "Residuals")

### with Scytothalia

p4 <- glmer(maxn ~ bait +  Scytothalia + (1|site),
            data = sum.stage,
            family = "poisson")
summary(p4)

deviance(p4)/df.residual(p4)

r4 <- residuals(p4)
plot(r4, main = "MaxN(stage)~Bait + Scytothalia", 
     xlab = "Index", ylab = "Residuals")

## Post - hoc 

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
  labs(x = "Pairwise Comparisons", y = "Estimate", title = "MaxN(stage) ~ bait + Scytothalia") +
  theme_minimal() +
  coord_flip()

###
# with Ecklonia

p5 <- glmer(maxn ~ bait +  Ecklonia + (1|site),
            data = sum.stage,
            family = "poisson")
summary(p5)

deviance(p5)/df.residual(p5)

post5 <- emmeans(p5, ~ bait)
pairs(post5)
pairwise_results <- contrast(post5, method = "pairwise")

# Convert pairwise results to a data frame
pairwise_df <- as.data.frame(pairwise_results)

#plot
ggplot(pairwise_df, aes(x = contrast, y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = estimate - SE, ymax = estimate + SE), width = 0.2) +
  labs(x = "Pairwise Comparisons", y = "Estimate", title = "MaxN(stage) ~ bait + Ecklonia") +
  theme_minimal() +
  coord_flip()


######## --- Poisson distribution looks overdispersed so repeating with NB model

## NEGATIVE BINOMIAL MODEl

nb1 <- glmmTMB(maxn~bait + (1|site), 
                data = sum.stage, 
                family = "nbinom2")  # Negative Binomial with two parameters

summary(nb1)
r.squaredGLMM(nb1) #mumin package

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

## Fit poisson model using glmmTMB package
pois1 <- glmmTMB(maxn~bait + (1|site), data = sum.stage, family = "poisson")

## Compare Poisson and Negative Binomial log likelihood tests
# with poisson listed first below, p<0.05 means NB model better
#lrtest(nb1, pois1) doesn't work
logLik(nb1, pois1)
logLik(nb1)
logLik(pois1)
AIC(pois1)

#with DHARMa package - check residuals
sim_res <- simulateResiduals(fittedModel = nb1)
plot(sim_res)

#####################
## with Mean Relief
nb2 <- glmmTMB(maxn~bait + mean.relief + (1|site), 
               data = sum.stage, 
               family = "nbinom2")

summary(nb2)
deviance(nb2)/df.residual(nb2)

#checking residuals
nbr2 <- residuals(nb2)

plot(nbr2, main = "NB: MaxN(stage)~bait + mean relief", 
     xlab = "Index", ylab = "Residuals")

pears2 <- residuals(nb2, type = "pearson")

var(pears2)

# Plot Pearson residuals
plot(pears2, main = "Pearson - MaxN(stage)~bait + mean.relief")

#with DHARMa package - check residuals
sim_res <- simulateResiduals(fittedModel = nb2)
plot(sim_res)

################################
### Mean Relief & Scytothalia
nb3 <- glmmTMB(maxn~bait + mean.relief + Scytothalia + (1|site), 
               data = sum.stage, 
               family = "nbinom2")
summary(nb3)
deviance(nb3)/df.residual(nb3)

#checking residuals
nbr3 <- residuals(nb3)
plot(nbr3, main = "NB: MaxN(stage)~bait + mean relief + Scyto", 
     xlab = "Index", ylab = "Residuals")

pears3 <- residuals(nb3, type = "pearson")

var(pears3)

plot(pears3, main = "Pearson - MaxN(stage)~bait + mean.relief + Scyto")

#with DHARMa package - check residuals
sim_res <- simulateResiduals(fittedModel = nb3)
plot(sim_res)

### Scytothalia
nb4 <- glmmTMB(maxn~bait +  Scytothalia + (1|site), 
               data = sum.stage, 
               family = "nbinom2")
summary(nb4)
deviance(nb4)/df.residual(nb4)

#checking residuals
nbr4 <- residuals(nb4)
plot(nbr4, main = "NB: MaxN(stage)~bait + Scyto", 
     xlab = "Index", ylab = "Residuals")

pears4 <- residuals(nb4, type = "pearson")

var(pears4)

plot(pears4, main = "Pearson - MaxN(stage)~bait + Scyto")

# post-hocs for significant result

post4 <- emmeans(nb4, ~bait)
pairs(post4) #default Tukey
#pairs(post5, adjust = "tukey")

post4_df <- as.data.frame(post4)
post4_df
ggplot(post4_df, aes(x = bait, y = emmean)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) + #throwing error
  theme_minimal()

plot(post4, main = "Post Hoc - NB:MaxN(stage)~bait + Scytothalia")

#with DHARMa package - check residuals
sim_res <- simulateResiduals(fittedModel = nb4)
plot(sim_res)

### Ecklonia
nb5 <- glmmTMB(maxn~bait +  Ecklonia + (1|site), 
               data = sum.stage, 
               family = "nbinom2")
summary(nb5)
deviance(nb5)/df.residual(nb5)

#checking residuals
nbr5 <- residuals(nb5)
plot(nbr5, main = "NB: MaxN(stage)~bait + Ecklonia", 
     xlab = "Index", ylab = "Residuals")

pears5 <- residuals(nb5, type = "pearson")

var(pears5)

plot(pears5, main = "Pearson - MaxN(stage)~bait + Ecklonia")

# post-hocs for significant result

post5 <- emmeans(nb5, ~bait)
pairs(post5) #default Tukey
#pairs(post5, adjust = "tukey")

post5_df <- as.data.frame(post5)
post5_df
ggplot(post5_df, aes(x = bait, y = emmean)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) + #throwing error
  theme_minimal()

plot(post5, main = "Post Hoc - NB:MaxN(stage)~bait + Ecklonia")

#with DHARMa package - check residuals
sim_res <- simulateResiduals(fittedModel = nb5)
plot(sim_res)

### Mean Relief & Ecklonia
nb6 <- glmmTMB(maxn~bait + mean.relief +  Ecklonia + (1|site), 
               data = sum.stage, 
               family = "nbinom2")
summary(nb6)
deviance(nb6)/df.residual(nb6)

#checking residuals
nbr6 <- residuals(nb6)
plot(nbr6, main = "NB: MaxN(stage)~bait + mean relief +  Ecklonia", 
     xlab = "Index", ylab = "Residuals")

pears6 <- residuals(nb6, type = "pearson")

var(pears6)

plot(pears6, main = "Pearson - MaxN(stage)~bait + mean relief + Ecklonia")


#with DHARMa package - check residuals
sim_res <- simulateResiduals(fittedModel = nb6)
plot(sim_res)


## mean relief & depth
nb7 <- glmmTMB(maxn~bait + mean.relief +  depth_m + (1|site), 
               data = sum.stage, 
               family = "nbinom2")
summary(nb7)
deviance(nb7)/df.residual(nb7)

#checking residuals
nbr7 <- residuals(nb7)
plot(nbr7, main = "NB: MaxN(stage)~bait + mean relief +  depth", 
     xlab = "Index", ylab = "Residuals")

pears7 <- residuals(nb7, type = "pearson")

var(pears7)



#with DHARMa package - check residuals
sim_res <- simulateResiduals(fittedModel = nb7)
plot(sim_res)

## depth

## mean relief & depth
nb8 <- glmmTMB(maxn~bait +   depth_m + (1|site), 
               data = sum.stage, 
               family = "nbinom2")
summary(nb8)
r.squaredGLMM(nb8)
deviance(nb8)/df.residual(nb8)


post8 <- emmeans(nb8, ~bait)
pairs(post8) #default Tukey
pairs(post8, adjust = "bonferroni")
pairs(post8, adjust = "holm")
pairs(post8, adjust = "sidak")


post8_df <- as.data.frame(post8)
post8_df
ggplot(post8_df, aes(x = bait, y = emmean)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) + #throwing error
  theme_minimal()

plot(post8, main = "Post Hoc - NB:MaxN(stage)~bait + depth")

#checking residuals
nbr8 <- residuals(nb8)
plot(nbr8, main = "NB: MaxN(stage)~bait + depth", 
     xlab = "Index", ylab = "Residuals")

pears8 <- residuals(nb8, type = "pearson")

var(pears8)

#with DHARMa package - check residuals
sim_res <- simulateResiduals(fittedModel = nb8)
plot(sim_res)


## depth & scytothalia

nb9 <- glmmTMB(maxn~bait + depth_m + Scytothalia + (1|site),
               data = sum.stage,
               family = "nbinom2")
summary(nb9)
deviance(nb9)/df.residual(nb9)

post9 <- emmeans(nb9, ~bait)
pairs(post9)

#checking residuals
nbr9 <- residuals(nb9)
plot(nbr9, main = "NB: MaxN(stage)~bait + depth + Scytothalia", 
     xlab = "Index", ylab = "Residuals")

pears9 <- residuals(nb9, type = "pearson")

var(pears9)

#with DHARMa package - check residuals
sim_res <- simulateResiduals(fittedModel = nb9)
plot(sim_res)
##############################################################################
################################################################################
##############################################################################
## First did an LMER - but its not normally distributed (here is the code anyway)

#mod3 <- lmer(maxn~bait + (1|location), data = maxn.stage)
#summary(mod3)
#anova(mod3)
#r.squaredGLMM(mod3)

## GLMER using lme4

glm1 <- glmer(maxn~bait + (1|location), data = sum.stage, family = "poisson")
summary(glm1)

## CHECKING OVERDISPERSION OF POISSON DISTRIBUTION

# Alternatively, calculate deviance and residual degrees of freedom
deviance2 <- deviance(glm1)
df.residual2 <- df.residual(glm1)

# Calculate the dispersion statistic
dispersion_statistic2 <- deviance2 / df.residual2
dispersion_statistic2

# Mean and variance of the response variable
mean_response2 <- mean(maxn.stage$maxn)
var_response2 <- var(maxn.stage$maxn)

# Compare the mean and variance
mean_response2
var_response2

#### plot observed vs predicted values to see how well model fits data
# Predicted values
predicted_values <- predict(glm1, type = "response")

# Plot observed vs predicted
plot(maxn.stage$maxn, predicted_values, 
     main = "Observed vs Predicted Values", 
     xlab = "Observed", ylab = "Predicted")

## with ggplot

# Get the observed values from the data
observed_values <- sum.stage$maxn

# Create a data frame for plotting
plot_data <- data.frame(observed = observed_values, predicted = predicted_values)

# Plot predicted vs. observed
ggplot(plot_data, aes(x = observed, y = predicted)) +
  geom_point() +  # Scatter plot of observed vs predicted
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # Line of perfect fit
  theme_minimal() +
  labs(title = "Observed vs. Predicted Values - Poisson", 
       x = "Observed Values", 
       y = "Predicted Values")


## if Overdispersed - next step is to do a negative binomial

######## 
## NEGATIVE BINOMIAL MODEl

# Fit a GLMM with Negative Binomial distribution
glm2 <- glmmTMB(maxn~bait + (1|location), 
                data = sum.stage, 
                family = "nbinom2")  # Negative Binomial with two parameters

summary(glm2)

## checking for overdispersion
# dispersion statistic 

deviance(glm2)/df.residual(glm2)

#plotting residuals

r <- residuals(glm2)

# Plot residuals - if systematic patterns (ie funnel shape) indicates heteroscedasticity
# also look for large residuals not explained by the model
plot(r, main = "Residuals from Negative Binomial GLMM", 
     xlab = "Index", ylab = "Residuals")

## Pearson residuals - 
# checking to see if variance of residuals is larger than expected

pearson_residuals <- residuals(glm2, type = "pearson")

#Calculate variance of the pearson residuals - if around 1 indicates that poisson assumption
# of constant variance is actually reasonable - but as this is a negative binomial would expect
# it to be larger than 1
var(pearson_residuals)

# Plot Pearson residuals
plot(pearson_residuals, main = "Pearson Residuals - Negative Binomial")

## Fit poisson model using glmmTMB package
glm3 <- glmmTMB(maxn~bait + (1|location), data = sum.stage, family = "poisson")

## Compare Poisson and Negative Binomial log likelihood tests
# with poisson listed first below, p<0.05 means NB model better
lrtest(glm3, glm2)

## plot observed vs predicted values to see how well model fits data
# Predicted values
predicted_values <- predict(glm2, type = "response")

# Plot observed vs predicted
plot(sum.stage$maxn, predicted_values, 
     main = "Observed vs Predicted Values", 
     xlab = "Observed", ylab = "Predicted")

## with ggplot

# Get the observed values from the data
observed_values <- sum.stage$maxn

# Create a data frame for plotting
plot_data <- data.frame(observed = observed_values, predicted = predicted_values)

# Plot predicted vs. observed
ggplot(plot_data, aes(x = observed, y = predicted)) +
  geom_point() +  # Scatter plot of observed vs predicted
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # Line of perfect fit
  theme_minimal() +
  labs(title = "Observed vs. Predicted Values - Negative Binomial", 
       x = "Observed Values", 
       y = "Predicted Values")


## compare AIC and BIc values
AIC(glm2)
AIC(glm3)
BIC(glm2)
BIC(glm3)
logLik(glm2)
logLik(glm3)


##### WITH TWEEDIE

twe1 <- glmmTMB(maxn~bait + (1|location), 
                data = sum.stage, 
                family = tweedie(link = "log"))

summary(twe1) #doesn't seem to be liking the random effects

twe2 <- glmmTMB(maxn~bait, 
                data = sum.stage, 
                family = tweedie(link = "log"))


summary(twe2)



#### with poisson log

poislog <- glmmTMB(maxn~bait + (1|location),
                   data = sum.stage,
                   family = poisson(link = "log"))

summary(poislog)

AIC(poislog)
BIC(poislog)
logLik(poislog)
####################################################################
##################################################################
# Likelihood ratio tests

lrt1.both <- glmmTMB(maxn ~ bait + mean.relief + depth_m + (1|location) + (1|site),
                   data = sum.stage,
                   family = "nbinom2")

lrt2.loc <- glmmTMB(maxn ~ bait + mean.relief + depth_m + (1|location),
                  data = sum.stage,
                  family = "nbinom2")


anova(lrt2.loc, lrt1.both)

lrt3.site <- glmmTMB(maxn ~ bait + mean.relief + depth_m + (1|site),
                   data = sum.stage,
                   family = "nbinom2")

anova(lrt3.site, lrt1.both)

lrt4.nest <- glmmTMB(maxn ~ bait + mean.relief + depth_m  + (site|location),
                   data = sum.stage,
                   family = "nbinom2")
r.squaredGLMM(lrt3.site)
summary(lrt3.site)

anova(lrt2.loc, lrt3.site)

anova(lrt3.site, lrt4.nest)

anova(lrt4.nest, lrt3.site)

citation()

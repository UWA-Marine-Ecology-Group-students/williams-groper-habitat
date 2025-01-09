###################################################################################
####    HYPOTHESIS 1.2 - MAXN BY SIZE CLASSWILL BE GREATER WITH AB BAIT      ######
###################################################################################

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

## MaxN by STAGE per OPCODE

maxn.stage <- readRDS("./data/tidy/2024_Wudjari_bait_comp_count.maxn.stage.RDS") %>%
  dplyr::mutate(species = "gouldii", bait = as.factor(bait), location = as.factor(location))%>%
  dplyr::mutate(depth_m = as.numeric(depth_m))%>%
  dplyr::mutate(period = as.factor(period))%>%
  dplyr::mutate(date = substr(date_time, 1, 10))%>%
  dplyr::mutate(time = substr(date_time, 12, 19))%>%
  dplyr::mutate(date = as.factor(date))%>%
  glimpse()

test <- maxn.stage %>%
  dplyr::group_by(opcode, stage)%>%
  dplyr::slice_max(order_by = maxn, n=1, with_ties = FALSE)%>%
  dplyr::ungroup()%>%
  glimpse()

maxn.stage <- test

maxn.stage <- maxn.stage %>%
  dplyr::group_by(opcode, family, genus, species, bait, longitude_dd, latitude_dd,date_time, location, depth_m, date, time) %>%
  dplyr::summarise(maxn=sum(maxn))%>%
  dplyr::ungroup()%>%
  glimpse()

## plot Freq. distribution of MaxNs 

ggplot(maxn.stage, aes(x = maxn)) +
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
ggplot(maxn.stage, aes(x = bait, y = maxn, fill = bait)) +
  geom_jitter(alpha = 0.5) +
  labs(x = "Bait", y = "MaxN", title = "MaxN by Bait with Size Class")+
  scale_y_continuous(
    breaks = c(0, 5, 10, 15), 
    limits = c(0,15)) +
  theme_cowplot() + 
  stat_summary( geom = "point", fun.y = "mean", col = "black", size = 3, shape = 24, fill = "red" )

## First did an LMER - but its not normally distributed (here is the code anyway)

#mod3 <- lmer(maxn~bait + (1|location), data = maxn.stage)
#summary(mod3)
#anova(mod3)
#r.squaredGLMM(mod3)

## GLMER using lme4

glm1 <- glmer(maxn~bait + (1|location), data = maxn.stage, family = "poisson")
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
observed_values <- maxn.stage$maxn

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


# Fit a GLMM with Negative Binomial distribution
glm2 <- glmmTMB(maxn~bait + (1|location), 
                data = maxn.stage, 
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
glm3 <- glmmTMB(maxn~bait + (1|location), data = maxn.stage, family = "poisson")

## Compare Poisson and Negative Binomial log likelihood tests
# with poisson listed first below, p<0.05 means NB model better
lrtest(glm3, glm2)

## plot observed vs predicted values to see how well model fits data
# Predicted values
predicted_values <- predict(glm2, type = "response")

# Plot observed vs predicted
plot(maxn.stage$maxn, predicted_values, 
     main = "Observed vs Predicted Values", 
     xlab = "Observed", ylab = "Predicted")

## with ggplot

# Get the observed values from the data
observed_values <- maxn.stage$maxn

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
help(glmer)
twe1 <- glmmTMB(maxn~bait + (1|location), data = maxn.stage, family = tweedie(link = "log"))

summary(twe1)



###############################################################################
###############################################################################
#### checking other variables
plot(maxn~location, data = maxn.stage)
plot(maxn~depth_m, 
     col = c(bait),
     data = maxn.stage)

ggplot(maxn.stage, aes(x = depth_m, y = maxn)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red")+
  labs(x = "Depth (m)", y = "MaxN", title = "MaxN with Size Class by Depth")+
  scale_y_continuous(
    breaks = c(0, 5, 10, 15), 
    limits = c(0,15)) +
  theme_cowplot()

## Linear Regression for depth & MaxN
model <- lm(depth_m ~ maxn, data = maxn.stage)
summary(model)

## location plot
ggplot(maxn.stage, aes(x = location, y = maxn)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  labs(x = "Location", y = "MaxN", title = "MaxN with Size Class by Location")+
  scale_y_continuous(
    breaks = c(0, 5, 10, 15), 
    limits = c(0,15)) +
  theme_cowplot()

## model maxn by location
lmod <- lmer(maxn~location + (1|date),
             data = maxn.stage)
summary(lmod)
anova(lmod)

lmod2 <- lm(maxn~location, data = maxn.stage)
anova(lmod2)

## MaxN by Date
ggplot(maxn.stage, aes(x = date, y = maxn)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  labs(x = "Date", y = "MaxN", title = "MaxN with Size Class by Date")+
  scale_y_continuous(
    breaks = c(0, 5, 10, 15), 
    limits = c(0,15)) +
  theme_cowplot()

## modelling Maxn & Date
dmod <- lm(maxn~date, data = maxn.stage)
summary(dmod)

####################
# summary stats - move to new script
##########

table(maxn.stage$location)
table(maxn.stage$bait)

summary(maxn.all$maxn)
summary(maxn.stage$maxn)

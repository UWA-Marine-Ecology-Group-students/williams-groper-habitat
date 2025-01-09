#############################################
####### Hypothesis - Time - BG will appear earlier in the video

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
#library(RColorBrewer)
#install.packages("paletteer")
library(paletteer)

maxn.all <- readRDS("./data/tidy/2024_Wudjari_bait_comp_count.maxn.all.RDS") %>%
  dplyr::mutate(species = "gouldii", bait = as.factor(bait), location = as.factor(location))%>%
  dplyr::mutate(depth_m = as.numeric(depth_m))%>%
  dplyr::mutate(period = as.factor(period))%>%
  dplyr::mutate(date = substr(date_time, 1, 10))%>%
  dplyr::mutate(time = substr(date_time, 12, 19))%>%
  dplyr::mutate(date = as.factor(date))%>%
  dplyr::mutate(periodtime = as.numeric(periodtime))%>%
  dplyr::mutate(periodtime = if_else(period == "B", periodtime + 15, 
                                     periodtime))%>%
  dplyr::mutate(periodtime = if_else(period == "C", periodtime + 30,  
                                     periodtime))%>%
  dplyr::mutate(periodtime = if_else(period == "D", periodtime + 45,  
                                     periodtime))%>%
  dplyr::group_by(opcode)%>% #just want to group by opcode to get maxn per opcode
  dplyr::slice(which.max(maxn))%>%
  dplyr::ungroup()%>%
  dplyr::filter(maxn != 0)%>% #filtered out videos with no bg
  glimpse()


####
plot(maxn~periodtime, data = maxn.all)

plot(periodtime ~ bait, data = maxn.all)

bait_col <- c("abalone" = "#27ae60", "octopus" = "#f39c12", 
              "pilchard" = "#1a5276")

ggplot(data = maxn.all, aes(x = bait, y = periodtime, colour = bait))+
  geom_jitter(alpha = 0.5)+
  theme_cowplot()+
  stat_summary( geom = "point", fun.y = "mean", col = "black", size = 3, shape = 24, fill = "red" )+
  scale_colour_manual(values = bait_col)

### LMER - lme4 package
## Gaussian as it is a continuous vairable (not count data) - which is LMER default

lm1 <- lmer(periodtime ~ bait + (1|location), data = maxn.all)

summary(lm1)

### CHECKING MODEL FIT
## Get residuals and fitted values
residuals <- residuals(lm1)
fitted_values <- fitted(lm1)

# Residual vs Fitted plot
ggplot(data = NULL, aes(x = fitted_values, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  theme_minimal() +
  labs(x = "Fitted Values", y = "Residuals", 
       title = "Residuals vs Fitted Values")

# QQ plot
qqnorm(residuals)
qqline(residuals, col = "red")

# Histogram of residuals
ggplot(data = NULL, aes(x = residuals)) +
  geom_histogram(binwidth = 0.1, color = "black", fill = "lightblue") +
  theme_minimal() +
  labs(x = "Residuals", y = "Frequency", title = "Histogram of Residuals")

### CHECKING FOR SIGNIFICANCE OF RANDOM EFFECTS
# Extract random effects
ranef_values <- ranef(lm1)$location

# Plot random effects
ggplot(data = ranef_values, aes(x = 1:nrow(ranef_values), y = `(Intercept)`)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Subject", y = "Random Intercepts", title = "Random Intercepts by Subject")


################ OTHER MAYBE MORE APPROPRIATE MODELS
#### GAMMA Model (lme4) - for duration or time-to-event data
gamma1 <- glmer(periodtime ~ bait + (1 | location), 
                     data = maxn.all, 
                     family = Gamma(link = "log"))
summary(gamma1)

## checking model fit

# Extract fitted values and residuals
fitted_vals <- fitted(gamma1)
residuals_vals <- residuals(gamma1)

# Scatter plot of residuals vs. fitted values
plot(fitted_vals, residuals_vals, 
     xlab = "Fitted values", ylab = "Residuals", 
     main = "Residuals vs. Fitted values - Gamma (log)")
abline(h = 0, col = "red", lwd = 2)

# Plot histogram of residuals
hist(residuals_vals, breaks = 30, main = "Histogram of Residuals - Gamma (log)", xlab = "Residuals")

# QQ Plot
qqnorm(residuals_vals, main = "Q-Q Plot of Residuals - Gamma (log)")
qqline(residuals_vals, col = "red")

# check for overdispersion
deviance(gamma1)/df.residual(gamma1)


##########################################
### Log-Normal Model - 

# Visualize periodtime histogram
ggplot(maxn.all, aes(x = periodtime)) + 
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(log(maxn.all$periodtime), na.rm = TRUE), 
                            sd = sd(log(maxn.all$periodtime), na.rm = TRUE)),
                color = "red", size = 1) + 
  labs(title = "Histogram of periodtime with Normal Fit on Log Scale",
       x = "periodtime", y = "Density")

# Q-Q plot of the log-transformed data
qqnorm(log(maxn.all$periodtime), main = "Q-Q Plot of Log(Periodtime)")
qqline(log(maxn.all$periodtime), col = "red")

## Log Normal Model

ln1 <- lmer(log(periodtime) ~ bait +  (1 | location), 
         data = maxn.all)

summary(ln1)


# Get fitted values (predictions) and residuals
fitted_values <- predict(ln1)
residuals <- residuals(ln1)

# Plot residuals vs. fitted values
plot(fitted_values, residuals,
     main = "Residuals vs Fitted Values - Log Normal",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")

# Histogram of residuals
hist(residuals, breaks = 30, main = "Histogram of Residuals - log normal", xlab = "Residuals")

## checking for overdispersion
# Calculate the residual sum of squares (RSS)
rss <- sum(residuals^2)

# Get the number of observations (n) and the number of estimated parameters (p)
n <- nrow(maxn.all)
p <- length(fixef(ln1))  # Number of fixed effects

# Calculate the dispersion statistic
dispersion <- rss / (n - p)
dispersion

AIC(ln1)
logLik(ln1)

# Extract random effects
ranef_vals <- ranef(ln1)$location  # Replace "group" with your random effect variable

# Plot random effects
plot(ranef_vals)

# Back-transform the predictions (from log scale to original scale)
predicted_values <- exp(fitted_values)

# Plot predicted vs actual values
plot(maxn.all$periodtime, predicted_values,
     main = "Predicted vs Actual Values - LogNormal",
     xlab = "Actual Duration", ylab = "Predicted Duration")
abline(0, 1, col = "red")


############################################################################
############################################################################
####    time to maxn with size class

maxn.stage <- readRDS("./data/tidy/2024_Wudjari_bait_comp_count.maxn.stage.rds") %>%
  dplyr::filter(maxn != 0)%>%
  dplyr::mutate(stage = as.factor(stage), opcode = as.factor(opcode))%>%
  dplyr::mutate(species = "gouldii", bait = as.factor(bait), location = as.factor(location))%>%
  dplyr::mutate(depth_m = as.numeric(depth_m))%>%
  dplyr::mutate(period = as.factor(period))%>%
  dplyr::mutate(date = substr(date_time, 1, 10))%>%
  dplyr::mutate(time = substr(date_time, 12, 19))%>%
  dplyr::mutate(date = as.factor(date))%>%
  dplyr::mutate(periodtime = as.numeric(periodtime))%>%
  dplyr::mutate(periodtime = if_else(period == "B", periodtime + 15, 
                                     periodtime))%>%
  dplyr::mutate(periodtime = if_else(period == "C", periodtime + 30,  
                                     periodtime))%>%
  dplyr::mutate(periodtime = if_else(period == "D", periodtime + 45,  
                                     periodtime))%>%
  dplyr::group_by(opcode, stage)%>% #just want to group by opcode to get maxn per opcode
  dplyr::slice(which.max(maxn))%>%
  dplyr::ungroup()%>%
  dplyr::filter(!stage %in% c("AD"))%>%
  glimpse()

unique(maxn.stage$stage)

##############################################################################
## exploratory plots
#colour scheme
bg_col <- c("0300-0499 mm" = "#2ecc71", "0500-0699 mm" = "#52be80", 
            "0700-0899 mm" = "#45b39d", "0900-1099 mm" = "#148f77",
            "1100-1299mm" = "#2471a3")

# distribution

ggplot(data = maxn.stage, aes(x = periodtime, y = maxn))+
  geom_jitter(alpha = 0.5)+
  labs(x = "Time to MaxN", y = "MaxN", 
       title = "Distribution of Time to MaxN with Size Class")+
  theme_cowplot()

#periodtime ~ bait
ggplot(data = maxn.stage, aes(x = maxn, y = periodtime))+
  geom_jitter(alpha = 0.5)+
  labs(x = "Bait Type", y = "Time to MaxN", 
       title = "Bait affect on Time to MaxN with Size Class")+
  theme_cowplot()+
  stat_summary(
    geom = "point",fun.y = "mean", col = "black", size = 3, shape = 24,
    fill = "red" )+
  facet_wrap(.~stage)

bait_col <- c("abalone" = "#27ae60", "octopus" = "#f39c12", 
              "pilchard" = "#1a5276")

ggplot(data = maxn.stage, aes(x = bait, y = periodtime, colour = bait))+
  geom_jitter(alpha = 0.5)+
  theme_cowplot()+
  stat_summary( geom = "point", fun.y = "mean", col = "black", size = 3, shape = 24, fill = "red" )+
  scale_colour_manual(values = bait_col)+
  facet_wrap(.~stage)


#########################################################################
### MODELLING
## gamma(log)

gamma2 <- glmer(periodtime ~ bait + (1 | location/opcode), 
                data = maxn.stage, 
                family = Gamma(link = "log"))
summary(gamma2)

## checking model fit

# Extract fitted values and residuals
fitted_vals <- fitted(gamma2)
residuals_vals <- residuals(gamma2)

# Scatter plot of residuals vs. fitted values
plot(fitted_vals, residuals_vals, 
     xlab = "Fitted values", ylab = "Residuals", 
     main = "Residuals vs. Fitted values - Gamma (log) - with size class")
abline(h = 0, col = "red", lwd = 2)

# Plot histogram of residuals
hist(residuals_vals, breaks = 30, 
     main = "Histogram of Residuals - Gamma (log) - with size class",
     xlab = "Residuals")

# QQ Plot
qqnorm(residuals_vals, 
       main = "Q-Q Plot of Residuals - Gamma (log) - with size class")
qqline(residuals_vals, col = "red")

# check for overdispersion
deviance(gamma2)/df.residual(gamma2)

#################################################
## Log Normal Model

ln2 <- lmer(log(periodtime) ~ bait +  (1 |location/opcode), 
            data = maxn.stage)

summary(ln2)


# Get fitted values (predictions) and residuals
fitted_values <- predict(ln2)
residuals <- residuals(ln2)

# Plot residuals vs. fitted values
plot(fitted_values, residuals,
     main = "Residuals vs Fitted Values - Log Normal with size class",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")

# Histogram of residuals
hist(residuals, breaks = 30, 
     main = "Histogram of Residuals - log normal - with size class",
     xlab = "Residuals")

## checking for overdispersion
# Calculate the residual sum of squares (RSS)
rss <- sum(residuals^2)

# Get the number of observations (n) and the number of estimated parameters (p)
n <- nrow(maxn.stage)
p <- length(fixef(ln2))  # Number of fixed effects

# Calculate the dispersion statistic
dispersion <- rss / (n - p)
dispersion

AIC(ln2)
logLik(ln2)

# Extract random effects
ranef_vals <- ranef(ln2)$location  # Replace "group" with your random effect variable

# Plot random effects
plot(ranef_vals)

# Back-transform the predictions (from log scale to original scale)
predicted_values <- exp(fitted_values)

# Plot predicted vs actual values
plot(maxn.all$periodtime, predicted_values,
     main = "Predicted vs Actual Values - LogNormal",
     xlab = "Actual Duration", ylab = "Predicted Duration")
abline(0, 1, col = "red")

#################################################
## Log Normal Model



lmer2 <- lmer(periodtime ~ bait +  (1 | location) + (1|opcode), 
              data = maxn.stage)

summary(lmer2)

lmer3 <- lmer(periodtime ~ bait +  (1 | location), 
              data = maxn.stage)

summary(lmer3)

lmer4 <- lmer(periodtime ~ bait +  (1 | location/opcode), 
              data = maxn.stage)
summary(lmer4)

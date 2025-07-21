###################################################################################
####    HYPOTHESIS 1.2 - MAXN BY SIZE CLASSWILL BE GREATER WITH AB BAIT      ######
###################################################################################

rm(list=ls())

# libraries----
#library(devtools)
library(tidyverse)
library(CheckEM)
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
# Set the predictors for modeling - don't include factors - just continuous var 
pred.vars <- c("depth_m", "macroalgae", "scytothalia", "ecklonia",
               "sessile_inverts", "mean_relief", "time_hr") 

# Check the correlations between predictor variables - looking for NAs
summary(sum.stage[,pred.vars])


#checking for correlations between variables
round(cor(sum.stage[ , pred.vars]), 2)

# check individual predictors to see if any need transformed
CheckEM::plot_transformations(pred.vars = pred.vars, dat = sum.stage) #throwing error but worked in MaxNmodels script

outdir <- "output/baitcomp/sum.stage"
for (var in pred.vars) {
  png(filename = file.path(outdir, paste0(var, ".png")), width = 800, height = 600, res = 150)
  CheckEM::plot_transformations(pred.vars = var, dat = sum.stage)
  dev.off()
}


##############################
## visualising relationships between covariates and response

ggplot() +
  geom_point(data = sum.stage, aes(x = bait, y = ecklonia))

#doesn't look like there's much of a relationship between any

##################################
## Stepwise modelling approach

p1 <- glmer(maxn_sum ~ bait + (1|location/site), data = sum.stage,
            family = "poisson")

summary(p1)
Anova(p1)

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

## Testing other predictors besides bait with one predictor only specified

b1 <- glmer(maxn_sum ~ ecklonia + (1|location/site), data = sum.stage,
            family = "poisson")

AIC(p1, b1)

b2 <- glmer(maxn_sum ~ canopy + (1|location/site), data = sum.stage,
            family = "poisson")

AIC(p1, b2)

b3 <- glmer(maxn_sum ~ depth_m + (1|location/site), data = sum.stage,
            family = "poisson")


AIC(p1, b3)

b4 <- glmer(maxn_sum ~ scytothalia + (1|location/site), data = sum.stage,
            family = "poisson")


AIC(p1, b4)

b5 <- glmer(maxn_sum ~ mean_relief + (1|location/site), data = sum.stage,
            family = "poisson")


AIC(p1, b5)

#time
b6 <- glmer(maxn_sum ~ time_hr + (1|location/site), data = sum.stage,
            family = "poisson")


AIC(p1, b6)

#sessile_inverts

b7 <- glmer(maxn_sum ~ sessile_inverts + (1|location/site), data = sum.stage,
            family = "poisson")


AIC(p1, b7)
#bait + depth_m 

p2 <- glmer(maxn_sum ~ bait + depth_m + (1|location/site), data = sum.stage,
            family = "poisson")
summary(p2)
Anova(p2)
AIC(p1, p2)

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

## 

###########################################################################
#depth + reef

p3 <- glmer(maxn_sum ~ bait + depth_m + reef + (1|location/site), data = sum.stage,
            family = "poisson")
summary(p3)
Anova(p3)
AIC(p1, p3)

#depth + ecklonia
p4 <- glmer(maxn_sum ~ bait + depth_m + ecklonia + (1|location/site), data = sum.stage,
            family = "poisson")
summary(p4)
Anova(p4)
AIC(p1, p4)

#ecklonia
p5 <- glmer(maxn_sum ~ bait + ecklonia + (1|location/site), data = sum.stage,
            family = "poisson")
summary(p5)
Anova(p5)
AIC(p1, p5)

#scyto
p6 <- glmer(maxn_sum ~ bait + scytothalia + (1|location/site), data = sum.stage,
            family = "poisson")
summary(p6)
Anova(p6)
AIC(p1, p6)
#reef
p7 <- glmer(maxn_sum ~ bait + reef + (1|location/site), data = sum.stage,
            family = "poisson")
summary(p7)
AIC(p1, p7)

#mean_relief
p8 <- glmer(maxn_sum ~ bait + mean_relief + (1|location/site), data = sum.stage,
            family = "poisson")
summary(p8)
AIC(p1, p8)

#macroalgae
p9 <- glmer(maxn_sum ~ bait + macroalgae + (1|location/site), data = sum.stage,
            family = "poisson")
summary(p9)
AIC(p1, p9)

#canopy
p10 <- glmer(maxn_sum ~ bait + canopy + (1|location/site), data = sum.stage,
            family = "poisson")
summary(p10)
AIC(p1, p10)

p11 <- glmer(maxn_sum ~ bait + time_hr +(1|location/site), data = sum.stage,
family = "poisson")

AIC(p1, p11)

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


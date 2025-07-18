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
#install.packages("glmm")
library(glmm)
#(performance)
#citation("emmeans")
#RStudio.Version()
library(leaflet)

name <- "2024_Wudjari_bait_comp"

# Read in the formatted data

############################################################
######        HYPOTHESIS 1 = MaxN will be higher with abalone
####################

# read in habitat data
habitat <- readRDS("./data/tidy/2024_Wudjari_bait_comp_full.habitat.rds")%>%
  glimpse()


### MaxN (all) 

maxn.all <- readRDS("./data/tidy/2024_Wudjari_bait_comp_count.maxn.all.RDS") %>%
  dplyr::mutate(bait = as.factor(bait), location = as.factor(location), 
                site = as.factor(site), date = as.factor(date))%>% #removed mutate(species = 'gouldii')
  dplyr::mutate(depth_m = as.numeric(depth_m), 
                longitude_dd = as.numeric(longitude_dd),
                latitude_dd = as.numeric(latitude_dd))%>%
  left_join(habitat)%>% #joining to habitat 
  dplyr::filter(opcode != "046")%>% #no habitat data for 046
  clean_names()%>%
  glimpse()

sum(maxn.all$maxn)
unique(maxn.all$species)
unique(maxn.all$opcode)
which(is.na(maxn.all$macroalgae))
unique(maxn.all$site)

which(is.na(maxn.all$titomaxn_s)) #time to maxns have NAs -- keep and potentially due hurdle model


########
## SUMMARY STATS
# MaxN summary per bait type
maxn.all %>%
  group_by(bait) %>%
  summarise(
    mean_maxn = mean(maxn, na.rm = TRUE),
    median_maxn = median(maxn, na.rm = TRUE),
    min_maxn = min(maxn, na.rm = TRUE),
    max_maxn = max(maxn, na.rm = TRUE),
    range_maxn = max_maxn - min_maxn)


summary(maxn.all$bait) #number of drops per bait type
summary(maxn.all$location)

# depth distributions across drops
maxn.all %>%
  group_by(bait) %>%
  summarise(
    mean_depth = mean(depth_m, na.rm = TRUE),
    median_depth = median(depth_m, na.rm = TRUE),
    min_depth = min(depth_m, na.rm = TRUE),
    max_depth = max(depth_m, na.rm = TRUE),
    range_depth = max_depth - min_depth)



## plot Freq. distribution of MaxNs ## plot Frmin()eq. distribution of MaxNs 
#
ggplot(maxn.all, aes(x = maxn)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Maxn Values",
       x = "Maxn Value",
       y = "Count") +
  # scale_y_continuous(
  #   breaks = c(0, 10, 20, 30),
  #   limits = c(0, 30)) +
  scale_x_continuous(
  breaks = c(0, 1, 2, 3, 4, 5, 6, 7))+
  theme_cowplot()
 
##visualise the data - leaflet
# MaxN
pal <- colorNumeric(
  palette = "YlOrRd",
  domain = maxn.all$maxn)

leaflet(maxn.all) %>%
  addTiles() %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(~longitude_dd, ~latitude_dd, radius = ~maxn,
                   color = ~pal(maxn),
                   stroke = FALSE, label = ~as.character(opcode),
                   fillOpacity = 0.8) %>%
  addLegend("bottomright",
            pal = pal,
            values = ~ maxn,
            title = "MaxN",
            opacity = 0.8)

#depth as colour and circle size as maxn
pal <- colorNumeric(
  palette = "YlOrRd",
  domain = maxn.all$depth_m)

leaflet(maxn.all) %>%
  addTiles() %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(~longitude_dd, ~latitude_dd, radius = ~maxn.all$maxn,
                   color = ~pal(depth_m),
                   stroke = FALSE, label = ~as.character(opcode),
                   fillOpacity = 0.8) %>%
  addLegend("bottomright",
            pal = pal,
            values = ~ depth_m,
            title = "Depth",
            opacity = 0.8)

# Checking predictor variables and running a Generalized Linear Model (GLM)
## Check correlation of co-variates
ggplot() +
  geom_point(data = maxn.all, aes(x = macroalgae, y = scytothalia))

cor(maxn.all$macroalgae, maxn.all$scytothalia)

cor(maxn.all$depth_m, maxn.all$macroalgae)


#### Stepwise model approach

t1 <- glmer(maxn ~ (1|location/site),
            data = maxn.all,
            family = poisson)

summary(t1)

t2 <- glmer(maxn ~ ecklonia + (1|location/site),
            data = maxn.all,
            family = poisson) #glmer() doesn't like tweedie

summary(t2)
            
t4 <- glmer(maxn ~ bait + (1|location/site),
            data = maxn.all,
            family = poisson)

summary(t4)
AIC(t1, t4)

## Post - hoc for location

post <- emmeans(t4, ~ bait)  # Specify the fixed factor of interest

# Perform pairwise comparisons
pairs(post)

#plotting residuals

r <- residuals(t4)

# Plot residuals - if systematic patterns (ie funnel shape) indicates heteroscedasticity
# also look for large residuals not explained by the model
plot(r, main = "Residuals from Poisson MaxN no RE", 
     xlab = "Index", ylab = "Residuals")



#######################################################


p1 <- glmer(maxn ~ bait + (1|site), data = maxn.all,
            family = "poisson")

summary(p1)
anova(p1)
AIC(p1)
#r.squaredGLMM(p1)

## CHECKING OVERDISPERSION OF POISSON DISTRIBUTION
#deviance / residual degrees of freedom

deviance(p1)/df.residual(p1) #should be close to 1 for good

# Compare the mean and variance of response
mean(maxn.all$maxn)
var(maxn.all$maxn)

mean(maxn.all$maxn)/var(maxn.all$maxn)

#plotting residuals

r <- residuals(p1)

# Plot residuals - if systematic patterns (ie funnel shape) indicates heteroscedasticity
# also look for large residuals not explained by the model
plot(r, main = "Residuals from Poisson MaxN", 
     xlab = "Index", ylab = "Residuals")

#comparing with location
l1 <- glmer(maxn ~ bait + (1|location), data = maxn.all,
            family = "poisson")

summary(l1)
AIC(p1, l1)
BIC(p1, l1)


deviance(l1)/df.residual(l1) #should be close to 1 for good

#plotting residuals

r <- residuals(l1)

# Plot residuals 
plot(r, main = "Residuals from Poisson MaxN", 
     xlab = "Index", ylab = "Residuals")

####
### with mean.relief
p2 <- glmer(maxn ~ bait + mean_relief + (1|location),
            data = maxn.all,
            family = "poisson")
summary(p2) ##AIC > than l1


### with Scytothalia

p3 <- glmer(maxn ~ bait + scytothalia + (1|location),
            data = maxn.all,
            family = "poisson")
summary(p3)

### with Ecklonia

p4 <- glmer(maxn ~ bait +  ecklonia + (1|location),
            data = maxn.all,
            family = "poisson")
summary(p4)

###
# with depth

p5 <- glmer(maxn ~ bait +  depth_m + (1|location),
            data = maxn.all,
            family = "poisson")
summary(p5)

###
# with site nested in location

l2 <-glmer(maxn~ bait + (site|location),
           data = maxn.all,
           family = "poisson")

summary(l2)
deviance(l2)/df.residual(l2)


##############
### Running with Location as fixed effect


mod2 <- glm(maxn ~ bait + location, data = maxn.all,
              family = "poisson")

summary(mod2)
Anova(mod2)
logLik(mod2)
AIC(mod2)
BIC(mod2)
#r.squaredGLMM(mod2)

## CHECKING OVERDISPERSION OF POISSON DISTRIBUTION

deviance(mod2)/df.residual(mod2)

# Mean and variance of the response variable
mean(maxn.all$maxn)/var(maxn.all$maxn)

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


################################################
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

#############################################################################
### Running as presence/absence



mod <- glmer(presence ~ bait + (1 | location),
             family = binomial, #if overdispersed use family = quasibinomial
             data = maxn.all)

summary(mod)
Anova(mod)

#testing for overdispersion
# Assuming your model is called 'mod'
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


###########################################
## TIME TO MAXN

omit.zeros <- maxn.all %>% #removing drops with no bg
  filter(!is.na(titomaxn_s))%>%
  glimpse()

which(is.na(omit.zeros$titomaxn_s)) #no zeros

#visualsing to select family
# histogram
ggplot(omit.zeros, aes(x = titomaxn_s)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(title = "Distribution of Time to MaxN (seconds)",
       x = "Time (seconds)", y = "Count")

#density plot
ggplot(omit.zeros, aes(x = titomaxn_s)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  theme_minimal() +
  labs(title = "Density Plot of Time to MaxN",
       x = "Time (seconds)", y = "Density")

#by bait type

ggplot(omit.zeros, aes(x = bait, y = titomaxn_s)) +
  geom_boxplot(fill = "lightgreen") +
  theme_minimal() +
  labs(title = "Time to MaxN by Bait Type",
       x = "Bait", y = "Time (seconds)")

##modelling
#library(glmmTMB) because glmer() in lme4 package doesn't support
#gamma distribution

model <- glmmTMB(titomaxn_s ~ bait + (1 | location),
                 data = omit.zeros,
                 family = Gamma(link = "log"))

summary(model)
Anova(model)

# Post hoc test for 'bait' variable
posthoc_results <- emmeans(model, ~ bait)

# Summary of the post hoc results
summary(posthoc_results)

# Pairwise comparisons between levels of 'bait'
pairwise_comparisons <- contrast(posthoc_results, method = "pairwise")
summary(pairwise_comparisons)


# Function to calculate overdispersion
check_overdispersion <- function(model) {
  rp <- residuals(model, type = "pearson")
  rdf <- df.residual(model)
  ratio <- sum(rp^2) / rdf
  cat("Dispersion ratio:", round(ratio, 2), "\n")
  if (ratio > 1.2) {
    cat("Potential overdispersion detected.\n")
  } else {
    cat("No strong evidence of overdispersion.\n")
  }
}

check_overdispersion(model) #1.06 so tiny bit overdispersed but not enough
#to affect the model results

#residuals vs fixed

res_df <- data.frame(
  fitted = fitted(model),
  resid_pearson = residuals(model, type = "pearson")
)

ggplot(res_df, aes(x = fitted, y = resid_pearson)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Pearson Residuals vs Fitted Values",
       x = "Fitted values", y = "Pearson residuals")

#QQ plot of residuals
res_df$resid_scaled <- scale(res_df$resid_pearson)

ggplot(res_df, aes(sample = resid_scaled)) +
  stat_qq() +
  stat_qq_line() +
  theme_minimal() +
  labs(title = "QQ Plot of Pearson Residuals")

# more robust testing with dharma
library(DHARMa)

simres <- simulateResiduals(fittedModel = model, plot = TRUE)

testZeroInflation(simres) #test zero inflation - 
#indicating that I don't need to worry about
#zero inflation

plotResiduals(simres, form = omit.zeros$bait)

summary(model)$varcor #checking variance of random effect -- very low so doesn't
#contribute much

#install.packages("sjPlot")
library(sjPlot)
plot_model(model, type = "re", sort.est = TRUE)

#seeing if zero inflation better
model_zi <- glmmTMB(
  titomaxn_s ~ bait + (1 | location),
  data = omit.zeros,
  ziformula = ~1,  # Adds a zero-inflation component
  family = Gamma(link = "log")
)

AIC(model, model_zi)
anova(model, model_zi)

## forward stepwise
model2 <- glmmTMB(titomaxn_s ~ bait + depth_m + (1 | location),
                 data = omit.zeros,
                 family = Gamma(link = "log"))

summary(model2)
Anova(model2)
AIC(model, model2)

summary(model)

model3 <- glmmTMB(titomaxn_s ~ bait + mean_relief + (1 | location),
                  data = omit.zeros,
                  family = Gamma(link = "log"))

AIC(model, model3)

model4 <- glmmTMB(titomaxn_s ~ bait + scytothalia + (1|location),
                  data = omit.zeros,
                  family = Gamma(link = "log"))

AIC(model, model4)
summary(model4)

model5 <- glmmTMB(titomaxn_s ~ bait + ecklonia + (1|location),
                  data = omit.zeros,
                  family = Gamma(link = "log"))

AIC(model, model5)

model6 <- glmmTMB(titomaxn_s ~ bait + macroalgae + (1|location),
                  data = omit.zeros,
                  family = Gamma(link = "log"))
AIC(model, model6)

model7 <- glmmTMB(titomaxn_s ~ bait + date + (1|location),
                  data = omit.zeros,
                  family = Gamma(link = "log"))
AIC(model, model7)

model.nore <- glmmTMB(titomaxn_s ~ bait + location,
                  data = omit.zeros,
                  family = Gamma(link = "log"))

AIC(model, model.nore)

## sussing Time of day -- move up when done

# Convert to POSIXct time (using any dummy date, since you only care about time)
omit.zeros$time_of_day <- as.POSIXct(omit.zeros$time, format = "%H:%M:%S")

# Convert to seconds since midnight

omit.zeros$time_sec <- as.numeric(format(omit.zeros$time_of_day, "%H")) * 3600 +
  as.numeric(format(omit.zeros$time_of_day, "%M")) * 60 +
  as.numeric(format(omit.zeros$time_of_day, "%S"))


omit.zeros$time_hr <- omit.zeros$time_sec / 3600

model_with_time <- glmmTMB(titomaxn_s ~ bait + time_hr + (1 | location),
                           data = omit.zeros,
                           family = Gamma(link = "log"))

summary(model_with_time)
AIC(model, model_with_time)

#with time as factor
omit.zeros$time_block <- cut(omit.zeros$time_hr,
                             breaks = c(0, 6, 12, 18, 24),
                             labels = c("Night", "Morning", "Afternoon", "Evening"),
                             right = FALSE)

model_block <- glmmTMB(titomaxn_s ~ bait + time_block + (1 | location),
                       data = omit.zeros,
                       family = Gamma(link = "log"))


AIC(model, model_block)
# ###########################################
# ## Likelihood ratio tests on best model
# 
# lrt1.both <- glmer(maxn ~ bait + Scytothalia + (1|location) + (1|site),
#                       data = maxn.all,
#                       family = "poisson")
# lrt2.loc <- glmer(maxn ~ bait + Scytothalia + (1|location),
#                   data = maxn.all,
#                   family = "poisson")
# 
# lrt3.site <- glmer(maxn ~ bait + Scytothalia + (1|site),
#                    data = maxn.all,
#                    family = "poisson")
# 
# lrt4.nest <- glmer(maxn ~ bait + Scytothalia + (site|location),
#                    data = maxn.all,
#                    family = "poisson")
# 
# anova(lrt2.loc, lrt1.both)
# 
# anova(lrt3.site, lrt1.both)
# 
# anova(lrt2.loc, lrt3.site)
# 
# anova(lrt3.site, lrt4.nest)
# 

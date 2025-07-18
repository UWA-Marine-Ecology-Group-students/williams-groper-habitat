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
  dplyr::select(-c(behaviour_success, approach_success))%>%
  # dplyr::select(-c(titomaxn_s, titomaxn_m, periodtime))%>% NAs only exist in these columns
  left_join(habitat)%>% #joining to habitat 
  dplyr::filter(opcode != "046")%>% #no habitat data for 046
  clean_names()%>%
  glimpse()

sum(maxn.all$maxn)
unique(maxn.all$species)
length(unique(maxn.all$opcode)) #should be 100
length(unique(maxn.all$site))
maxn.all[!complete.cases(maxn.all), ]
anyNA(maxn.all)
sum(is.na(maxn.all))

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

# maxn by location
maxn.all %>%
  group_by(location) %>%
  summarise(
    mean_maxn = mean(maxn, na.rm = TRUE),
    median_maxn = median(maxn, na.rm = TRUE),
    min_maxn = min(maxn, na.rm = TRUE),
    max_maxn = max(maxn, na.rm = TRUE),
    range_maxn = max_maxn - min_maxn, 
    sum_maxn=sum(maxn))

# maxn by location
maxn.all %>%
  group_by(site) %>%
  summarise(
    mean_maxn = mean(maxn, na.rm = TRUE),
    median_maxn = median(maxn, na.rm = TRUE),
    min_maxn = min(maxn, na.rm = TRUE),
    max_maxn = max(maxn, na.rm = TRUE),
    range_maxn = max_maxn - min_maxn, 
    sum_maxn=sum(maxn))

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

# depth as colour and circle size as maxn
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


# %ecklonia as colour and circle size as maxn
pal <- colorNumeric(
  palette = "YlOrRd",
  domain = maxn.all$ecklonia)

leaflet(maxn.all) %>%
  addTiles() %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(~longitude_dd, ~latitude_dd, radius = ~maxn.all$maxn,
                   color = ~pal(ecklonia),
                   stroke = FALSE, label = ~as.character(opcode),
                   fillOpacity = 0.8) %>%
  addLegend("bottomright",
            pal = pal,
            values = ~ ecklonia,
            title = "% Ecklonia",
            opacity = 0.8)


# Checking predictor variables and running a Generalized Linear Model (GLM)
## Check correlation of co-variates
# Set the predictors for modeling - don't include factors - just continuous var 
pred.vars <- c("depth_m", "macroalgae", "scytothalia", "ecklonia",
               "sessile_inverts", "mean_relief", "time_hr") 


# Check the correlations between predictor variables - looking for NAs
summary(maxn.all[,pred.vars])


#checking for correlations between variables
round(cor(maxn.all[ , pred.vars]), 2)

# check individual predictors to see if any need transformed
CheckEM::plot_transformations(pred.vars = pred.vars, dat = maxn.all)

outdir <- "output/baitcomp/maxn.all"
for (var in pred.vars) {
  png(filename = file.path(outdir, paste0(var, ".png")), width = 800, height = 600, res = 150)
  CheckEM::plot_transformations(pred.vars = var, dat = maxn.all)
  dev.off()
}


#### Stepwise model approach

# t1 <- glmer(maxn ~ (1|location/site),
#             data = maxn.all,
#             family = poisson)
# 
# summary(t1)

t2 <- glmer(maxn ~ ecklonia + (1|location/site),
            data = maxn.all,
            family = poisson) 

summary(t2)
            
t3 <- glmer(maxn ~ bait + (1|location/site),
            data = maxn.all,
            family = poisson)

summary(t3)
AIC(t2, t3)

t4 <- glmer(maxn ~ depth_m + (1|location/site),
            data = maxn.all,
            family = poisson)

summary(t4)
AIC(t2, t4)

t5 <- glmer(maxn ~ macroalgae + (1|location/site),
            data = maxn.all,
            family = poisson)
AIC(t2, t5)

t6 <- glmer(maxn ~ scytothalia + (1|location/site),
            data = maxn.all,
            family = poisson)
AIC(t2, t6)

t7 <- glmer(maxn ~ mean_relief + (1|location/site),
            data = maxn.all,
            family = poisson)

AIC(t2, t7)

t8 <- glmer(maxn ~ sessile_inverts + (1|location/site),
           data = maxn.all,
           family = poisson)

AIC(t2, t8)

t9 <- glmer(maxn ~ time_hr + (1|location/site),
            data = maxn.all,
            family = poisson)

AIC(t2, t9)

#### With 3 predictors
p1 <- glmer(maxn ~ ecklonia + bait + (1|location/site),
            data = maxn.all,
            family = "poisson")


summary(p1)
AIC(t2, p1)

### with mean.relief
p2 <- glmer(maxn ~ ecklonia + mean_relief + (1|location/site),
            data = maxn.all,
            family = "poisson")

summary(p2) 
AIC(t2, p2)

### with Scytothalia

p3 <- glmer(maxn ~ ecklonia + scytothalia + (1|location/site),
            data = maxn.all,
            family = "poisson")

AIC(t2, p3)


### with depth

p4 <- glmer(maxn ~ ecklonia + depth_m + (1|location/site),
            data = maxn.all,
            family = "poisson")
AIC(t2, p4)


p5 <- glmer(maxn ~ ecklonia + time_hr + (1|location/site),
            data = maxn.all,
            family = "poisson")
AIC(t2, p5)

p6 <- glmer(maxn ~ ecklonia + time_sec + (1|location/site),
            data = maxn.all,
            family = "poisson")

AIC(t2, p6)

## checking relationship between %ecklonia cover and bait type

anova_result <- aov(ecklonia ~ bait, data = maxn.all)

summary(anova_result)

# Check normality of residuals
plot(anova_result, 2)  # QQ plot
shapiro.test(residuals(anova_result))

# Check homogeneity of variance
plot(anova_result, 1)  # Residuals vs Fitted

TukeyHSD(anova_result)


kruskal.test(ecklonia ~ bait, data = maxn.all)
# install.packages("FSA")
library(FSA)
dunnTest(ecklonia ~ bait, data = maxn.all, method = "bonferroni")

bait_col <- c("abalone" = "#27ae60", 
              "octopus" = "#f39c12" , 
              "pilchard" = "#CC79A7" )

eck.plot <- ggplot(maxn.all, aes(x= bait, y = ecklonia, fill = bait))+
  stat_summary(fun = mean, geom = "bar", color = "black", width = 0.6) +  # Bars
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +  # Error bars with standard error
  labs(x = "Bait Type", y = "% cover Ecklonia ", title = "Bonferroni Adj.")+
  scale_fill_manual(values = bait_col)+
  scale_x_discrete(labels = c("Abalone", "Octopus", "Pilchard"))+
  theme_cowplot()+
  theme(legend.position = "none")
eck.plot
# # Abalone vs Octopus
eck.plot + 
  geom_segment(aes(x = 1, xend = 2, y = 41, yend = 41), linewidth = 0.6) +  
  geom_segment(aes(x = 1, xend = 1, y = 40.5, yend = 41.5), linewidth = 0.6) +  
  geom_segment(aes(x = 2, xend = 2, y = 40.5, yend = 41.5), linewidth = 0.6) +
  annotate("text", x = 1.5, y = 41.5, label = "p = 0.1", size = 3) +
  #Ab - Pilchard Line and Ticks
  geom_segment(aes(x = 1, xend = 3, y = 42, yend = 42), linewidth = 0.6) +  
  geom_segment(aes(x = 1, xend = 1, y = 41.5, yend = 42.5), linewidth = 0.6) +
  geom_segment(aes(x = 3, xend = 3, y = 41.5, yend = 42.5), linewidth = 0.6) +
  annotate("text", x = 2, y = 42.5, label = "p = 1.0", size = 3)+
  ## Octopus vs Pilchard
  geom_segment(aes(x = 2, xend = 3, y = 35, yend = 35), linewidth = 0.6) +  
  geom_segment(aes(x = 2, xend = 2, y = 34.5, yend = 35.5), linewidth = 0.6) +
  geom_segment(aes(x = 3, xend = 3, y = 34.5, yend = 35.5), linewidth = 0.6) +
  annotate("text", x = 2.5, y = 35.5, label = "p = 0.1", size = 3)


################################
##### 
## TODO == tidy below


## Post - hocs 
# 
# post <- emmeans(t, ~ bait)  # Specify the fixed factor of interest
# 
# # Perform pairwise comparisons
# pairs(post)
# 
# #plotting residuals
# 
# r <- residuals(t3)
# 
# # Plot residuals - if systematic patterns (ie funnel shape) indicates heteroscedasticity
# # also look for large residuals not explained by the model
# plot(r, xlab = "Index", ylab = "Residuals")



#######################################################

#TODO - qq plots

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



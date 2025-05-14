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
library(multcomp)
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
  dplyr::mutate(presence = ifelse(maxn > 0, 1, 0))%>%
  dplyr::mutate(titomaxn_s = periodtime * 60)%>% #creating covariate of time to maxn in seconds only
  dplyr::mutate(titomaxn_m = periodtime)%>% #creating covariate of titomaxn in mins (same as periodtime)
  clean_names() %>% 
  glimpse()


length(unique(maxn.stage$opcode))
length(unique(maxn.stage$stage))
#96*8 = 768
#96*5 = 480 #after removing AD, F, M


##TODO
## sussing Time of day -- move up when done

maxn.stage$time_of_day <- as.POSIXct(maxn.stage$time, format = "%H:%M:%S")
# Convert to seconds since midnight

maxn.stage$time_sec <- as.numeric(format(maxn.stage$time_of_day, "%H")) * 3600 +
  as.numeric(format(maxn.stage$time_of_day, "%M")) * 60 +
  as.numeric(format(maxn.stage$time_of_day, "%S"))
maxn.stage$time_hr <- maxn.stage$time_sec / 3600

#with time as factor
maxn.stage$time_block <- cut(maxn.stage$time_hr,
                             breaks = c(0, 6, 12, 18, 24),
                             labels = c("Night", "Morning", "Afternoon", "Evening"),
                             right = FALSE)


###########################################################################

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

##### -- comparing best model with inclusion of date as random effect
best <- glmer(maxn ~ bait + stage + depth_m + (1|location), 
              data = maxn.stage,
                  family = "poisson")
summary(best)

date.mod <- glmer(maxn ~ bait + stage + depth_m + (1|location) + (1|date), 
                  data = maxn.stage,
                  family = "poisson")

summary(date.mod)

tod <- glmer(maxn ~ bait + stage + depth_m + time_hr + (1|location), 
             data = maxn.stage,
             family = "poisson")
summary(tod)

tod_fact <- glmer(maxn ~ bait + stage + depth_m + time_block + (1|location), 
             data = maxn.stage,
             family = "poisson")

summary(tod_fact)

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
#############################################################################
### Running as presence/absence
#visualising presence/absence by bait

ggplot(maxn.stage, aes(x = bait, y = presence)) +
  geom_boxplot(alpha = 0.5) +
  stat_summary(fun = mean, geom = "bar", width = 0.6) +  # Bar plot with mean
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = 0.5, color = "black") +
  theme_cowplot()+
  facet_wrap(.~stage, ncol = 2)+
  theme(legend.position = "none")


mod <- glmer(presence ~ bait + stage + depth_m + (1 | location),
  family = binomial, #if overdispersed use family = quasibinomial
  data = maxn.stage)

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


# Create significance labels based on p-value
pairwise_df$significance <- case_when(
  pairwise_df$p.value < 0.001 ~ "***",
  pairwise_df$p.value < 0.01 ~ "**",
  pairwise_df$p.value < 0.05 ~ "*",
  TRUE ~ "ns"  # Not significant
)

# Plot with log-transformed odds ratio (log-odds) and significance asterisks
ggplot(pairwise_df, aes(x = log(odds.ratio), y = contrast)) + 
  geom_point(size = 3) +  # Show the log-odds ratio as points
  geom_errorbarh(aes(xmin = log(odds.ratio) - SE, xmax = log(odds.ratio) + SE), height = 0.2) +  # Add horizontal error bars for CI
  geom_text(aes(label = significance), hjust = -0.2, size = 5) +  # Add significance labels (asterisks)
  theme_minimal() +
  ylab("Pairwise Comparisons (Stage)") +
  xlab("Log-Odds Ratio") +
  ggtitle("Pairwise Comparisons for Stage (Log-Odds)") +
  theme(axis.text.y = element_text(size = 10))

### POSTHOC ON BAIT
# Run the pairwise comparisons for the 'stage' variable
emm_stage <- emmeans(mod, ~ bait, type = "response")
pairwise_stage <- pairs(emm_stage, adjust = "tukey")
pairwise_stage

# Convert to data frame
pairwise_df <- as.data.frame(pairwise_stage)
pairwise_df

# Create significance labels based on p-value
pairwise_df$significance <- case_when(
  pairwise_df$p.value < 0.001 ~ "***",
  pairwise_df$p.value < 0.01 ~ "**",
  pairwise_df$p.value < 0.05 ~ "*",
  TRUE ~ "ns"  # Not significant
)

# Plot with log-transformed odds ratio (log-odds) and significance asterisks
ggplot(pairwise_df, aes(x = log(odds.ratio), y = contrast)) + 
  geom_point(size = 3) +  # Show the log-odds ratio as points
  geom_errorbarh(aes(xmin = log(odds.ratio) - SE, xmax = log(odds.ratio) + SE), height = 0.2) +  # Add horizontal error bars for CI
  geom_text(aes(label = significance), hjust = -0.2, size = 5) +  # Add significance labels (asterisks)
  theme_minimal() +
  ylab("Pairwise Comparisons (Stage)") +
  xlab("Log-Odds Ratio") +
  ggtitle("Pairwise Comparisons for Bait (Log-Odds)") +
  theme(axis.text.y = element_text(size = 10))

############################################# 
###########################################
## TIME TO MAXN

omit.zeros <- maxn.stage %>% #removing rows with no bg
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
anova(model, model_zi) #zeroinflation mdoel not necessary

model_op <- glmmTMB(titomaxn_s ~ bait +  (1|opcode),
                    data = omit.zeros,
                    family = Gamma(link = "log"))
summary(model_op)
anova(model, model_op)
AIC(model_op)

model_nest <- glmmTMB(titomaxn_s ~ bait +  (1|location/opcode),
                    data = omit.zeros,
                    family = Gamma(link = "log"))
summary(model_nest)

model2 <- glmmTMB(titomaxn_s ~ bait + depth_m +  (1|opcode),
        data = omit.zeros,
        family = Gamma(link = "log"))
summary(model2)

model3 <- glmmTMB(titomaxn_s ~ bait + stage + depth_m +  (1|opcode),
                  data = omit.zeros,
                  family = Gamma(link = "log"))
summary(model3)
Anova(model3)
AIC(model3)

# Post hoc test for 'bait' variable
posthoc_results <- emmeans(model_op, ~ bait)

# Summary of the post hoc results
summary(posthoc_results)

# Pairwise comparisons between levels of 'bait'
pairwise_comparisons <- contrast(posthoc_results, method = "pairwise")
summary(pairwise_comparisons)


# Function to calculate overdispersion
check_overdispersion <- function(model) {
  rp <- residuals(model_op, type = "pearson")
  rdf <- df.residual(model_op)
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
  fitted = fitted(model_op),
  resid_pearson = residuals(model_op, type = "pearson")
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
#library(DHARMa)

simres <- simulateResiduals(fittedModel = model_op, plot = TRUE)

testZeroInflation(simres) #test zero inflation - 
#indicating that I don't need to worry about
#zero inflation

#plotResiduals(simres, form = omit.zeros$bait)

summary(model_op)$varcor #checking variance of random effect -- very low so doesn't
#contribute much

mod_stage <- glmmTMB(titomaxn_s ~ bait + stage +  (1|opcode),
                     data = omit.zeros,
                     family = Gamma(link = "log"))
summary(mod_stage)

mod_eck <- glmmTMB(titomaxn_s ~ bait + ecklonia +  (1|opcode),
                     data = omit.zeros,
                     family = Gamma(link = "log"))
summary(mod_eck)

mod_scy <- glmmTMB(titomaxn_s ~ bait + scytothalia +  (1|opcode),
                   data = omit.zeros,
                   family = Gamma(link = "log"))
summary(mod_scy)

mod_date <- glmmTMB(titomaxn_s ~ bait + date +  (1|opcode),
                    data = omit.zeros,
                    family = Gamma(link = "log"))

summary(mod_date)

mod_date_nested <- glmmTMB(titomaxn_s ~ bait  +  (1|date/opcode),
                           data = omit.zeros,
                           family = Gamma(link = "log"))
summary(mod_date_nested)

## sussing Time of day -- move up when done

# Convert to POSIXct time (using any dummy date, since you only care about time)
omit.zeros$time_of_day <- as.POSIXct(omit.zeros$time, format = "%H:%M:%S")

# Convert to seconds since midnight

omit.zeros$time_sec <- as.numeric(format(omit.zeros$time_of_day, "%H")) * 3600 +
  as.numeric(format(omit.zeros$time_of_day, "%M")) * 60 +
  as.numeric(format(omit.zeros$time_of_day, "%S"))


omit.zeros$time_hr <- omit.zeros$time_sec / 3600

model_with_time <- glmmTMB(titomaxn_s ~ bait + time_hr + (1 | opcode),
                           data = omit.zeros,
                           family = Gamma(link = "log"))

summary(model_with_time)
AIC(model, model_with_time)

model_time_nest <- glmmTMB(titomaxn_s ~ bait + time_hr + (1 | date/opcode),
                           data = omit.zeros,
                           family = Gamma(link = "log"))

summary(model_time_nest)
#with time as factor
omit.zeros$time_block <- cut(omit.zeros$time_hr,
                             breaks = c(0, 6, 12, 18, 24),
                             labels = c("Night", "Morning", "Afternoon", "Evening"),
                             right = FALSE)

model_block <- glmmTMB(titomaxn_s ~ bait + time_block + (1 | opcode),
                       data = omit.zeros,
                       family = Gamma(link = "log"))


summary(model_block)

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
  left_join(habitat)%>%
  dplyr::filter(opcode != "046")%>%
  dplyr::filter(opcode != "078")%>% #remove drops only M F and AD recorded
  dplyr::filter(opcode != "082")%>% #remove drops only M F and AD recorded
  dplyr::filter(!stage %in% c("AD", "M", "F"))%>% #filtering out these
  dplyr::mutate(presence = ifelse(maxn > 0, 1, 0))%>%
  clean_names() %>% 
  glimpse()


length(unique(maxn.stage$opcode))
length(unique(maxn.stage$stage))
#96*8 = 768
#96*5 = 480 #after removing AD, F, M



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


##occurrence of each size class 
stage_sums <- maxn.stage %>%
  group_by(stage) %>%
  summarise(total_maxn = sum(maxn, na.rm = TRUE)) %>%
  arrange(stage)%>%
  glimpse()

#plot
ggplot(stage_sums, aes(x = stage, y = total_maxn)) +
  geom_col(fill = "darkgreen") +
  labs(
    x = "Size class (mm)",
    y = "Total MaxN",
    title = "Total MaxN per Size Class"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



##################################
## Stepwise modelling approach

sc1 <- glmer(maxn ~ bait + stage + (1|location/site), data = maxn.stage,
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



## interaction between bait and stage
sc3 <- glmer(maxn ~ bait*stage + (1|location/site), data = maxn.stage,
             family = "poisson")
summary(sc3)
Anova(sc3) #no interaction effect - moving on to next model

## depth
sc4 <- glmer(maxn ~ bait + stage + depth_m +(1|location/stage), data = maxn.stage,
             family = "poisson")


AIC(sc1, sc4)

## mean relief
sc5 <- glmer(maxn ~ bait + stage + mean_relief +(1|location/site), data = maxn.stage,
             family = "poisson")
AIC(sc4, sc5)

## ecklonia
sc6 <- glmer(maxn ~ bait + stage + ecklonia +(1|location/site), data = maxn.stage,
             family = "poisson")

AIC(sc4, sc6)

## scytothalia
sc7 <- glmer(maxn ~ bait + stage + scytothalia +(1|location/site), data = maxn.stage,
             family = "poisson")

AIC(sc4, sc7)

## macroalgae
sc8 <- glmer(maxn ~ bait + stage + macroalgae +(1|location/site
                                                ), data = maxn.stage,
             family = "poisson")
AIC(sc4, sc8)

sc9 <- glmer(maxn ~ bait + stage + time_hr +(1|location/site), 
             data = maxn.stage,
              family = "poisson")

AIC(sc4, sc9)

### models without bait
## depth_m no bait
nob1 <- glmer(maxn ~ stage + depth_m + (1|location/site), data = maxn.stage,
             family = "poisson")

AIC(sc4, nob1)


## ecklonia no bait
nob2 <- glmer(maxn ~ stage + ecklonia + (1|location/site), data = maxn.stage,
              family = "poisson")

AIC(sc4, nob2)


nob3 <- glmer(maxn ~ stage + time_hr + (1|location/site), data = maxn.stage,
              family = "poisson")

AIC(sc4, nob3)

### with depth_m, ecklonia & bait
big1 <- glmer(maxn ~ bait + stage + ecklonia + depth_m +  (1|location/site), 
              data = maxn.stage,
              family = "poisson")

AIC(sc4, big1)

#with stage as a random effect
big2 <- glmer(maxn ~ bait + ecklonia + depth_m + (1|stage) + (1|location/site), 
              data = maxn.stage,
              family = "poisson")

AIC(sc4, big2) #shouldn't compare with AIC but out of interest
summary(big2)

###########
## BEST MODEL = sc4
## depth
sc4 <- glmer(maxn ~ bait + stage + depth_m +(1|location/stage), data = maxn.stage,
             family = "poisson")


AIC(sc1, sc4)

############################################################################3
##############################################################################
## best model
sc4 <- glmer(maxn ~ bait + stage + depth_m + (1|location/stage), data = maxn.stage,
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

# running some posthocs
post <- emmeans(sc4, ~ bait)  # Specify the fixed factor of interest

# Perform pairwise comparisons
pairs(post)

baitpairs <- pairs(post)
print(baitpairs)

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
post2 <- emmeans(sc4, ~ stage)  # Specify the fixed factor of interest

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

# ### MaxN by Depth with the actual model line
new_data <- expand.grid(
  depth_m = seq(min(maxn.stage$depth_m), max(maxn.stage$depth_m), length.out = 100),
  bait = unique(maxn.stage$bait)[1],  # Choose first level if categorical
  stage = unique(maxn.stage$stage)[1]
)

# Get predictions (fixed effects only)
new_data$predicted <- predict(sc4, newdata = new_data, re.form = NA, type = "response")

# Get predictions and standard errors
pred <- predict(sc4, newdata = new_data, re.form = NA, type = "link", se.fit = TRUE)

# Convert to response scale (Poisson uses log link)
new_data$predicted <- exp(pred$fit)
new_data$lower <- exp(pred$fit - 1.96 * pred$se.fit)  # 95% CI lower bound
new_data$upper <- exp(pred$fit + 1.96 * pred$se.fit)  # 95% CI upper bound

# Plot data with model predictions
depth<-
  ggplot(maxn.stage, aes(x = depth_m, y = maxn)) +
  geom_jitter(alpha = 0.5) +
  labs(x = "Depth (m)", y = "WBG Abundance") +
  geom_line(data = new_data, aes(x = depth_m, y = predicted),
            colour = "darkblue", linewidth = 1, inherit.aes = F) +
  geom_ribbon(data = new_data, aes(x = depth_m, ymin = lower, ymax = upper),
              fill = "darkblue", alpha = 0.2, inherit.aes = F) +  # Shaded confidence band
  theme_cowplot() +
  theme(legend.position = "none")

depth

#################
## plot saving ##
#################

# 
folder_path <- "./plots/baitcomp/maxn.stage"

# change title
png(file.path(folder_path, "maxn.stage.bestmodel-depth.png"), width = 600, height = 400)

# plot code

depth

# Close the PNG device
dev.off()

##############################

bait_col <- c("abalone" = "#27ae60", 
              "octopus" = "#f39c12" , 
              "pilchard" = "#CC79A7" )

# 'Dynamite' Plot of MaxN by Bait with post hocs
print(baitpairs)

maxnSC.bait.post <-
  ggplot(maxn.stage, aes(x= bait, y = maxn, fill = bait))+
  stat_summary(fun = mean, geom = "bar", color = "black", width = 0.6) +  # Bars
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +  # Error bars with standard error
  labs(x = "Bait Type", y = "Mean Abundance")+
  scale_fill_manual(values = bait_col)+
  scale_x_discrete(labels = c("Abalone", "Octopus", "Pilchard"))+
  theme_cowplot()+
  theme(legend.position = "none")+
  # # Abalone vs Octopus
  geom_segment(aes(x = 1, xend = 2, y = 0.6, yend = 0.6), linewidth = 0.6) +  
  geom_segment(aes(x = 1, xend = 1, y = 0.6, yend = 0.575), linewidth = 0.6) +  
  geom_segment(aes(x = 2, xend = 2, y = 0.6, yend = 0.575), linewidth = 0.6) +
  annotate("text", x = 1.5, y = 0.61, label = "p = 0.021", size = 3) +
  #Ab - Pilchard Line and Ticks
  geom_segment(aes(x = 1, xend = 3, y = 0.48, yend = 0.48), linewidth = 0.6) +  
  geom_segment(aes(x = 1, xend = 1, y = 0.48, yend = 0.475), linewidth = 0.6) +
  geom_segment(aes(x = 3, xend = 3, y = 0.48, yend = 0.475), linewidth = 0.6) +
  annotate("text", x = 2, y = 0.49, label = "p = 0.305", size = 3)+
  ## Octopus vs Pilchard
  geom_segment(aes(x = 2, xend = 3, y = 0.39, yend = 0.39), linewidth = 0.6) +  
  geom_segment(aes(x = 2, xend = 2, y = 0.39, yend = 0.3875), linewidth = 0.6) +
  geom_segment(aes(x = 3, xend = 3, y = 0.39, yend = 0.3875), linewidth = 0.6) +
  annotate("text", x = 2.5, y = 0.4, label = "p = 0.386", size = 3)

maxnSC.bait.post

#################
## plot saving ##
#################

# 
folder_path <- "./plots/baitcomp/maxn.stage"

# change title
png(file.path(folder_path, "maxn.stage.bestmodel-bait.png"), width = 600, height = 400)

# plot code

maxnSC.bait.post

# Close the PNG device
dev.off()

######################################3
## without the big ones
noelders <- maxn.stage %>%
  dplyr::filter(!stage %in% c("1100-1299mm"))%>%
  glimpse()


mod1 <- glmer(maxn ~ bait + stage + (1|location/site),
              data = noelders,
              family = poisson)
summary(mod1)

mod2 <- glmer(maxn ~ ecklonia + stage + (1|location/site),
              data = noelders,
              family = poisson)
AIC(mod1, mod2)

mod3 <- glmer(maxn ~ depth_m + stage + (1|location/site),
              data = noelders,
              family = poisson)
AIC(mod1, mod3) #mod 3 better


mod4 <- glmer(maxn ~ depth_m + bait + (1|location/site),
              data = noelders,
              family = poisson)
AIC(mod3, mod4) #mod 4 better - but also need to include stage hah

mod5 <- glmer(maxn ~ time_hr + stage + (1|location/site),
                   data = noelders,
                   family = poisson)
AIC(mod3, mod5)

mod6 <- glmer(maxn ~ depth_m + bait + stage + (1|location/site),
              data = noelders,
              family = poisson)
AIC(mod3, mod6) #mod6 better

mod7 <- glmer(maxn ~ ecklonia + bait + stage + (1|location/site),
              data = noelders,
              family = poisson)
AIC(mod6, mod7)

mod8 <- glmer(maxn ~ depth_m + bait + stage +  ecklonia + (1|location/site),
              data = noelders,
              family = poisson)
AIC(mod6, mod8)

### best model without elders 

best <- glmer(maxn ~ depth_m + bait + stage + (1|location/site),
              data = noelders,
              family = poisson)
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

##############################
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
folder_path <- "./plots/baitcomp/"

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
folder_path <- "./plots/baitcomp/"

# change title
png(file.path(folder_path, "maxn.stage.bestmodel-bait.png"), width = 600, height = 400)

# plot code

maxnSC.bait.post

# Close the PNG device
dev.off()



###################################################################################
####    HYPOTHESIS 1.2 - MAXN BY SIZE CLASSWILL BE GREATER WITH AB BAIT      ######
###################################################################################

rm(list=ls())

# libraries----
#library(devtools)
library(tidyverse)
#library(mgcv)
#library(MuMIn)
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
  clean_names()%>%
  glimpse()


##TODO
## sussing Time of day -- move up when done

sum.stage$time_of_day <- as.POSIXct(sum.stage$time, format = "%H:%M:%S")
# Convert to seconds since midnight

sum.stage$time_sec <- as.numeric(format(sum.stage$time_of_day, "%H")) * 3600 +
  as.numeric(format(sum.stage$time_of_day, "%M")) * 60 +
  as.numeric(format(sum.stage$time_of_day, "%S"))
sum.stage$time_hr <- sum.stage$time_sec / 3600

#with time as factor
sum.stage$time_block <- cut(sum.stage$time_hr,
                             breaks = c(0, 6, 12, 18, 24),
                             labels = c("Night", "Morning", "Afternoon", "Evening"),
                             right = FALSE)


################################################################################
################################################################################ 
## BEST MODEl

best <-  glmer(maxn_sum ~ bait + depth_m + (1|location), data = sum.stage,
              family = "poisson")

summary(best)
Anova(best, type = "III")
post <- emmeans(best, ~ bait)
pairs(post)


#### MODEL PLOTS

bait_col <- c("abalone" = "#27ae60", 
              "octopus" = "#f39c12" , 
              "pilchard" = "#CC79A7" )

folder_path <- "./plots/baitcomp/" # for saving plots

# 'Dynamite' Plot of MaxN by Bait with post hocs


maxnSCsum.w.post <-
  ggplot(sum.stage, aes(x= bait, y = maxn_sum, fill = bait))+
  stat_summary(fun = mean, geom = "bar", color = "black", width = 0.6) +  # Bars
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +  # Error bars with standard error
  labs(x = "Bait Type", y = "Mean Abundance")+
  scale_fill_manual(values = bait_col)+
  scale_x_discrete(labels = c("Abalone", "Octopus", "Pilchard"))+
  theme_cowplot()+
  theme(legend.position = "none")+
  # # Abalone vs Octopus
  geom_segment(aes(x = 1, xend = 2, y = 2.7, yend = 2.7), linewidth = 0.6) +  
  geom_segment(aes(x = 1, xend = 1, y = 2.7, yend = 2.675), linewidth = 0.6) +  
  geom_segment(aes(x = 2, xend = 2, y = 2.7, yend = 2.675), linewidth = 0.6) +
  annotate("text", x = 1.5, y = 2.75, label = "p = 0.03", size = 3) +
  #Ab - Pilchard Line and Ticks
  geom_segment(aes(x = 1, xend = 3, y = 2.8, yend = 2.8), linewidth = 0.6) +  
  geom_segment(aes(x = 1, xend = 1, y = 2.8, yend = 2.775), linewidth = 0.6) +
  geom_segment(aes(x = 3, xend = 3, y = 2.8, yend = 2.775), linewidth = 0.6) +
  annotate("text", x = 2, y = 2.85, label = "p = 0.33", size = 3)+
  ## Octopus vs Pilchard
  geom_segment(aes(x = 2, xend = 3, y = 2.9, yend = 2.9), linewidth = 0.6) +  
  geom_segment(aes(x = 2, xend = 2, y = 2.9, yend = 2.875), linewidth = 0.6) +
  geom_segment(aes(x = 3, xend = 3, y = 2.9, yend = 2.875), linewidth = 0.6) +
  annotate("text", x = 2.5, y = 2.95, label = "p = 0.44", size = 3)

maxnSCsum.w.post
#################
## plot saving ##
#################
# 
folder_path <- "./plots/baitcomp/"

# change title
png(file.path(folder_path, "maxnSCsum.w.post.png"), width = 600, height = 400)

# plot code

maxnSCsum.w.post

# Close the PNG device
dev.off()



### MaxN by Depth with the actual model line
new_data <- expand.grid(
  depth_m = seq(min(sum.stage$depth_m), max(sum.stage$depth_m), length.out = 100),
  bait = unique(sum.stage$bait)[1],  # Choose first level if categorical
  mean.relief = mean(sum.stage$mean.relief, na.rm = TRUE)
)

# Get predictions (fixed effects only)
new_data$predicted <- predict(best, newdata = new_data, re.form = NA, type = "response")

# Get predictions and standard errors
pred <- predict(best, newdata = new_data, re.form = NA, type = "link", se.fit = TRUE)

# Convert to response scale (Poisson uses log link)
new_data$predicted <- exp(pred$fit)
new_data$lower <- exp(pred$fit - 1.96 * pred$se.fit)  # 95% CI lower bound
new_data$upper <- exp(pred$fit + 1.96 * pred$se.fit)  # 95% CI upper bound

# Plot data with model predictions
depth<-
  ggplot(sum.stage, aes(x = depth_m, y = maxn)) +
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

# change title
png(file.path(folder_path, "depth.png"), width = 600, height = 400)

# plot code

depth

# Close the PNG device
dev.off()


#######################################################
date <- glmer(maxn_sum ~ bait + depth_m + date + (1|location), data = sum.stage,
      family = "poisson")

summary(date)

date_nest <- glmer(maxn_sum ~ bait + depth_m + (1|date) + (1|location), data = sum.stage,
              family = "poisson")

summary(date_nest)

tod <- glmer(maxn_sum ~ bait + depth_m + time_hr + (1|location), data = sum.stage,
             family = "poisson")
summary(tod)
AIC(best, tod)

tod2 <- glmer(maxn_sum ~ bait + time_hr + (1|location), data = sum.stage,
             family = "poisson")
summary(tod2)
AIC(best, tod2)

tod_fact <- glmer(maxn_sum ~ bait + depth_m + time_block + (1|location), data = sum.stage,
             family = "poisson")
summary(tod_fact)
AIC(best, tod_fact)

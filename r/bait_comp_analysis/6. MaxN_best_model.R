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
#(performance)
#citation("emmeans")
#RStudio.Version()

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


##########################################################################
### BEST MODEl -- 
## TODO - tidy
####

best <- glmer(maxn ~ ecklonia + (1|location/site), data = maxn.all,
                    family = "poisson")

summary(best)

# Anova(best, type = "III")

post <- emmeans(best, ~ bait)
pairs(post)

## MODEL PLOTS
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






## 'Dynamite' Plot of MaxN by Bait with post hocs
# 
# 
# maxn.w.post <-
#   ggplot(maxn.all, aes(x= bait, y = maxn, fill = bait))+
#   stat_summary(fun = mean, geom = "bar", color = "black", width = 0.6) +  # Bars
#   stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +  # Error bars with standard error
#   labs(x = "Bait Type", y = "Mean Abundance")+
#   scale_fill_manual(values = bait_col)+
#   scale_x_discrete(labels = c("Abalone", "Octopus", "Pilchard"))+
#   theme_cowplot()+
#   theme(legend.position = "none")+
#   # # Abalone vs Octopus
#   geom_segment(aes(x = 1, xend = 2, y = 1.7, yend = 1.7), linewidth = 0.6) +  
#   geom_segment(aes(x = 1, xend = 1, y = 1.7, yend = 1.675), linewidth = 0.6) +  
#   geom_segment(aes(x = 2, xend = 2, y = 1.7, yend = 1.675), linewidth = 0.6) +
#   annotate("text", x = 1.5, y = 1.75, label = "p = 0.22", size = 3) +
#   #Ab - Pilchard Line and Ticks
#   geom_segment(aes(x = 1, xend = 3, y = 1.8, yend = 1.8), linewidth = 0.6) +  
#   geom_segment(aes(x = 1, xend = 1, y = 1.8, yend = 1.775), linewidth = 0.6) +
#   geom_segment(aes(x = 3, xend = 3, y = 1.8, yend = 1.775), linewidth = 0.6) +
#   annotate("text", x = 2, y = 1.85, label = "p = 0.67", size = 3)+
#   ## Octopus vs Pilchard
#   geom_segment(aes(x = 2, xend = 3, y = 1.9, yend = 1.9), linewidth = 0.6) +  
#   geom_segment(aes(x = 2, xend = 2, y = 1.9, yend = 1.875), linewidth = 0.6) +
#   geom_segment(aes(x = 3, xend = 3, y = 1.9, yend = 1.875), linewidth = 0.6) +
#   annotate("text", x = 2.5, y = 1.95, label = "p = 0.67", size = 3)
# 
# maxn.w.post


#################
## plot saving ##
#################
# # 
# folder_path <- "./plots/baitcomp/"
# 
# # change title
# png(file.path(folder_path, "maxn.w.post.png"), width = 600, height = 400)
# 
# # plot code
# 
# maxn.w.post
# 
# # Close the PNG device
# dev.off()


##################
### MaxN by ecklonia with the actual model line
new_data <- expand.grid(
  ecklonia = seq(min(maxn.all$ecklonia), max(maxn.all$ecklonia), length.out = 100)#,
  # bait = unique(maxn.all$bait)[1]  # Choose first level if categorical
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
eck.mod.plot <-
  ggplot(maxn.all, aes(x = ecklonia, y = maxn)) +
  geom_jitter(alpha = 0.5) +
  labs(x = "% cover of Ecklonia", y = "WBG Abundance") +
  geom_line(data = new_data, aes(x = ecklonia, y = predicted),
            colour = "darkgreen", linewidth = 1, inherit.aes = F) +
  geom_ribbon(data = new_data, aes(x = ecklonia, ymin = lower, ymax = upper),
              fill = "darkgreen", alpha = 0.2, inherit.aes = F) +  # Shaded confidence band
  theme_cowplot() +
  theme(legend.position = "none")+
  annotate("text", x = 25, y = 6, label = "p = 0.104", size = 5)

eck.mod.plot

#################
## plot saving ##
#################
#
folder_path <- "./plots/baitcomp/"

# change title
png(file.path(folder_path, "bestmaxn_model.png"), width = 600, height = 400)

# plot code

eck.mod.plot

# Close the PNG device
dev.off()



########################################
##
##    MAXN subsetted  = individual size classes

rm(list=ls())

# libraries----
#library(devtools)
library(tidyverse)
library(CheckEM)
#library(mgcv)
# library(MuMIn)
library(car)
library(ggplot2)
library(lme4)
library(cowplot)
library(multcomp)
library(emmeans)
#install.packages("glmmTMB")
library(glmmTMB)
#install.packages("DHARMa")
library(DHARMa)
library(leaflet)

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

####################
####### SUBSETTING BY SIZE CLASSES
### Juveniles 
juvies <- maxn.stage%>%
  filter(stage == "0300-0499 mm") %>%
  glimpse()

### Visualising data
# Histogram of distribution
ggplot(juvies, aes(x = maxn)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Maxn Values",
       x = "Maxn Value",
       y = "Count") +
  facet_wrap(.~stage, ncol = 2)+
  theme_cowplot()

## maxn.stage dynamite plot by bait
ggplot(juvies, aes(x = bait, y = maxn, fill = bait)) +
  stat_summary(fun = mean, geom = "bar", width = 0.6) +  # Bar plot with mean
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5, color = "black") +  # SE as error bars
  labs(x = "Bait", y = "mean MaxN +/- se") +
  theme_cowplot()+
  theme(legend.position = "none")

ggplot(juvies, aes(x = bait, y = macroalgae, fill = bait)) +
  stat_summary(fun = mean, geom = "bar", width = 0.6) +  # Bar plot with mean
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5, color = "black") +  # SE as error bars
  #labs(x = "Bait", y = "mean MaxN +/- se") +
  theme_cowplot()+
  theme(legend.position = "none")

#maxn shown in size and colour of dots
pal <- colorNumeric(
  palette = "YlOrRd",
  domain = juvies$maxn)

leaflet(juvies) %>%
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

###################################################
### Habitat composition across samples
library(sf)
library(scatterpie)
### Australia shapefile
sf <- st_read("data/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp")

ggplot() + 
  geom_sf(data = sf, fill = "lightgray", color = "black")+
  geom_scatterpie(aes(x=longitude_dd, y=latitude_dd, r = maxn/200), 
                  data=juvies,
                  cols=c("macroalgae", "scytothalia", "ecklonia", "sargassum", "canopy",
                         "sessile_inverts", "substrate_hard", "sand", "posidonia")) + 
  coord_sf(xlim = c(122.6, 123.22), ylim = c(-34.16,-33.83)) +
  labs(x = "Longitude", y = "Latitude", colour = "Habitat")+
  theme_minimal()


###############################################
#### modelling
#

#don't really think it's worth it to model any of the individual size class
#now we know about the ecklonia stuff

# jv1 <- glmer(maxn ~ bait + depth_m + (1|location),
#              data = juvies,
#              family = "poisson")
# summary(jv1)
# Anova(jv1)

#############################################################################3
## copied and pasted below from other code
############################################################################
############################################################################
## SUBSETTING SIZE CLASSES

unique(maxn.stage$stage)


elders <- maxn.stage%>%
  filter(stage == "1100-1299mm") %>%
  glimpse()

eld1 <- glmer(maxn ~ bait + (1|location/site), data = elders,
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

jv1 <- glmer(maxn ~ bait + depth_m + (1|location/site),
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


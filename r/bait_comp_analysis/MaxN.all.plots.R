###########################################################
####    5.0 -- MaxN exploration & visualisation      ######
###########################################################

rm(list=ls())

# libraries----
#library(devtools)
library(tidyverse)
library(ggplot2)
library(cowplot)
#RStudio.Version()
library(leaflet)
#install.packages("ggforce")
library(ggforce)
#install.packages('scatterpie')
library(scatterpie)
library(RColorBrewer)
library(paletteer)
library(sf)

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
                site = as.factor(site))%>% #removed mutate(species = 'gouldii')
  dplyr::mutate(depth_m = as.numeric(depth_m), 
                longitude_dd = as.numeric(longitude_dd),
                latitude_dd = as.numeric(latitude_dd))%>%
  dplyr::mutate(date = substr(date_time, 1, 10))%>%
  dplyr::mutate(time = substr(date_time, 12, 19))%>%
  dplyr::mutate(date = as.factor(date))%>%
  left_join(habitat)%>% #joining to habitat 
  dplyr::filter(opcode != "046")%>% #no habitat data for 046
  dplyr::mutate(presence = ifelse(maxn > 0, 1, 0))%>%
  dplyr::mutate(titomaxn_s = periodtime * 60)%>% #creating covariate of time to maxn in seconds only
  dplyr::mutate(titomaxn_m = periodtime)%>% #creating covariate of titomaxn in mins (same as periodtime)
  clean_names()%>%
  glimpse()

sum(maxn.all$maxn)
unique(maxn.all$species)
unique(maxn.all$opcode)
which(is.na(maxn.all$macroalgae))
unique(maxn.all$site)

which(is.na(maxn.all$titomaxn_s)) #nas coerced from drops with 0 BG
#remove from dataset or keep in as 60 mins?



##### COLOUR SCHEMES
## Bait Colour

bait_col <- c("abalone" = "#27ae60", 
              "octopus" = "#f39c12" , 
              "pilchard" = "#CC79A7" )

## Locations

#scale_fill_paletteer_d("MetBrewer::Hokusai2")+


## Date

#scale_fill_paletteer_d("rcartocolor::BluGrn")+


## plot Freq. distribution of MaxNs 
## plot Frmin()eq. distribution of MaxNs 
#
ggplot(maxn.all, aes(x = maxn)) +
   geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
   labs(title = "Histogram of Maxn Values",
        x = "Maxn Value",
        y = "Count") +
   #scale_y_continuous(
    # breaks = c(0, 10, 20, 30), 
     #limits = c(0, 30)) +
   scale_x_continuous(
   breaks = c(0, 1, 2, 3, 4, 5, 6, 7))+
   theme_cowplot()



##############
## habitat plots

# visualise the habitat distributions without maxn

#density plot of ecklonia facetted by bait type
ggplot(maxn.all, aes(x = ecklonia)) +
  geom_density(fill = "darkgreen", alpha = 0.5) +
  theme_minimal() +
  labs(x = "% Cover Ecklonia", y = "Density")+
  facet_wrap(.~bait, ncol = 3)


ggplot(maxn.all, aes(x= bait, y = ecklonia, fill = bait))+
  stat_summary(fun = mean, geom = "bar", color = "black", width = 0.6) +  # Bars
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +  # Error bars with standard error
  labs(x = "Bait Type", y = "% cover Ecklonia ")+
  scale_fill_manual(values = bait_col)+
  # scale_y_discrete(
  #     breaks = c(0, 0.5, 1.0, 1.5, 2.0),
  #     limits = c(0,2)) +
  scale_x_discrete(labels = c("Abalone", "Octopus", "Pilchard"))+
  theme_cowplot()+
  theme(legend.position = "none")

ggplot(maxn.all, aes(x= bait, y = scytothalia, fill = bait))+
  stat_summary(fun = mean, geom = "bar", color = "black", width = 0.6) +  # Bars
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +  # Error bars with standard error
  labs(x = "Bait Type", y = "% cover scytothalia ")+
  scale_fill_manual(values = bait_col)+
  # scale_y_discrete(
  #     breaks = c(0, 0.5, 1.0, 1.5, 2.0),
  #     limits = c(0,2)) +
  scale_x_discrete(labels = c("Abalone", "Octopus", "Pilchard"))+
  theme_cowplot()+
  theme(legend.position = "none")


#########################################################################
########## MaxN by habitat types
## ECKLONIA
ggplot(maxn.all, aes(x = ecklonia, y = maxn)) +
  geom_jitter(alpha = 0.5) +
  labs(x = "%Ecklonia", y = "MaxN", title = "MaxN by %Ecklonia Cover")+
  geom_smooth(method = "lm", colour = "darkgreen", se = TRUE)+
  theme_cowplot()+
  theme(legend.position = "none")  
  
## SCYTOTHALIA
ggplot(maxn.all, aes(x = Scytothalia, y = maxn)) +
  geom_jitter(alpha = 0.5) +
  labs(x = "%Scytothalia", y = "MaxN", title = "MaxN by %Scytothalia Cover")+
  geom_smooth(method = "lm", colour = "darkblue", se = TRUE)+
  theme_cowplot()+
  theme(legend.position = "none")  

## Sargassum
ggplot(maxn.all, aes(x = Sargassum, y = maxn)) +
  geom_jitter(alpha = 0.5) +
  labs(x = "%Sargassum", y = "MaxN", title = "MaxN by %Sargassum Cover")+
  geom_smooth(method = "lm", colour = "grey2", se = TRUE)+
  theme_cowplot()+
  theme(legend.position = "none")  

## Macroalgae
ggplot(maxn.all, aes(x = Macroalgae, y = maxn)) +
  geom_jitter(alpha = 0.5) +
  labs(x = "%Macroalgae", y = "MaxN", title = "MaxN by %Macroalgae Cover (Non Canopy Forming")+
  geom_smooth(method = "lm", colour = "grey2", se = TRUE)+
  theme_cowplot()+
  theme(legend.position = "none")  

##Sessile Inverts
ggplot(maxn.all, aes(x = inverts, y = maxn)) +
  geom_jitter(alpha = 0.5) +
  labs(x = "%inverts", y = "MaxN", title = "MaxN by %Sessile Invertebrate Cover")+
  geom_smooth(method = "lm", colour = "grey2", se = TRUE)+
  theme_cowplot()+
  theme(legend.position = "none")  

##Mean Relief
ggplot(maxn.all, aes(x = mean.relief, y = maxn)) +
  geom_jitter(alpha = 0.5) +
  labs(x = "mean Relief", y = "MaxN", title = "MaxN by mean Relief")+
  geom_smooth(method = "lm", colour = "grey2", se = TRUE)+
  theme_cowplot()+
  theme(legend.position = "none")  




####################################
### MaxN by Bait
# Boxplot
ggplot(maxn.all, aes(x = bait, y = maxn, fill = bait)) +
  geom_boxplot() +
  labs(x = "Bait", y = "MaxN", title = "MaxN by Bait")+
  # scale_y_continuous(
  #     breaks = c(0, 5, 10, 15),
  #     limits = c(0,15)) +
  stat_summary( geom = "point", fun.y = "mean", col = "black", size = 3, shape = 24, fill = "grey")+
  scale_fill_manual(values = bait_col)+
  theme_cowplot()+
  theme(legend.position = "none")

## Jitter Plot
ggplot(maxn.all, aes(x = bait, y = maxn, colour = bait)) +
  geom_jitter(alpha = 0.5) +
  labs(x = "Bait", y = "MaxN", title = "MaxN by Bait")+
  stat_summary( geom = "point", fun.y = "mean", col = "black", size = 3, shape = 24, fill = "red" )+
  #scale_y_continuous(
   # breaks = c(0, 5, 10, 15), 
   # limits = c(0,15)) +
  scale_colour_manual(values = bait_col)+
  theme_cowplot() +
  theme(legend.position = "none")

## 'Dynamite' Plot of MaxN by Bait
ggplot(maxn.all, aes(x= bait, y = maxn, fill = bait))+
  stat_summary(fun = mean, geom = "bar", color = "black", width = 0.6) +  # Bars
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +  # Error bars with standard error
  labs(x = "Bait Type", y = "Mean MaxN", title = "Dynamite Plot MaxN by Bait Type")+
  scale_fill_manual(values = bait_col)+
  # scale_y_discrete(
  #     breaks = c(0, 0.5, 1.0, 1.5, 2.0),
  #     limits = c(0,2)) +
  scale_x_discrete(labels = c("Abalone", "Octopus", "Pilchard"))+
  theme_cowplot()+
  theme(legend.position = "none")


##########################################
### MaxN by depth
# make lm for depth first
model <- lm(depth_m ~ maxn, data = maxn.all)
modsum <- summary(model)

p <- modsum$coefficients[2,4] 
p

#then plot
ggplot(maxn.all, aes(x = depth_m, y = maxn)) +
  geom_jitter(alpha = 0.5) +
  geom_smooth(method = "lm", se = T, colour = "blue", fill = "lightblue", alpha = 0.3 )+
  labs(x = "Depth (m)", y = "MaxN", title = "Linear relationship between MaxN and Depth")+
  annotate("text", x = 25, y = 5, 
           label = paste("P = ", format(p, digits = 4)), 
           size = 5, color = "black")+
  #scale_y_continuous(
   # breaks = c(0, 5, 10, 15), 
   # limits = c(0,15)) +
  theme_cowplot()

########################################
## MaxN by Location
# Boxplot
ggplot(maxn.all, aes(x = location, y = maxn)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  labs(x = "Location", y = "MaxN", title = "MaxN by Location")+
  # scale_y_continuous(
  #   breaks = c(0, 5, 10, 15), 
  #   limits = c(0,15)) +
  stat_summary( geom = "point", fun.y = "mean", col = "black", size = 3, shape = 24, fill = "red" )+
  theme_cowplot()

unique(maxn.all$location)

#Dynamite Plot

ggplot(maxn.all, aes(x= location, y = maxn, fill = location))+
  stat_summary(fun = mean, geom = "bar", color = "black", width = 0.6) +  # Bars
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +  # Error bars with standard error
  labs(x = "Location", y = "Mean MaxN", title = "Dynamite Plot MaxN by Location")+
  scale_fill_paletteer_d("MetBrewer::Hokusai2")+
  scale_x_discrete(labels = c(
    "Mart & York Is.", "Twin Peak Is.", "Cape Arid", "Middle Is.", "Mondrain Is.", "Cape Le Grande)"))+
  theme_cowplot()+
  theme(legend.position = "none")


########################################
### MaxN by site
ggplot(maxn.all, aes(x= site, y = maxn, fill = location))+
  stat_summary(fun = mean, geom = "bar", color = "black", width = 0.6) +  # Bars
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +  # Error bars with standard error
  labs(x = "Site", y = "Mean MaxN", title = "Dynamite Plot MaxN by Site")+
  #scale_fill_paletteer_d("MetBrewer::Hokusai2")+
  #scale_x_discrete(labels = c(
  #  "Mart Is.", "Twin Peak Is.", "CT Group", "Ruby Is.", "Cape Arid", "Middle Is."))+
  theme_cowplot()+
  labs(fill = "Location")
  #theme(legend.position = "none")


########
### MaxN by Date


ggplot(maxn.all, aes(x = date, y = maxn, fill = date)) +
  stat_summary(fun = mean, geom = "bar", color = "black", width = 0.6) +  # Bars
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +  # Error bars with standard error
  labs(x = "Date", y = "Mean MaxN", title = "Dynamite Plot MaxN by Date")+
  scale_x_discrete(labels = c(
    "3rd Nov", "4th Nov", "5th Nov", "6th Nov", "7th Nov"))+
  scale_fill_paletteer_d("rcartocolor::BluGrn")+
  theme_cowplot()+
  theme(legend.position = "none")


#################################################
#################################################
## MaxN by STAGE per OPCODE
##################################################

###############################################################################
###############################################################################
## exploratory plots
#colour scheme

# distribution

ggplot(data = maxn.stage, aes(x = periodtime, y = maxn))+
  geom_jitter(alpha = 0.5)+
  labs(x = "Time to MaxN", y = "MaxN", 
       title = "Distribution of Time to MaxN with Size Class")+
  theme_cowplot()



###############################################################################
###############################################################################
##### TIME TO MAXN ############################################################

## TODO -- add in the Time to MaxN readRDS part


bait_col <- c("abalone" = "#27ae60", "octopus" = "#f39c12" , "pilchard" = "#3498db")


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


ggplot(data = maxn.stage, aes(x = bait, y = periodtime, colour = bait))+
  geom_jitter(alpha = 0.5)+
  theme_cowplot()+
  stat_summary( geom = "point", fun.y = "mean", col = "black", size = 3, shape = 24, fill = "red" )+
  scale_colour_manual(values = bait_col)+
  facet_wrap(.~stage)


############

plot(habitat$rock)
min(habitat$rock)
(habitat$sand)

plot(habitat$sand)
which(habitat$rock > 0)
plot(habitat$inverts)

#################
## plot saving ##
#################

folder_path <- "./plots/"

# change title
png(file.path(folder_path, "poster_pred.png"), width = 600, height = 400)

# plot code

ggmod.pred1

# Close the PNG device
dev.off()



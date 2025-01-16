##############################################################################
#####         PLOTTING        ####################
####################################################

rm(list=ls())

# libraries----

library(tidyverse)
library(ggplot2)
#install.packages("ggforce")
library(ggforce)
#install.packages('scatterpie')
library(scatterpie)
library(cowplot)
library(RColorBrewer)
library(paletteer)

name <- "2024_Wudjari_bait_comp"

#######################################
######      MaxN by Bait ##############
#######################################
habitat <- readRDS("./data/tidy/2024_Wudjari_bait_comp_full.habitat.rds")%>%
  dplyr::rename(inverts = "Sessile invertebrates", rock = "Consolidated (hard)", 
                sand = "Unconsolidated (soft)")%>%
  glimpse()

#creating a df for adding more specific site names
site <- data.frame(
        opcode = sprintf("%03d", 001:108), 
        stringsAsFactors = FALSE) %>%
  dplyr::mutate(site = case_when(
    between(as.numeric(opcode), 1, 18)  ~ "middle",
    between(as.numeric(opcode), 19, 30) ~ "arid",
    between(as.numeric(opcode), 31, 36) ~ "ruby",
    between(as.numeric(opcode), 37, 48 ) ~ "ct",
    between(as.numeric(opcode), 49,54 ) ~ "twin",
    between(as.numeric(opcode), 55,66 ) ~ "mart",
    between(as.numeric(opcode), 67,72 ) ~ "york",
    between(as.numeric(opcode), 73,78 ) ~ "finger",
    between(as.numeric(opcode), 79, 90 ) ~ "mondrain",
    between(as.numeric(opcode), 91, 93 ) ~ "miss",
    between(as.numeric(opcode), 94,102 ) ~ "lucky",
    between(as.numeric(opcode), 103, 108) ~ "ram"))%>%
  dplyr::mutate(opcode = as.character(opcode))%>%
  glimpse()
  
#read in maxn data and joining

maxn.all <- readRDS("./data/tidy/2024_Wudjari_bait_comp_count.maxn.all.RDS") %>%
  dplyr::mutate(species = "gouldii", bait = as.factor(bait), location = as.factor(location))%>%
  dplyr::mutate(depth_m = as.numeric(depth_m))%>%
  dplyr::mutate(period = as.factor(period))%>%
  dplyr::mutate(date = substr(date_time, 1, 10))%>%
  dplyr::mutate(time = substr(date_time, 12, 19))%>%
  dplyr::mutate(date = as.factor(date))%>%
  dplyr::group_by(opcode)%>%
  dplyr::slice_max(order_by = maxn, n=1, with_ties = FALSE)%>% # sliced the highest maxN by opcode 
  dplyr::ungroup()%>%
  dplyr::mutate(location = factor(location, levels = c("mart", "twin", "arid", "middle")))%>% #reordering
  left_join(habitat)%>%
  left_join(site)%>%
  dplyr::mutate(longitude_dd = as.numeric(longitude_dd), 
                latitude_dd = as.numeric(latitude_dd))%>%
  dplyr::mutate(site = as.factor(site))%>%
  dplyr::mutate(site = factor(site, levels = c("mart", "twin", "ct", "ruby", "arid", "middle")))%>% 
  glimpse()

##### COLOUR SCHEMES
## Bait Colour

bait_col <- c("abalone" = "#27ae60", 
              "octopus" = "#f39c12" , 
              "pilchard" = "#3498db" )

## Locations

#scale_fill_paletteer_d("MetBrewer::Hokusai2")+


## Date

#scale_fill_paletteer_d("rcartocolor::BluGrn")+

##############
## habitat plots

#1. Bubble plot of MaxNs with lat & long as axis

ggplot(maxn.all, aes(x = longitude_dd, y = latitude_dd))+
  geom_point(aes(size = maxn, colour = Ecklonia)) +
  scale_color_gradient(
    low = "lightgreen", high = "darkgreen",  # Gradient from blue (low %) to red (high %)
    name = "% Ecklonia Cover") +    # Legend title
  labs(title = "Bubble Plot of MaxN with % Ecklonia Cover",
    x = "Longitude",
    y = "Latitude",
    size = "MaxN") +
  theme_minimal()

#2 bubble plot with relief  
ggplot(maxn.all, aes(x = longitude_dd, y = latitude_dd))+
  geom_point(aes(size = maxn, colour = mean.relief)) +
  scale_color_gradient(
    low = "grey", high = "black",  # Gradient from blue (low %) to red (high %)
    name = "Mean Relief") +
  labs(title = "Bubble Plot of MaxN with mean Relief",
       x = "Longitude",
       y = "Latitude",
       size = "MaxN") +
  theme_minimal()


#3 Bubble plotwith habitat classes - maybe col = the highest value? with alpha = %cover of that hab?
#Scytothalia
ggplot(maxn.all, aes(x = longitude_dd, y = latitude_dd))+
  geom_point(aes(size = maxn, colour = Scytothalia)) +
  scale_color_gradient(
    low = "lightblue", high = "darkblue",  # Gradient from blue (low %) to red (high %)
    name = "% Scytothalia Cover") +    # Legend title
  labs(title = "Bubble Plot of MaxN with % Scytothalia Cover",
       x = "Longitude",
       y = "Latitude",
       size = "MaxN") +
  theme_minimal() 

#3. spatial plot with pie chart of habitat composition & maxn by size
#TODO - trouble shoot this one


ggplot() + 
  geom_scatterpie(aes(x=longitude_dd, y=latitude_dd, r = maxn / 350), 
                  data=maxn.all,
                  cols=c("Macroalgae", "Scytothalia", "Ecklonia", "Sargassum", "Canopy",
                         "inverts", "rock", "sand")) + 
  coord_equal()+
  theme_minimal()

########## MaxN by habitat types
## ECKLONIA
ggplot(maxn.all, aes(x = Ecklonia, y = maxn)) +
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

# Jitter Plot
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
  # scale_y_continuous(
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
  theme_cowplot()+


#Dynamite Plot

ggplot(maxn.all, aes(x= location, y = maxn, fill = location))+
  stat_summary(fun = mean, geom = "bar", color = "black", width = 0.6) +  # Bars
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +  # Error bars with standard error
  labs(x = "Location", y = "Mean MaxN", title = "Dynamite Plot MaxN by Location")+
  scale_fill_paletteer_d("MetBrewer::Hokusai2")+
  scale_x_discrete(labels = c(
    "Mart & York Is.", "Twin Peak Is.", "Cape Arid", "Middle Is."))+
  theme_cowplot()+
  theme(legend.position = "none")


########################################
### MaxN by site
ggplot(maxn.all, aes(x= site, y = maxn, fill = location))+
  stat_summary(fun = mean, geom = "bar", color = "black", width = 0.6) +  # Bars
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +  # Error bars with standard error
  labs(x = "Site", y = "Mean MaxN", title = "Dynamite Plot MaxN by Site")+
  #scale_fill_paletteer_d("MetBrewer::Hokusai2")+
  scale_x_discrete(labels = c(
    "Mart Is.", "Twin Peak Is.", "CT Group", "Ruby Is.", "Cape Arid", "Middle Is."))+
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
    "3rd Nov", "4th Nov", "5th Nov"))+
  scale_fill_paletteer_d("rcartocolor::BluGrn")+
  theme_cowplot()+
  theme(legend.position = "none")


#################################################
#################################################
## MaxN by STAGE per OPCODE
##################################################
## habitat

habitat <- readRDS("./data/tidy/2024_Wudjari_bait_comp_full.habitat.rds")%>%
  dplyr::rename(inverts = "Sessile invertebrates", rock = "Consolidated (hard)", 
                sand = "Unconsolidated (soft)")%>%
  glimpse()


#creating a df for adding more specific site names
site <- data.frame(
  opcode = sprintf("%03d", 001:108), 
  stringsAsFactors = FALSE) %>%
  dplyr::mutate(site = case_when(
    between(as.numeric(opcode), 1, 18)  ~ "middle",
    between(as.numeric(opcode), 19, 30) ~ "arid",
    between(as.numeric(opcode), 31, 36) ~ "ruby",
    between(as.numeric(opcode), 37, 48 ) ~ "ct",
    between(as.numeric(opcode), 49,54 ) ~ "twin",
    between(as.numeric(opcode), 55,66 ) ~ "mart",
    between(as.numeric(opcode), 67,72 ) ~ "york",
    between(as.numeric(opcode), 73,78 ) ~ "finger",
    between(as.numeric(opcode), 79, 90 ) ~ "mondrain",
    between(as.numeric(opcode), 91, 93 ) ~ "miss",
    between(as.numeric(opcode), 94,102 ) ~ "lucky",
    between(as.numeric(opcode), 103, 108) ~ "ram"))%>%
  dplyr::mutate(opcode = as.character(opcode))%>%
  glimpse()

## MaxN(stage) dataframe

maxn.stage <- readRDS("./data/tidy/2024_Wudjari_bait_comp_count.maxn.stage.RDS") %>%
  dplyr::mutate(species = "gouldii", bait = as.factor(bait), location = as.factor(location))%>%
  dplyr::mutate(depth_m = as.numeric(depth_m))%>%
  dplyr::mutate(period = as.factor(period))%>%
  dplyr::mutate(date = substr(date_time, 1, 10))%>%
  dplyr::mutate(time = substr(date_time, 12, 19))%>%
  dplyr::mutate(date = as.factor(date))%>%
  dplyr::group_by(opcode, stage)%>%
  dplyr::slice_max(order_by = maxn, n=1, with_ties = FALSE)%>%
  dplyr::ungroup()%>%
  dplyr::mutate(location = factor(location, levels = c("mart", "twin", "arid", "middle")))%>% #reordering
  left_join(habitat)%>%
  left_join(site)%>%
  dplyr::mutate(longitude_dd = as.numeric(longitude_dd), 
                latitude_dd = as.numeric(latitude_dd))%>%
  dplyr::mutate(site = as.factor(site))%>%
  dplyr::mutate(site = factor(site, levels = c("mart", "twin", "ct", "ruby", "arid", "middle")))%>% 
  glimpse()

##DF with the MaxN per Stage summed for each opcode

sum.stage <- maxn.stage %>% ##DF with the MaxN per Stage summed for each opcode
  dplyr::group_by(opcode, family, genus, species, bait, 
                  longitude_dd, latitude_dd, date_time, 
                  location, depth_m, date, time) %>%
  dplyr::summarise(maxn=sum(maxn))%>%
  dplyr::ungroup()%>%
  left_join(habitat)%>%
  left_join(site)%>%
  dplyr::mutate(longitude_dd = as.numeric(longitude_dd), 
                latitude_dd = as.numeric(latitude_dd))%>%
  dplyr::mutate(site = as.factor(site))%>%
  dplyr::mutate(site = factor(site, 
                levels = c("mart", "twin", "ct", "ruby", "arid", "middle")))%>% 
  glimpse()

############################# 
#### COLOUR SCHEMES
## Blue Gropers
bg_col <- c("0300-0499 mm" = "#2ecc71", "0500-0699 mm" = "#52be80", 
            "0700-0899 mm" = "#45b39d", "0900-1099 mm" = "#148f77",
            "1100-1299mm" = "#2471a3")

## Bait Types
bait_col <- c("abalone" = "#27ae60", "octopus" = "#f39c12" , "pilchard" = "#3498db" )

## Locations
#scale_fill_paletteer_d("MetBrewer::Hokusai2")+

## Date
#scale_fill_paletteer_d("rcartocolor::BluGrn")+

######################
#### MaxN(stage) by bait
## Boxplot
ggplot(sum.stage, aes(x = bait, y = maxn, fill = bait)) +
  geom_boxplot() +
  labs(x = "Bait", y = "MaxN", title = "MaxN(stage) by Bait")+
  scale_fill_manual(values = bait_col)+
  scale_x_discrete(labels = c("Abalone", "Octopus", "Pilchard"))+
  stat_summary( geom = "point", fun.y = "mean", col = "black", size = 3, shape = 24, fill = "grey" )+
  theme_cowplot()+
  theme(legend.position = "none")

# Jitter 

ggplot(sum.stage, aes(x = bait, y = maxn, colour = bait)) +
  geom_jitter(alpha = 0.5) +
  labs(x = "Bait Type", y = "MaxN", title = "MaxN(Stage) by Bait")+
  stat_summary( geom = "point", fun.y = "mean", col = "black", size = 3, shape = 24, fill = "red" )+
  scale_colour_manual(values = bait_col)+
  scale_x_discrete(labels = c("Abalone", "Octopus", "Pilchard"))+
  theme_cowplot() +
  theme(legend.position = "none")

# dynamite
ggplot(sum.stage, aes(x= bait, y = maxn, fill = bait))+
  stat_summary(fun = mean, geom = "bar", color = "black", width = 0.6) +  # Bars
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +  # Error bars with standard error
  labs(x = "Bait Type", y = "Mean MaxN", title = "Dynamite Plot MaxN(Stage) by Bait Type")+
  scale_fill_manual(values = bait_col)+
  scale_x_discrete(labels = c("Abalone", "Octopus", "Pilchard"))+
  theme_cowplot()+
  theme(legend.position = "none")


######################################
## MaxN (staged) by depth

#modelling depth
depmod<- lm(maxn~depth_m, data = sum.stage)

summary(depmod)
modsum2 <- summary(depmod)

p2 <- modsum2$coefficients[2,4] 
p2

#then plot
ggplot(sum.stage, aes(x = depth_m, y = maxn)) +
  geom_jitter(alpha = 0.5) +
  geom_smooth(method = "lm", se = T, colour = "blue", fill = "lightblue", alpha = 0.3 )+
  labs(x = "Depth (m)", y = "MaxN", title = "Linear relationship between MaxN(stage) and Depth")+
  annotate("text", x = 25, y = 10, 
           label = paste("P = ", format(p2, digits = 4)), 
           size = 5, color = "black")+
  theme_cowplot()

######################################
## MaxN(stage) by Location
# Boxplot

ggplot(sum.stage, aes(x = location, y = maxn)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  labs(x = "Location", y = "MaxN", title = "MaxN with Size Class by Location")+
  scale_y_continuous(
    breaks = c(0, 5, 10, 15), 
    limits = c(0,15)) +
  scale_x_discrete(limits = c("mart", "twin", "arid", "middle"),
                   labels = c("Mart & York Is.", "Twin Peak Is.", "Cape Arid", "Middle Is."))+
  stat_summary( geom = "point", fun.y = "mean", col = "black", size = 3, shape = 24, fill = "red" )+
  theme_cowplot()


#Dynamite Plot

ggplot(sum.stage, aes(x= location, y = maxn, fill = location))+
  stat_summary(fun = mean, geom = "bar", color = "black", width = 0.6) +  # Bars
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +  # Error bars with standard error
  labs(x = "Location", y = "Mean MaxN", title = "Dynamite Plot MaxN(stage) by Location")+
  scale_fill_paletteer_d("MetBrewer::Hokusai2")+
  scale_x_discrete(labels = c(
    "Mart & York Is.", "Twin Peak Is.", "Cape Arid", "Middle Is."))+
  theme_cowplot()+
  theme(legend.position = "none")

######################################
### By Site

ggplot(sum.stage, aes(x= site, y = maxn, fill = location))+
  stat_summary(fun = mean, geom = "bar", color = "black", width = 0.6) +  # Bars
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +  # Error bars with standard error
  labs(x = "Site", y = "Mean MaxN", title = "Dynamite Plot MaxN(stage) by Site")+
  #scale_fill_paletteer_d("MetBrewer::Hokusai2")+
  scale_x_discrete(labels = c(
    "Mart Is.", "Twin Peak Is.", "CT Group", "Ruby Is.", "Cape Arid", "Middle Is."))+
  theme_cowplot()+
  labs(fill = "Location")
#theme(legend.position = "none")

######################################
## by date
# boxplot

ggplot(sum.stage, aes(x = date, y = maxn, fill = date)) +
  stat_summary(fun = mean, geom = "bar", color = "black", width = 0.6) +  # Bars
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +  # Error bars with standard error
  labs(x = "Date", y = "Mean MaxN", title = "Dynamite Plot MaxN(stage) by Date")+
  scale_x_discrete(labels = c(
    "3rd Nov", "4th Nov", "5th Nov"))+
  scale_fill_paletteer_d("rcartocolor::BluGrn")+
  theme_cowplot()+
  theme(legend.position = "none")


######################################
### MaxN with Rows for Size Class 
dat <- maxn.stage %>%
  dplyr::filter(maxn != 0)%>% #filtering out zeros
  dplyr::mutate(stage = as.factor(stage))%>%
  dplyr::filter(!stage %in% c("AD"))%>% #filtering out these
  dplyr::mutate(location = factor(location, levels = c("mart", "twin", "arid", "middle")))%>% #reordering
  glimpse()

summary(dat$maxn)
unique(dat$stage)

## jitter plot for the maxn by size class, faceted by location

ggplot(dat, aes(x = stage, y = maxn))+
  geom_jitter(alpha = 0.5)+
  facet_grid(.~location,
             labeller = labeller(location = c("mart" = "Mart & York Islands", 
                                              "twin" = "Twin Peak Islands", 
                                              "arid" = "Cape Arid",
                                              "middle" = "Middle Island")))

## bar plot for maxn by size class, faceted by location

ggplot(dat, aes(x = stage, y = maxn, fill = stage)) +
  stat_summary(fun = mean, geom = "bar", width = 0.6) +  # Bar plot with mean
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5, color = "black") +  # SE as error bars
  labs(x = "Size Class", y = "mean MaxN +/- se") +
  facet_wrap(.~location, nrow = 2, ncol = 2,
             labeller = labeller(location = c("mart" = "Mart & York Islands", 
                                              "twin" = "Twin Peak Islands", 
                                              "arid" = "Cape Arid",
                                              "middle" = "Middle Island")))+
  scale_fill_manual(values = color_palette)+
  theme_cowplot()+
  theme(legend.position = "none")


################
## HAbitat
# Bubble Plot

#1. Bubble plot of MaxNs with lat & long as axis

ggplot(sum.stage, aes(x = longitude_dd, y = latitude_dd))+
  geom_point(aes(size = maxn * 2, colour = Ecklonia)) +
  scale_color_gradient(
    low = "lightgreen", high = "darkgreen",  # Gradient from blue (low %) to red (high %)
    name = "% Ecklonia Cover") +    # Legend title
  labs(title = "MaxN(stage) with % Ecklonia Cover",
       x = "Longitude",
       y = "Latitude",
       size = "MaxN") +
  theme_minimal()

#2 bubble plot with relief  
ggplot(sum.stage, aes(x = longitude_dd, y = latitude_dd))+
  geom_point(aes(size = maxn, colour = mean.relief)) +
  scale_color_gradient(
    low = "grey", high = "black",  # Gradient from blue (low %) to red (high %)
    name = "Mean Relief") +
  labs(title = "MaxN(stage) with mean Relief",
       x = "Longitude",
       y = "Latitude",
       size = "MaxN") +
  theme_minimal()


#3 Bubble plotwith habitat classes - maybe col = the highest value? with alpha = %cover of that hab?
#Scytothalia
ggplot(sum.stage, aes(x = longitude_dd, y = latitude_dd))+
  geom_point(aes(size = maxn, colour = Scytothalia)) +
  scale_color_gradient(
    low = "lightblue", high = "darkblue",  # Gradient from blue (low %) to red (high %)
    name = "% Scytothalia Cover") +    # Legend title
  labs(title = "MaxN(stage) with % Scytothalia Cover",
       x = "Longitude",
       y = "Latitude",
       size = "MaxN") +
  theme_minimal() 

#3. spatial plot with pie chart of habitat composition & maxn by size
#TODO - trouble shoot this one

ggplot() + 
  geom_scatterpie(aes(x=longitude_dd, y=latitude_dd, r = maxn / 350), 
                  data=sum.stage,
                  cols=c("Macroalgae", "Scytothalia", "Ecklonia", "Sargassum", "Canopy",
                         "inverts", "rock", "sand")) + 
  coord_equal()+
  theme_minimal()

########## MaxN by habitat types
## ECKLONIA
ggplot(maxn.all, aes(x = Ecklonia, y = maxn)) +
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



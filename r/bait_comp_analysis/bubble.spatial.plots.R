######################################
##### Bubble & Spatial plots
#####
#######################################
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
library(sf)



name <- "2024_Wudjari_bait_comp"

#######################################
######      MaxN by Bait ##############
#######################################
habitat <- readRDS("./data/tidy/2024_Wudjari_bait_comp_full.habitat.rds")%>%
  dplyr::rename(inverts = "Sessile invertebrates", rock = "Consolidated (hard)", 
                sand = "Unconsolidated (soft)")%>%
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

## Australia shapefile
sf <- st_read("data/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp")

###################################################
### Habitat composition across samples

ggplot() + 
  geom_sf(data = sf, fill = "lightgray", color = "black")+
  geom_scatterpie(aes(x=longitude_dd, y=latitude_dd, r = 0.01), 
                  data=maxn.all,
                  cols=c("Macroalgae", "Scytothalia", "Ecklonia", "Sargassum", "Canopy",
                         "inverts", "rock", "sand")) + 
  coord_sf(xlim = c(122.6, 123.22), ylim = c(-34.16,-33.83)) +
  labs(x = "Longitude", y = "Latitude", colour = "Habitat")+
  theme_minimal()



#################################################
#### Bubble plot of MaxNs with lat & long as axis

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


###############################################
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

##################################################
#3 Bubble plotwith habitat classes - maybe col 

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


############
scy<- maxn.all%>%
  dplyr::mutate(Scytothalia = ifelse(Scytothalia >0,1,0))%>%
  glimpse()

sum(scy$Scytothalia)

eck<- maxn.all%>%
  dplyr::mutate(Ecklonia = ifelse(Ecklonia >0,1,0))%>%
  glimpse()

sum(eck$Ecklonia)

max(maxn.all$Ecklonia)
min(maxn.all$Ecklonia > 0)

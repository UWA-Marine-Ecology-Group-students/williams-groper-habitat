##########################################################
####          MAKING MAPS               #######
########################################################
rm(list = ls())


library(ggplot2)
library(sf)

# habitat <- readRDS("./data/tidy/2024_Wudjari_bait_comp_full.habitat.rds")%>%
#   dplyr::rename(inverts = "Sessile invertebrates", rock = "Consolidated (hard)", 
#                 sand = "Unconsolidated (soft)")%>%
#   glimpse()

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

#read in metadata
metadata <- readRDS("./data/tidy/2024_Wudjari_bait_comp_Metadata.rds")%>%
  dplyr::select(-c("period", "sample"))%>% # don't need period or sample for this
  dplyr::distinct(across(everything()))%>% # removing duplicates of opcodes 
  left_join(site)%>%
  dplyr::mutate(longitude_dd = as.numeric(longitude_dd), 
                latitude_dd = as.numeric(latitude_dd))%>%
  dplyr::mutate(site = as.factor(site), location = as.factor(location),
                bait = as.factor(bait))%>%
  dplyr::mutate(site = factor(site, 
                levels = c("ram", "lucky", "miss","mondrain", "finger", "york",
                           "mart", "twin", "ct", "ruby", "arid", "middle")))%>% 
  dplyr::mutate(location = factor(location, 
            levels = c("legrande","mondrain", "mart", "twin", "arid", "middle")))%>%
  dplyr::mutate(depth_m = as.numeric(depth_m))%>%
  glimpse()

min(metadata$longitude_dd)
max(metadata$longitude_dd)
min(metadata$latitude_dd)
max(metadata$latitude_dd)

bait_col <- c("abalone" = "#27ae60",
              "octopus" = "#f39c12" ,
              "pilchard" = "#3498db" )

bait_shape <- c("octopus" = 18, "pilchard" = 15, "abalone" = 16)


#### Using a shapefile (library(sf))

sf <- st_read("data/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp")

#changing my data into a shapefile
sample_sf <- st_as_sf(metadata, 
                      coords = c("longitude_dd", "latitude_dd"), crs = 4326)

#### MAP of LOCATIONS with samples as colours by location
mapv1 <- 
  ggplot() +
  geom_sf(data = sf, fill = "lightgray", color = "black") +
  geom_sf(data = sample_sf, aes(color = location), size = 2) +
  #scale_color_manual(values = bait_col) +
  coord_sf(xlim = c(121.89, 123.22), ylim = c(-34.16,-33.83)) +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal()+
  theme(legend.position = "none",
        axis.title.x = element_text(margin = margin(t=10)), #t adds space above label
        axis.title.y = element_text(margin = margin(r=10)))+ #r adds space to the right
    annotate("point", x = 121.892, y = -33.862, color = "red", size = 4) + # Add point
    annotate("text", x = 121.95, y = -33.862, label = "Esperance", color = "black", size = 4)
mapv1

####################################################################
#####################################################################
# map with locations as boxes

  ggplot() +
  geom_sf(data = sf, fill = "lightgray", color = "black") +
  coord_sf(xlim = c(121.89, 123.22), ylim = c(-34.16,-33.83)) +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal()+
  theme(legend.position = "none",
        axis.title.x = element_text(margin = margin(t=10)), #t adds space above label
        axis.title.y = element_text(margin = margin(r=10)))+ #r adds space to the right
  annotate("point", x = 121.892, y = -33.862, color = "black", size = 4) + # Add point
  annotate("text", x = 121.95, y = -33.862, 
           label = "Esperance", color = "black", size = 4)+
  annotate("rect", 
           xmin = 122.1266, xmax = 122.2849, 
           ymin = -34.03534, ymax = -33.99424, 
           alpha = 0.1, color = "black") + #le grande
  annotate("rect", 
           xmin = 122.2502, xmax = 122.3480, 
           ymin = -34.15337, ymax = -34.10002, 
           alpha = 0.1, color = "black")+ # mondrain
  annotate("rect", 
           xmin = 122.5711, xmax = 122.7213, 
           ymin = -34.00881, ymax = -33.98454, 
           alpha = 0.1, color = "black")+ #mart & york
  annotate("rect", 
           xmin = 122.8111, xmax = 122.9406, 
           ymin = -34.00384, ymax = -33.96104, 
           alpha = 0.1, color = "black") + #twin peaks 
  annotate("rect", 
           xmin = 123.1103, xmax = 123.1843, 
           ymin = -34.01794, ymax = -33.95003, 
           alpha = 0.1, color = "black") + #cape arid
  annotate("rect", 
           xmin = 123.1552, xmax = 123.2174, 
           ymin = -34.12428, ymax = -34.07862, 
           alpha = 0.1, color = "black")+  #middle
  annotate("text", x = 122.197, y = -34.025, 
           label = "a", color = "black", size = 6)+
  annotate("text", x = 122.282, y = -34.135, 
           label = "b", color = "black", size = 6)+
  annotate("text", x = 122.639, y = -33.975, 
           label = "c", color = "black", size = 6)+
  annotate("text", x = 122.915, y = -33.991, 
           label = "d", color = "black", size = 6)+
  annotate("text", x = 123.127, y = -33.994, 
           label = "e", color = "black", size = 6)+
  annotate("text", x = 123.14, y = -34.075, 
           label = "f", color = "black", size = 6)


##############################################################################
#############################################################################
## map of aus with WA box to be used as an insert later
aus.ins <-  
  ggplot() +
  geom_sf(data = sf, fill = "#FFF5EE", color = "grey") +
  theme_cowplot()+
  theme(legend.position = "none",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank())+
  annotate(
    "rect", xmin = 112.55, xmax = 129.0, ymin = -35.27, ymax = -13.27,
    alpha = 0.1, color = "black" 
  )

##############################################################################
#############################################################################
## map of south west WA

ggplot() +
  geom_sf(data = sf, fill = "#FFF5EE", color = "grey") +
  coord_sf(xlim = c(114, 125), ylim = c(-35.27, -30))



#######################################################
#### MINIMAPS --
## getting coordinates for each location
maptable <- metadata %>%
  dplyr::group_by(location) %>%
  dplyr::summarize(
    min.lat = min(latitude_dd),
    max.lat = max(latitude_dd),
    min.long = min(longitude_dd),
    max.long = max(longitude_dd)
  )

maptable

## TODO - assign values to each of the sites for a shape

site.shape <- c("middle" = 18)

## MIDDLE ISLAND

ggplot() +
  geom_sf(data = sf, fill = "lightgray", color = "black") +
  geom_sf(data = sample_sf, aes(color = bait), size = 2) +
  #scale_color_manual(values = bait_col) +
  coord_sf(xlim = c(123.15, 123.22), ylim = c(-34.13,-34.075)) +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal()+
  theme(legend.position = "none",
        axis.title.x = element_text(margin = margin(t=10)), #t adds space above label
        axis.title.y = element_text(margin = margin(r=10)))




#################
## plot saving ##
#################

folder_path <- "plots/baitcomp"

# change title
png(file.path(folder_path, "mapv1.png"), width = 8, height = 4, units = "in", res = 300)

# plot details

mapv1
# Close the PNG device
dev.off()

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
              "pilchard" = "#CC79A7" )

# bait_shape <- c("octopus" = 18, "pilchard" = 15, "abalone" = 16)

folder_path <- "plots/baitcomp/" ## to save plots

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
mapv2 <-
  ggplot() +
  geom_sf(data = sf, fill = "#FFF5EE", color = "darkgrey") +
  coord_sf(xlim = c(121.89, 123.22), ylim = c(-34.16,-33.83)) +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal()+
  theme(legend.position = "none",
        axis.title.x = element_text(margin = margin(t=10)), #t adds space above label
        axis.title.y = element_text(margin = margin(r=10)))+ #r adds space to the right
  annotate("point", x = 121.892, y = -33.862, color = "black", size = 3) + # Add point
  annotate("text", x = 121.98, y = -33.862, 
           label = "Esperance", color = "black", size = 4)+
  annotate("rect", 
           xmin = 122.1266, xmax = 122.2849, 
           ymin = -34.04, ymax = -33.99424, 
           alpha = 0.1, color = "black") + #le grande
  annotate("text", x = (122.1266 + 122.2849) / 2, y = (-34.04 + -33.99424) / 2, 
           label = "A", size = 6) +
  annotate("rect", 
           xmin = 122.24, xmax = 122.36, 
           ymin = -34.16, ymax = -34.10002, 
           alpha = 0.1, color = "black")+ # mondrain
  annotate("text", x = (122.24 + 122.36) / 2, y = (-34.16 + -34.10002) / 2, 
           label = "B", size = 6) +
  annotate("rect", 
           xmin = 122.56, xmax = 122.722, 
           ymin = -34.021, ymax = -33.98, 
           alpha = 0.1, color = "black")+ #mart & york
  annotate("text", x = (122.56 + 122.722) / 2, y = (-34.021 + -33.98) / 2, 
           label = "C", size = 6) +
  annotate("rect", 
           xmin = 122.79, xmax = 122.9406, 
           ymin = -34.00384, ymax = -33.96104, 
           alpha = 0.1, color = "black") + #twin peaks 
  annotate("text", x = (122.79 + 122.9406) / 2, y = (-34.00384 + -33.96104) / 2, 
           label = "D", size = 6) +
  annotate("rect", 
           xmin = 123.1103, xmax = 123.1843, 
           ymin = -34.01794, ymax = -33.95003, 
           alpha = 0.1, color = "black") + #cape arid
  annotate("text", x = (123.1103 + 123.1843) / 2, y = (-34.01794 + -33.95003) / 2, 
           label = "E", size = 6) +
  annotate("rect", 
           xmin = 123.1552, xmax = 123.2174, 
           ymin = -34.12428, ymax = -34.07862, 
           alpha = 0.1, color = "black")+  #middle
  annotate("text", x = (123.1552 + 123.2174) / 2, y = (-34.12428 + -34.07862) / 2, 
           label = "F", size = 6)

mapv2

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

#site.shape <- c("middle" = 18)

## MIDDLE ISLAND (F)

f <- 
ggplot() +
  geom_sf(data = sf, fill = "#FFF5EE", color = "darkgrey") +
  geom_sf(data = sample_sf, aes(color = bait), size = 4) +
  scale_color_manual(values = bait_col) +
  coord_sf(xlim = c(123.15, 123.22), ylim = c(-34.13,-34.075)) +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal()+
  theme(legend.position = "none",
        axis.title.x = element_text(margin = margin(t=10)), #t adds space above label
        axis.title.y = element_text(margin = margin(r=10)),
        plot.background = element_rect(colour = "black", fill = NA, linewidth = 2))+
  annotate("text", x = (123.15 + 123.22) / 2, y = (-34.13 + -34.075) / 2, 
           label = "13", size = 6)

f

# save plot
png(file.path(folder_path, "f.png"), width = 8, height = 6, units = "in", res = 300)

f #plot deets

dev.off() #turn off png device

## E = Cape Arid
e <-
ggplot() +
  geom_sf(data = sf, fill = "#FFF5EE", color = "darkgrey") +
  geom_sf(data = sample_sf, aes(color = bait), size = 4) +
  scale_color_manual(values = bait_col) +
  coord_sf(xlim = c(123.1103, 123.1843), ylim = c(-34.01794,-33.95003)) +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal()+
  theme(legend.position = "none",
        axis.title.x = element_text(margin = margin(t=10)), #t adds space above label
        axis.title.y = element_text(margin = margin(r=10)),
        plot.background = element_rect(colour = "black", fill = NA, linewidth = 2))+
  annotate("text", x = 123.13, y = -33.955, 
           label = "11", size = 6)+
  annotate("text", x = 123.15, y = -33.995, 
           label = "12", size = 6)

e
# save plot
png(file.path(folder_path, "e.png"), width = 8, height = 6, units = "in", res = 300)

e #plot deets

dev.off() #turn off png device



## D = Twin Peak Islands
d<- 
ggplot() +
  geom_sf(data = sf, fill = "#FFF5EE", color = "darkgrey") +
  geom_sf(data = sample_sf, aes(color = bait), size = 4) +
  scale_color_manual(values = bait_col) +
  coord_sf(xlim = c(122.79, 122.9406), ylim = c(-34.015,-33.96104)) +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal()+
  theme(legend.position = "none",
        axis.title.x = element_text(margin = margin(t=10)), #t adds space above label
        axis.title.y = element_text(margin = margin(r=10)),
        plot.background = element_rect(colour = "black", 
                                       fill = NA, linewidth = 2))+
  annotate("text", x = 122.91, y = -33.975, label = "10", size = 6)+
  annotate("text", x = 122.84, y = -34, label = "9", size = 6)

d
# save plot
png(file.path(folder_path, "d.png"), width = 8, height = 6, units = "in", res = 300)

d #plot deets

dev.off() #turn off png device


## C = MArt & York Islands
c<-
ggplot() +
  geom_sf(data = sf, fill = "#FFF5EE", color = "darkgrey") +
  geom_sf(data = sample_sf, aes(color = bait), size = 4) +
  scale_color_manual(values = bait_col) +
  coord_sf(xlim = c(122.56, 122.722), ylim = c(-34.021,-33.98)) +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal()+
  theme(legend.position = "none",
        axis.title.x = element_text(margin = margin(t=10)), #t adds space above label
        axis.title.y = element_text(margin = margin(r=10)),
        plot.background = element_rect(colour = "black", 
                                       fill = NA, linewidth = 2))+
  annotate("text", x = 122.72, y = -34, label = "8", size = 6)+
  annotate("text", x = 122.65, y = -33.99, label = "7", size = 6)+
  annotate("text", x = 122.58, y = -34, label = "6", size = 6)
c

png(file.path(folder_path, "c.png"), width = 8, height = 6, units = "in", res = 300)

c #plot deets

dev.off() #turn off png device

## B = Mondrain Island
b<-
ggplot() +
  geom_sf(data = sf, fill = "#FFF5EE", color = "darkgrey") +
  geom_sf(data = sample_sf, aes(color = bait), size = 4) +
  scale_color_manual(values = bait_col) +
  coord_sf(xlim = c(122.22, 122.36), ylim = c(-34.16,-34.10002)) +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal()+
  theme(legend.position = "none",
        axis.title.x = element_text(margin = margin(t=10)), #t adds space above label
        axis.title.y = element_text(margin = margin(r=10)),
        plot.background = element_rect(colour = "black", 
                                       fill = NA, linewidth = 2))+
  annotate("text", x = 122.34, y = -34.105, label = "5", size = 6)+
  annotate("text", x = 122.265, y = -34.135, label = "4", size = 6)

b
png(file.path(folder_path, "b.png"), width = 8, height = 6, units = "in", res = 300)

b #plot deets

dev.off() #turn off png device

## A = Cape Le Grande
a <-
ggplot() +
  geom_sf(data = sf, fill = "#FFF5EE", color = "darkgrey") +
  geom_sf(data = sample_sf, aes(color = bait), size = 4) +
  scale_color_manual(values = bait_col) +
  coord_sf(xlim = c(122.1266, 122.2849), ylim = c(-34.04,-33.99)) +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal()+
  theme(legend.position = "none",
        axis.title.x = element_text(margin = margin(t=10)), #t adds space above label
        axis.title.y = element_text(margin = margin(r=10)),
        plot.background = element_rect(colour = "black", 
                                       fill = NA, linewidth = 2))+
  annotate("text", x = 122.14, y = -34.024, label = "1", size = 6)+
  annotate("text", x = 122.23, y = -34.005, label = "2", size = 6)+
  annotate("text", x = 122.285, y = -34, label = "3", size = 6)

a

# change title
png(file.path(folder_path, "a.png"), width = 8, height = 6, units = "in", res = 300)

# plot details

a
# Close the PNG device
dev.off()


########################################################################
## putting all my maps together in one plot
#cowplot
plot_grid(a, b, c, d, e, f, 
          # labels = c("A", "B", "C"),
          ncol = 3, nrow = 2)

# grid.arrange(bxp, dp, bp + rremove("x.text"), 
             # ncol = 2, nrow = 2)

#################
## plot saving ##
#################

# change title
png(file.path(folder_path, "a.png"), width = 8, height = 6, units = "in", res = 300)

# plot details

a
# Close the PNG device
dev.off()

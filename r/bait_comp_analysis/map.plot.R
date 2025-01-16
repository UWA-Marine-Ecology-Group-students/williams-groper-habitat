##########################################################
####          MAKING MAPS               #######
########################################################
rm(list = ls())


library(ggplot2)
library(sf)

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


max(maxn.all$longitude_dd)
min(maxn.all$longitude_dd)
max(maxn.all$latitude_dd)
min(maxn.all$latitude_dd)

bait_col <- c("abalone" = "#27ae60", 
              "octopus" = "#f39c12" , 
              "pilchard" = "#3498db" )

bait_shape <- c("octopus" = 18, "pilchard" = 15, "abalone" = 16)

#### Using a shapefile (library(sf))

sf <- st_read("data/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp")

#changing my data into a shapefile
sample_sf <- st_as_sf(maxn.all, 
                      coords = c("longitude_dd", "latitude_dd"), crs = 4326)

# Plot the shapefile and sample points
mapv1 <- ggplot() +
  geom_sf(data = sf, fill = "lightgray", color = "black") +
  geom_sf(data = sample_sf, aes(color = bait), size = 2) +
  scale_color_manual(values = bait_col) +
  coord_sf(xlim = c(122.64, 123.2), ylim = c(-34.125,-33.95)) +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal()+
  theme(legend.position = "none",
        axis.title.x = element_text(margin = margin(t=10)), #t adds space above label
        axis.title.y = element_text(margin = margin(r=10))) #r adds space to the right

mapv1

## with shapes
mapv2 <- ggplot() +
  geom_sf(data = sf, fill = "lightgray", color = "black") +
  geom_sf(data = sample_sf, aes(shape = bait, color = bait), size = 2) +
  scale_color_manual(values = bait_col) +
  scale_shape_manual(values = bait_shape)+
  coord_sf(xlim = c(122.64, 123.2), ylim = c(-34.125,-33.95)) +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal()+
  theme(legend.position = "none",
        axis.title.x = element_text(margin = margin(t=10)), #t adds space above label
        axis.title.y = element_text(margin = margin(r=10))) #r adds space to the right

mapv2

#?pch #-- for shapes

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

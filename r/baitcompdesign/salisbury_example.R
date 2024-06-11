###
# Project: Ancient Coastlines
# Data:    250m resolution bathymetry - Geosciences Australia
# Task:    Site selection for stereo-BRUVs
# author:  Claude Spencer
# date:    May 2024
##

# NOTES
# Data must be in projected (flat) CRS
# Need to make a shapefile of inclusion probs

rm(list = ls())

# Load libraries
library(spsurvey)
library(tidyverse)
library(sf)
library(terra)
library(stars)
library(starsExtra)
library(tidyterra)
library(ggnewscale)
library(nngeo)
#library(ggsn)

# Set the seed for reproducible plans
set.seed(27)

# Load the bathymetry data and crop ----
preds <- readRDS("output/sampling-design/salisbury_ga250-derivatives.rds") %>%
  crop(ext(123.25013888885, 123.687638888849, -34.6023608398971, -34.3274)) %>%
  trim()
plot(preds)

# Make inclusion probabilities ----
# Using detrended bathymetry

hist(preds$detrended)
detrended_qs <- c(0, 0.7, 0.95, 1) 
detrended_cuts   <- global(preds$detrended, probs = detrended_qs, fun = quantile, na.rm = T)
cat_detrended <- classify(preds$detrended, rcl = as.numeric(detrended_cuts[1,]))
par(mfrow = c(1, 2))
plot(cat_detrended)
plot(preds[[1]])

levels(cat_detrended) <- c("1", "2", "3") # Wrong way to do it but it works
plot(cat_detrended)

writeRaster(cat_detrended, filename = "output/sampling-design/inclusion-probabilities-salisbury.tiff",
            overwrite = T)

# Convert the detrended categorical raster into a stars object
inp_stars <- st_as_stars(cat_detrended)
plot(inp_stars)

# To simple features - and intersect with zones to create final strata
# Going to do 240 BRUVs
inp_sf <- st_as_sf(inp_stars) %>%
  dplyr::mutate(strata = as.integer(detrended)) %>%
  group_by(strata) %>%
  dplyr::summarise(geometry = st_union(geometry)) %>%
  ungroup() %>%
  dplyr::mutate(nsamps = case_when(strata == 1 ~ 50,
                                   strata == 2 ~ 50,
                                   strata == 3 ~ 50),
                detrended = case_when(strata == 1 ~ as.character(mean(detrended_cuts[[1]] + detrended_cuts[[2]])),
                                      strata == 2 ~ as.character(mean(detrended_cuts[[2]] + detrended_cuts[[3]])),
                                      strata == 3 ~ as.character(mean(detrended_cuts[[3]] + detrended_cuts[[4]]))),
                strata_new = paste0("strata_", strata)) %>%
  st_make_valid() %>%
  st_transform(9473) %>%
  glimpse()

# GRTS needs the number of samples in this horrible wide format for some reason
# caty_n <- data.frame(nsamps = inp_sf$nsamps,
#                          strata_new = inp_sf$strata_new) %>%
#   pivot_wider(names_from = strata_new,
#               values_from = nsamps) %>%
#   glimpse()

unique(inp_sf$detrended)
caty_n <- c("-29.6151632666588" = 30, "20.9865458130837" = 35, "84.2590920448303" = 35)

# Run the sampling design ----
sample.design <- grts(inp_sf, 
                      n_base = 100,
                      caty_var = "detrended",
                      caty_n = caty_n, 
                      DesignID = "SAL-BV",
                      mindis = 500)

# Select useful columns and export the design ----
# tempdat <- st_nn(sample.design$sites_base, sample.design$sites_base, 
#                  returnDist = T, progress = F, k = 5, maxdist = 500)
# 
# over <- sample.design$sites_over %>%
#   # dplyr::filter(lon_WGS84 == 121.3614 & lat_WGS84 == -33.87488) %>%
#   dplyr::filter(siteID %in% "INV-BV-275") %>%
#   glimpse()
# 
# samples_sf <- sample.design$sites_base %>%
#   dplyr::mutate(nn = sapply(tempdat[[1]], "[", 2),
#                 dists = sapply(tempdat[[2]], "[", 2)) %>%
#   # Remove sites within 500m of each other
#   dplyr::filter(!siteID %in% c("INV-BV-183")) %>% # Strata 3 - replace with 263
#   # dplyr::filter(!lon_WGS84 == 121.2941 & !lat_WGS84 == -33.88165) %>%
#   bind_rows(over) %>%
#   # dplyr::select(siteID, siteuse, lon_WGS84, lat_WGS84, ip, stratum) %>%
#   glimpse()
# 
# # Check again manually below - all good
# tempdat <- st_nn(samples_sf, samples_sf, 
#                  returnDist = T, progress = F, k = 5, maxdist = 500)



aus <- st_read("data/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp") %>%
  glimpse()

aumpa <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp") %>%
  dplyr::filter(ZoneName %in% c("Special Purpose Zone", 
                                "National Park Zone")) %>%
  arrange(desc(ZoneName)) %>%
  glimpse()

png("plots/sampling-design/bruv-design-salisbury.png",
    height = 4.5, width = 8, units = "in", res = 300)

ggplot() +
  geom_spatraster(data = cat_detrended, aes(fill = detrended)) +
  scale_fill_viridis_d(na.value = NA, option = "D") +
  labs(fill = "Inclusion probability \n(detrended)", title = "BRUV design") +
  new_scale_fill() +
  geom_sf(data = aus) +
  geom_sf(data = aumpa, aes(colour = ZoneName), fill = NA, linewidth = 1) +
  geom_sf(data = sample.design$sites_base, colour = "red") +
  coord_sf(crs = 4326, xlim = c(123.43513888885,
                                123.657638888849),
           ylim = c(-34.6023608398971,
                    -34.3273608398973)) +
  theme_minimal()

dev.off()

shoals <- st_read("data/spatial/shapefiles/Shoaling_Waves_v2.shp") %>%
  dplyr::mutate(label = "!") %>%
  glimpse()

names <- st_read("data/spatial/shapefiles/Geographic_Names_Geonoma_LGATE_013.shp") %>%
  dplyr::filter(feature__1 %in% c("Inlet", 'Island', "Reef", "Shoal", "Beach", "Wreck"),
                geographic %in% "RODONDO WRECK")

test <- sample.design$sites_base %>%
  st_transform(4326)

png("plots/sampling-design/salisbury-bruv-vessel-sample-map.png",
    height = 7, width = 4.5, units = "in", res = 300)
ggplot() +
  # geom_spatraster(data = preds, aes(fill = depth), show.legend = F) +
  # scale_fill_viridis_c(na.value = NA, option = "D") +
  # new_scale_fill() +
  geom_spatraster_contour_filled(data = preds, aes(z = depth), breaks = seq(-100, 0, 10),
                                 show.legend = F) +
  scale_fill_hypso_d(palette = "colombia_bathy") +
  new_scale_fill() +
  geom_spatraster_contour(data = preds, aes(z = depth), breaks = seq(-100, 0, 5),
                          show.legend = F, colour = "black") +
  labs(title = "BRUVs") +
  geom_sf(data = aus) +
  # geom_sf(data = zones, colour = "black", aes(fill = tidy_name), alpha = 0.35,
  #         show.legend = F) +
  # scale_fill_manual(values = c("Multiple Use Zone" = "#b9e6fb",
  #                              "Habitat Protection Zone" = "#fff8a3",
  #                              "National Park Zone" = "#7bbc63",
  #                              "Special Purpose Zone" = "#368ac1",
  #                              "Recreation Zone" = "#f4e952",
  #                              "Sanctuary Zone" = "#bfd054",
  #                              "General Use Zone" = "#bddde1"),
  #                   name = "Marine Parks") +
  # geom_sf_text(data = shoals, aes(label = label), colour = "red", size = 5) +
  scalebar(data = test, dist = 2, dist_unit = "nm",
           transform = T, model = "WGS84", location = "topright") +
  geom_sf(data = sample.design$sites_base, colour = "red", size = 4) +
  geom_sf_text(data = sample.design$sites_base, aes(label = str_remove_all(siteID, "SAL-BV-")),
               size = 2) +
  annotate(geom = "text", x = c(123.552, 123.6011, 123.5682), y = c(-34.35982, -34.44889 + 0.005, -34.56538 + 0.005), 
           label = c("Salisbury \nIsland", "Chester Reef", "Pollock Reef"), fontface = "italic", size = 2) +
  geom_sf(data = names, size = 3, colour = "red", shape = 8) +
  
  coord_sf(crs = 4326, xlim = c(123.43513888885,
                                123.657638888849),
           ylim = c(-34.6023608398971,
                    -34.3273608398973)) +
  theme_void()

dev.off()

samples <- sample.design$sites_base %>%
  as.data.frame() %>%
  dplyr::select(-geometry) %>%
  dplyr::mutate(ip = as.numeric(ip)) %>%
  glimpse()

write.csv(samples, file = "output/sampling-design/bruv_sampling-design_salisbury.csv",
          row.names = F)
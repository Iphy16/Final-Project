library(pacman)
p_load(sf, sp, terra, raster, tidyverse)



#######################################################################################
#loading the data ---- 
#######################################################################################

Quivira_NWR <- st_read("Data/Quivira_NWR.shp")
WHCR_Telemetry <- st_read("Data/Telemetry/WHCR_KS_Telemetry_GPS.shp")
KS_Roads <- st_read("Data/Tiger_2020_Roads/Tiger_2020_Roads.shp")
KS_Railroads <- st_read("Data/Tiger_2020_Railroads/Tiger_2020_Railroads.shp")
PLSS <- st_read("Data/Kansas_PLSS.shp")
power_lines <- st_read("Data/Electric_Power_Transmission_Lines.shp")
bridges <- st_read("Data/KDOT_NonStateBridges.shp")

LULC_2019 <- raster("Data/Cropland_Data/CDL_2019_20.tif")
DEM <- raster("Data/Qui_DEM.tif")

path <- "Data/KS_shapefile_wetlands"
Wetland_shf <- list.files(path, pattern = "\\.shp$", full.names = TRUE)
Wetland_shpf_list <- lapply(Wetland_shf, st_read)
KS_Wetlands <- do.call(rbind, Wetland_shpf_list)

#removing intermediate datalayers

rm(path, Wetland_shf, Wetland_shpf_list)

#######################################################################################
#Tidying and Cropping the data ---- 
#######################################################################################

# Setting coordinate systems
Quivira_NWR <- st_transform(Quivira_NWR, st_crs(KS_Wetlands))
WHCR_Telemetry <- st_transform(WHCR_Telemetry, st_crs(KS_Wetlands))
KS_Roads <- st_transform(KS_Roads, st_crs(KS_Wetlands))
KS_Railroads <- st_transform(KS_Railroads, st_crs(KS_Wetlands))
PLSS <- st_transform(PLSS, st_crs(KS_Wetlands))
power_lines <- st_transform(power_lines, st_crs(KS_Wetlands))
bridges <- st_transform(bridges, st_crs(KS_Wetlands))

LULC_2019 <- projectRaster(LULC_2019, crs = crs(KS_Wetlands))
DEM <- projectRaster(DEM, crs = crs(KS_Wetlands))


# making a 10km buffer around the study area
quivira_10km <- st_buffer(Quivira_NWR, dist = 10000)


# clipping out study area
quivira_wetland <- st_intersection(KS_Wetlands, quivira_10km)
rm(KS_Wetlands)

quivira_telemetry <- st_intersection(WHCR_Telemetry, quivira_10km)
quivira_roads <- st_intersection(KS_Roads, quivira_10km)
quivira_railroads <- st_intersection(KS_Railroads, quivira_10km)
quivira_PLSS <- st_intersection(PLSS, quivira_10km)
quivira_powerlines <- st_intersection(power_lines, quivira_10km)
quivira_bridges <- st_intersection(bridges, quivira_10km)

quivira_LULC2019 <- mask(LULC_2019, quivira_10km)
quivira_DEM <- mask(DEM, quivira_10km)

res(quivira_DEM)  # Check the resolution of the DEM
extent(quivira_DEM)  # Check the extent of the DEM

#Plotting the study area

ggplot() +
  geom_sf(data = Quivira_NWR, aes(fill = "lightblue"), alpha = 0.6, color = "#31a354") +
  geom_sf(data = quivira_10km, color = "yellow", fill = NA, lwd = 2) +
  #scale_fill_viridis_c() +
  theme_minimal() +
  theme(axis.text = element_blank(), legend.position = "bottom") + #Axis.text removes the lat and long axis 
  guides(fill = guide_legend(title = "Percent of State GDP")) +
  scale_fill_distiller(palette = "RdYlBu") 


#######################################################################################
### Habitat Unsuitability Screening
#######################################################################################

# Identifying Wetlands near human disturbances
## Create buffers from human disturbances

### Buffer distance from different road types

calcBufferDist <- function(MTFCC) {
  if (MTFCC %in% c("S1500", "S1740", "S1810", "S1830")) {
    return(200)
  } else {
    return(400)
  }
}

quivira_roads <- quivira_roads %>%
  mutate(BufferDist = sapply(MTFCC, calcBufferDist))

roads_buffered <- st_buffer(quivira_roads, dist = quivira_roads$BufferDist)


### railroad buffer distance

railroad_buffered <- st_buffer(quivira_railroads, dist = 400)

### powerlines buffer distance

powerline_buffered <- st_buffer(quivira_powerlines, dist = 100)

### bridges buffer distance

bridges_buffered <- st_buffer(quivira_bridges, dist = 400)


## merge unsuitable locations

unsuitable_areas <- st_union(roads_buffered, railroad_buffered, powerline_buffered, bridges_buffered)

## remove wetlands that fall within disturbance buffers

wetlandsErased <- st_difference(quivira_wetland, unsuitable_areas)

## Convert all different parts to one.

wetlandsSinglepart <- st_cast(wetlandsErased, "POLYGON")

# Calculate wetland areas in acres 
wetlandsSinglepart$ACRES <- st_area(wetlandsSinglepart) * 0.000247105  # From square meters to acres

# Select only wetlands with areas greater than the 2.5 acres
screenedWetlands <- wetlandsSinglepart %>% 
  filter(ACRES > areaThreshold)




#######################################################################################
### Habitat Quality Assessment - Model Variables
#######################################################################################

# Using a MCDSM approach, model variables that will be used to assess the quality of remaining wetlands after screening include:
# 1. Water Regime
# 2. Wetland Size
# 3. Wetland Type (Natural vs. Artificial)
# 4. Wetland density
# 5. Distance to food


## Water Regime ---------

### define water regime function
calcWaterRegime <- function(WATER_REGI) {
  if (WATER_REGI == "Permanently Flooded") {
    return(5)
  } else if (WATER_REGI == "Intermittently Exposed") {
    return(4)
  } else if (WATER_REGI == "Semipermanently Flooded") {
    return(3)
  } else if (WATER_REGI == "Seasonally Flooded") {
    return(2)
  } else if (WATER_REGI == "Artificially Flooded") {
    return(1)
  } else if (WATER_REGI == "Temporary Flooded") {
    return(1)
  } else {
    return(9999)
  }
}

### calculate water regime score
WaterRegimeScore <- screenedWetlands %>% 
  mutate(RegScore = calcWaterRegime(WATER_REGI))


### Convert to raster
WaterRegimeScore_raster <- rasterize(WaterRegimeScore, quivira_DEM, field = "RegScore", fun = max)




## Wetland Size -----------

### define Wetland Size function based on Acres
calcWetlandSize <- function(Acres) {
  if (Acres >= 7) {
    return(5)
  } else if (Acres >= 5 && Acres < 7) {
    return(4)
  } else if (Acres >= 3 && Acres < 5) {
    return(3)
  } else if (Acres >= 1 && Acres < 3) {
    return(2)
  } else if (Acres < 1) {
    return(1)
  } else {
    return(9999)
  }
}

### calculate Wetland Size score
WetlandSizeScore <- screenedWetlands %>% 
  mutate(SizeScore = calcWetlandSize(Acres))


### Convert to raster
WetlandSizeScore_raster <- rasterize(WetlandSizeScore, quivira_DEM, field = "SizeScore", fun = max)



## Wetland Type (Natural vs. Artificial) -----------

### define Wetland Size function based on Acres
calcWetlandType <- function(FIRST_MODI) {
  if (FIRST_MODI == "Diked/Impounded") {
    return(0)
  } else if (FIRST_MODI == "Excavated") {
    return(0)
  } else {
    return(2)
  }
}


### calculate Wetland Type score
WetlandTypeScore <- screenedWetlands %>% 
  mutate(TypeScore = calcWetlandType(FIRST_MODI))


### Convert to raster
WetlandTypeScor_raster <- rasterize(WetlandTypeScor, quivira_DEM, field = "TypeScore", fun = max)




## Wetland Density ---------

### define water regime function
calcWaterRegime <- function(WATER_REGI) {
  if (WATER_REGI == "Permanently Flooded") {
    return(5)
  } else if (WATER_REGI == "Intermittently Exposed") {
    return(4)
  } else if (WATER_REGI == "Semipermanently Flooded") {
    return(3)
  } else if (WATER_REGI == "Seasonally Flooded") {
    return(2)
  } else if (WATER_REGI == "Artificially Flooded") {
    return(1)
  } else if (WATER_REGI == "Temporary Flooded") {
    return(1)
  } else {
    return(9999)
  }
}

### calculate water regime score
WaterRegimeScore <- screenedWetlands %>% 
  mutate(RegScore = calcWaterRegime(WATER_REGI))


### Convert to raster
WaterRegimeScore_raster <- rasterize(WaterRegimeScore, quivira_DEM, field = "RegScore", fun = max)




## Distance to Food ---------

### define water regime function
calcWaterRegime <- function(WATER_REGI) {
  if (WATER_REGI == "Permanently Flooded") {
    return(5)
  } else if (WATER_REGI == "Intermittently Exposed") {
    return(4)
  } else if (WATER_REGI == "Semipermanently Flooded") {
    return(3)
  } else if (WATER_REGI == "Seasonally Flooded") {
    return(2)
  } else if (WATER_REGI == "Artificially Flooded") {
    return(1)
  } else if (WATER_REGI == "Temporary Flooded") {
    return(1)
  } else {
    return(9999)
  }
}

### calculate water regime score
WaterRegimeScore <- screenedWetlands %>% 
  mutate(RegScore = calcWaterRegime(WATER_REGI))


### Convert to raster
WaterRegimeScore_raster <- rasterize(WaterRegimeScore, quivira_DEM, field = "RegScore", fun = max)



 

#######################################################################################
### Habitat Quality Assessment - Composite Habitat Quality Scores
#######################################################################################


















#######################################################################################
### Visualize Distribution of Composite Scores
#######################################################################################









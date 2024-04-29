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


rm(Wetland_shpf_list)
rm(path, Wetland_shf)

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

quivira_LULC2019 <- crop(LULC_2019, quivira_10km)
quivira_DEM <- mask(DEM, quivira_10km)


# creating margin buffers










#######################################################################################
### Habitat Unsuitability Screening
#######################################################################################






#######################################################################################
### Habitat Suitability Modeling
#######################################################################################






#######################################################################################
### Random Forest MDS Plot.
#######################################################################################









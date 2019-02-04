#Function to find out parameter values (e.g. SIC, chlorophyll, PAR) at selected coordinates (e.g. sample locations)
clim_locate <- function(sat_data, point_data, coord_vars=c("Longitude", "Latitude"), sat_vars=sat_data[,!colnames(sat_data) %in% coord_vars],
                        sat_varlabs=NA, separ=",", proj_init="WGS84", proj_orig, rast_res=c(NA, NA, 3), export_path=getwd()) {

#Install and load the required libraries
list.of.packages <- c("data.table", "raster", "rgeos", "sp", "sf", "rgdal", "broom", "dplyr")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

invisible(lapply(list.of.packages, require, character.only = TRUE))

library(data.table)
library(raster)
library(rgeos)
library(sp)
library(sf)
library(rgdal)
library(broom)
library(dplyr)

#PREP STAGE: Sorting and assigning projections
proj_ids <- c("ONP", "LANP", "SNP", "AENP", "OSP", "SSP", "SSP_alt", "LASP", "WGS84")
proj_defs <- c("+proj=ortho +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
               "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +a=6371228 +b=6371228 +units=m +no_defs +init=epsg:3408",
               "+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs +init=epsg:3411",
               "+proj=aeqd +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
               "+proj=laea +y_0=0 +lon_0=155 +lat_0=-90 +ellps=WGS84 +units=m +no_defs",
               "+proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs +init=epsg:3031",
               "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +init=epsg:3031",
               "+proj=laea +lat_0=-90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
               "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs")

proj_mapped <- setNames(proj_defs, proj_ids)

if(any(proj_ids %in% proj_init)) {
  
  proj_init <- proj_mapped[[proj_init]]
  
} else if(!any(proj_ids %in% proj_init) & !is.character(proj_init)) {
  print("Projection was not provided in the correct format! Switching to the default: WGS84 (EPSG 4326)...")
  proj_init <- proj_defs[9]
}

if(any(proj_ids %in% proj_orig)) {
  
  proj_orig <- proj_mapped[[proj_orig]]
  
} else if(!any(proj_ids %in% proj_orig) & !is.character(proj_orig)) {
  print("Projection was not provided in the correct format! Switching to the default: Orthographic North Polar...")
  proj_orig <- proj_defs[1]
}

#PREP STAGE: Error Checks
if(any(is.na(sat_varlabs)) | length(sat_varlabs)!=length(sat_vars)) {
  print("Satellite variable labels were not provided is the correct format! Using sat_vars as labels...")
  sat_varlabs <- sat_vars
}

#Load satellite data
print("Loading and processing satellite data...")

if(is.object(sat_data)) {
  satmap <- sat_data[,c(coord_vars, sat_vars)]
} else if(is.character(sat_data)) {
  satmap <- data.table::fread(sat_data, sep=separ, select=c(coord_vars, sat_vars), data.table = FALSE)
}

#Rasterize satellite data
print("Projecting and rasterising satellite data...")

if(!is.na(rast_res[1])) {
  satmap_rast <- suppressWarnings(try(rasterFromXYZ(as.matrix(satmap), crs=proj_init, res=rast_res[1], digits=rast_res[3]), silent=TRUE))
} else if(is.na(rast_res[1])) {
  satmap_rast <- suppressWarnings(try(rasterFromXYZ(as.matrix(satmap), crs=proj_init, digits=rast_res[3]), silent=TRUE))
}

if(inherits(satmap_rast, "try-error")) {
  print("Failed to rasterize data via proj_init... Trying to force-convert to proj_orig, then rasterize!")
  satmap_coord.init <- SpatialPoints(satmap[,coord_vars], proj4string = CRS(proj_init))
  satmap_coord.orig <- spTransform(satmap_coord.init, CRS(proj_orig))
  satmap_origproj <- cbind(coordinates(satmap_coord.orig), satmap[,sat_vars])
  colnames(satmap_origproj) <- c(coord_vars, sat_vars)
  
  satmap_origproj[,coord_vars] <- round(satmap_origproj[,coord_vars], 5) #FOR SOME REASON THIS PREVENTS SOME RASTERISATION FAILURES... WHY?
  
  if(!is.na(rast_res[2])) {
    satmap_rast <- suppressWarnings(try(rasterFromXYZ(as.matrix(satmap_origproj), crs=proj_orig, res=rast_res[2], digits=rast_res[3]), silent=TRUE))
  } else if(is.na(rast_res[2])) {
    satmap_rast <- suppressWarnings(try(rasterFromXYZ(as.matrix(satmap_origproj), crs=proj_orig, digits=rast_res[3]), silent=TRUE))
  }
}

if(inherits(satmap_rast, "try-error")) {
  stop("Failed to rasterize the satellite data using the initial projection (proj_init), most likely due to an irregular grid. Cancelling execution...")
  
} else if (!inherits(satmap_rast, "try-error")) {
  
  if(!is.na(rast_res[2])) {
    satmap_final <- projectRaster(satmap_rast, crs=proj_orig, res=rast_res[2]) #Points not finite warning does NOT pose a problem for the data!
  } else if(is.na(rast_res[2])) {
    satmap_final <- projectRaster(satmap_rast, crs=proj_orig)
  }
  
  satmap_final <- as.data.frame(satmap_final, xy=TRUE)
  colnames(satmap_final)[which(colnames(satmap_final) %in% c("x", "y"))] <- coord_vars
  print("Conversion to raster successful!")
  
  }

#Load point data
print("Loading point data...")

if(is.object(point_data)) {
  pointmap <- point_data[,c(coord_vars)]
} else if(is.character(point_data)) {
  pointmap <- data.table::fread(point_data, sep=separ, data.table = FALSE)
  #pointmap <- data.table::fread(point_data, sep=separ, select=coord_vars, data.table = FALSE)
}

#Convert point data to SF object, assign projection (proj_init then proj_orig), and extract coordinates...
print("Extracting satellite data for each provided coordinate pair...")
points_sf <- st_as_sf(pointmap, coords=coord_vars)
points_sf <- st_sf(points_sf, crs=proj_init)
points_sf <- st_transform(points_sf, crs=proj_orig)
point_coordinates <-st_coordinates(points_sf)

#Use extracted coordinates to obtain satellite data values for each sample location
extracted_satvars <- raster::extract(satmap_rast, point_coordinates, method="bilinear", fun=mean, na.rm=TRUE)
colnames(extracted_satvars) <- sat_varlabs

sat_results_final <- cbind(pointmap, extracted_satvars)

#Export results
if(!is.na(export_path) & is.character(export_path)) {
  print("Exporting the results as a .CSV file...")
  write.csv(sat_results_final, paste0(export_path, "/ClimLocate_Results_", format(Sys.time(), "%Y-%m-%d %Hhr %Mmin %Ssec") , ".csv"))
}

print("Processing complete!")
return(sat_results_final)

}
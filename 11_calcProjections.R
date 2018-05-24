
# Load libraries
require(raster)
require(rgdal)
require(tidyverse)
require(gtools)
require(sf)

# Initial setup
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use☻
zonalSt <- function(crn, ftr, adm, vr){
  # Current
  crn <- grep(vr, crn, value = T) %>% raster()
  crn <- crn * 1
  crn <- raster::crop(crn, adm) %>% raster::mask(adm)
  
  # Future
  ftr <- grep(vr, ftr, value = T) %>% stack()
  ftr <- ftr * 1
  ftr <- mean(ftr)
  ftr <- raster::crop(ftr, adm) %>% raster::mask(adm)
  
  print('To calculate the difference')
  
  # Difference
  dif <- ftr - crn
  prc <- (dif / crn) * 100
  
  print('To rasterize')
  
  # Rasterization and zonal statistics
  lyr <- rasterize(adm, dif, field = 'OBJECTID')
  znl.dif <- raster::zonal(dif, lyr, fun = 'mean', na.rm = TRUE) %>% as.tibble()
  znl.prc <- raster::zonal(prc, lyr, fun = 'mean', na.rm = TRUE) %>% as.tibble()
  
  # Join with the shapefile
  sf <- st_as_sf(as(adm, 'SpatialPolygonsDataFrame'))
  sf.dif <- inner_join(sf, znl.dif, by = c('OBJECTID' = 'zone')) %>% dplyr::select(OBJECTID, NAME_1, mean) %>% mutate(mean = round(mean/10, 2))
  sf.prc <- inner_join(sf, znl.prc, by = c('OBJECTID' = 'zone')) %>% dplyr::select(OBJECTID, NAME_1, mean) %>% mutate(mean = round(mean/10, 2))
  
  print('To Write the final files')
  
  # Write files
  writeRaster(dif, paste0('../_data/_asc/_znl/dif_', vr), overwrite = FALSE)
  writeRaster(prc, paste0('../_data/_asc/_znl/prc_', vr), overwrite = FALSE)
  write_sf(sf.dif, dsn = '../_data/_shp', layer = paste0('dif_', vr), driver = 'ESRI Shapefile', update = TRUE)
  write_sf(sf.prc, dsn = '../_data/_shp', layer = paste0('prc_', vr), driver = 'ESRI Shapefile', update = TRUE)
  
  print(paste0('Done ', vr))
}

# Load data
path_climate <- 'Z:/_cam/_raster/_climate'
fls.crn <- paste0(path_climate, '/_current/_asc') %>%
  list.files(full.names = T, pattern = '.asc$') %>%
  mixedsort()
gcms <- paste0(path_climate, '/_future/_rcp60/_asc/_2050') %>% 
  list.files()
fls.ftr <- paste0(path_climate, '/_future/_rcp60/_asc/_2050/', gcms) %>% 
  list.files(full.names = T, pattern = '.asc$') %>% 
  grep('bio', ., value = T) %>%
  mixedsort()
gtm <- getData('GADM', country = 'GTM', level = 1)

# Apply the function
bio_1 <- zonalSt(crn = fls.crn, ftr = fls.ftr, adm = gtm, vr = 'bio_1.asc')
bio_12 <- zonalSt(crn = fls.crn, ftr = fls.ftr, adm = gtm, vr = 'bio_12.asc')








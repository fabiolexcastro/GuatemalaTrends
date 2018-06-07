
# Load libraries
require(raster)
require(rgdal)
require(tidyverse)
require(gtools)
require(rgeos)
require(sf)

# Initial setup
g <- gc(reset = TRUE)
rm(list = ls())

# Function to use
makeSlp <- function(lyr, pos){
  
  print('To make the zonal')
  znl <- raster::zonal(lyr[[pos]], gtm1.lyr, fun = 'mean', na.rm = TRUE) %>%
    as.data.frame() %>% 
    cbind(gtm1.lyr@data@attributes) %>%
    as.tibble() %>% 
    dplyr::select(zone, mean, NAME_1) %>%
    .[complete.cases(.),]
  vls <- znl %>% pull(mean) 
  max <- which.max(vls)
  dpt <- znl[max,]
  
  print('To make the subset for the Guatemala shapefile')
  gtm1 <- st_as_sf(gtm1) %>% dplyr::select(ID_1, NAME_1)
  gtm1 <- as(gtm1, "Spatial")
  
  print('Estimating the point with the greatest slope value')
  pnts <- rasterToPoints(lyr[[pos]]) %>% 
    as.tibble()
  pnts.adm <- raster::extract(gtm1, pnts[,1:2]) %>%
    as.tibble() %>%
    cbind(pnts) %>%
    as.tibble() %>% 
    dplyr::select(-point.ID, -poly.ID)
  position <- pnts.adm %>% 
    pull() %>%
    which.max()
  max <- pnts.adm[position,]  
  return(list(dpt, max))
}

# Load data
fls <- list.files('../_data/_tif/_ext/_slp', full.names = T, pattern = '.tif$') %>% 
  grep('_s_', ., value = TRUE) %>% 
  mixedsort()
vrs.main <- c('bio_6.tif', 'bio_8.tif', 'bio_17.tif', 'bio_31.tif', 'bio_32.tif')
fls.main <- grep(paste0(vrs.main, collapse = '|'), fls, value = TRUE)
gtm1 <- getData('GADM', country = 'GTM', level = 1)
gtm2 <- getData('GADM', country = 'GTM', level = 2)
msk <- raster('../_data/_tif/_ext/_slp/rst_s_bio_1.tif') * 0

# Stack
lyr <- stack(fls.main)

# Rasterize the administrative
gtm1.lyr <- raster::rasterize(gtm1, msk)
gtm2.lyr <- raster::rasterize(gtm2, msk)

# Apply the function
slps <- lapply(1:nlayers(lyr), function(k) makeSlp(lyr = lyr, pos = k))

# Extracting the greatest values by department
dptos <- lapply(1:nlayers(lyr), function(k) slps[[k]][[1]])
dptos <- lapply(1:length(dptos), function(k) dptos[[k]] %>% mutate(var = vrs.main[[k]]))
names(dptos) <- gsub('.tif', '', vrs.main)

# Extracting the greatest values by pixel
pxls <- lapply(1:nlayers(lyr), function(k) slps[[k]][[2]])
pxls <- lapply(1:length(pxls), function(k) pxls[[k]] %>% mutate(var = vrs.main[[k]]) %>% setNames(c('ID', 'NAME_1', 'x', 'y', 'value', 'var')))
names(pxls) <- gsub('.tif', '', vrs.main)

# Combining the dataframe into only one
dptos <- bind_rows(dptos)
pxls <- bind_rows(pxls)

# Write the final tables
write.csv(dptos, '../_data/_tbl/_slp/_znl/dptos.csv', row.names = FALSE)
write.csv(pxls, '../_data/_tbl/_slp/_znl/pxls.csv', row.names = FALSE)

print('Done!')
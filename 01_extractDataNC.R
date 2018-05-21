
# Load libraries
require(raster)
require(rgdal)
require(tidyverse)
require(velox)
require(readr)
require(stringr)
require(gtools)
require(foreach)
require(doSNOW)

# Initial setup 
g <- gc(reset = TRUE)
rm(list = ls())
setwd('//mnt/workspace_cluster_9/Coffee_Cocoa2/_guatemala/_codes')

# Function to use
extMskNC <- function(vr, yr){
  fle <- grep(vr, fls, value = T) %>%
    grep(yr, ., value = T)
  bck <- raster::brick(fle)
  nms <- names(bck) %>% gsub('\\.', '_', .)
  print(paste0('To convert to velox object ', vr, ' ', yr))
  vlx <- velox(bck)
  vlx$crop(ext)
  stk <- vlx$as.RasterStack()
  lyr <- unstack(stk)
  Map('writeRaster', x = lyr, filename = paste0('../_data/_asc/_gtm/', vr, '_', nms, '.asc'))
  print(paste0('To convert to a table ', vr, ' ', yr))
  coords <- vlx$getCoordinates() %>%
    as_data_frame() %>%
    setNames(c('Lon', 'Lat'))
  sp <- SpatialPoints(coords)
  vls <- vlx$extract_points(sp = sp) %>%
    cbind(coordinates(sp)) %>%
    as.tibble() %>%
    setNames(c(month.name, sp %>% coordinates %>% colnames)) %>%
    mutate(yr = yr,
           vr = vr,
           id = 1:nrow(.)) %>%
    gather(month, value, -Lon, -Lat, -yr, -vr, -id)
  write_csv(vls, paste0('../_data/_tbl/', vr, '_', yr, '.csv'))
  print('Done')
  return(vls)
}

# Load data
fls <- list.files('../_data/_nc/_world', full.names = TRUE, pattern = '.nc$')
fls <- list.files('//mnt/data_cluster_4/observed/gridded_products/terra-climate', full.names = T, pattern = '.nc')
fls <- grep(paste0(1980:2017, collapse = '|'), fls, value = TRUE)
vrs <- c('pet', 'ppt', 'tmax', 'tmin')
yrs <- basename(fls) %>% readr::parse_number() %>% unique()
gtm <- getData('GADM', country = 'GTM', level = 0)
ext <- extent(gtm)

# Potential Evapotranspiration
cl <- makeCluster(length(yrs)/2)
registerDoSNOW(cl)
registerDoMC(length(yrs)/2)
pet <- foreach(i = 1:length(yrs), .packages = c('raster', 'rgdal', 'tidyverse', 'velox'), .verbose = TRUE) %dopar% {
  extMskNC(vr = 'pet', yr = yrs[i])
}

# Precipitation
registerDoMC(length(yrs)/2)
ppt <- foreach(i = 1:length(yrs), .packages = c('raster', 'rgdal', 'tidyverse', 'velox'), .verbose = TRUE) %dopar% {
  extMskNC(vr = 'ppt', yr = yrs[i])
}

# Maximum temperature
registerDoMC(length(yrs)/2)
tmax <- foreach(i = 1:length(yrs), .packages = c('raster', 'rgdal', 'tidyverse', 'velox'), .verbose = TRUE) %dopar% {
  extMskNC(vr = 'tmax', yr = yrs[i])
}

# Minimum temperature
registerDoMC(length(yrs)/2)
tmax <- foreach(i = 1:length(yrs), .packages = c('raster', 'rgdal', 'tidyverse', 'velox'), .verbose = TRUE) %dopar% {
  extMskNC(vr = 'tmin', yr = yrs[i])
}





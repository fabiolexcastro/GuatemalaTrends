
# Load libraries
require(raster)
require(rgdal)
require(tidyverse)
require(gtools)
require(rgeos)
require(velox)
require(dismo)

# Initial setup
g <- gc(reset = TRUE)
rm(list = ls())
source('ClimateFunctions.R')
source('functionsBios.R')

# Load data
fls <- list.files('../_data/_asc/_gtm', full.names = TRUE, pattern = '.asc$')
vrs <- c('pet', 'ppt', 'tmax', 'tmin')
yrs <- basename(fls) %>% readr::parse_number() %>% unique()

# Make biovars
makeBiovar(fls = fls, yr = yrs[1])

# Parallelization
cl <- makeCluster(18) # registerDoMC(18)
registerDoSNOW(cl)
foreach(i = 1:length(yrs), .packages = c('dplyr', 'raster', 'rgdal', 'stringr', 'rgeos', 'dismo', 'gtools'), .verbose = TRUE) %dopar% {
  print(yrs[[i]])
  makeBiovar(yr = yrs[i], fls = fls)
} 








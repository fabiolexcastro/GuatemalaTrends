
# Load libraries
require(raster)
require(rgdal)
require(tidyverse)
require(gtools)
require(rgeos)
require(doMC)
require(doSNOW)
require(parallel)
require(velox)

# Initial setup
g <- gc(reset = TRUE)
rm(list = ls())
setwd('//mnt/workspace_cluster_9/Coffee_Cocoa2/_guatemala/_codes')

# Load data
fls <- list.files('../_data/_tif/_ext/_bios', full.names = TRUE, pattern = '.asc$') %>% mixedsort()
stk <- stack(fls)  
gtm <- getData('GADM', country = 'GTM', level = 0)
vrs <- paste0('bio_', 1:33)

# Extract by mask
stk <- stk %>% raster::crop(gtm) %>% raster::mask(gtm)
msk <- stk[[1]] * 0 + 1

# Mean by bioclimatic variables for all the years
cl <- makeCluster(17); registerDoSNOW(cl)
mnsBios <- foreach(i = 1:length(vrs), .packages = c('raster', 'rgeos', 'tidyverse', 'sp'), .verbose = TRUE) %dopar% { raster::subset(stk, grep(paste0(vrs[i], '$'), names(stk), value = T)) %>% mean()}
Map('writeRaster', x = mnsBios, filename = paste0('../_data/_tif/_ext/_meanBios/bio_', 1:33, '.tif'), overwrite = TRUE)

# Raster to table
vlx <- velox(stk)
crd <- rasterToPoints(stk[[1]])  %>% as.tibble() %>% dplyr::select(x, y)
crd <- crd[complete.cases(crd),] %>% setNames(c('Lon', 'Lat'))
spn <- SpatialPoints(coords = crd)
vls <- vlx$extract_points(sp = spn) %>%
  tbl_df() %>%
  setNames(names(stk))
vls <- vls[complete.cases(vls),] 
vls <- vls %>%
  mutate(id = 1:nrow(.),
         lon = pull(crd, 1), 
         lat = pull(crd, 2)) %>%
  gather(var, value, -id, -lon, -lat) %>%
  mutate(year = str_sub(var, start = 2, end = 5), 
         variable = str_sub(var, start = 7, end = nchar(var)))

# Saving the main objects
saveRDS(object = vlx, file = '../_rds/vlx_bios.rds')
saveRDS(object = msk, file = '../_rds/msk.rds')
saveRDS(object = crd, file = '../_rds/coords.rds')
saveRDS(object = vls, file = '../_rds/vls_bios.rds')




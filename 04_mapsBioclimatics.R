
# Load libraries
require(raster)
require(rgdal)
require(tidyverse)
require(gtools)
require(rasterVis)
require(RColorBrewer)

# Initial setup 
g <- gc(reset = TRUE)
rm(list = ls())

# Function to use
makeMap <- function(rst, pllte){
  rst <- lyr[[1]]
  plltee <- colorRampPalette(c('#543005','#8c510a','#bf812d','#dfc27d','#f6e8c3','#f5f5f5','#c7eae5', '#80cdc1', '#35978f', '#01665e', '#003c30'))
  gg <- rasterVis::gplot(rst) +
    geom_tile(aes(fill = value)) +
    scale_fill_manual(values = RColorBrewer::brewer.pal(n = 100, name = 'Set2'))
  gg
  
}


# Load data
fls <- list.files('../_data/_tif/_ext/_meanBios', full.names = TRUE, pattern = '.tif$') %>% mixedsort()
lyr <- lapply(fls, FUN = raster)






















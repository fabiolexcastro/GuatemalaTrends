
# Load libraries
require(raster)
require(rgdal)
require(tidyverse)
require(doSNOW)
require(foreach)
require(trend)
require(doMC)
require(ggpubr)

# Initial setup
g <- gc(reset = TRUE)
rm(list = ls())
setwd('//mnt/workspace_cluster_9/Coffee_Cocoa2/_guatemala/_codes')
source('functionsSlope.R')

# Functions
makeMapSlp <- function(lyr){ 
  # Create pallete colors
  pllt <- colorRampPalette(c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7','#f7f7f7','#d1e5f0', '#92c5de', '#4393c3', '#2166ac', '#053061'))
  # Make graph
  gg <- rasterVis::gplot(lyr) +
    geom_tile(aes(fill = value)) +
    scale_fill_gradientn(colours = pllt(100), na.value = 'white') + # na.value = 'white'
    geom_polygon(data = gtm, aes(x=long, y = lat, group = group), color = 'grey30', fill='NA') +
    coord_equal() +
    xlab('Longitude') + 
    ylab('Latitude') +
    theme_bw() + 
    theme(panel.background = element_rect(fill = 'white', colour = 'black'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = 'right') +
    labs(fill = 'Slope value')
  return(gg)
}
makeLyrBio <- function(df, vr){
  # df <- rslts2
  # vr <- 'bio_14'
  x <- filter(df, variable == vr)
  y <- x
  coordinates(y) <- ~ lon + lat
  p <- rasterize(y, msk, field = 'valor_p', na.rm = FALSE)
  s <- rasterize(y, msk, field = 'slope', na.rm = FALSE)
  s[which(p[] > 0.05)] <- NA
  p[which(p[] > 0.05)] <- NA
  writeRaster(p, paste0('../_data/_tif/_ext/_slp/rst_p_',  vr, '.tif'), overwrite = TRUE)
  writeRaster(s, paste0('../_data/_tif/_ext/_slp/rst_s_', vr, '.tif'), overwrite = TRUE)
  print(paste0('Done', vr))
  return(s)
}
calc_slp <- function(tb, vr){
  x <- tb %>% filter(variable == vr)
  s <- x %>% pull(5) %>% sens.slope()
  p <- s$p.value
  i <- s$conf.int
  m <- s$estimates
  d <- data.frame(int_min = i[1], int_max = i[2], slope = m, valor_p = p, variable = as.character(vr))
  print('Done Calculate Slope...!')
  return(list(x, d))
}
calcSlpAll <- function(df, ID){
  # df <- vls
  # ID <- 1

  df_sub <- df %>% filter(id == ID)
  vars <- paste0('bio_', 1:33)
  for(i in 1:length(vars)){
    eval(parse(text = paste0(vars[i], '<- calc_slp(tb = df_sub, vr = vars[', i, '])')))
  }
  bio_list <- list(bio_1, bio_2, bio_3, bio_4, bio_5, bio_6, bio_7, bio_8, bio_9, bio_10,
                   bio_11, bio_12, bio_13, bio_14, bio_15, bio_16, bio_17, bio_18, bio_19,
                   bio_20, bio_21, bio_22, bio_23, bio_24, bio_25, bio_26, bio_27, bio_28, 
                   bio_29, bio_30, bio_31, bio_32, bio_33)
  annual_means <- sapply(1:length(vars), function(x) bio_list[[x]][1]) %>% bind_rows()
  slp_values <- sapply(1:length(vars), function(x) bio_list[[x]][2])
  slp_values <- lapply(1:length(slp_values), function(x) slp_values[[x]] %>% mutate(variable = as.character(variable)))
  slp_values <- bind_rows(slp_values)
  slp_values <- slp_values %>% mutate(id = ID)
  print('Done!')
  return(slp_values)
}

# Load data
vls <- readRDS(file = '../_rds/vls_bios.rds')
vls <- vls[complete.cases(vls),]
vls <- vls %>% mutate(year = as.numeric(year))  
gtm <- getData('GADM', country = 'GTM', level = 1)
yrs <- 1980:2017
msk <- readRDS(file = '../_rds/msk.rds')

# Calculating the slope
n <- unique(vls$id) %>% length(); ids <- 1:n
cl <- makeCluster(30); registerDoSNOW(cl)
pb <- txtProgressBar(max = length(ids), style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

# Foreach
rslts <- foreach(i = ids, .packages = c('dplyr', 'raster', 'rgdal', 'stringr', 'rgeos', 'trend'), .verbose = TRUE, .export = 'calc_slp') %dopar% { #ls(.GlobalEnv)
  calcSlpAll(df = vls, ID = ids[i])
}
rslts <- bind_rows(rslts)
rslts <- as.tibble(rslts)

# Join with the coordinates
crds <- vls %>% filter(variable == 'bio_1', year == 1980) %>% dplyr::select(id, lon, lat)
rslts2 <- inner_join(rslts, crds, by = c('id' = 'id')) 
saveRDS(object = rslts2, file = '../_rds/slp_values.rds')

# Table to Raster
slp <- rslts2 %>%
  mutate(variable1 = variable) %>%
  nest(-variable1) %>%
  mutate(slp = purrr::map2(.x = data, .y = variable1, .f = makeLyrBio))
slplyrs <- slp$slp %>% stack()
names(slplyrs) <- paste0('bio_', 1:33)

# Making the maps
allMaps <- lapply(1:nlayers(slplyrs), function(i) makeMapSlp(lyr = slplyrs[[i]]))

# Grouping the variables into different plots
gg_temp <- ggarrange(allMaps[[1]], allMaps[[2]], allMaps[[3]], allMaps[[4]], allMaps[[5]], allMaps[[6]], allMaps[[7]], allMaps[[8]], allMaps[[9]], allMaps[[10]], allMaps[[11]], labels = paste0('Bio ', 1:11), ncol = 4, nrow = 3)
ggsave(plot = gg_temp, filename = paste0('../_png/_maps/_slp/map_trends_temp', '.png'), width = 22, height = 14, units = 'in', dpi = 400)
gg_prec <- ggarrange(allMaps[[12]], allMaps[[13]], allMaps[[14]], allMaps[[15]], allMaps[[16]], allMaps[[17]], allMaps[[18]], allMaps[[19]], allMaps[[20]], labels = paste0('Bio ', 12:20), ncol = 3, nrow = 3)
ggsave(plot = gg_prec, filename = paste0('../_png/_maps/_slp/map_trends_prec', '.png'), width = 22, height = 14, units = 'in', dpi = 400)
gg_clim <- ggarrange(allMaps[[21]], allMaps[[22]], allMaps[[23]], allMaps[[24]], labels = paste0('Bio ', 21:24), ncol = 2, nrow = 2)
ggsave(plot = gg_clim, filename = paste0('../_png/_maps/_slp/map_trends_clim', '.png'), width = 17, height = 12, units = 'in', dpi = 250)
gg_pet <- ggarrange(allMaps[[25]], allMaps[[26]], allMaps[[27]], allMaps[[28]], allMaps[[29]], allMaps[[30]], allMaps[[31]], allMaps[[32]], allMaps[[33]], labels = paste0('Bio ', 25:33), ncol = 3, nrow = 3)
ggsave(plot = gg_pet, filename = paste0('../_png/_maps/_slp/map_trends_pet', '.png'), width = 22, height = 14, units = 'in', dpi = 250)





# setup -------------------------------------------------------------------

library(tbeptools)
library(here)
library(doParallel)
library(foreach)
library(sf)
library(tidyverse)
library(httr)
library(XML)

data(sgseg)

# NAD83(HARN) / Florida West (ftUS)
# this is the projection for the seagrass segment layer from the district
prj <- 2882

# import seagrass layers from source --------------------------------------

# tb segment boundaries
tbsgseg <- sgseg %>% 
  filter(segment %in% c('Old Tampa Bay', 'Hillsborough Bay', 'Middle Tampa Bay', 'Lower Tampa Bay', 'Terra Ceia Bay', 
                         'Manatee River','Boca Ciega Bay')) %>% 
  st_geometry() %>%
  st_cast('POLYGON')
  

# all zipped files on amazon s3
# downloaded from here https://data-swfwmd.opendata.arcgis.com/
fls <- c('88', '90', '92', '94', '96', '99', '01', '04', '06', '08', '10', '12', '14', '16', '18', '20') %>% 
  paste0('https://swfwmd-seagrass.s3.amazonaws.com/sg', ., '.zip')

# setup parallel backend
ncores <- detectCores() - 1 
cl <- makeCluster(ncores)
registerDoParallel(cl)
strt <- Sys.time()

res <- foreach(i = 1:length(fls), .packages = c('tidyverse', 'sf', 'here')) %dopar% {
  
  sink('log.txt')
  cat(i, 'of', nrow(fls), '\n')
  print(Sys.time()-strt)
  sink()
  
  ## import file
  
  # download from s3, unzip
  tmpfl <- tempfile(fileext = '.zip')
  tmpdr <- tempdir()
  download.file(fls[i], destfile = tmpfl)
  unzip(tmpfl, exdir = tmpdr)
  
  # import shapefile
  allfls <- list.files(tmpdr, full.names = T)
  toimp <- grep('\\.shp$', allfls, value = T)
  dat_raw <- st_read(toimp)
  
  # delete files
  unlink(tmpdr, recursive = T)
  
  if('FLUCCS_CODE' %in% names(dat_raw))
    dat_raw <- dat_raw %>% 
    rename(FLUCCSCODE = FLUCCS_CODE)
  
  # crop by watershed and select fluccs
  # 9113 is patchy, 9116 is continuous
  dat_crp <- dat_raw %>%
    st_transform(crs = prj) %>%
    dplyr::select(FLUCCSCODE) %>% 
    filter(FLUCCSCODE %in% c('9113', '9116')) %>%
    st_intersection(tbsgseg)
  
  # name assignment and save
  flnm <- gsub('^sg|\\.zip$', '', basename(fls[i]))
  if(as.numeric(flnm) < 0)
    flnm <- paste0('19', flnm)
  if(as.numeric(flnm) >= 0)
    flnm <- paste0('20', sprintf("%02d", flnm))
  flnm <- paste0('sgdat', flnm)
  assign(flnm, dat_crp)
  save(list = flnm, file = here('data', paste0('/', flnm, '.RData')), compress = 'xz')
  
}

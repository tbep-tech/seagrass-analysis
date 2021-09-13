
# setup -------------------------------------------------------------------

library(tbeptools)
library(here)
library(sf)
library(tidyverse)
library(httr)
library(XML)

data(sgseg)
data(sgmanagement)

# NAD83(HARN) / Florida West (ftUS)
# this is the projection for the seagrass segment layer from the district
prj <- 2882

# import seagrass layers from source --------------------------------------

# tb segment boundaries
tbsgseg <- sgseg %>% 
  filter(segment %in% c('Old Tampa Bay', 'Hillsborough Bay', 'Middle Tampa Bay', 'Lower Tampa Bay', 'Terra Ceia Bay', 
                         'Manatee River','Boca Ciega Bay')) %>% 
  st_union() %>% 
  st_buffer(dist = 0) %>% 
  st_geometry() %>%
  st_cast('POLYGON') %>% 
  st_buffer(dist = 0) 

# management areas
tbsgmng <- sgmanagement %>% 
  st_union() %>% 
  st_buffer(dist = 0) %>% 
  st_geometry() %>%
  st_cast('POLYGON') %>% 
  st_buffer(dist = 0) 
  
# combine tb boundaries and management areas
tbbnds <- st_union(tbsgmng, tbsgseg) %>% 
  st_union() %>% 
  st_buffer(dist = 0) %>% 
  st_geometry() %>%
  st_cast('POLYGON') %>% 
  st_buffer(dist = 0) 

# all zipped files on amazon s3
# downloaded from here https://data-swfwmd.opendata.arcgis.com/
fls <- c('88', '90', '92', '94', '96', '99', '01', '04', '06', '08', '10', '12', '14', '16', '18', '20') %>% 
  paste0('https://swfwmd-seagrass.s3.amazonaws.com/sg', ., '.zip')

for(i in 1:length(fls)){
  
  cat(i, 'of', length(fls), '\n')
  
  ## import file
  
  # download from s3, unzip
  tmpdir <- here('data/tmp')
  tmpzip <- here('data/tmp/tmp.zip')
  dir.create(tmpdir)
  download.file(fls[i], destfile = tmpzip)
  unzip(tmpzip, exdir = tmpdir)
  
  # import shapefile
  toimp <- list.files(tmpdir, pattern = '\\.shp$', full.names = T)
  dat_raw <- st_read(toimp, quiet = T)
  
  # delete files
  unlink(tmpdir, recursive = T)
  
  if(any(c('FLUCCS_CODE', 'FLUCCS_COD') %in% names(dat_raw)))
    names(dat_raw) <- gsub('^FLUCCS\\_COD$|^FLUCCS\\_CODE$', 'FLUCCSCODE', names(dat_raw))
  
  # crop by watershed and select fluccs
  # 9113 is patchy, 9116 is continuous
  dat_crp <- dat_raw %>%
    st_transform(crs = prj) %>%
    dplyr::select(FLUCCSCODE) %>% 
    filter(FLUCCSCODE %in% c('9113', '9116')) %>%
    st_intersection(tbbnds)
  
  # name assignment and save
  flnm <- gsub('^sg|\\.zip$', '', basename(fls[i])) %>% 
    as.numeric
  if(flnm > 80){
    flnm <- paste0('19', flnm)
  } else {
    flnm <- paste0('20', sprintf("%02d", flnm))
  }
  flnm <- paste0('sgdat', flnm)
  assign(flnm, dat_crp)
  save(list = flnm, file = here('data', paste0('/', flnm, '.RData')), compress = 'xz')
  
}


# setup -------------------------------------------------------------------

library(tbeptools)
library(here)
library(doParallel)
library(foreach)
library(sf)
library(tidyverse)

# import seagrass layers from source --------------------------------------

data(tbshed)

# https://data-swfwmd.opendata.arcgis.com/search?groupIds=d9a4213eb9ea4713bb710e03bdcc6648
urls <- list(
  `2020` = 'https://opendata.arcgis.com/datasets/2b5e369edd244969bc05baa8e713cffc_1.geojson',
  `2018` = 'https://opendata.arcgis.com/datasets/8d0d473468924423bf0f1682aaca790f_0.geojson',
  `2016` = 'https://opendata.arcgis.com/datasets/f0ecff0cf0de491685f8fb074adb278b_20.geojson',
  `2014` = 'https://opendata.arcgis.com/datasets/f530f972ded749adb1c6b20c2651e7f9_18.geojson',
  `2012` = 'https://opendata.arcgis.com/datasets/619bd267e4c54e70968abd86eb92318e_17.geojson',
  `2010` = 'https://opendata.arcgis.com/datasets/82153be25a3340a0abdb3ec713425f29_16.geojson',
  `2008` = 'https://opendata.arcgis.com/datasets/4ddc60a8c9f845a2912c4e7cb14a3b7b_15.geojson',
  `2006` = 'https://opendata.arcgis.com/datasets/5a72bbd64bc9486696fa0bc47ca4e30c_13.geojson',
  `2004` = 'https://opendata.arcgis.com/datasets/bb6b117c8eab40209d8125c3c95f6150_12.geojson',
  `2001` = 'https://opendata.arcgis.com/datasets/e2ce063712f34654a4f371240f541479_11.geojson', 
  `1999` = 'https://opendata.arcgis.com/datasets/e27b6e5148514f29a1f1483813297fd7_10.geojson', 
  `1996` = 'https://opendata.arcgis.com/datasets/38f62dd9b6e5482888b2c0bb51716b6e_9.geojson',
  `1994` = 'https://opendata.arcgis.com/datasets/a2fb9d100cfd441cbdd24b16a3b0ce53_8.geojson',
  `1992` = 'https://opendata.arcgis.com/datasets/ea9fab53f2f74236b0cba8980dffe363_7.geojson',
  `1990` = 'https://opendata.arcgis.com/datasets/bcc955216c62468c9a6dafffc0545a40_6.geojson',
  `1988` = 'https://opendata.arcgis.com/datasets/092df867ece945b787557c9a7cf811d8_5.geojson'
) %>% 
  enframe 

# setup parallel backend
ncores <- detectCores() - 1 
cl <- makeCluster(ncores)
registerDoParallel(cl)
strt <- Sys.time()

res <- foreach(i = 1:nrow(urls), .packages = c('tidyverse', 'sf', 'here')) %dopar% {
  
  sink('log.txt')
  cat(i, 'of', nrow(urls), '\n')
  print(Sys.time()-strt)
  sink()
  
  # import file
  dat_raw <- urls[i, ] %>% 
    pull(value) %>% 
    .[[1]] %>% 
    st_read
  
  if('FLUCCS_CODE' %in% names(dat_raw))
    dat_raw <- dat_raw %>% 
    rename(FLUCCSCODE = FLUCCS_CODE)
  
  # crop by watershed and select fluccs
  # 9113 is patchy, 9116 is continuous
  dat_crp <- dat_raw %>%
    st_transform(crs = prj) %>%
    dplyr::select(FLUCCSCODE) %>% 
    filter(FLUCCSCODE %in% fluccs$FLUCCSCODE) %>%
    st_buffer(dist = 0) %>% 
    st_intersection(tbshed)
  
  # name assignment and save
  flnm <- paste0('sgdat', urls$name[i])
  assign(flnm, dat_crp)
  save(list = flnm, file = here('data', paste0('/', flnm, '.RData')), compress = 'xz')
  
}
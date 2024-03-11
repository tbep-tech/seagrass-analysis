
# setup -------------------------------------------------------------------

library(tbeptools)
library(here)
library(sf)
library(tidyverse)
library(httr)
library(XML)
library(raster)

# NAD83(HARN) / Florida West (ftUS)
# this is the projection for the seagrass segment layer from the district
prj <- 2882

# bay segments clipped to shore -------------------------------------------

load(file = url('https://github.com/tbep-tech/benthic-dash/raw/main/data/segmask.RData'))

segclp <- tbsegshed %>%
  st_make_valid() %>%
  st_intersection(segmask) %>%
  st_simplify(dTolerance = 20, preserveTopology = TRUE) %>% 
  rename(segment = long_name)

save(segclp, file = here('data/segclp.RData'))

# dem data ----------------------------------------------------------------

# # https://www.ngdc.noaa.gov/mgg/bathymetry/estuarine/
# dem <- raster('~/Desktop/TBEP/tampa_bay_G070_2017.nc')
# dem <- readAll(dem)
# save(dem, file = 'data/dem.RData', compress = 'xz')

# import seagrass layers from source --------------------------------------

# district layer
toint <- swfwmdtbseg %>% 
  st_transform(crs = prj) %>% 
  st_union()
  
# all zipped files on amazon s3
# downloaded from here https://data-swfwmd.opendata.arcgis.com/
fls <- c('88', '90', '92', '94', '96', '99', '01', '04', '06', '08', '10', '12', '14', '16', '18', '20', '22') %>% 
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
    st_intersection(toint)
  
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

# union of all sg layers as "potential" -------------------------------------------------------

library(sf)
library(tidyverse)
library(here)

res <- list.files('data', '^sgdat') %>% 
  enframe() %>% 
  group_by(value) %>% 
  nest %>% 
  mutate(
    dat = purrr::map(value, function(x){
      
      cat(x, '\t')
      
      # import file
      load(file = here(paste0('data/', x)))
      dat <- get(gsub('\\.RData', '', x))
      
      dat_out <- dat |> 
        filter(FLUCCSCODE %in% c(9113, 9116)) |> 
        st_union() |> 
        st_geometry()
      
      return(dat_out)
      
    })
  )

out <- res$dat[[1]]
for(i in 2:nrow(res)){
  
  cat(i, '\t')
  out <- st_union(out, res$dat[[i]])
  
}

allsgdat <- out
save(allsgdat, file = here('data/allsgdat.RData'))     


# area sum of allsgdat ------------------------------------------------------------------------

load(file = here('data/allsgdat.RData'))

# area sum
allsgacres <- allsgdat %>%
  st_transform(crs = prj) %>%
  st_intersection(sgseg, .) %>%
  filter(!segment %in% c('Upper Sarasota Bay-m', 'Gulf of Mexico')) %>% 
  mutate(
    Acres = st_area(.), 
    Acres = units::set_units(Acres, 'acres'), 
    Acres = as.numeric(Acres)
  ) %>%
  st_set_geometry(NULL) |> 
  arrange(segment)

save(allsgacres, file = here('data/allsgacres.RData'))

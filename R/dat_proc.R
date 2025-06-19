
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
  
toint2024 <- swfwmdtbseg2024 %>% 
  st_transform(crs = prj) %>% 
  st_union()

# all zipped files on amazon s3
# downloaded from here https://data-swfwmd.opendata.arcgis.com/
fls <- c('88', '90', '92', '94', '96', '99', '01', '04', '06', '08', '10', '12', '14', '16', '18', '20', '22', '24') %>% 
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
  
  if(any(c('FLUCCS_CODE', 'FLUCCS_COD', 'FLUCCS_Cod') %in% names(dat_raw)))
    names(dat_raw) <- gsub('^FLUCCS\\_COD$|^FLUCCS\\_CODE$|^FLUCCS\\_Cod$', 'FLUCCSCODE', names(dat_raw))
  
  # crop by watershed and select fluccs
  # 9113 is patchy, 9116 is continuous
  dat_crp <- dat_raw %>%
    st_transform(crs = prj) %>%
    dplyr::select(FLUCCSCODE) %>% 
    filter(FLUCCSCODE %in% c('9113', '9116')) 
  
  if(!grepl('sg24', fls[i]))
    dat_crp <- dat_crp %>% 
      st_intersection(toint)
  
  if(grepl('sg24', fls[i]))
    dat_crp <- dat_crp %>% 
      st_intersection(toint2024)
  
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

allsgdat <- out |> 
  st_as_sf() |> 
  st_collection_extract(type = 'POLYGON') |> 
  st_geometry()

save(allsgdat, file = here('data/allsgdat.RData'))     

# area sum of allsgdat ------------------------------------------------------------------------

load(file = here('data/allsgdat.RData'))

# area sum
allsgacres <- allsgdat %>%
  st_transform(crs = prj) %>%
  st_intersection(sgseg, .) %>%
  filter(!segment %in% c('Upper Sarasota Bay-m', 'Gulf of Mexico', 'St Joseph Sound')) %>% 
  mutate(
    Acres = st_area(.), 
    Acres = units::set_units(Acres, 'acres'), 
    Acres = as.numeric(Acres)
  ) %>%
  st_set_geometry(NULL) %>%
  summarise(
    Acres = sum(Acres), 
    .by = segment
  ) %>%
  arrange(segment)

save(allsgacres, file = here('data/allsgacres.RData'))

# segment coverages by year -------------------------------------------------------------------

data("swfwmdtbseg", package = 'tbeptools')
data("sgseg", package = 'tbeptools')

swfwmdtbseg <- st_make_valid(swfwmdtbseg)
sgseg <- st_make_valid(sgseg) %>%
  dplyr::filter(segment %in% c('Boca Ciega Bay', 'Hillsborough Bay', 'Old Tampa Bay', 'Middle Tampa Bay',
                                        'Lower Tampa Bay', 'Manatee River', 'Terra Ceia Bay'))

fls <- list.files(path = 'T:/04_STAFF/MARCUS/03_GIT/hmpu-workflow/data', pattern = 'sgdat', full.names = T)

for(fl in fls){
  cat(fl, '\n')
  load(file = fl)
}

sgyrs <- fls %>% 
  basename %>% 
  gsub('\\.RData$', '', .) 
sgsegest <- NULL

for(sgyr in sgyrs){
  
  cat(sgyr, '\n')
  
  yr <- gsub('sgdat', '', sgyr)
  
  dat <- get(sgyr) %>% 
    filter(FLUCCSCODE %in% c(9113, 9116)) %>% 
    st_union() |> 
    st_make_valid()
  
  if(!grepl('2024', sgyr))
    dat <- dat %>%  
      st_transform(crs = st_crs(swfwmdtbseg)) %>% 
      st_make_valid() %>%
      st_intersection(swfwmdtbseg, .) 
  
  if(grepl('2024', sgyr))
    dat <- dat %>%  
      st_transform(crs = st_crs(sgseg)) %>% 
      st_make_valid() %>%
      st_intersection(sgseg, .)  
  
  dat <- dat %>%
      mutate(
        acres = st_area(.), 
        acres = units::set_units(acres, 'acres'), 
        acres = as.numeric(acres)
      ) %>% 
      st_set_geometry(NULL) %>% 
      mutate(
        year = as.numeric(yr)
      )
  
  sgsegest <- rbind(sgsegest, dat)
  
}

save(sgsegest, file = here('data/sgsegest.RData'))

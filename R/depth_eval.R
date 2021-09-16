library(sf)
library(tidyverse)
library(tbeptools)
library(mapview)
library(units)
library(stars)
library(raster)

data(dem)
data(sgseg)
data(sgdat2018)
data(sgdat2020)

fluccs <- tibble(
  FLUCCSCODE = c(9113, 9116), 
  Category = c('Patchy', 'Cont.')
)

bnds <- sgseg %>% 
  filter(segment %in% c('Old Tampa Bay', 'Hillsborough Bay', 'Middle Tampa Bay', 'Lower Tampa Bay', 'Terra Ceia Bay', 
                        'Manatee River','Boca Ciega Bay'))

# this is the projstring for NAD83(HARN) / Florida West (ftUS)
# https://spatialreference.org/ref/epsg/2882/
prj4 <- '+proj=tmerc +lat_0=24.33333333333333 +lon_0=-82 +k=0.999941177 +x_0=200000.0001016002 +y_0=0 +ellps=GRS80 +to_meter=0.3048006096012192 +no_defs'
demprj <- projectRaster(dem, crs = prj4)
demstr <- st_as_stars(demprj)

# 2020 data
maxdat <- sgdat2020 %>% 
  st_union(by_feature = TRUE) %>%
  left_join(fluccs,by = 'FLUCCSCODE') %>% 
  mutate(
    Category = paste0(Category, ', 2020')
  )

a <- sgdat2018 %>% 
  left_join(fluccs, by = 'FLUCCSCODE') %>%
  st_union(by_feature = TRUE) %>%
  mutate(
    Category = paste0(Category, ', 2018')
  )
b <- maxdat
  
# so intersect doesnt complain about attributes
st_agr(a) = "constant"
st_agr(b) = "constant"

# some clean up stuff for slivers
a <- a %>% 
  st_set_precision(1e5) %>% 
  st_make_valid() %>% 
  st_buffer(dist = 0)
b <- b %>% 
  st_set_precision(1e5) %>% 
  st_make_valid() %>% 
  st_buffer(dist = 0)
aunion <- a %>% 
  st_union %>% 
  st_set_precision(1e5) %>% 
  st_make_valid() %>% 
  st_buffer(dist = 0)
bunion <- b %>% 
  st_union %>% 
  st_set_precision(1e5) %>% 
  st_make_valid() %>% 
  st_buffer(dist = 0)
  
# get full union
op1 <- st_difference(a, bunion)
op2 <- st_difference(b, aunion) %>%
  rename(Category.1 = Category)
op3 <- st_intersection(a, b)

union <- bind_rows(op1, op2, op3) %>%
  mutate(
    yr = unique(na.omit(sub('^.*\\,\\s([0-9]+)$', '\\1', Category))),
    yr.1 = unique(na.omit(sub('^.*\\,\\s([0-9]+)$', '\\1', Category.1))),
    Category = ifelse(is.na(Category), paste0('other, ', yr), as.character(Category)),
    Category.1 = ifelse(is.na(Category.1), paste0('other, ', yr.1), as.character(Category.1)),
    Acres = st_area(.),
    Acres = set_units(Acres, 'acres'),
    Acres = as.numeric(Acres)
  ) %>%
  dplyr::select(Category.1, Category, Acres) %>%
  dplyr::select(source = Category, target = Category.1, value = Acres)

chgdat <- union %>% 
  filter(target == 'other, 2020' | source == 'other, 2018') %>% 
  mutate(
    var = case_when(
      target == 'other, 2020' ~ 'lost', 
      source == 'other, 2018' ~ 'gained'
    )
  )

mapview::mapview(chgdat, zcol = 'var')

chgdat <- chgdat %>% 
  st_transform(crs = prj4)

tmp <- aggregate(demstr, chgdat, mean)

?# tmp <- st_intersection(chgdat, bnds) %>% 
#   st_set_geometry(NULL) %>% 
#   group_by(segment, var) %>% 
#   summarise(
#     value = sum(value, na.rm = T), 
#     .groups = 'drop'
#   ) %>% 
#   spread(var, value) %>% 
#   mutate(
#     chg = gained - lost
#   )

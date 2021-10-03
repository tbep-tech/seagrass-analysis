library(tbeptools)
library(tidyverse)
library(sf)
library(leaflet)
library(mapview)

colgrn <- c("#F7FCF5", "#E5F5E0", "#C7E9C0", "#A1D99B", "#74C476", "#41AB5D", 
            "#238B45", "#006D2C", "#00441B")
colred <- c("#FFF5F0", "#FEE0D2", "#FCBBA1", "#FC9272", "#FB6A4A", "#EF3B2C", 
            "#CB181D", "#A50F15", "#67000D")               

prj4 <- '+proj=tmerc +lat_0=24.33333333333333 +lon_0=-82 +k=0.999941177 +x_0=200000.0001016002 +y_0=0 +ellps=GRS80 +to_meter=0.3048006096012192 +no_defs'

data(chgdatarea)

pth <- 'data-raw/epchc.xlsx'
epcraw <- read_importwq(pth, download_latest = T) 

# reproject layers
tbseg <- tbseg %>% 
  st_transform(crs = prj4)
sgmanagement <- sgmanagement %>% 
  st_transform(crs = prj4)

wqdat <- epcraw %>% 
  select(station = epchc_station, SampleTime, yr, mo, lat = Latitude, lng = Longitude, sd = sd_raw_m, turb = `Turbidity_JTU-NTU`, col = Color_345_F45_PCU) %>% 
  filter(yr %in% c(2019, 2017)) %>% 
  # filter(mo %in% c(7:9)) %>% 
  st_as_sf(coords = c('lng', 'lat'), crs = 4326) %>% 
  st_transform(crs = prj4) %>% 
  st_intersection(tbseg) %>% 
  st_intersection(sgmanagement) %>% 
  st_set_geometry(NULL) %>% 
  unite('allbnds', bay_segment, areas, sep = ', ', remove = F) %>% 
  select(yr, allbnds, sd, turb, col) %>% 
  gather('var', 'val', sd, turb, col) %>% 
  filter(!grepl('[:alpha:]', val)) %>% 
  mutate(val = as.numeric(val)) %>% 
  group_by(yr, allbnds, var) %>% 
  summarise(val = mean(val, na.rm = T), .groups = 'drop') %>% 
  spread(yr, val) %>% 
  mutate(
    wqdlt = (`2019` - `2017`) / `2017`
  ) %>% 
  separate(allbnds, c('bay_segment', 'areas'), sep = ', ', remove = F)

sgdat <- chgdatarea %>% 
  st_intersection(tbseg) %>% 
  st_intersection(sgmanagement) %>% 
  st_set_geometry(NULL) %>% 
  unite('allbnds', bay_segment, areas, sep = ', ', remove = F) %>% 
  group_by(allbnds, var) %>% 
  summarise(meandepth = weighted.mean(meandepth, w = Acres), .groups = 'drop') %>% 
  spread(var, meandepth) %>% 
  mutate(
    sgdlt = gained - lost
  ) %>% 
  na.omit() %>% 
  separate(allbnds, c('bay_segment', 'areas'), sep = ', ', remove = F)


maxv <- 0.97
colfun <- colorNumeric(
  palette = c(rev(colgrn), colred),
  domain = c(-1 * maxv, maxv)
)
tomap <- sgdat %>% 
  select(areas, sgdlt) %>% 
  mutate(areas = as.numeric(areas)) %>% 
  inner_join(sgmanagement, ., by = 'areas') %>% 
  mutate(
    fillhx = colfun(sgdlt)
  ) %>% 
  st_transform(crs = 4326)

mapin <- mapview(tomap, homebutton = F, popup = NULL, legend = F) %>% 
  .@map %>% 
  leafem::removeMouseCoordinates() %>% 
  clearShapes()
vls <- seq(-1 * maxv, maxv, length.out = 10)
mapin %>% 
  addPolygons(
    data = tomap,
    stroke = T,
    color = 'black',
    weight = 1,
    layerId = ~areas,
    fillColor = ~fillhx,
    fillOpacity = 0.8
  ) %>% 
  addLegend("topright", pal = colfun, values = vls, opacity = 0.8)  

toplo <- inner_join(wqdat, sgdat, by = c('allbnds', 'bay_segment', 'areas') %>% 
  select(allbnds, var, sgdlt, wqdlt)

ggplot(toplo, aes(x = wqdlt, y = sgdlt)) + 
  geom_point(aes(color = bay_segment)) + 
  facet_wrap(~var, scales = 'free_x') +
  geom_smooth(method = 'lm')

tomod <- toplo %>% 
  spread(var, wqdlt)

toplo <- wqdat %>% 
  select(station, yr, mo, sd, turb, col, areas) %>% 
  gather('var', 'val', sd, turb, col) %>% 
  filter(!grepl('[:alpha:]', val)) %>% 
  mutate(val = as.numeric(val))

ggplot(toplo, aes(x = val, color = factor(yr), group = yr)) +
  stat_ecdf() + 
  facet_grid(areas ~ var, scales = 'free_x')
  

wqtrnds <- epcraw %>% 
  select(yr, bay_segment, sd = sd_raw_m, turb = `Turbidity_JTU-NTU`, col = Color_345_F45_PCU) %>% 
  gather('var', 'val', sd, turb, col) %>% 
  group_by(yr, bay_segment, var) %>% 
  filter(!grepl('[:alpha:]', val)) %>% 
  mutate(val = as.numeric(val)) %>% 
  summarise(val = mean(val, na.rm = T), .groups = 'drop') %>% 
  filter(yr > 1990 & yr < 2020) %>% 
  arrange(bay_segment, yr) %>% 
  group_by(bay_segment, var) %>% 
  mutate(
    pct_change = ((val - lag(val))/lag(val)) * 100
  ) %>% 
  ungroup()

ggplot(wqtrnds, aes(x = yr, y = val)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(var~bay_segment, scales = 'free_y')

ggplot(wqtrnds, aes(x = yr, y = pct_change, fill = sign(pct_change))) + 
  geom_bar(stat = 'identity') + 
  facet_wrap(var~bay_segment, scales = 'free_y')

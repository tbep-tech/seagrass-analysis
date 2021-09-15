
# setup -------------------------------------------------------------------

box::use(
  units[set_units], 
  dplyr[...],
  sf[st_read, st_set_geometry, st_intersection, st_transform, st_crs, st_area], 
  here[...],
  purrr[pmap],
  tidyr[nest, unnest]
)
data(file = 'sgseg', package = 'tbeptools')
data(file = 'sgmanagement', package = 'tbeptools')

bnds <- sgseg %>% 
  filter(!segment %in% c('Gulf of Mexico'))

mngs <- sgmanagement

flcat <- list(
  code = c('9113', '9116'),
  name = c('patchy', 'cont.')
)

# estimates by segments ---------------------------------------------------

allsegests <- tibble(
    fls = list.files(here('data'), pattern = '^sg', full.names = T)
  ) %>% 
  mutate(
    yr = gsub('^sgdat|\\.RData$', '', basename(fls))
  ) %>% 
  group_by(yr) %>% 
  nest() %>% 
  mutate(
    data = pmap(list(yr, data), function(yr, data){
      
      cat(yr, '\n')

      # load and get object name
      load(file = here(data))
      obj <- gsub('\\.RData$', '', basename(data[[1]])) 
      
      # make sure crs is the same, get relevant fluccs
      sgrs <- get(obj) %>% 
        st_transform(crs = st_crs(bnds)) %>% 
        filter(FLUCCSCODE %in% flcat[['code']])
      
      # estimate coverage by flucss, segment
      ests <- st_intersection(sgrs, bnds) %>% 
        mutate(area = st_area(.)) %>% 
        st_set_geometry(NULL) %>% 
        group_by(segment, FLUCCSCODE) %>% 
        summarise(area = sum(area), .groups = 'drop') %>% 
        mutate(
          Acres = as.numeric(set_units(area, 'acres')), 
          Hectares = as.numeric(set_units(area, 'hectares'))
        ) %>% 
        select(Segment = segment, Habitat = FLUCCSCODE, Acres, Hectares) %>% 
        mutate(
          Habitat = factor(Habitat, levels = flcat$code, labels = flcat$name)
          )
      
      return(ests)

    })
  ) %>% 
  unnest('data')

save(allsegests, file = here('data/allsegests.RData'))

# estimates by management areas -------------------------------------------

allmngests <- tibble(
  fls = list.files(here('data'), pattern = '^sg', full.names = T)
) %>% 
  mutate(
    yr = gsub('^sgdat|\\.RData$', '', basename(fls))
  ) %>% 
  group_by(yr) %>% 
  nest() %>% 
  mutate(
    data = pmap(list(yr, data), function(yr, data){
      
      cat(yr, '\n')
      
      # load and get object name
      load(file = here(data))
      obj <- gsub('\\.RData$', '', basename(data[[1]])) 
      
      # make sure crs is the same, get relevant fluccs
      sgrs <- get(obj) %>% 
        st_transform(crs = st_crs(bnds)) %>% 
        filter(FLUCCSCODE %in% flcat[['code']])
      
      # estimate coverage by flucss, area
      ests <- st_intersection(sgrs, mngs) %>% 
        mutate(area = st_area(.)) %>% 
        st_set_geometry(NULL) %>% 
        group_by(areas, FLUCCSCODE) %>% 
        summarise(area = sum(area), .groups = 'drop') %>% 
        mutate(
          Acres = as.numeric(set_units(area, 'acres')), 
          Hectares = as.numeric(set_units(area, 'hectares'))
        ) %>% 
        select(Areas = areas, Habitat = FLUCCSCODE, Acres, Hectares) %>% 
        mutate(
          Habitat = factor(Habitat, levels = flcat$code, labels = flcat$name)
        )
      
      return(ests)
      
    })
  ) %>% 
  unnest('data')

save(allmngests, file = here('data/allmngests.RData'))


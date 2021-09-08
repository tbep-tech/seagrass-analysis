#' @export
sgrctfun <- function(datin, colnm = c('Segment', 'Areas'), yrsel = '1988', topyr = '2020', firstwidth = 150){

  box::use(
    dplyr[...], 
    reactable[...]
  )

  colnm <- match.arg(colnm)
  
  sticky_style <- list(position = "sticky", left = 0, background = "#fff", zIndex = 1,
                       borderRight = "1px solid #eee")
  
  jsfun <- JS("function(rowInfo) {
    var value = rowInfo.row.chg
    if (parseInt(value) >= 0) {
      var color = '#008000E6'
    } else if (parseInt(value) < 0) {
      var color = '#e00000E6'
    } 
    return { color: color, fontWeight: 'bold' }
    }"
  )

  # calc diffs if yrsel not equal to topyr  
  if(yrsel != topyr){
    sums <- datin %>%
      rename(chgyr = !!yrsel) %>% 
      mutate(
        chg = `2020` -  chgyr,
        chgper = 100 * (`2020` - chgyr) / chgyr
      ) %>% 
      rename(val = !!colnm)
    names(sums)[names(sums) == 'chgyr'] <- yrsel
  }
  
  # NA if yrsel equal topyr
  if(yrsel == topyr){
    sums <- datin %>% 
      mutate(
        chg = NA, 
        chgper = NA
      ) %>% 
      rename(val = !!colnm)
  }
    
  totab <- sums %>% 
    mutate(
      chg = formatC(round(chg, 0), format = "d", big.mark = ","),
      chgper = as.character(round(chgper, 0)), 
      val = as.character(val)
    )

  out <- reactable(
    totab, 
    columns = list(
      val = colDef(name = colnm, footer = 'Total', minWidth = firstwidth, class = 'sticky left-col-1-bord', headerClass = 'sticky left-col-1-bord', footerClass = 'sticky left-col-1-bord'), 
      chg = colDef(name = paste0(yrsel, '-', topyr, ' change'), minWidth = 140,
                   style = jsfun, class = 'sticky right-col-2', headerClass = 'sticky right-col-2', footerClass = 'sticky right-col-2'
      ), 
      chgper = colDef(name = '% change', minWidth = 85,
                      style = jsfun,
                      format = colFormat(suffix = '%', digits = 0), 
                      class = 'sticky right-col-1', headerClass = 'sticky right-col-1', footerClass = 'sticky right-col-1'
                      
      )
    ),
    defaultColDef = colDef(
      footer = function(values){
        if(!is.numeric(values))
          return()
        
        formatC(round(sum(values), 0), format= "d", big.mark = ",")
        
      },
      footerStyle = list(fontWeight = "bold"),
      format = colFormat(digits = 0, separators = TRUE), 
      minWidth = 80, resizable = TRUE
    ),
    # defaultPageSize = 7,
    pagination = F, 
    height = 325,
    showPageSizeOptions = F,
    highlight = T,
    wrap = F
  )
  
  return(out)
  
}

#' @export
sgmapfun <- function(datin, colnm = c('Segment', 'Areas'), yrsel, bndin, maxv){
  
  box::use(
    mapview[...], 
    leaflet[...], 
    dplyr[...], 
    leafem[removeMouseCoordinates]
  )
  
  colnm <- match.arg(colnm)
  
  # color palette function
  colfun <- colorNumeric(
    palette = c("#F7FCF5", "#E5F5E0", "#C7E9C0", "#A1D99B", "#74C476", "#41AB5D", 
                "#238B45", "#006D2C", "#00441B"),
    domain = c(0, maxv)
    )
  
  # rename polygons to common
  names(bndin)[names(bndin) %in% tolower(colnm)] <- 'bnds'
  
  # polygon boundaries
  toint <- bndin %>% 
    filter(bnds %in% unique(datin[[colnm]]))

  # estimates to map
  tomap <- datin %>% 
    select(!!colnm, !!yrsel) %>% 
    rename(
      bnds = !!colnm, 
      fillv = !!yrsel
    ) %>% 
    mutate(
      fillhx = colfun(fillv)
    ) %>% 
    full_join(toint, ., by = 'bnds')
  
  # values for legend
  vls <- seq(0, maxv, length.out = 10)

  # map
  out <- mapview(tomap, homebutton = F, popup = NULL, legend = F) %>% 
    .@map %>% 
    removeMouseCoordinates() %>% 
    clearShapes() %>% 
    addPolygons(
      data = tomap, 
      stroke = T, 
      color = 'grey',
      weight = 1, 
      layerId = ~bnds, 
      fillColor = ~fillhx, 
      fillOpacity = 0.8,
      label = ~paste0(bnds, ': ', round(fillv, 0), ' acres')
    ) %>% 
    addLegend("topright", pal = colfun, values = vls, title = paste(yrsel, 'acres'), opacity = 0.8)  

  return(out)
  
}
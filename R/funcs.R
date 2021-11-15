#' @export
sgchgfun <- function(datin, yrsel, colnm){
  
  box::use(
    dplyr[...]
  )
  
  # yrs in input data
  yrs <- names(datin)[!names(datin) %in% colnm] 
  
  # calc diffs if both yrsel present
  if(sum(unique(yrsel) %in% yrs) == 2){
    out <- datin %>%
      rename(chgyr1 = !!yrsel[1]) %>% 
      rename(chgyr2 = !!yrsel[2]) %>% 
      mutate(
        chg = chgyr2 - chgyr1,
        chgper = 100 * (chgyr2 - chgyr1) / chgyr1
      ) %>% 
      rename(val = !!colnm)
    names(out)[names(out) == 'chgyr1'] <- yrsel[1]
    names(out)[names(out) == 'chgyr2'] <- yrsel[2]
  }
  
  # NA if yrsel is equal or missing a yrsel
  if(yrsel[1] == yrsel[2] | any(!yrsel %in% yrs)){
    out <- datin %>% 
      mutate(
        chg = NA, 
        chgper = NA
      ) %>% 
      rename(val = !!colnm)
  }
  
  return(out)
  
}

#' @export
sgtotchgfun <- function(sums, yrsel){
  
  box::use(
    dplyr[...]
  )

  if(yrsel[1] == yrsel[2] | any(!yrsel %in% names(sums))){
    totchg <- ''
    totchgper <- ''
  }

  if(yrsel[1] != yrsel[2] & all(yrsel %in% names(sums))){
    
    totchg <- sums %>%
      .[, c(yrsel)] %>%
      colSums() %>%
      as.numeric
    totchgper <- 100 * (totchg[2] - totchg[1]) / totchg[1]
    totchgper <- totchgper %>% round(0) %>% paste0('%')
    totchg <- diff(totchg) %>% round(0)
    
  }
    
  out <- list(totchg = totchg, totchgper = totchgper)

  return(out)
  
}

#' @export
sgrctfun <- function(datin, colnm = c('Segment', 'Areas'), yrsel, firstwidth = 150){

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

  # get change summary
  sums <- sgchgfun(datin, yrsel, colnm)
  
  # get total change
  totsums <- sgtotchgfun(sums, yrsel)
  totchg <- totsums[['totchg']]
  totchgper <- totsums[['totchgper']]

  # format for table 
  totab <- sums %>% 
    mutate(
      chg = formatC(round(chg, 0), format = "d", big.mark = ","),
      chgper = as.character(round(chgper, 0)), 
      val = as.character(val)
    )

  # function for footer style on totals columns
  footstylfun <- function(value){
    color <- if (value > 0) {
      "#008000"
    } else if (value < 0) {
      "#e00000"
    }
    list(fontWeight = 600, color = color)
  }

  out <- reactable(
    totab, 
    columns = list(
      val = colDef(name = colnm, footer = 'Total', minWidth = firstwidth, class = 'sticky left-col-1-bord', headerClass = 'sticky left-col-1-bord', footerClass = 'sticky left-col-1-bord'), 
      chg = colDef(name = paste0(yrsel[1], '-', yrsel[2], ' change'), minWidth = 140, 
                   style = jsfun, class = 'sticky right-col-2', headerClass = 'sticky right-col-2', footerClass = 'sticky right-col-2',
                   footer = formatC(totchg, format= "d", big.mark = ","),  
                   footerStyle = footstylfun(totchg)
      ), 
      chgper = colDef(name = '% change', minWidth = 85,
                      style = jsfun,
                      format = colFormat(suffix = '%', digits = 0), 
                      class = 'sticky right-col-1', headerClass = 'sticky right-col-1', footerClass = 'sticky right-col-1', 
                      footer = totchgper, 
                      footerStyle = footstylfun(totchgper)
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
    leafem[removeMouseCoordinates], 
    sf[st_transform]
  )
  
  colnm <- match.arg(colnm)
  
  # colors
  colgrn <- c("#F7FCF5", "#E5F5E0", "#C7E9C0", "#A1D99B", "#74C476", "#41AB5D", 
               "#238B45", "#006D2C", "#00441B")
  colred <- c("#FFF5F0", "#FEE0D2", "#FCBBA1", "#FC9272", "#FB6A4A", "#EF3B2C", 
              "#CB181D", "#A50F15", "#67000D")               
  colfun <- colorNumeric(
    palette = c(rev(colred), colgrn),
    domain = c(-1 * maxv, maxv)
    )
  
  # rename polygons to common
  names(bndin)[names(bndin) %in% tolower(colnm)] <- 'bnds'
  
  # polygon boundaries
  toint <- bndin %>% 
    filter(bnds %in% unique(datin[[colnm]]))

  # empty base map
  mapin <- mapview(toint, homebutton = F, popup = NULL, legend = F) %>% 
    .@map %>% 
    removeMouseCoordinates() %>% 
    clearShapes()

  # get change summary
  sums <- sgchgfun(datin, yrsel, colnm)

  # empty map if selected year is unavailable
  if(anyNA(sums$chg))
    out <- mapin
  
  # plot if selected year is available
  if(!anyNA(sums$chg)){

    # estimates to map
    tomap <- sums %>% 
      select(val, chg) %>% 
      rename(
        bnds = val
      ) %>% 
      full_join(toint, ., by = 'bnds') %>% 
      mutate(
        fillhx = colfun(chg)
      ) %>% 
      st_transform(crs = 4326)
    
    # values for legend
    vls <- seq(-1 * maxv, maxv, length.out = 10)

    # map
    out <- mapin %>% 
      addPolygons(
        data = tomap,
        stroke = T,
        color = 'black',
        weight = 1,
        layerId = ~bnds,
        fillColor = ~fillhx,
        fillOpacity = 0.8,
        label = ~paste0(yrsel[1], '-', yrsel[2], ' change for ', bnds, ': ', round(chg, 0), ' acres')
      ) %>% 
      addLegend("topright", pal = colfun, values = vls, title = paste0(yrsel[1], '-', yrsel[2], ' change'), opacity = 0.8)  

  }
  
  return(out)
  
}
#' @export
sgrctfun <- function(datin, colnm, yrsel = '1988', topyr = '2020', firstwidth = 150){

  box::use(
    dplyr[...], 
    reactable[...]
  )

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
  
  sums <- datin %>%
    rename(chgyr = !!yrsel) %>% 
    mutate(
      chg = `2020` -  chgyr,
      chgper = 100 * (`2020` - chgyr) / chgyr
    ) %>% 
    rename(val = !!colnm)
  names(sums)[names(sums) == 'chgyr'] <- yrsel

  totab <- sums %>% 
    mutate(
      chg = formatC(round(chg, 0), format = "d", big.mark = ","),
      chgper = as.character(round(chgper, 0))
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
    defaultPageSize = nrow(sums),
    showPageSizeOptions = F,
    highlight = T,
    wrap = F
  )
  
  return(out)
  
}
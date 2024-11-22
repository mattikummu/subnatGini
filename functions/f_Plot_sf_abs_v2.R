
f_Plot_sf_abs_v2<-function(sf_in,column_in, plt = "nuuk", dirPal = -1){
  
  
  minValue <- quantile( sf_in[[column_in]], .025, na.rm=T)
  maxValue <- quantile( sf_in[[column_in]], .975, na.rm=T) 
  
  
  accur = round((maxValue - minValue)/20, digits = 2)
  
  ValueRange <- seq( plyr::round_any( minValue,accuracy=accur,f=floor ), 
                     plyr::round_any( maxValue,accuracy=accur,f=ceiling ) ,
                     by= plyr::round_any( (maxValue - minValue)/20,accuracy=0.01,f=floor ) )
  
  
  pal <-  scico::scico(20, begin = 0.1, end = 0.9,direction = dirPal, palette = plt)
  
  
  plt_subnatMigr <- 
    tm_shape(sf_in, projection = "+proj=robin") +
    tm_fill(col = column_in,
            palette = pal,
            #contrast = c(0.1, 0.9),
            breaks = ValueRange,
            colorNA = 'grey90',
            legend.is.portrait = FALSE,
            title = paste0(column_in,
                           " [",plyr::round_any( minValue,accuracy=accur,f=floor ),
                           ", ",plyr::round_any( maxValue,accuracy=accur,f=ceiling ) ,"]"))+
    
    
    tm_shape(sf_adm0, projection = "+proj=robin") +
    tm_borders(col = "white",
               lwd = 0.05)+
    
    tm_layout(#main.title = "CCC",
      main.title.position = "center",
      legend.outside = TRUE,
      legend.outside.position = "bottom",
      legend.text.size = .25,
      legend.title.size = .75,
      legend.width = 0.6,
      frame = FALSE)
}
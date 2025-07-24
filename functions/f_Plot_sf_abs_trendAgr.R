
f_Plot_sf_abs_trendAgr<-function(sf_in,column_in,breaks_in, 
                        colPal = scico(9, begin = 0.1, end = 0.9,direction = 1, palette = "nuuk")){
  
  pal <-  colPal
  
  plt_subnatMigr <- tm_shape(sf_in, projection = "+proj=robin", raster.warp = FALSE) +
    tm_fill(col = column_in,
            palette = pal,
            #contrast = c(0, 0.7),
            colorNA = NULL,
            breaks = breaks_in,
            lwd=0.0,
            border.col = "transparent",
            legend.is.portrait = FALSE)+
    tm_shape(sf_adm0, projection = "+proj=robin") +
    tm_borders(col = "grey15",
               lwd = 0.075)+
    tm_layout(#main.title = "Origin of data",
      main.title.position = "center",
      legend.outside = TRUE,
      legend.outside.position = "bottom",
      legend.text.size = .25,
      legend.title.size = .75,
      legend.width = 0.6,
      frame = FALSE,
      asp = 0)
}
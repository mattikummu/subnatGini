
f_Plot_sfAbs_bbox<-function(sf_in,column_in,breaks_in, boundBox, colPal){
  
  #pal <-  scico(9, begin = 0.1, end = 0.9,direction = -1, palette = "nuuk")
  
  plt_subnatMigr <- tm_shape(sf_in, projection = "+proj=robin", bbox = boundBox) +
    tm_fill(col = column_in,
            palette = colPal,
            #contrast = c(0, 0.7),
            colorNA = 'white',
            breaks = breaks_in,
            lwd=0.0,
            legend.is.portrait = FALSE)+
    tm_shape(sf_adm0, projection = "+proj=robin", bbox = boundBox) +
    tm_borders(col = "grey35",
               lwd = 0.2)+
    tm_layout(#main.title = "Origin of data",
      main.title.position = "center",
      legend.outside = TRUE,
      legend.outside.position = "bottom",
      legend.text.size = .25,
      legend.title.size = .75,
      legend.width = 0.6,
      frame = FALSE)
}
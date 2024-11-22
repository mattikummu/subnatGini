
f_gini_data2raster <- function(inYears = 1990:2021, IndexName, inDataAdm0, inDataAdm1) {
  
  coll_raster = rast()
  
  ratioName <- 'ratio_gini'
  
  tempDataAdm0 <- inDataAdm0 %>% 
    #filter(year == inYear) %>% 
    select(iso3, year, !!IndexName) %>% 
    left_join(cntryID[,c(1,3)]) %>% 
    mutate(admID = cntry_code) %>% 
    dplyr::select(c(!!IndexName,year, cntry_code, admID, iso3))
  
  tempDataAdm1_Ratio <- inDataAdm1 %>% 
    #filter(year == inYear) %>% 
    select(iso3, year, GID_nmbr, !!ratioName) %>% 
    rename(admID = GID_nmbr) %>% 
    left_join(tempDataAdm0[,c(1,2,5)]) %>% 
    rename(adm0Value = !!IndexName) %>% 
    mutate(!!as.name(IndexName) := !!as.name(ratioName) * adm0Value) %>% 
    dplyr::select(c(!!IndexName,year, 'admID', iso3))
  
  tempDataAdm0Adm1 <- tempDataAdm0 %>% 
    filter(!iso3 %in% unique(tempDataAdm1_Ratio$iso3)) %>% 
    select(-cntry_code) %>% 
    bind_rows(tempDataAdm1_Ratio) %>% 
    select(-iso3) %>% 
    drop_na()
  
  
  idNoData <- adm0adm1_polyg %>% 
    st_drop_geometry() %>% 
    select(admID) %>% 
    filter(!admID %in% unique(tempDataAdm0Adm1$admID)) %>% 
    drop_na()
  
  adm0adm1_raster_5arcmin[adm0adm1_raster_5arcmin %in% as.numeric(as.matrix(idNoData))] <- NA
  
  for (iYear in inYears) {
    
    tempDataAdm0Adm1_selYear <- tempDataAdm0Adm1 %>% 
      filter(year == iYear)
    
    temp_id <-  as.numeric(tempDataAdm0Adm1_selYear$admID)
    temp_v <- as.numeric(tempDataAdm0Adm1_selYear[[IndexName]])
    
    # reclassify
    temp_raster <- classify(adm0adm1_raster_5arcmin,
                            cbind(temp_id, temp_v))
    
    terra::add(coll_raster) <- temp_raster
  }
  
  names(coll_raster) <- paste0(IndexName,'_',inYears[1]:inYears[length(inYears)])
  
  terra::writeRaster(coll_raster,paste0('results/rast_',IndexName,'_',inYears[1],'_',inYears[length(inYears)],'.tif'), gdal="COMPRESS=LZW",overwrite=TRUE)
  
  return(coll_raster)
}
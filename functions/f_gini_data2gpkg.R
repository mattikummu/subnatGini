
f_gini_data2gpkg <- function(inYears = 1990:2021, IndexName, inDataAdm0, inDataAdm1) {
  
  ratioName <- 'ratio_gini'
  
  tempDataAdm0 <- inDataAdm0 %>% 
    select(iso3, year, !!IndexName) %>% 
    left_join(cntryID[,c(1,3)]  %>% distinct(cntry_code, iso3, .keep_all = T)) %>% 
    mutate(admID = cntry_code) %>% 
    dplyr::select(c(!!IndexName,year, cntry_code, admID, iso3))
  
  tempDataAdm1_Ratio <- inDataAdm1 %>% 
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
  
  # calculate trend
  
  # https://stackoverflow.com/questions/72922288/group-wise-linear-models-function-nest-by
  tempDataAdm0Adm1_trend <- tempDataAdm0Adm1 %>% 
    group_by(admID) %>% 
    mutate(time = row_number()) %>% 
    ungroup() %>% 
    select(-year) %>% 
    nest(data = -admID) %>% 
    mutate(
      model = map(data,  ~ mblm::mblm(!!as.name(IndexName) ~ time, data = .))
    ) %>% 
    mutate(
      tidy_summary = map(model, tidy)
    ) %>% 
    unnest(tidy_summary) %>% 
    filter(term == 'time') %>% 
    select(admID, estimate, p.value)
  
  
  
  
  # # https://stackoverflow.com/questions/32274779/extracting-p-values-from-multiple-linear-regression-lm-inside-of-a-ddply-funct
  # 
  # tempDataAdm0Adm1_trend <- tempDataAdm0Adm1 %>% 
  #   group_by(admID) %>% 
  #   #nest() %>% 
  #   do({model = lm(gnic~year, data=.)    # create your model
  #   data.frame(tidy(model),              # get coefficient info
  #              glance(model))})   %>% 
  #   filter(term == 'year')
  
  adm0adm1_polyg_noGeom <- adm0adm1_polyg_simp %>%
    st_drop_geometry() %>% 
    select(iso3, admID)
  
  tempDataAdm0Adm1_wTrend <- tempDataAdm0Adm1 %>% 
    pivot_wider(names_from = 'year', values_from = as.name(!!IndexName)) %>% 
    left_join(tempDataAdm0Adm1_trend) %>% 
    mutate(p.value = p.value < 0.1) %>% 
    mutate(slope = p.value * estimate) %>% 
    right_join(adm0adm1_polyg_simp) %>% 
    left_join(adm0adm1_polyg_noGeom) %>% 
    select(admID, iso3, slope, everything()) 
  
  #ttemp <- tempDataAdm0Adm1_wTrend %>% st_drop_geometry() %>% select(-geom)
  
  # tempDataAdm0Adm1 %>%
  #   dplyr::group_by(admID, year) %>%
  #   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  #   dplyr::filter(n > 1L) 
  
  st_write(tempDataAdm0Adm1_wTrend,paste0('results/vect_',IndexName,'_',inYears[1],'_',inYears[length(inYears)],'.gpkg'), delete_dsn=T)
  
  
  # slope to raster
  
  
  idNoData <- adm0adm1_polyg_simp %>% 
    st_drop_geometry() %>% 
    select(admID) %>% 
    filter(!admID %in% unique(tempDataAdm0Adm1$admID)) %>% 
    drop_na()
  
  adm0adm1_raster_5arcmin[adm0adm1_raster_5arcmin %in% as.numeric(as.matrix(idNoData))] <- NA
  
  temp_id <-  as.numeric(tempDataAdm0Adm1_wTrend$admID)
  temp_v <- as.numeric(tempDataAdm0Adm1_wTrend$slope)
  
  # reclassify
  slope_raster <- classify(adm0adm1_raster_5arcmin,
                           cbind(temp_id, temp_v))
  
  names(slope_raster) <- paste0(IndexName,'_slope_',inYears[1],'_',inYears[length(inYears)] )
  
  terra::writeRaster(slope_raster,paste0('results/rast_slope_',IndexName,'_',inYears[1],'_',inYears[length(inYears)],'.tif'), gdal="COMPRESS=LZW",overwrite=TRUE)
  
  
}

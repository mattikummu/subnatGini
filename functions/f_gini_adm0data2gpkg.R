
f_gini_adm0data2gpkg <- function(inYears = 1990:2021, 
                                 IndexName = 'gini_disp', 
                                 inDataAdm0 = adm0_comb_interpExtrap) {
  
  ratioName <- 'ratio_gini'
  
  tempDataAdm0 <- inDataAdm0 %>% 
    select(iso3, year, !!IndexName) %>% 
    left_join(cntryID[,c(1,3)]) %>% 
    mutate(admID = cntry_code) %>% 
    dplyr::select(c(!!IndexName,year, cntry_code, admID, iso3))
  
  
  
  # calculate trend
  
  # https://stackoverflow.com/questions/72922288/group-wise-linear-models-function-nest-by
  tempDataAdm0_trend <- tempDataAdm0 %>% 
    group_by(admID) %>% 
    mutate(time = row_number()) %>% 
    ungroup() %>% 
    select(-year) %>% 
    nest(data = -admID) %>% 
    mutate(
      model = map(data,  ~ mblm(!!as.name(IndexName) ~ time, data = .))
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
  
  adm0_polyg_noGeom <- sf_adm0_polyg_topo %>%
    st_drop_geometry() %>% 
    rename(admID = cntry_code) %>% 
    select(iso3, admID)
  
  tempDataAdm0_wTrend <- tempDataAdm0 %>% 
    pivot_wider(names_from = 'year', values_from = as.name(!!IndexName)) %>% 
    left_join(tempDataAdm0_trend) %>% 
    mutate(p.value = p.value < 0.1) %>% 
    mutate(slope = p.value * estimate) %>% 
    right_join(adm0_polyg_simp) %>% 
    left_join(adm0_polyg_noGeom) %>% 
    select(admID, iso3, slope, everything()) 
  
  
  # tempDataAdm0Adm1 %>%
  #   dplyr::group_by(admID, year) %>%
  #   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  #   dplyr::filter(n > 1L) 
  
  st_write(tempDataAdm0_wTrend,paste0('results/vect_adm0_',IndexName,'_',inYears[1],'_',inYears[length(inYears)],'.gpkg'), delete_dsn=T)
  
  
  # slope to raster
  
  
  idNoData <- adm0_gis %>% 
    st_drop_geometry() %>% 
    rename(admID = cntry_code) %>% 
    select(admID) %>% 
    filter(!admID %in% unique(tempDataAdm0$admID)) %>% 
    drop_na()
  
  adm0_raster_5arcmin[adm0_raster_5arcmin %in% as.numeric(as.matrix(idNoData))] <- NA
  
  temp_id <-  as.numeric(tempDataAdm0_wTrend$admID)
  temp_v <- as.numeric(tempDataAdm0_wTrend$slope)
  
  # reclassify
  slope_raster <- classify(adm0_raster_5arcmin,
                           cbind(temp_id, temp_v))
  
  names(slope_raster) <- paste0(IndexName,'_slope_',inYears[1],'_',inYears[length(inYears)] )
  
  terra::writeRaster(slope_raster,paste0('results/rast_slope_adm0_',IndexName,'_',inYears[1],'_',inYears[length(inYears)],'.tif'), gdal="COMPRESS=LZW",overwrite=TRUE)
  
  
}


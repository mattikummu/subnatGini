
f_estimateMissingGini <- function(iso3_inBoth, iso3_missing, giniVar, scalingData, nRand, nPred) {
  
  # iso3_inBoth = 'GHA'
  # iso3_missing = 'CIV'
  # giniVar = 'gini_mkt' 
  # scalingData = temp_GDL_nationalMetaData
  # nRand = 10
  # nPred = 10
  
  
  # estimate missing data in SWIID with the help of WID
  
  iso3_temp <- iso3_inBoth
  SWIID_temp <- swiidData %>% 
    filter(iso3 == iso3_temp) %>% 
    #select(iso3, year, gini_disp) %>% 
    #pivot_wider(names_from = 'year', values_from = 'gini_mkt') %>% 
    mutate(year = as.character(year)) %>% 
    mutate(across(where(is.numeric), ~./100)) %>% 
    select(-iso3, -Country) 
  
  # SWIID_temp$year <- as.character(SWIID_temp$year )
  
  
  WID_temp <- scalingData %>% 
    filter(iso3 == iso3_temp) %>% 
    select(-Country, - iso3) %>% 
    pivot_longer(everything())%>% 
    rename(year = name) %>% rename(giniWID = value)
  
  comb_temp <- WID_temp %>% 
    left_join(SWIID_temp) %>% 
    drop_na()
  
  # WID data to be used for predictions
  iso3_pred <- iso3_missing
  WID_pred <- scalingData %>% 
    filter(iso3 == iso3_pred) %>% 
    select(-Country, - iso3) %>% 
    pivot_longer(everything())%>% 
    rename(year = name) %>% rename(giniWID = value) %>% 
    filter(year %in% comb_temp$year)  %>% 
    drop_na()
  
  
  
  # do the predictions for n=100
  seed = 21
  #nRand = 100 # how many predictions
  #nPred = 100 # how many total rounds
  dfRes <- matrix(nrow = nrow(SWIID_temp), ncol = nRand*nPred)
  
  # iPred = 1
  for (iPred in 1:nPred) {
    
    # get data from SWIID (has 100 different dataseries for each country)
    i_tempSWIID <- swiid[[iPred]] %>% 
      rename(Country = country) %>% 
      left_join(swiidMeta) %>% 
      filter(year %in% comb_temp$year) %>% 
      filter(!is.na(iso3)) %>% 
      filter(iso3==iso3_temp) %>% 
      select(-c(Country, abs_red, rel_red, iso3)) %>% 
      mutate(year = as.character(year)) %>% 
      mutate(across(where(is.numeric), ~./10000)) 
    
    dataTemp <- cbind(i_tempSWIID[giniVar], comb_temp$giniWID) %>% 
      as_tibble()
    names(dataTemp) <- c('giniSWIID', 'giniWID')
    
    # lm model
    lmModel <- lm(giniSWIID  ~ giniWID, data = dataTemp)
    # summary(lmModel)
    
    # use lm model to predict
    
    
    lmPred_SWIID_iso3 <- stats::predict.lm(lmModel,data = WID_pred$giniWID, 
                                           se.fit = TRUE, interval = "confidence", level = 0.95)
    
    # SWIID_BRN <- t(rbind(lmPred_SWIID_BRN$fit[,1], lmPred_SWIID_BRN$se.fit)) %>% 
    #   as_tibble()
    # 
    # names(SWIID_BRN) <-  c('mean', 'sd')
    # 
    # write_csv(SWIID_BRN,"results/example_timeseries.csv")
    
    
    for (i in 1:nrow(i_tempSWIID)) {
      dfRes[i,(1+nRand*(iPred-1)):(nRand*iPred)] <- rnorm(nRand, mean = lmPred_SWIID_iso3$fit[i,1], sd = lmPred_SWIID_iso3$se.fit[i])
    }
    
    
    
  }
  
  # calculate mean and sd for this
  dfResMeanSd <- t(rbind(i_tempSWIID$year, apply(dfRes, 1, mean, na.rm=TRUE), apply(dfRes, 1, sd, na.rm=TRUE))) %>% 
    as_tibble() %>% 
    mutate(iso3 = iso3_pred) %>% 
    select(iso3,  everything()) %>% 
    mutate(across(V2:V3, as.numeric))
  
  names(dfResMeanSd) <- c('iso3', 'year', paste0(giniVar), paste0(giniVar,'_se'))
  
  dfResMeanSd[,3:4] <- round(dfResMeanSd[,3:4],digits = 5)
  
  return(dfResMeanSd)
}


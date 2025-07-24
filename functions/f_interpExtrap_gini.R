
f_interpExtrap_gini <- function(nameIndic = 'gini_WID') {
  
  source('functions/f_extrapol.R')
  source('functions/f_interpAdm0.R')
  
  
  indicWider_org <- indicDataFilled %>% 
    select(iso3, year, !!as.name(nameIndic)) %>% 
    arrange(iso3, year) %>% 
    pivot_wider(names_from = 'year', values_from = !!nameIndic) %>% 
    select(c(iso3, as.character(seq(years_in[1],years_in[length(years_in)],1))))
  # 
  
  
  # # count reported years for each country
  # df_reported <- indicWider_org %>%
  #   rowwise() %>%
  #   mutate(NA_count = sum(is.na(c_across(-iso3)))) %>%
  #   ungroup() %>% 
  #   select(iso3, NA_count)
  # 
  # hist(df_reported$NA_count)
  # 
  # 
  # 
  # ggplot(df_reported, aes(x = NA_count)) +
  #   geom_histogram(binwidth = 1, fill = "#69b3a2", color = "white") +
  #   labs(title = "Distribution of Reported Years per Country",
  #        x = "Number of NA Years",
  #        y = "Number of Countries") +
  #   theme_minimal()
  
  
  
  ### interpolation
  
  
  indic_longer <- indicWider_org %>% 
    pivot_longer(-iso3, names_to = 'year', values_to = nameIndic)
  
  indicInterp <-   f_interpAdm0(HDIadm0Comb = indic_longer, 
                                nameIndicator = nameIndic)
  
  indicWider <- indicInterp %>% 
    select(iso3, year, !!as.name(nameIndic)) %>% 
    arrange(iso3, year) %>% 
    pivot_wider(names_from = 'year', values_from = !!nameIndic) %>% 
    select(c(iso3, as.character(seq(years_in[1],years_in[length(years_in)],1))))
  # 
  
  ### extrapolation
  
  
  # full timeseries
  dataFull <- indicWider %>% 
    set_names(c('iso3', paste0('n',years_in))) %>% 
    drop_na()
  
  
  # data where no data
  data0 <- indicDataFilled %>% 
    select(iso3, year, !!nameIndic) %>% 
    rename(value := !!nameIndic) %>% 
    group_by(iso3) %>% 
    summarise(across(where(is.numeric), ~sum(!is.na(.)))) %>% 
    filter(value == 0)
  
  # data where data for less than five years
  dataLess5 <- indicDataFilled %>% 
    select(iso3, year, !!nameIndic) %>% 
    rename(value := !!nameIndic) %>% 
    group_by(iso3) %>% 
    summarise(across(where(is.numeric), ~sum(!is.na(.)))) %>% 
    filter(!value == 0) %>% 
    filter(value < 5)
  
  # select data that is missing max three years from beginning and end of the time period (1990-2021)
  dataNearlyFull <- indicWider %>% 
    set_names(c('iso3', paste0('n',years_in))) %>% 
    filter(!iso3 %in% dataFull$iso3) %>% 
    mutate(nearFullTimeser = ifelse(!is.na(n1993)&!is.na(n2018), 1, 0 )) %>% 
    filter(nearFullTimeser == 1) %>% 
    select(-nearFullTimeser)
  
  # missing between dataLess5 and dataNearlyFull
  dataNotFull <- indicWider %>% 
    filter(!iso3 %in% c(unique(dataFull$iso3), unique(dataLess5$iso3), unique(dataNearlyFull$iso3), unique(data0$iso3))) %>% 
    set_names(c('iso3', paste0('n',years_in))) 
  
  
  # for countries with just few entries missing
  
  if (exists('collectFilledData_nearlyFull')) {
    remove('collectFilledData_nearlyFull') # remove if exist
  } else {
    # do nothing
  }
  
  if (nrow(dataNearlyFull) == 0) {
    dataFull_NearlyFull <- dataFull
  } else{
    for (i in 1:nrow(dataNearlyFull)) { # 
      
      # extrapolate a country in question
      tempExtrapolCntry <- f_extrapol(dataAll = indicWider, 
                                      dataFull = dataFull, 
                                      dataToBeExtrap = dataNearlyFull, 
                                      iCntry = i)
      
      # collect to new data frame
      if (exists('collectFilledData_nearlyFull')) {
        collectFilledData_nearlyFull <- collectFilledData_nearlyFull %>% 
          bind_rows(tempExtrapolCntry)
      } else {
        collectFilledData_nearlyFull <- tempExtrapolCntry
      }
      
    }
    collectFilledData_nearlyFullWide <- collectFilledData_nearlyFull %>% 
      select(iso3, year, valueOrg_filled) %>% 
      pivot_wider(names_from = 'year', values_from = 'valueOrg_filled')
    
    dataFull_NearlyFull <- dataFull %>% 
      bind_rows(collectFilledData_nearlyFullWide )
  }
  
  write_csv(dataFull_NearlyFull, "data_out/dataFull_NearlyFull.csv")
  
  
  # check by region
  
  
  dataFull_reg <- dataFull %>% 
    left_join(cntryID_reg %>% select(iso3, RegionID)) %>% 
    group_by(RegionID) %>% 
    summarise(n_region = n()) %>% 
    ungroup()
  
  dataFull_NearlyFull_reg <- dataFull_NearlyFull %>% 
    left_join(cntryID_reg %>% select(iso3, RegionID)) %>% 
    group_by(RegionID) %>% 
    summarise(n_region = n()) %>% 
    ungroup()
  
  
  # #### 4. for countries with less than 5 observations, we'll use trend from the closest country with full or nearly full data ----
  
  
  if (exists('collectFilledData_less5')) {
    remove('collectFilledData_less5') # remove if exist
  } else {
    # do nothing
  }
  
  
  if (nrow(dataLess5) == 0) {
    collectFilledData_less5Wide <- NULL
  } else {
    for (i in 1:nrow(dataLess5)) {
      
      v_fullData <- subset(p_adm0_centroids, p_adm0_centroids$iso3 %in% dataFull_NearlyFull$iso3)
      v_target <- subset(p_adm0_centroids, p_adm0_centroids$iso3 %in% dataLess5[i,]$iso3)
      
      distTemp <- distance(v_target, v_fullData) %>% 
        as_tibble() 
      
      distTempCntry <- t(distTemp) %>% 
        bind_cols(as_tibble(v_fullData$iso3))
      
      dataClosest <- distTempCntry[which(distTempCntry$...1 == min(distTempCntry$...1)),]
      
      refData <- dataFull_NearlyFull %>% 
        filter(iso3 == dataClosest$value) %>% 
        pivot_longer(-iso3, names_to = 'year', values_to = 'value') %>% 
        select(value)
      
      dataToBeExtrap <- indicWider  %>% 
        set_names(c('iso3', paste0('n',years_in))) %>% 
        filter(iso3 == dataLess5[i,]$iso3)
      
      filledData <- pivot_longer(dataToBeExtrap, -iso3, names_to = 'year', values_to = 'valueOrg') %>% 
        bind_cols(refData) %>% 
        # calculate ratio of the first and last non_NA value over modelled data
        #mutate(modDataSel = ifelse(is.na(gini_mkt), NA, value)) %>% 
        mutate(ratioFirst = first(na.omit(valueOrg / value))) %>% 
        mutate(ratioLast = last(na.omit(valueOrg / value))) %>% 
        #mutate(nrow = row_number()) %>% 
        # identify where first and last NAs are located
        mutate(valueTEMP = ifelse(is.na(valueOrg), 0, valueOrg )) %>% 
        mutate(isLeadingNA = cumsum(valueTEMP) == 0,
               isTrailingNA = rev(cumsum(rev(valueTEMP))) ==0 ) %>% 
        
        # mutate(#isTrailingNA_1 <- is.na(lead(valueOrg)) & is.na(valueOrg) & lag(is.na(lead(valueOrg)) & is.na(valueOrg)),
        #        isLeadingNA = is.na(lag(valueOrg, n=1:nrow)) & is.na(valueOrg),
        #        isTrailingNA = is.na(lead(valueOrg)) & is.na(valueOrg) & !isLeadingNA) %>%
        # fill NA values with the scaled value of the target data, based on the trend in modelled data
        mutate(valueOrg_filled = valueOrg, 
               valueOrg_filled = case_when(
                 isLeadingNA ~ value * ratioFirst,
                 isTrailingNA ~ value * ratioLast,
                 TRUE ~ valueOrg_filled
               )) %>% 
        select(-c(ratioFirst, ratioLast, valueTEMP, isLeadingNA, isTrailingNA))
      
      
      
      # collect to new data frame
      if (exists('collectFilledData_less5')) {
        collectFilledData_less5 <- collectFilledData_less5 %>% 
          bind_rows(filledData)
      } else {
        collectFilledData_less5 <- filledData
      }
      
    }
    collectFilledData_less5Wide <- collectFilledData_less5 %>% 
      select(iso3, year, valueOrg_filled) %>% 
      pivot_wider(names_from = 'year', values_from = 'valueOrg_filled')
  }
  
  
  
  #### 5. then extrapolate the other countries, using the full and extrapolated nearlyFull data ----
  
  if (exists('collectFilledData_rest')) {
    remove('collectFilledData_rest') # remove if exist
  } else {
    # do nothing
  }
  
  
  for (i in 1:nrow(dataNotFull)) { # 
    
    # extrapolate a country in question
    tempExtrapolCntry <- f_extrapol(dataAll = indicWider, 
                                    dataFull = dataFull_NearlyFull, 
                                    dataToBeExtrap = dataNotFull, 
                                    iCntry = i)
    
    # collect to new data frame
    if (exists('collectFilledData_rest')) {
      collectFilledData_rest <- collectFilledData_rest %>% 
        bind_rows(tempExtrapolCntry)
    } else {
      collectFilledData_rest <- tempExtrapolCntry
    }
    
  }
  collectFilledData_restWide <- collectFilledData_rest %>% 
    select(iso3, year, valueOrg_filled) %>% 
    pivot_wider(names_from = 'year', values_from = 'valueOrg_filled')
  
  
  
  # put all together
  dataFull_NearlyFull_Rest_Less5 <- dataFull_NearlyFull %>% 
    bind_rows(collectFilledData_restWide) %>% 
    bind_rows(collectFilledData_less5Wide) %>% 
    set_names('iso3', paste0(years_in)) %>% 
    pivot_longer(-iso3, names_to = 'year', values_to = nameIndic)
  
  return(dataFull_NearlyFull_Rest_Less5)
}
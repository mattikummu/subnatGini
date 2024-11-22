
f_Mkt2Disp <- function(dataFull, iCntry) {
  # iCntry = 1
  #dataFull = swiidDataFilled_mktExt
  # example of regression in dplyr
  # https://stackoverflow.com/questions/22713325/fitting-several-regression-models-with-dplyr
  
  temp_iso3 <- unique(dataFull$iso3)[iCntry]
  
  lm_DispMkt_sel <- lm_DispMkt %>% 
    filter(iso3 == temp_iso3)
  
  modelledData <- predict(lm_DispMkt_sel$mod[[1]], dataFull %>% filter(iso3 == temp_iso3)) %>%
    as_tibble() #%>% 
  #rename(gini_dispModelled = value)
  
  filledData <- dataFull %>%
    filter(iso3 == temp_iso3) %>% 
    bind_cols(modelledData) %>% 
    mutate(ratioFirst = first(na.omit(gini_disp / value))) %>% 
    mutate(ratioLast = last(na.omit(gini_disp / value))) %>% 
    # identify where first and last NAs are located
    mutate(valueTEMP = ifelse(is.na(gini_disp), 0, gini_disp )) %>% 
    mutate(isLeadingNA = cumsum(valueTEMP) == 0,
           isTrailingNA = rev(cumsum(rev(valueTEMP))) ==0 ) %>% 
    # fill NA values with the scaled value of the target data, based on the trend in modelled data
    mutate(gini_disp_filled = gini_disp, 
           gini_disp_filled = case_when(
             isLeadingNA ~ value * ratioFirst,
             isTrailingNA ~ value * ratioLast,
             TRUE ~ gini_disp_filled
           )) %>% 
    select(-c(ratioFirst, ratioLast, isLeadingNA, isTrailingNA))
  
  return(filledData)
  
}
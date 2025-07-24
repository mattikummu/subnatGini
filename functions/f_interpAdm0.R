f_interpAdm0 <- function(HDIadm0Comb = adm0_comb_filled, nameIndicator = 'lifexp') {
  
  temp_indic <- HDIadm0Comb %>% 
    select(iso3, year, !!nameIndicator) %>% 
    rename(value := !!nameIndicator) %>% 
    # remove countries without data
    #filter(!iso3 == temp_indicNoData$iso3) %>% 
    # interpolate
    group_by(iso3) %>% 
    #https://stackoverflow.com/questions/70155104/interpolate-na-values-when-column-ends-on-na
    mutate(value = na.approx(value, rule = 1, na.rm=F)) %>% 
    ungroup() %>%
    rename(!!nameIndicator := value)# %>% 
  # pivot_wider( names_from = 'year', values_from = 'value')
  
}
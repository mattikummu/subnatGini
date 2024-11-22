

#library(raster)
library(terra)
library(sf)
library(dplyr)
library(tmap)
library(tidyverse)
library(scico)
library(rnaturalearth)
library(rmapshaper)
library(openxlsx)
library(tidyterra)
library(ggplot2)
library(rcartocolor)


library(exactextractr)

# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



### 1. load data ----

cntryID <- read_csv("data_in/countries_codes_and_coordinates.csv") %>% 
  dplyr::select(-cntry_code) %>% 
  rename(cntry_code = GADM_code) %>% # use GADM code instead of UN code
  #select(cntry_code,iso2,iso3,Country) %>% 
  mutate(iso2 = ifelse(Country == 'Namibia','NB',iso2)) %>% 
  distinct(iso3, .keep_all = T)

sf_gini_disp <- read_sf('results/vect_gini_disp_1990_2021.gpkg') %>% 
  mutate(slopeGiniDisp = 32*slope) %>% 
  rename(giniDisp2021 = '2021') %>% 
  rename(giniDisp1990 = '1990') %>% 
  filter(!iso3 == 'ATA')

sf_gini_mkt <- read_sf('results/vect_gini_mkt_1990_2021.gpkg')%>% 
  mutate(slopeGiniMkt = 32*slope) %>% 
  rename(giniMkt2021 = '2021')  %>% 
  filter(!iso3 == 'ATA')


sf_gini_disp_adm0 <- read_sf('results/vect_adm0_gini_disp_1990_2021.gpkg') %>% 
  mutate(slopeGiniDisp = 32*slope) %>% 
  rename(giniDisp2021 = '2021')  %>% 
  rename(giniDisp1990 = '1990') %>% 
  filter(!iso3 == 'ATA')


sf_adm0 <- read_sf("/Users/mkummu/R/GIS_data_common/ne_50m_adm0_all_ids/adm0_NatEarth_all_ids.shp") %>% 
  # simplify the shapefile
  rmapshaper::ms_simplify(keep = 0.05, keep_shapes = T) %>%
  st_as_sf()  %>% 
  filter(!iso_a3 == 'ATA')


### 2. map functions ----

source('functions/f_Plot_sf_abs.R')

source('functions/f_Plot_sf_trend.R')


#### 3. plot maps ----

minGini <- quantile( sf_gini_disp$giniDisp2021, .05, na.rm=T)
maxGini <- quantile( sf_gini_disp$giniDisp2021, .95, na.rm=T) 

giniRange <- seq( plyr::round_any( minGini,accuracy=0.05,f=floor ), 
                  plyr::round_any( maxGini,accuracy=0.05,f=ceiling ) ,
                  by= 0.01) 


minSlope <- quantile(sf_gini_disp$slopeGiniDisp, .05, na.rm = T)
maxSlope <- quantile(sf_gini_disp$slopeGiniDisp, .95, na.rm = T)

slopeRange <- seq( plyr::round_any( minSlope,accuracy=0.01,f=floor ), 
                   plyr::round_any( maxSlope,accuracy=0.01,f=ceiling ) ,
                   by= 0.01) 

scico_palette_names()

giniPal <- scico(9, begin = 0.1, end = 0.9,direction = -1, palette = "glasgow")

p_giniDisp <- f_Plot_sf_abs(sf_gini_disp,'giniDisp2021',giniRange, colPal = giniPal )
p_giniMkt <- f_Plot_sf_abs(sf_gini_mkt,'giniMkt2021',giniRange, colPal = giniPal )

slopePal <- scico(9, begin = 0.1, end = 0.9,direction = 1, palette = "vik")

p_giniDispSlope <- f_Plot_sf_trend(sf_gini_disp,'slopeGiniDisp',slopeRange, pal = slopePal)
p_giniMktSlope <- f_Plot_sf_trend(sf_gini_mkt,'slopeGiniMkt',slopeRange, pal = slopePal)


p_gini <- tmap_arrange(p_giniMkt, p_giniMktSlope,
                       p_giniDisp, p_giniDispSlope,
                       
                       ncol = 2)

tmap_save(p_gini,filename = paste0('figures/fig1_gini2021_slope_',Sys.Date(),'.pdf'),width = 180, height=120, units='mm')


##### 3.1 adm 1 and adm 0 level maps ----



minGini <- quantile( sf_gini_disp$giniDisp2021, .05, na.rm=T)
maxGini <- quantile( sf_gini_disp$giniDisp2021, .95, na.rm=T) 

giniRange <- seq( plyr::round_any( minGini,accuracy=0.05,f=floor ), 
                  plyr::round_any( maxGini,accuracy=0.05,f=ceiling ) ,
                  by= 0.01) 


# minSlope <- quantile(sf_gini_disp$slopeGiniDisp, .05, na.rm = T)
# maxSlope <- quantile(sf_gini_disp$slopeGiniDisp, .95, na.rm = T)
# 
# slopeRange <- seq( plyr::round_any( minSlope,accuracy=0.01,f=floor ), 
#                    plyr::round_any( maxSlope,accuracy=0.01,f=ceiling ) ,
#                    by= 0.01) 


slopeRange <- seq(-0.10, 0.10, .0125)


scico_palette_names()

giniPal <- scico(9, begin = 0.1, end = 0.9,direction = -1, palette = "glasgow")

p_giniDisp_adm1_2021 <- f_Plot_sf_abs(sf_gini_disp,'giniDisp2021',giniRange, colPal = giniPal )
p_giniDisp_adm0_2021 <- f_Plot_sf_abs(sf_gini_disp_adm0,'giniDisp2021',giniRange, colPal = giniPal )

p_giniDisp_adm1_1990 <- f_Plot_sf_abs(sf_gini_disp,'giniDisp1990',giniRange, colPal = giniPal )
p_giniDisp_adm0_1990 <- f_Plot_sf_abs(sf_gini_disp_adm0,'giniDisp1990',giniRange, colPal = giniPal )

slopePal <-  scico::scico(9, begin = 0.1, end = 0.9,direction = 1, palette = "vik")

p_giniDispSlope_adm1 <- f_Plot_sf_trend(sf_gini_disp,'slopeGiniDisp',slopeRange, slopePal)
p_giniDispSlope_adm0 <- f_Plot_sf_trend(sf_gini_disp_adm0,'slopeGiniDisp',slopeRange, slopePal)




if (dir.exists('figures/figSuppl/')) {
  
} else {
  dir.create('figures/figSuppl/')  
}

layers <- list(p_giniDisp_adm1_2021, p_giniDisp_adm0_2021, 
               p_giniDisp_adm1_1990, p_giniDisp_adm0_1990,
               p_giniDispSlope_adm1, p_giniDispSlope_adm0)

nameLayers <- c('p_giniDisp_adm1_2021', 'p_giniDisp_adm0_2021', 
                'p_giniDisp_adm1_1990', 'p_giniDisp_adm0_1990',
                'p_giniDispSlope_adm1', 'p_giniDispSlope_adm0')

for (i in 1:length(layers)) {
  
  p_fig <- layers[[i]] + 
    tm_layout(legend.show=FALSE)
  
  tmap_save(p_fig,filename = paste0('figures/figSuppl/fig_',nameLayers[i],'.png'),width = 80, units='mm', dpi = 450)
  
}




p_gini_suppl <- tmap_arrange(p_giniDisp_adm0_1990, p_giniDisp_adm1_1990,
                             p_giniDisp_adm0_2021, p_giniDisp_adm1_2021,
                             p_giniDispSlope_adm0, p_giniDispSlope_adm1,
                             ncol = 2)

tmap_save(p_gini_suppl,filename = paste0('figures/fig1_gini_suppl_1990_2021_slope','.pdf'),
          width = 180, height=180, units='mm')




#### 3.2 plot percentage of slope -----


sf_gnic <- read_sf('../hdi_subnat/results/vect_gnic_1990_2021.gpkg') %>% 
  mutate(slope = slope*32) %>% 
  rename(log10_1990 = '1990') %>% 
  mutate(log10_1990 = log10(log10_1990)) %>% 
  mutate(perChange = slope / log10_1990) %>% 
  filter(!iso3 == 'ATA')

sf_gini <- read_sf('results/vect_gini_disp_1990_2021.gpkg') %>% 
  mutate(slopeGiniDisp = 32*slope) %>% 
  rename(giniDisp1990 = '1990') %>% 
  mutate(perChange = slopeGiniDisp / giniDisp1990) %>% 
  filter(!iso3 == 'ATA') %>% 
  select(admID, iso3, slope, giniDisp1990, slopeGiniDisp, perChange)

# minSlope <- quantile(sf_gnic$perChange, .025, na.rm = T)
# maxSlope <- quantile(sf_gnic$perChange, .975, na.rm = T)

# slopeRangeGnic <- seq( plyr::round_any( minSlope,accuracy=0.01,f=floor ), 
#                        plyr::round_any( maxSlope,accuracy=0.01,f=ceiling ) ,
#                        by= 0.02) 

slopeRangeGnic <- seq(-0.20, 0.20, .025)

palGnic <-  scico(9, begin = 0.1, end = 0.9,direction = -1, palette = "vik")

p_gnicSlope <- f_Plot_sf_trend(sf_gnic,'perChange',slopeRangeGnic, palGnic)

p_fig <- p_gnicSlope + 
  tm_layout(legend.show=FALSE)

tmap_save(p_fig,filename = paste0('figures/figSuppl/fig_','gnicPercSlope','.png'),width = 80, units='mm', dpi = 450)




minSlope <- quantile(sf_gini$perChange, .025, na.rm = T)
maxSlope <- quantile(sf_gini$perChange, .975, na.rm = T)

slopeRangeGini <- seq( plyr::round_any( minSlope,accuracy=0.01,f=floor ),
                       plyr::round_any( maxSlope,accuracy=0.01,f=ceiling ) ,
                       by= 0.04)

palGini <-  scico(9, begin = 0.1, end = 0.9,direction = 1, palette = "vik")


p_giniDispSlope <- f_Plot_sf_trend(sf_gini,'perChange',slopeRangeGini, palGini)

p_perChange <- tmap_arrange(p_giniDispSlope, p_gnicSlope, 
                            ncol = 2)


tmap_save(p_perChange,filename = paste0('figures/fig_gini_gnic_percChange',Sys.Date(),'.pdf'),width = 180, height=120, units='mm')




### 4. plot time series ----

region_names <- cntryID %>% 
  select(RegionID, RegName) %>% 
  distinct() %>% 
  arrange(RegionID)

df_gini_disp <- sf_gini_disp %>% 
  st_drop_geometry() %>% 
  left_join(cntryID[,c(2,5)]) %>% 
  rename('1990' = giniDisp1990) %>% rename('2021' = giniDisp2021)  %>% 
  select(iso3,admID,RegionID, '1990':'2021') %>% 
  pivot_longer(-c(iso3,admID, RegionID), values_to = 'giniDisp', names_to = 'year') %>% 
  mutate(year = as.numeric(year)) %>% 
  left_join(region_names) %>% 
  mutate(RegionID = as.character(RegionID)) 


# 4.1 average for each region

r_popCount <- rast('data_gis/r_pop_GHS_1990_2022_5arcmin.tif')
r_popCount_mod <- subset(r_popCount, 1:32)



ext_pop <- exact_extract(x= r_popCount_mod,
                         y=sf_gini_disp,
                         fun='sum') 

df_gini_disp_wide <- sf_gini_disp %>% 
  st_drop_geometry() %>% 
  left_join(cntryID[,c(2,5)]) %>% 
  rename('1990' = giniDisp1990) %>% rename('2021' = giniDisp2021)  %>% 
  select(iso3,admID,RegionID, '1990':'2021') %>% 
  set_names(c('iso3',  'admID', 'RegionID',paste0('gini',1990:2021))) %>% 
  mutate(RegionID = as.character(RegionID)) 

df_ext_pop_X_gini <- as.matrix(ext_pop) * as.matrix(df_gini_disp_wide %>% select(gini1990:gini2021)) %>% 
  as_tibble()  

names(df_ext_pop_X_gini) <- paste0('popGini',1990:2021)

regional_popXgini <- df_ext_pop_X_gini %>% 
  bind_cols(df_gini_disp_wide %>% select('iso3',  'admID', 'RegionID')) %>% 
  mutate(RegionID = as.numeric(RegionID)) %>% 
  group_by(RegionID) %>% 
  
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) 

df_ext_pop <- as.matrix(ext_pop) %>% 
  as_tibble()
names(df_ext_pop) <- paste0('pop',1990:2021)

regional_pop <-  df_ext_pop%>% 
  as_tibble() %>% 
  bind_cols(df_gini_disp_wide %>% select('iso3',  'admID', 'RegionID')) %>% 
  mutate(RegionID = as.numeric(RegionID)) %>% 
  group_by(RegionID) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) 

regional_gini <- regional_popXgini %>% select(popGini1990:popGini2021) / 
  regional_pop%>% select(pop1990:pop2021) %>% 
  as_tibble() 

names(regional_gini) <- paste0(1990:2021)



long_regional_gini <- regional_gini %>% 
  mutate(RegionID = row_number()) %>% 
  pivot_longer(-RegionID, values_to = 'gini', names_to = 'year') %>% 
  left_join(region_names) %>% 
  mutate(RegionID = as.character(RegionID)) %>% 
  mutate(year = as.numeric(year)) 




safe_pal <- carto_pal(12, "Safe")

# library(rcartocolor)
# my_colors1 = carto_pal(7, "Safe")
# scales::show_col(my_colors1)
# 
# my_colors2 = carto_pal(17, "Burg")
# my_colors2
# 
# my_colors3 = carto_pal(5, "Vivid")
# scales::show_col(my_colors3)
# display_carto_all(7, type = "qualitative")

p_test <-   ggplot() +
  geom_line(data = df_gini_disp, aes(x = year, y = giniDisp, group = admID, colour = RegName),
            linewidth = 0.1, alpha = 0.3) +
  geom_line(data = long_regional_gini, aes(x = year, y = gini, colour = RegName),
            linewidth = 2, alpha = 0.7) +
  facet_wrap(~ RegName, ncol = 4) +
  scale_color_manual(values = safe_pal)+
  ylim(0.1,0.8) +
  theme_classic() +
  theme( axis.text = element_text( size = 8 ),
         axis.text.x = element_text( size = 8 ),
         axis.title = element_text( size = 8, face = "bold" ),
         legend.position="none",
         # The new stuff
         strip.text = element_text(size = 8),
         axis.line = element_line(colour = 'black', size = 0.25),
         axis.ticks = element_line(colour = "black", size = 0.25))

p_test


ggsave(filename = 'figures/fig_gini_reg_timeseries.pdf',p_test, width = 180, height= 135, units = 'mm')



## 4.2 plot regional map



# dissolve regions
#v_adm0_reg <- vect("/Users/mkummu/R/GIS_data_common/ne_50m_adm0_all_ids/adm0_NatEarth_all_ids.shp") %>%  
v_adm0_reg <- vect(sf_adm0) %>%  
  rename(iso3 = iso_a3) %>% 
  left_join(cntryID) %>% 
  select(RegionID, RegName) %>% 
  drop_na() %>% 
  aggregate(by = 'RegionID') %>% 
  project("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m")


sf_adm0_reg <- v_adm0_reg %>% 
  st_as_sf()

safe_pal <- carto_pal(12, "Safe")


p_map_regions <- tm_shape(sf_adm0_reg, projection = "+proj=robin") +
  tm_fill(col = 'RegName',
          palette = safe_pal,
          alpha = 0.7,
          #contrast = c(0, 0.7),
          #breaks = breaks_in,
          #lwd=0.0,
          legend.is.portrait = FALSE)+
  tm_shape(sf_adm0, projection = "+proj=robin") +
  tm_borders(col = "black",
             lwd = 0.1)+
  tm_layout(#main.title = "Origin of data",
    main.title.position = "center",
    legend.outside = TRUE,
    legend.outside.position = "bottom",
    legend.text.size = .25,
    legend.title.size = .75,
    legend.width = 0.6,
    frame = FALSE)


p_map_regions

tmap_save(p_map_regions, "figures/fig_map_regions.pdf")


#### 5. plot variability -----

region_names <- cntryID %>% 
  select(RegionID, RegName) %>% 
  distinct() %>% 
  arrange(RegionID)

df_gini_disp <- sf_gini_disp %>% 
  st_drop_geometry() %>% 
  left_join(cntryID[,c(2,5)]) %>% 
  rename('1990' = giniDisp1990) %>% rename('2021' = giniDisp2021)  %>% 
  select(iso3,admID,RegionID, '1990':'2021') %>% 
  pivot_longer(-c(iso3,admID, RegionID), values_to = 'giniDisp', names_to = 'year') %>% 
  mutate(year = as.numeric(year)) %>% 
  left_join(region_names) %>% 
  mutate(RegionID = as.character(RegionID)) 


## 5.1 calculate CV for each nation and region

CVadm0 <- df_gini_disp %>% 
  filter(year == 2021) %>% 
  summarise(meanGini = mean(giniDisp, na.rm =T),
            sdGini = sd(giniDisp,  na.rm =T),
            #cvGini = cv(giniDisp),
            .by = 'iso3') %>% 
  mutate(CVgini = sdGini / meanGini)


CVreg <- df_gini_disp  %>% 
  filter(year == 2021) %>% 
  summarise(meanGini = mean(giniDisp, na.rm =T),
            sdGini = sd(giniDisp,  na.rm =T),
            #cvGini = cv(giniDisp),
            .by = 'RegName') %>% 
  mutate(CVgini = sdGini / meanGini) 

CVreg %>% select(RegName, CVgini)


## for GNI per capita

# df_gnic_adm1 <- st_read('../hdi_subnat/results/polyg_gnic_adm0_1990_2021.gpkg') %>% 
#   st_drop_geometry() %>% 
#   select(iso3, GID_nmbr, X2021)  %>% 
#   as_tibble() 
# 
# CVadm0_gnic <- df_gnic_adm1 %>% 
#   summarise(meanGnic = mean(X2021, na.rm =T),
#             sdGnic = sd(X2021,  na.rm =T),
#             #cvGini = cv(giniDisp),
#             .by = 'iso3') %>% 
#   mutate(CVgnic = sdGnic / meanGnic)


df_gnic_adm1 <- st_read('../hdi_subnat/results/vect_gnic_1990_2021.gpkg') %>% 
  st_drop_geometry() %>% 
  select(iso3, admID, X2021)  %>% 
  as_tibble() 

CVadm0_gnicAdm1 <- df_gnic_adm1 %>% 
  summarise(meanGnic = mean(X2021, na.rm =T),
            sdGnic = sd(X2021,  na.rm =T),
            #cvGini = cv(giniDisp),
            .by = 'iso3') %>% 
  mutate(CVgnicAdm1 = sdGnic / meanGnic)



## 5.3 plot

sf_gini_disp_adm0_cv <- sf_gini_disp_adm0 %>% 
  select(iso3) %>% 
  left_join(CVadm0 %>% select(iso3, CVgini)) %>% 
  left_join(CVadm0_gnicAdm1 %>% select(iso3, CVgnicAdm1))


sf_adm0 <- read_sf("/Users/mkummu/R/GIS_data_common/ne_50m_adm0_all_ids/adm0_NatEarth_all_ids.shp") %>% 
  # simplify the shapefile
  rmapshaper::ms_simplify(keep = 0.25, keep_shapes = T) %>%
  st_as_sf()  %>% 
  filter(!iso_a3 == 'ATA')

colPalettes_all <- scico::scico_palette_names(categorical = F)

colPalettes <- scico::scico_palette_names(categorical = T) [-c(3,4,11)]
colPalettes[3] = "broc"

source('functions/f_Plot_sf_abs_v2.R')


p_fig_gini <- f_Plot_sf_abs_v2(sf_in = sf_gini_disp_adm0_cv,column_in = 'CVgini', plt = colPalettes[12], 
                           dirPal = -1 )

p_fig_gnicAdm1 <- f_Plot_sf_abs_v2(sf_in = sf_gini_disp_adm0_cv,column_in = 'CVgnicAdm1', plt = colPalettes[12], 
                               dirPal = -1 )

p_fig_gini_noLegend <- p_fig_gini + 
  tm_layout(legend.show=FALSE)

p_fig_gnicAdm1_noLegend <- p_fig_gnicAdm1 + 
  tm_layout(legend.show=FALSE)

tmap_save(p_fig_gini_noLegend,filename = paste0('figures/fig_','cvGini','.png'),width = 110, units='mm', dpi = 450)

tmap_save(p_fig_gnicAdm1_noLegend,filename = paste0('figures/fig_','cvGnicAdm1','.png'),width = 110, units='mm', dpi = 450)

tmap_save(p_fig_gini,filename = paste0('figures/fig_','cvGini','.pdf'),width = 110, height=50, units='mm')

tmap_save(p_fig_gnicAdm1,filename = paste0('figures/fig_','cvGnic','.pdf'),width = 110, height=50, units='mm')



### 6. plot bounding boxes -----

source('functions/f_Plot_sfAbs_bbox.R')

source('functions/f_Plot_sfTrend_bbox.R')


sf_adm0 <- read_sf("/Users/mkummu/R/GIS_data_common/ne_50m_adm0_all_ids/adm0_NatEarth_all_ids.shp") %>% 
  # simplify the shapefile
  rmapshaper::ms_simplify(keep = 0.5, keep_shapes = T) %>%
  st_as_sf()  %>% 
  filter(!iso_a3 == 'ATA')

# bounding boxes
bbox_india <- st_bbox(sf_adm0 %>% filter(iso_a3 == "IND"))
bbox_brazil <- st_bbox(sf_adm0 %>% filter(iso_a3 == "BRA"))
bbox_centralEur <- st_bbox(sf_adm0 %>% filter(iso_a3 %in% c("DEU", "ITA", "POL", "AND") ))
bbox_chn_india <- st_bbox(sf_adm0 %>% filter(iso_a3 %in% c("CHN", "IND")))


# colour palettes
giniPal <- scico(9, begin = 0.1, end = 0.9,direction = -1, palette = "glasgow")
slopePal <-  scico::scico(9, begin = 0.1, end = 0.9,direction = 1, palette = "vik")


# 
# p_gini_adm0_IND <- f_Plot_sfAbs_bbox(sf_gini_disp_adm0,'giniDisp2021',giniRange, bbox_india , colPal = giniPal)
# p_gini_adm1_IND <- f_Plot_sfAbs_bbox(sf_gini_disp,'giniDisp2021',giniRange, bbox_india, colPal = giniPal )

# gini

p_gini_adm0_BRA <- f_Plot_sfAbs_bbox(sf_gini_disp_adm0,'giniDisp2021',giniRange, bbox_brazil, colPal = giniPal )
p_gini_adm1_BRA <- f_Plot_sfAbs_bbox(sf_gini_disp,'giniDisp2021',giniRange, bbox_brazil, colPal = giniPal )

p_gini_adm0_CE <- f_Plot_sfAbs_bbox(sf_gini_disp_adm0,'giniDisp2021',giniRange, bbox_centralEur, colPal = giniPal )
p_gini_adm1_CE <- f_Plot_sfAbs_bbox(sf_gini_disp,'giniDisp2021',giniRange, bbox_centralEur, colPal = giniPal)

p_gini_adm0_CHN_IND <- f_Plot_sfAbs_bbox(sf_gini_disp_adm0,'giniDisp2021',giniRange, bbox_chn_india, colPal = giniPal )
p_gini_adm1_CHN_IND <- f_Plot_sfAbs_bbox(sf_gini_disp,'giniDisp2021',giniRange, bbox_chn_india, colPal = giniPal)


# gini slope
# p_giniDispSlope_adm1 <- f_Plot_sf_trend(sf_gini_disp,'slopeGiniDisp',slopeRange, slopePal)
# p_giniDispSlope_adm0 <- f_Plot_sf_trend(sf_gini_disp_adm0,'slopeGiniDisp',slopeRange, slopePal)

p_gini_slope_adm0_BRA <- f_Plot_sfAbs_bbox(sf_gini_disp_adm0,'slopeGiniDisp',slopeRange, bbox_brazil, colPal = slopePal )
p_gini_slope_adm1_BRA <- f_Plot_sfAbs_bbox(sf_gini_disp,'slopeGiniDisp',slopeRange, bbox_brazil, colPal = slopePal )

p_gini_slope_adm0_CE <- f_Plot_sfAbs_bbox(sf_gini_disp_adm0,'slopeGiniDisp',slopeRange, bbox_centralEur, colPal = slopePal )
p_gini_slope_adm1_CE <- f_Plot_sfAbs_bbox(sf_gini_disp,'slopeGiniDisp',slopeRange, bbox_centralEur, colPal = slopePal)

p_gini_slope_adm0_CHN_IND <- f_Plot_sfAbs_bbox(sf_gini_disp_adm0,'slopeGiniDisp',slopeRange, bbox_chn_india, colPal = slopePal )
p_gini_slope_adm1_CHN_IND <- f_Plot_sfAbs_bbox(sf_gini_disp,'slopeGiniDisp',slopeRange, bbox_chn_india, colPal = slopePal)



if (dir.exists('figures/figGiniMaps_bbox/')) {
  
} else {
  dir.create('figures/figGiniMaps_bbox/')  
}

layers <- list(p_gini_adm0_BRA,  p_gini_adm1_BRA, 
               p_gini_adm0_CE,p_gini_adm1_CE, 
               p_gini_adm0_CHN_IND, p_gini_adm1_CHN_IND,
               
               p_gini_slope_adm0_BRA,  p_gini_slope_adm1_BRA, 
               p_gini_slope_adm0_CE,p_gini_slope_adm1_CE, 
               p_gini_slope_adm0_CHN_IND, p_gini_slope_adm1_CHN_IND)

nameLayers <- c('p_gini_adm0_BRA',  'p_gini_adm1_BRA', 
                'p_gini_adm0_CE','p_gini_adm1_CE', 
                'p_gini_adm0_CHN_IND', 'p_gini_adm1_CHN_IND',
                
                'p_gini_slope_adm0_BRA',  'p_gini_slope_adm1_BRA', 
                'p_gini_slope_adm0_CE','p_gini_slope_adm1_CE', 
                'p_gini_slope_adm0_CHN_IND', 'p_gini_slope_adm1_CHN_IND')

for (i in 1:length(layers)) {
  
  p_fig <- layers[[i]] + 
    tm_layout(legend.show=FALSE)
  
  tmap_save(p_fig,filename = paste0('figures/figGiniMaps_bbox/fig_',nameLayers[i],'.png'),width = 100, units='mm', dpi = 600)
  
}







## 7. plot data origin ----

# 7.1 collect data

sf_adm0 <- read_sf('results/vect_adm0_gini_disp_1990_2021.gpkg')


cntry_metaData_gini_upd <- read_csv("results/cntry_metaData_gini_upd.csv")

n_origin <- cntry_metaData_gini_upd %>% 
  drop_na() %>% 
  summarise(nOrig = n(), .by = 'SUBNAT_USED') %>% 
  mutate(label = paste0(SUBNAT_USED, ' (n=',nOrig,')')) %>% 
  arrange(label)

cntry_metaData_gini_upd %>% 
  drop_na() %>% 
  summarise(nTotal = n())

sf_giniDataOrigin <- read_sf('results/vect_gini_disp_1990_2021.gpkg') %>% 
  #st_drop_geometry() %>% 
  select(admID, iso3) 

sf_giniDataOrigin %>% 
  st_drop_geometry() %>% 
  drop_na() %>% 
  filter(admID > 1000) %>% 
  summarise(nAdm0 = n(), .by = iso3)

# 7.2 check how many people live in subnational areas we have data for 

r_popCount <- rast('data_gis/r_pop_GHS_1990_2022_5arcmin.tif')

globPop <- global(subset(r_popCount,32), fun=sum, na.rm=T)

ext_in_x_pop <- exactextractr::exact_extract(x= subset(r_popCount,32),
                                             y= sf_giniDataOrigin %>% filter(admID > 1000), 
                                             fun='sum') %>% 
  as_tibble()

subnatPop <- ext_in_x_pop %>% 
  summarise(totalPop = sum(value))

subnatPop / globPop

### 7.3 range and interval


sf_dataReported_range <- read_sf("results/gisData_gini_combined.gpkg") %>% 
  st_drop_geometry() %>% 
  select(-c(Country, cntry_code, Subnat, GID_1)) %>% 
  pivot_longer(-c('iso3','GID_nmbr'), names_to = 'year', values_to = 'gini') %>% 
  drop_na() %>% 
  group_by(iso3) %>% 
  summarise(minYear = min(year), maxYear=max(year)) %>% 
  ungroup() %>% 
  mutate(rangeYear = as.numeric(maxYear) - as.numeric(minYear) + 1)


sf_dataReported_meanInterval <- read_sf('results/gisData_gini_combined.gpkg') %>% 
  st_drop_geometry() %>% 
  select(-c(Country, cntry_code, Subnat, GID_1)) %>% 
  pivot_longer(-c('iso3','GID_nmbr'), names_to = 'year', values_to = 'gini') %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(!is.na(gini)) %>%  # Filter rows where GDP is not NA
  # filter(GID_nmbr == 1004001) %>% 
  group_by(GID_nmbr) %>% 
  arrange(GID_nmbr, year) %>%   # Ensure data is sorted by year
  mutate(interval = year - lag(year)) %>%  # Calculate the interval
  select(iso3,GID_nmbr, year, interval) %>% # %>%  # Select the relevant columns
  reframe(intervalMean = mean(interval, na.rm = T), iso3 = iso3) %>% 
  ungroup() %>% 
  group_by(iso3) %>% 
  reframe(intervalMean_iso3 = mean(intervalMean, na.rm = T) ) %>% 
  ungroup()

summary(sf_dataReported_meanInterval$intervalMean_iso3)




### 7.4 plot

sf_adm0_giniDataOrigin <- read_sf('results/vect_adm0_gini_disp_1990_2021.gpkg') %>% 
  #st_drop_geometry() %>% 
  select(admID, iso3) %>% 
  left_join(cntry_metaData_gini_upd) %>% 
  #left_join(sf_dataReported) %>% 
  left_join(sf_dataReported_range %>% distinct(iso3, .keep_all = T))  %>% 
  left_join(sf_dataReported_meanInterval %>% distinct(iso3, .keep_all = T)) %>% 
  #mutate(intervalMean_iso3 = ifelse(is.nan(intervalMean_iso3)&rangeYear>0, 1, intervalMean_iso3))
  filter(!iso3 == 'AIA')

safe_pal <- rcartocolor::carto_pal(10, "Safe")

labelDataOrg <- n_origin$label

plt_giniDataOrigin <- tm_shape(sf_adm0_giniDataOrigin, projection = "+proj=robin") +
  tm_polygons(col = "SUBNAT_USED",
              palette = safe_pal,
              label = labelDataOrg,
              #labels = birthDataOrigin,
              colorNA = 'white',
              contrast = c(0.2, 0.7),
              lwd = 0.1)+
  tm_shape(sf_giniDataOrigin, projection = "+proj=robin") +
  tm_borders(col = "grey",
             lwd = 0.05)+
  tm_layout(#main.title = "Origin of data",
    main.title.position = "center",
    legend.outside = TRUE,
    legend.outside.position = "right",
    frame = FALSE)


pal = scico(8, begin = 0.1, end = 0.9, direction = -1, palette = 'lapaz')
breaksNmbr = c(0,2,5,10,15,20,25,30)
breaksInterval = c(0,1.1,2,3,4,5,6,Inf)

plt_giniDataNmbrYears <- tm_shape(sf_adm0_giniDataOrigin, projection = "+proj=robin") +
  tm_polygons(col = "nmbrObs",
              palette = pal,
              breaks = breaksNmbr,
              colorNA = 'white',
              #contrast = c(0.2, 0.7),
              lwd = 0.1
  )+
  tm_shape(sf_giniDataOrigin, projection = "+proj=robin") +
  tm_borders(col = "grey",
             lwd = 0.05)+
  tm_layout(#main.title = "Origin of data",
    main.title.position = "center",
    legend.outside = TRUE,
    legend.outside.position = "right",
    frame = FALSE)



plt_giniDataRangeYears <- tm_shape(sf_adm0_giniDataOrigin, projection = "+proj=robin") +
  tm_fill(col = "rangeYear",
          palette = pal,
          breaks = breaksNmbr,
          colorNA = 'white',
          #labels = birthDataOrigin,
          #contrast = c(0, 0.9)
  )+
  tm_shape(sf_giniDataOrigin, projection = "+proj=robin") +
  tm_borders(col = "grey",
             lwd = 0.1)+
  tm_layout(#main.title = "Origin of data",
    main.title.position = "center",
    legend.outside = TRUE,
    legend.outside.position = "right",
    frame = FALSE)

plt_giniDataIntervalYears <- tm_shape(sf_adm0_giniDataOrigin, projection = "+proj=robin") +
  tm_fill(col = "intervalMean_iso3",
          palette = pal,
          breaks = breaksInterval,
          colorNA = 'white',
          #labels = birthDataOrigin,
          #contrast = c(0, 0.9)
  )+
  tm_shape(sf_giniDataOrigin, projection = "+proj=robin") +
  tm_borders(col = "grey",
             lwd = 0.1)+
  tm_layout(#main.title = "Origin of data",
    main.title.position = "center",
    legend.outside = TRUE,
    legend.outside.position = "right",
    frame = FALSE)





plt_dataOrigin <- tmap_arrange(plt_giniDataOrigin, plt_giniDataNmbrYears, 
                               plt_giniDataRangeYears, plt_giniDataIntervalYears,
                               ncol = 1 )

plt_giniDataOrigin_noLegend <- plt_giniDataOrigin + 
  tm_layout(legend.show=FALSE)

plt_giniDataNmbrYears_noLegend <- plt_giniDataNmbrYears + 
  tm_layout(legend.show=FALSE)

plt_giniDataRangeYears_noLegend <- plt_giniDataRangeYears + 
  tm_layout(legend.show=FALSE)

plt_giniDataIntervalYears_noLegend <- plt_giniDataIntervalYears + 
  tm_layout(legend.show=FALSE)

tmap_save(plt_giniDataOrigin_noLegend,filename = paste0('figures/fig_','plt_giniDataOrigin_noLegend','.png'),
          width = 110, units='mm', dpi = 450)

tmap_save(plt_giniDataNmbrYears_noLegend,filename = paste0('figures/fig_','plt_giniDataNmbrYears_noLegend','.png'),
          width = 110, units='mm', dpi = 450)

tmap_save(plt_giniDataRangeYears_noLegend,filename = paste0('figures/fig_','plt_giniDataRangeYears_noLegend','.png'),
          width = 110, units='mm', dpi = 450)

tmap_save(plt_giniDataIntervalYears_noLegend,filename = paste0('figures/fig_','plt_giniDataIntervalYears_noLegend','.png'),
          width = 110, units='mm', dpi = 450)


tmap_save(plt_dataOrigin,filename = "figures/plt_giniDataOrigin.pdf", width = 130, height = 180, units = "mm")



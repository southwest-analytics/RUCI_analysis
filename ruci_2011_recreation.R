library(tidyverse)
library(sf)
library(leaflet)
library(htmlwidgets)
library(conflicted)

# 2011 ----
# ═════════
df_ruc11_oa <- read.csv('C:/Users/richard.blackwell/OneDrive - Health Innovation South West/Data/OpenGeography/Lookups/RUC11/RUC11_OA11.csv') %>% 
  select(1:3) %>% 
  mutate(RUC11NM = RUC11,
         RUC11 = if_else(grepl('^Urban', RUC11NM), 'Urban', 'Rural'))

df_bua11_popn <- read.csv('C:/Users/richard.blackwell/OneDrive - Health Innovation South West/Workspace/RUCI_analysis/data/BUA11_POPN/BUA11_POPN.csv')

sf_bua11 <- st_read(dsn = 'C:/Users/richard.blackwell/OneDrive - Health Innovation South West/Workspace/RUCI_analysis/data/BUA11',
                    layer = 'BUA11') %>%
  st_transform(crs = 4326) %>%
  st_make_valid()

sf_oa11 <- st_read(dsn = 'C:/Users/richard.blackwell/OneDrive - Health Innovation South West/Data/OpenGeography/Shapefiles/OA11',
                   layer = 'OA11') %>%
  st_transform(crs = 4326) %>%
  st_make_valid()

sf_oa11_pwc <- read.csv('C:/Users/richard.blackwell/OneDrive - Health Innovation South West/Data/OpenGeography/Shapefiles/OA11/OA11_PWC.csv') %>% 
  select(-3) %>%
  mutate(EASTING = x, NORTHING = y) %>%
  st_as_sf(coords = c('x', 'y'), dim = 'XY', crs = 27700) %>%
  st_transform(crs = 4326) %>%
  mutate(LONGITUDE = st_coordinates(.)[,1],
         LATITUDE = st_coordinates(.)[,2])

sf_lsoa11 <- st_read(dsn = 'C:/Users/richard.blackwell/OneDrive - Health Innovation South West/Data/OpenGeography/Shapefiles/LSOA11',
                     layer = 'LSOA11') %>%
  st_transform(crs = 4326) %>%
  st_make_valid()

# If an output area PWC is within a BUA of population >= 10000 it is urban
df_tmp <- sf_bua11 %>% 
  semi_join(df_bua11_popn %>% dplyr::filter(POPN_KS101EW >= 10000), by = 'BUA11CD') %>%
  st_join(sf_oa11_pwc, st_contains) %>%
  st_drop_geometry() %>%
  distinct(OA11CD)

df_unmatched_2021 <- df_ruc11_oa %>% dplyr::filter(RUC11=='Urban') %>% anti_join(df_tmp)

# 2021 ----
# ═════════

df_bua21_popn <- read.csv('C:/Users/richard.blackwell/OneDrive - Health Innovation South West/Workspace/RUCI_analysis/data/BUA21_POPN/BUA21_POPN.csv')

sf_bua22 <- st_read(dsn = 'C:/Users/richard.blackwell/OneDrive - Health Innovation South West/Workspace/RUCI_analysis/data/BUA22',
                    layer = 'BUA22') %>%
  st_transform(crs = 4326) %>%
  st_make_valid()

sf_oa21 <- st_read(dsn = 'C:/Users/richard.blackwell/OneDrive - Health Innovation South West/Data/OpenGeography/Shapefiles/OA21',
                   layer = 'OA21') %>%
  st_transform(crs = 4326) %>%
  st_make_valid()

sf_oa21_pwc <- read.csv('C:/Users/richard.blackwell/OneDrive - Health Innovation South West/Data/OpenGeography/Shapefiles/OA21/OA21_PWC.csv') %>% 
  select(c(2,4:5)) %>%
  mutate(EASTING = x, NORTHING = y) %>%
  st_as_sf(coords = c('x', 'y'), dim = 'XY', crs = 27700) %>%
  st_transform(crs = 4326) %>%
  mutate(LONGITUDE = st_coordinates(.)[,1],
         LATITUDE = st_coordinates(.)[,2])

sf_lsoa21 <- st_read(dsn = 'C:/Users/richard.blackwell/OneDrive - Health Innovation South West/Data/OpenGeography/Shapefiles/LSOA21',
                     layer = 'LSOA21') %>%
  st_transform(crs = 4326) %>%
  st_make_valid()

write.csv(sf_bua22 %>% st_drop_geometry() %>% select(BUA22CD, BUA22NM), 'bua22.csv')
write.csv(sf_bua24 %>% st_drop_geometry() %>% select(BUA24CD, BUA24NM), 'bua24.csv')

# If an output area PWC is within a BUA of population >= 10000 it is urban
df_tmp <- sf_bua24 %>% 
  semi_join(df_bua21_popn %>% dplyr::filter(POPN_KS101EW >= 10000), by = 'BUA11CD') %>%
  st_join(sf_oa11_pwc, st_contains) %>%
  st_drop_geometry() %>%
  distinct(OA11CD)

df_unmatched <- df_ruc11_oa %>% dplyr::filter(RUC11=='Urban') %>% anti_join(df_tmp)

map <- leaflet() %>%
  addTiles() %>%
  addPolygons(data = sf_bua22,
              highlightOptions = highlightOptions(fillColor = '#ffff00'),
              popup = ~BUA22CD)
saveWidget(map, file = 'map.html')


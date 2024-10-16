library(tidyverse)
library(sf)
library(leaflet)
library(htmlwidgets)
library(conflicted)

# London BUA codes ----
# ─────────────────────
# london_bua <- c('E63004679' = 342,283, 'E63004747' = 419,797, 'E63004781' = 273,825, 'E63004790' = 318,321, 'E63004793', 
london_bua <- c('E63004679', 'E63004747', 'E63004781', 'E63004790', 'E63004793',
                'E63004796', 'E63004797', 'E63004844', 'E63004850', 'E63004858', 
                'E63004859', 'E63004860', 'E63004881', 'E63004882', 'E63004894', 
                'E63004898', 'E63004906', 'E63004916', 'E63004944', 'E63004950', 
                'E63004965', 'E63004986', 'E63004992', 'E63005014', 'E63005033', 
                'E63005035', 'E63005063', 'E63005073', 'E63005121', 'E63005164', 
                'E63005189', 'E63005250', 'E63005267')

# Health Innovation South West Lower Tier Local Authorities ----
# ──────────────────────────────────────────────────────────────
hisw_lad <- c('Cornwall', 'Isles of Scilly',
              'Exeter', 'Plymouth', 'Torbay',
              'East Devon', 'Mid Devon', 'North Devon', 'West Devon',
              'Teignbridge', 'Torridge', 'South Hams',
              'Sedgemoor', 'Somerset West and Taunton',
              'South Somerset', 'Mendip')

# Load OA21 shapefile ----
# ────────────────────────
sf_oa21 <- st_read(dsn = 'C:/Users/richard.blackwell/OneDrive - Health Innovation South West/Data/OpenGeography/Shapefiles/OA21',
                   layer = 'OA21') %>%
  st_transform(crs = 4326) %>%
  st_make_valid()


# Load LSOA21 shapefile ----
# ──────────────────────────
sf_lsoa21 <- st_read(dsn = 'C:/Users/richard.blackwell/OneDrive - Health Innovation South West/Data/OpenGeography/Shapefiles/LSOA21',
                     layer = 'LSOA21') %>%
  st_transform(crs = 4326) %>%
  st_make_valid()

# Load BUA22 shapefile ----
# ─────────────────────────
sf_bua22 <- st_read(dsn = 'C:/Users/richard.blackwell/OneDrive - Health Innovation South West/Workspace/RUCI_analysis/data/BUA22',
                    layer = 'BUA22') %>%
  st_transform(crs = 4326) %>%
  st_make_valid()

# Load OA21 to BUA22 lookup ----
# ──────────────────────────────
df_oa21_bua22_lu <- read.csv('C:/Users/richard.blackwell/OneDrive - Health Innovation South West/Workspace/RUCI_analysis/data/OA21_BUA22/OA21_BUA22.csv') %>%
  select(-ObjectId)

# Load BUA21 Census Population ----
# ─────────────────────────────────
df_bua21_popn <- read.csv('C:/Users/richard.blackwell/OneDrive - Health Innovation South West/Workspace/RUCI_analysis/data/BUA21_POPN/BUA21_POPN.csv')

# Load COASTAL22 lookup ----
# ──────────────────────────
df_coastal22 <- read.csv('C:/Users/richard.blackwell/OneDrive - Health Innovation South West/Workspace/RUCI_analysis/data/COASTAL22/COASTAL22.csv')

# Load Census 2021 Population Data for Population Estimate of London BUAs ----
# ────────────────────────────────────────────────────────────────────────────
df_london_bua_popn <- read.csv('C:/Users/richard.blackwell/OneDrive - Health Innovation South West/Data/NOMIS/Census_2021/TS/census2021-ts001/census2021-ts001-oa.csv') %>% 
  select(3, 4) %>%
  rename_with(.fn = ~c('OA21CD', 'POPN')) %>%
  left_join(df_oa21_bua22_lu %>% select(OA21CD, BUA22CD, BUA22NM), by = 'OA21CD') %>%
  dplyr::filter(BUA22CD %in% london_bua) %>%
  group_by(BUA22CD, BUA22NM) %>%
  summarise(POPN = sum(POPN), .groups = 'keep') %>%
  ungroup()

df_bua21_popn <- df_bua21_popn %>% 
  bind_rows(df_london_bua_popn %>% mutate(BUA21CD = BUA22CD,
                                          BUA21NM = BUA22NM,
                                          BUA_CLASS = if_else(POPN < 5000, 'Minor',
                                                        if_else(POPN < 20000, 'Small',
                                                          if_else(POPN < 75000, 'Medium',
                                                            if_else(POPN < 200000, 'Large',
                                                              if_else(POPN >= 200000, 'Major', NA))))),
                                          POPN = POPN,
                                          .keep = 'none'))

# Add BUA population to OA21 to BUA22 lookup
df_ruci <- df_oa21_bua22_lu %>% 
  left_join(df_bua21_popn, by = c('BUA22CD' = 'BUA21CD')) %>%
  mutate(RUC = 'Rural') %>%
  mutate(RUC = if_else(!is.na(POPN) & POPN >= 10000, 'Urban', RUC)) %>%
  mutate(OA21CD, BUA22CD, BUA22NM, LAD22CD, LAD22NM, RGN22CD, RGN22NM, BUA_CLASS, BUA_POPN = POPN, RUC, .keep = 'none') %>%
  left_join(df_coastal22 %>% mutate(BUA22CD, TMP = BUA22NM, .keep = 'none'), by = 'BUA22CD') %>%
  mutate(CIC = if_else(is.na(TMP), 'Inland', 'Coastal')) %>%
  select(-TMP)

write.csv(df_ruci, 'ruci.csv')

# Rural - Inland '#52731C'
# Rural - Coastal '#0084CC'
# Urban - Inland '#B99B84'
# Urban - Coastal '#9BBDDB'

  
# sf_bua22 %>% dplyr::filter(BUA22CD %in% london_bua) %>% st_drop_geometry() %>% .$BUA22NM
# 
# sf_map <- sf_bua22 %>% 
#   left_join(df_bua21_popn, by = c('BUA22CD' = 'BUA21CD')) %>%
#   semi_join(df_oa21_bua22_lu %>% 
#               dplyr::filter(LAD22NM %in% hisw_lad) %>% distinct(BUA22CD),
#             by = 'BUA22CD')
# 
# palBUA <- colorFactor(palette = 'Set2', levels = c('Minor','Small','Medium','Large','Major'), na.color = '#cecece')
# 
# map <- leaflet() %>%
#   addTiles(urlTemplate = 'https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png') %>%
#   addPolygons(data = sf_map %>% dplyr::filter(BUA_CLASS == 'Minor'),
#               weight = 1,
#               fillColor = ~palBUA(BUA_CLASS),
#               fillOpacity = .7,
#               popup = ~paste0(BUA21NM, ' - ', BUA_CLASS),
#               group = 'Minor') %>%
#   addPolygons(data = sf_map %>% dplyr::filter(BUA_CLASS == 'Small'),
#               weight = 1,
#               fillColor = ~palBUA(BUA_CLASS),
#               fillOpacity = .7,
#               popup = ~paste0(BUA21NM, ' - ', BUA_CLASS),
#               group = 'Small') %>%
#   addPolygons(data = sf_map %>% dplyr::filter(BUA_CLASS == 'Medium'),
#               weight = 1,
#               fillColor = ~palBUA(BUA_CLASS),
#               fillOpacity = .7,
#               popup = ~paste0(BUA21NM, ' - ', BUA_CLASS),
#               group = 'Medium') %>%
#   addPolygons(data = sf_map %>% dplyr::filter(BUA_CLASS == 'Large'),
#               weight = 1,
#               fillColor = ~palBUA(BUA_CLASS),
#               fillOpacity = .7,
#               popup = ~paste0(BUA21NM, ' - ', BUA_CLASS),
#               group = 'Large') %>% 
#   addPolygons(data = sf_map %>% dplyr::filter(BUA_CLASS == 'Major'),
#               weight = 1,
#               fillColor = ~palBUA(BUA_CLASS),
#               fillOpacity = .7,
#               popup = ~paste0(BUA21NM, ' - ', BUA_CLASS),
#               group = 'Major') %>%
#   addPolygons(data = sf_oa21 %>% left_join(df_ruci, by = 'OA21CD') %>% dplyr::filter(LAD22NM %in% hisw_lad & RUC == 'Rural' & CIC == 'Coastal'),
#               stroke = FALSE,
#               
#               
# )
#   addLayersControl(overlayGroups = c('Minor', 'Small', 'Medium', 'Large', 'Major'))
# saveWidget(map, file = 'map.html')

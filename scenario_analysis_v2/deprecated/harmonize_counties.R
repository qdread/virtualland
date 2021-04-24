# Harmonization of counties from Census Tiger shapefile with counties from county personal income data
# QDR / Virtualland / 19 April 2021

library(sf)
library(tidyverse)

aea_p4s <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs' # Albers equal area
 
county_income <- read_csv('data/raw_data/BEA/countypersonalincome2012.csv', skip = 4, n_max = 3138)
county_map <- st_read(dsn = '/nfs/public-data/census-tiger-2013/cb_2014_us_county_500k', layer = 'cb_2014_us_county_500k') %>%
  filter(!STATEFP %in% c("60", "66", "69", "72", "78")) %>% # Remove Puerto Rico and other overseas dependencies
  mutate(fips = paste0(STATEFP, COUNTYFP)) %>%
  st_zm() %>% # Get rid of Z dimension
  st_transform(aea_p4s) # Transform to equal area projection

sort(setdiff(x = county_map$fips, y = county_income$GeoFips)) # In map but not data
sort(setdiff(y = county_map$fips, x = county_income$GeoFips)) # In data but not map

# Summary of differences:
# Harmonization must be done on the map to dissolve independent cities in VA with less than 100K population with the surrounding county
# Kalawao County and Maui County also need to be dissolved on the map.
# code 55901 in data is for an aggregation of two counties in WI that no longer exists so can be ignored
# 46113 recoded to 46102, 02270 recoded to 02158
# The remaining inconsistencies are with Alaska, but they are all "legacy rows" that are NA in the data so can be ignored too.

fips_harmonization <- read_csv('data/crossreference_tables/fips_harmonization.csv', col_types = 'ccccc')

# For each row of fips_harmonization, dissolve those map polygons into a single one and rename it.
# first reshape the data frame 
fips_harmonization <- fips_harmonization %>% 
  pivot_longer(starts_with("FIPS_map"), values_to = 'FIPS_map') %>%
  select(-name, -name_data) %>%
  filter(!is.na(FIPS_map))

# Join map with harmonization, group by harmonized fips, and dissolve
county_harmonized <- county_map %>%
  left_join(fips_harmonization, by = c('fips' = 'FIPS_map')) %>%
  mutate(FIPS_data = if_else(is.na(FIPS_data), fips, FIPS_data)) %>%
  group_by(FIPS_data) %>%
  summarize %>%
  mutate(fips_state = substr(FIPS_data, 1, 2), fips_county = substr(FIPS_data, 3, 5)) %>%
  rename(fips = FIPS_data)
  

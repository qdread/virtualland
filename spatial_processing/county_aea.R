# Transform county map to AEA and write to GPKG
# QDR / Virtualland
# Last modified 27 Jan 2021

library(sf)
library(tidyverse)
system2('gdalsrsinfo', 'data/raw_data/landuse/ecoregions/tnc_usa_aea.prj')
tnc_p4s <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs'
countymap <- st_read(dsn = '/nfs/public-data/census-tiger-2013/cb_2014_us_county_500k', layer = 'cb_2014_us_county_500k')

county_aea <- st_transform(countymap, tnc_p4s)

# Update the FIPS codes to reflect the mismatches between this map and the data.
# The differences are Washington DC (different code used in the data), Aleutians (two counties split), and Oglala Lakota (fips code changed when name changed)
# For DC and Oglala Lakota we just need to change the code.
# For Aleutians, we need to merge two counties.

# Correct the codes.
county_aea <- county_aea %>%
  mutate(COUNTYFP = case_when(STATEFP == '11' ~ '000',
                              STATEFP == '46' & COUNTYFP == '113' ~ '102',
                              TRUE ~ COUNTYFP),
         county = paste0(STATEFP, COUNTYFP)) 

# Merge the two Aleutians counties 
aleutians <- county_aea %>%
  filter(county %in% c('02013', '02016')) %>%
  st_combine

# Replace one of the Aleutians geometries with the merged one.
county_aea$geometry[county_aea$county == '02013'] <- aleutians

county_aea <- county_aea %>%
  filter(!county %in% '02016') %>%
  mutate(NAME = if_else(NAME == 'Aleutians East', 'Aleutians (East & West merged)', NAME),
         county = if_else(county == '02013', '02010', county))

st_write(county_aea, 'data/raw_data/landuse/USA/USA_county_2014_aea.gpkg', driver = 'GPKG', append = FALSE)

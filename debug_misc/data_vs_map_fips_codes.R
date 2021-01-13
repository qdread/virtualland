# Check whether county fips codes in the shapefile match county fips codes from the data

library(sf)
library(readxl)
countymap <- st_read(dsn = '/nfs/public-data/census-tiger-2013/cb_2014_us_county_500k', layer = 'cb_2014_us_county_500k')

fp_lin <- 'data/raw_data/commodity_flows/Lin_supp_info'
county_income <- read_xlsx(file.path(fp_lin, 'County Personal Income.xlsx'), sheet = 'Total County Income', skip = 5)

# Check matches.
map_fips <- paste0(countymap$STATEFP, countymap$COUNTYFP)
data_fips <- paste0(ifelse(nchar(county_income$State.County.FIPS) == 4, '0', ''), county_income$State.County.FIPS)

setdiff(x = data_fips, y = map_fips)
# All are statewide values except for 51515. This is in the data but not in the map. I guess that's Bedford City, VA.
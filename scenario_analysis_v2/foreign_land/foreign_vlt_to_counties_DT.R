# Allocate virtual land imports to the USA from each country and ecoregion to counties for each scenario
# Do this using the strong assumption that counties receive imports from all foreign countries in equal proportions
# QDR / Virtualland / 18 Feb 2021

# Load and clean data -----------------------------------------------------

library(data.table)
library(readxl)
library(Rutilitybelt)
library(purrr)

# Read foreign VLT imports by ecoregion and income by county used to split up the VLT by recipient county.

foreign_vlt_eco <- fread('data/cfs_io_analysis/foreign_VLT_by_TNC.csv')

county_income <- read_csv('data/raw_data/BEA/countypersonalincome2012.csv', skip = 4, n_max = 3138, col_types = 'ccn', na = '(NA)') 
county_income$`2012`[county_income$GeoFips == '02010'] <- sum(county_income$`2012`[county_income$GeoFips %in% c('02013', '02016')]) #Aleutians correction
county_income <- county_income %>%
  filter(!is.na(`2012`), !GeoFips %in% c('02013', '02016'))
setDT(county_income)

# Normalize the county income 2012 vector
county_income_norm2012 <- setNames(county_income$`2012`/sum(county_income$`2012`), county_income$GeoFips)


# Multiply VLT by normalized income vector --------------------------------

# For each ecoregion and each scenario, multiply the 3142x1 vector times the 1x4 VLT to get a 3142x4 vector of VLT types x county.

# Nest foreign VLT by ecoregion
foreign_vlt_eco[, c('pasture_area', 'crop_area') := NULL]
foreign_vlt_eco <- group_nest_dt(foreign_vlt_eco, scenario_diet, scenario_waste, ECO_CODE, ECO_NAME)

# function to apply to each VLT vector
assign_vlt_to_counties <- function(VLT) {
  VLT <- t(unlist(VLT)) # Convert to a row vector
  VLT_product <- county_income_norm2012 %*% VLT
  cbind(county = county_income$FIPS, as.data.frame(VLT_product))
}

foreign_vlt_eco[, VLT_counties := map(data, assign_vlt_to_counties)]

# Process output and sum up -----------------------------------------------

# Convert the list column to a large data.table
foreign_vlt_eco[, data := NULL]

foreign_vlt_eco_counties <- unnest_dt(foreign_vlt_eco, col = VLT_counties, id = .(scenario_diet, scenario_waste, ECO_CODE, ECO_NAME))

# Calculate the sums by county, across all ecoregions
foreign_vlt_counties <- foreign_vlt_eco_counties[, lapply(.SD, sum, na.rm = TRUE), by = .(scenario_diet, scenario_waste, county), .SDcols = patterns('VLT')]

# NOTE: all foreign VLT are in hectares, later must be converted to m^2 by multiplying by 1e4

# Write output
fwrite(foreign_vlt_eco_counties, 'data/cfs_io_analysis/foreign_VLT_by_TNC_x_county.csv')
fwrite(foreign_vlt_counties, 'data/cfs_io_analysis/foreign_VLT_to_counties.csv')

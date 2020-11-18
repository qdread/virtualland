# Calculate proportions of annual to permanent crops produced in each FAF region 
# Use this to divide cropland into annual and permanent by FAF region
# Output: percentage of outgoing cropland flow that is annual crops, by FAF

# Needed: NASS by NAICS code by state
# Needed: cropland value by county

# QDR 18 Nov 2020

library(tidyverse)

is_local <- dir.exists('Q:/')

fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_out <- file.path(fp, 'cfs_io_analysis')

nass_data <- read_csv(file.path(fp_out, 'NASS2012_receipts_workers_land_NAICS_imputed.csv'))
county_land <- read_csv(file.path(fp_out, 'cropland_by_county_FAF_joined.csv'))

# Annual crops: 1111 (oilseeds/grains), 1112 (veg), 11191 (tobacco), 11192 (cotton)
# Permanent crops: 1113 (fruit), 11193+4+9 (mix of crops incl. sugarcane and hay, basically permanent)
# Mix: 1114 (greenhouse crops and tree nurseries)

# Widen cropland x NAICS
annual_by_state <- nass_data %>% 
  select(state_fips, state_abbrev, state_name, NAICS, cropland) %>%
  pivot_wider(names_from = NAICS, values_from = cropland, values_fill = 0) %>%
  mutate(annual = `1111` + `1112` + `11191` + `11192` + `1114`/2,
         permanent = `1113` + `11193 & 11194 & 11199` + `1114`/2) %>%
  select(state_fips, state_abbrev, state_name, annual, permanent) %>%
  mutate(prop_annual = annual / (annual + permanent)) %>%
  select(-annual, -permanent)



# Use these proportions to get the annual and permanent croplands
county_land_with_annual <- county_land %>%
  left_join(annual_by_state) %>%
  mutate(annual_cropland = cropland * prop_annual,
         permanent_cropland = cropland * (1 - prop_annual))

write_csv(county_land_with_annual, file.path(fp_out, 'cropland_by_county_FAF_with_annual.csv'))

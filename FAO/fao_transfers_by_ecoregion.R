# Convert incoming land transfers from each country to incoming land transfers from ecoregions

# Needed: incoming virtual cropland and pastureland countries by country
# Needed: total area of cropland and pastureland in each ecoregion in each country

# After we get the incoming total transfers to USA from each ecoregion, we will use the proportional area of 
# ecoregions in each of the 8 "rest of the world" regions to roughly estimate the amount each ecoregion sends to each FAF region

library(tidyverse)
library(units)
library(sf)

is_local <- dir.exists('Q:/')

fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_cfs <- file.path(fp, 'raw_data/commodity_flows/CFS')
fp_faf <- file.path(fp, 'raw_data/commodity_flows/FAF')
fp_crosswalk <- file.path(fp, 'crossreference_tables')
fp_out <- file.path(fp, 'cfs_io_analysis')
fp_github <- ifelse(is_local, '~/Documents/GitHub/foodwaste/virtualland', '~/virtualland')
fp_eco <- file.path(fp, 'raw_data/landuse/ecoregions')

# Read the intersected country x TNC polygon file
country_tnc <- st_read(file.path(fp_out, 'countries_tnc_intersect.gpkg'))

# Read the country polygon only for joining
countrymap <- st_read(file.path(fp_eco, 'countries_global_equalarea.gpkg')) # 241 regions

# Read the tabulated counts of crop and pasture by country x TNC
fp_csvs <- file.path(fp, 'raw_data/landuse/output_csvs')
count_cropd <- read_csv(file.path(fp_csvs, 'global_count_cropdominance.csv'))
count_cropm <- read_csv(file.path(fp_csvs, 'global_count_cropmask.csv'))
count_pasture <- read_csv(file.path(fp_csvs, 'global_count_pasture.csv'))

# Extract the useful information from the cropland summaries
# Use crop dominance. Conservative estimate by excluding the minor fragment pixels. 
# Should not be sensitive to this because it is only for relative areas.
# In crop dominance, classes 1-7 are cropland.
count_cropd <- count_cropd %>% mutate(crop_sum = rowSums(count_cropd[,3:9], na.rm = TRUE))

# Read the land transfers by FAO
fao_vlt <- read_csv(file.path(fp_out, 'foreign_VLT_by_country.csv'))

# Join the VLT country list with the intersected country x TNC polygon
# There is no code to join on so we have to do it by name

dput(setdiff(fao_vlt$country_name, countrymap$NAME_LONG)) # Only 5 don't match.
# Bolivia, mainland China, Hong Kong, Iran, Tanzania

# Change the names of the FAO VLT dataframe to match the country names in countrymap.
names_to_match <- c("Bolivia (Plurinational State of)", "China, mainland", "China, Hong Kong SAR", 
  "Iran (Islamic Republic of)", "United Republic of Tanzania")
names_corrected <- c("Bolivia", "China", "Hong Kong", "Iran", "Tanzania")

fao_vlt$country_name[fao_vlt$country_name %in% names_to_match] <- names_corrected

# Reduce columns of country_tnc
country_tnc_data <- country_tnc %>%
  mutate(crop_area = count_cropd$crop_sum, pasture_area = count_pasture$sum) %>%
  select(ECO_CODE, ECO_NAME, WWF_REALM, NAME_LONG, ISO_A3, REGION_UN, SUBREGION, area, crop_area, pasture_area) %>%
  st_set_geometry(NULL)

# Determine, for each country, the relative proportion of cropland and pastureland in each ecoregion.
# This will be used to get the relative proportion of virtual cropland and pastureland transfers 
# from each ecoregion x country combination.
country_tnc_data <- country_tnc_data %>%
  mutate(crop_area = if_else(is.na(crop_area), 0, crop_area),
         pasture_area = if_else(is.na(pasture_area), 0, pasture_area)) %>%
  group_by(NAME_LONG) %>%
  mutate(crop_proportion = crop_area / sum(crop_area),
         pasture_proportion = pasture_area / sum(pasture_area))
  

# Join country_tnc with the virtual land transfers, proportionally split by area, then sum by "ROW" region x ecoregion.

foreign_vlt_eco <- fao_vlt %>%
  left_join(country_tnc_data, by = c('country_name' = 'NAME_LONG')) %>%
  mutate(VLT_crop_region = VLT_crop * crop_proportion,
         VLT_pasture_region = VLT_pasture * pasture_proportion) 

# Also, for each FAF region, get the proportion of the cropland and pastureland IN THE ENTIRE REGION
# that each ecoregion makes up (accounting only for countries that sent goods to the USA)

ecoregion_land_prop_x_faf_foreign <- foreign_vlt_eco %>%
  ungroup %>%
  group_by(FAF_foreign_region, FAF_foreign_region_code, ECO_CODE, ECO_NAME) %>%
  summarize(crop_area = sum(crop_area), pasture_area = sum(pasture_area)) %>%
  group_by(FAF_foreign_region, FAF_foreign_region_code) %>%
  mutate(crop_proportion_by_FAF_region = crop_area / sum(crop_area),
         pasture_proportion_by_FAF_region = pasture_area / sum(pasture_area))

foreign_vlt_eco_sum <- foreign_vlt_eco %>%
  group_by(FAF_foreign_region_code, FAF_foreign_region, ECO_CODE, ECO_NAME) %>%
  summarize(VLT_crop = sum(VLT_crop_region), VLT_pasture = sum(VLT_pasture_region),
            crop_area = sum(crop_area), pasture_area = sum(pasture_area))
# Now for each incoming shipment from a rest of the world region, we can assign a proportion of its land transfer to a given ecoregion.

write_csv(foreign_vlt_eco, file.path(fp_out, 'foreign_VLT_by_country_x_TNC.csv'))
write_csv(foreign_vlt_eco_sum, file.path(fp_out, 'foreign_VLT_by_region_x_TNC.csv'))
write_csv(ecoregion_land_prop_x_faf_foreign, file.path(fp_out, 'foreign_ecoregion_land_by_FAF.csv'))

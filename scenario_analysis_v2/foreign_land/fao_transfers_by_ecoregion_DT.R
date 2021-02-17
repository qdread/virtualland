# Convert incoming land transfers from each country to incoming land transfers from ecoregions

# Needed: incoming virtual cropland and pastureland countries by country
# Needed: total area of cropland and pastureland in each ecoregion in each country

library(data.table)
library(units)
library(Rutilitybelt)

fp_out <- 'data/cfs_io_analysis'
fp_eco <- 'data/raw_data/landuse/ecoregions'
fp_csvs <- 'data/raw_data/landuse/output_csvs'

# Read the country polygon only for joining
countrymap <- st_read(file.path(fp_eco, 'countries_global_equalarea.gpkg')) # 241 regions

# Read the tabulated counts of crop and pasture by country x TNC
count_cropd <- fread(file.path(fp_csvs, 'global_count_cropdominance.csv'))
count_cropm <- fread(file.path(fp_csvs, 'global_count_cropmask.csv'))
count_pasture <- fread(file.path(fp_csvs, 'global_count_pasture.csv'))

# Extract the useful information from the cropland summaries
# Use crop dominance. Conservative estimate by excluding the minor fragment pixels. 
# Should not be sensitive to this because it is only for relative areas.
# In crop dominance, classes 1-7 are cropland.
count_cropd <- count_cropd[, crop_sum := rowSums(count_cropd[,c(as.character(1:7))], na.rm = TRUE)]

# Read the land transfers by FAO
fao_vlt <- fread(file.path(fp_out, 'fao_VLT_provisional.csv'))

# Remove zero VLT countries
fao_vlt[, VLT_sum := rowSums(.SD, na.rm = TRUE), .SDcols = patterns('VLT_')]
fao_vlt <- fao_vlt[VLT_sum > 0]

# Join the VLT country list with the intersected country x TNC polygon
# There is no code to join on so we have to do it by name

#FIXME all below here does not work.
dput(setdiff(fao_vlt$country_name, countrymap$NAME_LONG)) # About 20 don't match.
# Bolivia, mainland China, Hong Kong, Iran, Tanzania

# Change the names of the FAO VLT dataframe to match the country names in countrymap.
names_to_match <- c("Bolivia (Plurinational State of)", "Cabo Verde", "China, mainland", 
                    "Congo", "Gambia", "China, Hong Kong SAR", "Iran (Islamic Republic of)", 
                    "C\xf4te d'Ivoire", "Republic of Moldova", "North Macedonia", 
                    "Czechia", "Sao Tome and Principe", "Eswatini", "Syrian Arab Republic", 
                    "China, Taiwan Province of", "United Republic of Tanzania", "United Kingdom of Great Britain and Northern Ireland", 
                    "Venezuela (Bolivarian Republic of)")
names_corrected <- c("Bolivia", "Republic of Cabo Verde", "China", 
                     "Democratic Republic of the Congo", "The Gambia", "Hong Kong", "Iran",
                     "Côte d'Ivoire", "Moldova", "Macedonia",
                     "Czech Republic", "São Tomé and Principe", "eSwatini", "Syria",
                     "Taiwan", "Tanzania", "United Kingdom", "Venezuela")

identical(fao_vlt$country_name[fao_vlt$country_name %in% names_to_match], names_to_match) # Yes.
fao_vlt$country_name[fao_vlt$country_name %in% names_to_match] <- names_corrected

# Extract needed columns from the crop dominance and pasture count data frames, and join the two.
count_cropd <- count_cropd[, .(ECO_CODE, ECO_NAME, WWF_REALM, NAME_LONG, ISO_A3, REGION_UN, SUBREGION, area, crop_sum)]
count_pasture <- count_pasture[, .(ECO_CODE, ECO_NAME, WWF_REALM, NAME_LONG, ISO_A3, REGION_UN, SUBREGION, area, sum)]
setnames(count_cropd, old = 'crop_sum', new = 'crop_area')
setnames(count_pasture, old = 'sum', new = 'pasture_area')

country_tnc_data <- count_pasture[count_cropd, on = .(ECO_CODE, ECO_NAME, WWF_REALM, NAME_LONG, ISO_A3, REGION_UN, SUBREGION, area)]

# Determine, for each country, the relative proportion of cropland and pastureland in each ecoregion.
# This will be used to get the relative proportion of virtual cropland and pastureland transfers 
# from each ecoregion x country combination.

replace_na_dt(country_tnc_data, cols = c('crop_area', 'pasture_area'))
country_tnc_data[, c('crop_proportion', 'pasture_proportion') := .(crop_area / sum(crop_area), pasture_area / sum(pasture_area)), by = NAME_LONG]

# Join country_tnc_data with the virtual land transfers, proportionally split by area
foreign_vlt_eco <- country_tnc_data[fao_vlt, on = c('NAME_LONG' = 'country_name')]
foreign_vlt_eco[, VLT_annual_region := VLT_annual * crop_proportion]
foreign_vlt_eco[, VLT_mixed_region := VLT_mixed * crop_proportion]
foreign_vlt_eco[, VLT_permanent_region := VLT_permanent * crop_proportion]
foreign_vlt_eco[, VLT_pasture_region := VLT_pasture * pasture_proportion]

# Sum the land transfers across ecoregions
foreign_vlt_eco_sum <- foreign_vlt_eco[, lapply(.SD, sum),
                                       by = .(ECO_CODE, ECO_NAME),
                                       .SDcols = patterns('region|pasture_area|crop_area')]


fwrite(foreign_vlt_eco, file.path(fp_out, 'foreign_VLT_by_country_x_TNC.csv'))
fwrite(foreign_vlt_eco_sum, file.path(fp_out, 'foreign_VLT_by_region_x_TNC.csv'))

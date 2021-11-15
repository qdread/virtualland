# COMPILE DATA FOR SHINY APP
# QDR / Virtualland / 04 June 2021

source('figs/figs_v2_loaddata.R')


# Additional processing from figs_v2_foreign.R

library(data.table)
setDT(foreign_vlt_export)
setDT(county_land_flow_sums)
setnames(foreign_vlt_export, 
         old = c('ECO_CODE', 'ECO_NAME', 'NAME_LONG', 'REGION_UN', 'SUBREGION'),
         new = c('TNC', 'TNC_name', 'country_name', 'region_UN', 'subregion_UN'))

foreign_vlt_countries <- foreign_vlt_export[, lapply(.SD, sum, na.rm = TRUE), 
                                            by = .(scenario_diet, scenario_waste, country_name, ISO_A3, region_UN, subregion_UN), 
                                            .SDcols = patterns('^V')]

setDT(foreign_extinction_import)
setDT(foreign_extinction_export)
setDT(county_extinction_flow_sums)

foreign_extinction_sum <- foreign_extinction_import[, .(foreign = sum(species_lost)), by = .(scenario_diet, scenario_waste, land_use, taxon)]

county_extinction_flow_sums <- tidyr::separate(county_extinction_flow_sums, scenario, into = c('D', 'scenario_diet', 'W', 'scenario_waste'), sep = '_')
county_extinction_flow_sums[, c('D','W') := NULL]

# Threats shown as exports from the originating ecoregion and originating country, depending on how they are totaled.
foreign_extinction_export_tnc <- foreign_extinction_export[, .(species_lost = sum(species_lost, na.rm = TRUE)), by = .(scenario_diet, scenario_waste, TNC, TNC_name, land_use, taxon)]
foreign_extinction_export_country <- foreign_extinction_export[, .(species_lost = sum(species_lost, na.rm = TRUE)), by = .(scenario_diet, scenario_waste, country_name, ISO_A3, land_use, taxon)]

# For both TNC and country maps, calculate sums by land use and by taxon (animals and total)
foreign_extinction_export_tnc_animals <- foreign_extinction_export_tnc[!taxon %in% 'plants', .(species_lost = sum(species_lost)), by = .(scenario_diet, scenario_waste, TNC, TNC_name, land_use)][,
                                                                                                                                                                                                  taxon := 'animals'
]
foreign_extinction_export_country_animals <- foreign_extinction_export_country[!taxon %in% 'plants', .(species_lost = sum(species_lost)), by = .(scenario_diet, scenario_waste, country_name, ISO_A3, land_use)][,
                                                                                                                                                                                                                 taxon := 'animals'
]

foreign_extinction_export_tnc_total <- foreign_extinction_export_tnc[, .(species_lost = sum(species_lost)), by = .(scenario_diet, scenario_waste, TNC, TNC_name, land_use)][,
                                                                                                                                                                            taxon := 'total'
]
foreign_extinction_export_country_total <- foreign_extinction_export_country[, .(species_lost = sum(species_lost)), by = .(scenario_diet, scenario_waste, country_name, ISO_A3, land_use)][,
                                                                                                                                                                                           taxon := 'total'
]

foreign_extinction_export_tnc <- rbindlist(list(foreign_extinction_export_tnc, foreign_extinction_export_tnc_animals, foreign_extinction_export_tnc_total), use.names = TRUE)
foreign_extinction_export_country <- rbindlist(list(foreign_extinction_export_country, foreign_extinction_export_country_animals, foreign_extinction_export_country_total), use.names = TRUE)

foreign_extinction_export_tnc_totalland <- foreign_extinction_export_tnc[, .(species_lost = sum(species_lost)), by = .(scenario_diet, scenario_waste, TNC, TNC_name, taxon)][,
                                                                                                                                                                             land_use := 'total'
]

foreign_extinction_export_country_totalland <- foreign_extinction_export_country[, .(species_lost = sum(species_lost)), by = .(scenario_diet, scenario_waste, country_name, ISO_A3, taxon)][,
                                                                                                                                                                                            land_use := 'total'
]

foreign_extinction_export_tnc <- rbindlist(list(foreign_extinction_export_tnc, foreign_extinction_export_tnc_totalland), use.names = TRUE)
foreign_extinction_export_country <- rbindlist(list(foreign_extinction_export_country, foreign_extinction_export_country_totalland), use.names = TRUE)

# Calculate the difference between each scenario and the baseline.
foreign_ext_tnc_base <- foreign_extinction_export_tnc[scenario_waste %in% 'baseline' & scenario_diet %in% 'baseline']
setnames(foreign_ext_tnc_base, old = 'species_lost', new = 'species_lost_baseline')
foreign_ext_tnc_base[, c('scenario_diet', 'scenario_waste') := NULL]

foreign_ext_country_base <- foreign_extinction_export_country[scenario_waste %in% 'baseline' & scenario_diet %in% 'baseline']
setnames(foreign_ext_country_base, old = 'species_lost', new = 'species_lost_baseline')
foreign_ext_country_base[, c('scenario_diet', 'scenario_waste') := NULL]

foreign_extinction_export_tnc <- foreign_ext_tnc_base[foreign_extinction_export_tnc, on = .NATURAL]
foreign_extinction_export_tnc[, species_vs_baseline := species_lost/species_lost_baseline - 1]

foreign_extinction_export_country <- foreign_ext_country_base[foreign_extinction_export_country, on = .NATURAL]
foreign_extinction_export_country[, species_vs_baseline := species_lost/species_lost_baseline - 1]

# Nest into panels
library(Rutilitybelt)

foreign_extinction_tnc_panels <- group_nest_dt(foreign_extinction_export_tnc, scenario_diet, scenario_waste, land_use, taxon)
foreign_extinction_country_panels <- group_nest_dt(foreign_extinction_export_country, scenario_diet, scenario_waste, land_use, taxon)


# Process data: foreign land exports to counties --------------------------

# Get rid of the columns that were not divided out by region weights
cols_remove <- names(foreign_vlt_export)[grepl('VLT', names(foreign_vlt_export)) & !grepl('region', names(foreign_vlt_export))]
foreign_vlt_export[, (cols_remove) := NULL]

# Melt to long form
foreign_vlt_export_long <- melt(foreign_vlt_export, measure.vars = patterns('VLT'), variable.name = 'land_use', value.name = 'VLT')
foreign_vlt_export_long <- foreign_vlt_export_long[, .(scenario_diet, scenario_waste, TNC, TNC_name, country_name, ISO_A3, land_use, VLT)]
foreign_vlt_export_long[, land_use := gsub('(VLT_)|(_region)', '', land_use)]

# Total VLT across land types and then bind.
foreign_vlt_export_long_total <- foreign_vlt_export_long[, .(VLT = sum(VLT, na.rm = TRUE)), by = .(scenario_diet, scenario_waste, TNC, TNC_name, country_name, ISO_A3)]
foreign_vlt_export_long_total[, land_use := 'total']

foreign_vlt_export_long <- rbindlist(list(foreign_vlt_export_long, foreign_vlt_export_long_total), use.names = TRUE)

# Calculate sums by TNC ecoregion and by country separately.
foreign_vlt_tnc <- foreign_vlt_export_long[, .(VLT = sum(VLT)), by = .(scenario_diet, scenario_waste, TNC, TNC_name, land_use)]
foreign_vlt_country <- foreign_vlt_export_long[, .(VLT = sum(VLT)), by = .(scenario_diet, scenario_waste, country_name, ISO_A3, land_use)]

# Calculate the difference between each scenario and the baseline.
foreign_vlt_tnc_base <- foreign_vlt_tnc[scenario_waste %in% 'baseline' & scenario_diet %in% 'baseline']
setnames(foreign_vlt_tnc_base, old = 'VLT', new = 'VLT_baseline')
foreign_vlt_tnc_base[, c('scenario_diet', 'scenario_waste') := NULL]

foreign_vlt_tnc <- foreign_vlt_tnc_base[foreign_vlt_tnc, on = .NATURAL]
foreign_vlt_tnc[, VLT_vs_baseline := VLT/VLT_baseline - 1]
foreign_vlt_tnc[is.nan(VLT_vs_baseline), VLT_vs_baseline := 0]

foreign_vlt_country_base <- foreign_vlt_country[scenario_waste %in% 'baseline' & scenario_diet %in% 'baseline']
setnames(foreign_vlt_country_base, old = 'VLT', new = 'VLT_baseline')
foreign_vlt_country_base[, c('scenario_diet', 'scenario_waste') := NULL]

foreign_vlt_country <- foreign_vlt_country_base[foreign_vlt_country, on = .NATURAL]
foreign_vlt_country[, VLT_vs_baseline := VLT/VLT_baseline - 1]
foreign_vlt_country[is.nan(VLT_vs_baseline), VLT_vs_baseline := 0]

# Nest to panels
foreign_vlt_tnc_panels <- group_nest_dt(foreign_vlt_tnc, scenario_diet, scenario_waste, land_use)
foreign_vlt_country_panels <- group_nest_dt(foreign_vlt_country, scenario_diet, scenario_waste, land_use)


# Combine inbound domestic and foreign ------------------------------------
foreign_extinction_import[, county := sprintf('%05d', county)]
setnames(foreign_extinction_import, 'species_lost', 'extinction_inbound_foreign')

county_extinction_flow_sums <- merge(county_extinction_flow_sums, foreign_extinction_import, all.x = TRUE, all.y = TRUE)
county_extinction_flow_sums[, extinction_inbound_total := extinction_inbound + extinction_inbound_foreign]

# Add foreign imported land.
foreign_vlt_import_long <- foreign_vlt_import %>%
  mutate(VLT_annual_region = VLT_annual_region + VLT_mixed_region / 2,
         VLT_permanent_region = VLT_permanent_region + VLT_mixed_region / 2) %>%
  select(-VLT_mixed_region) %>%
  rename(annual_cropland = VLT_annual_region, permanent_cropland = VLT_permanent_region, pastureland = VLT_pasture_region) %>%
  pivot_longer(contains('land'), names_to = 'land_type', values_to = 'flow_inbound_foreign')

county_land_flow_sums <- merge(county_land_flow_sums, foreign_vlt_import_long, all.x = TRUE, all.y = TRUE)
county_land_flow_sums[, flow_inbound_foreign := flow_inbound_foreign * 10000]
county_land_flow_sums[, flow_inbound_total := flow_inbound + flow_inbound_foreign]
replace_na_dt(county_land_flow_sums)


# Load foreign goods data -------------------------------------------------

import_crop <- fread('data/cfs_io_analysis/fao_VLT_provisional_crops_disaggregated.csv')
import_animal <- fread('data/cfs_io_analysis/fao_VLT_provisional_animalonly.csv')

import_crop <- tidyr::separate(import_crop[scenario != '', ], scenario, into = c('D', 'scenario_diet', 'W', 'scenario_waste'), sep = '_')
import_crop[, c('D','W') := NULL]
import_animal <- tidyr::separate(import_animal[scenario != '', ], scenario, into = c('D', 'scenario_diet', 'W', 'scenario_waste'), sep = '_')
import_animal[, c('D','W') := NULL]

import_animal[, item := paste0(livestock_product_type_broad, ', ', livestock_name)]

foreign_goods_country <- rbind(
  import_crop[export_qty > 0, .(scenario_diet, scenario_waste, country_name, item, export_qty)],
  import_animal[export_qty > 0, .(scenario_diet, scenario_waste, country_name, item, export_qty)]
)

# Standardize column names and categories ---------------------------------

# Also add ISO_A3 codes to the countries for joining purposes

setnames(county_goods_flow_sums, old = c('flow_inbound', 'flow_outbound'), new = c('flow_inbound_domestic', 'flow_outbound_domestic'))
setnames(county_land_flow_sums, old = c('flow_inbound', 'flow_outbound'), new = c('flow_inbound_domestic', 'flow_outbound_domestic'))
setnames(county_extinction_flow_sums, old = c('land_use', 'extinction_inbound', 'extinction_outbound', 'extinction_inbound_foreign', 'extinction_inbound_total'), new = c('land_type', 'flow_inbound_domestic', 'flow_outbound_domestic', 'flow_inbound_foreign', 'flow_inbound_total'))

global_country_map <- st_read('data/raw_data/landuse/ecoregions/countries_global_equalarea.gpkg') %>%
  select(NAME_LONG, ISO_A3)

iso_lookup <- st_drop_geometry(global_country_map) %>% rename(country_name = NAME_LONG)

foreign_goods_flow_sums <- copy(foreign_goods_country)
foreign_land_flow_sums <- copy(foreign_vlt_country)
foreign_extinction_flow_sums <- copy(foreign_extinction_export_country)

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

for (i in 1:length(names_to_match)) {
  foreign_goods_flow_sums$country_name[foreign_goods_flow_sums$country_name %in% names_to_match[i]] <- names_corrected[i]
}

setdiff(foreign_goods_flow_sums$country_name, iso_lookup$country_name)
setdiff(foreign_land_flow_sums$country_name, iso_lookup$country_name)
setdiff(foreign_extinction_flow_sums$country_name, iso_lookup$country_name)

setnames(foreign_goods_flow_sums, old = 'export_qty', new = 'flow_outbound_foreign')
setnames(foreign_land_flow_sums, old = c('land_use', 'VLT'), new = c('land_type', 'flow_outbound_foreign'))
setnames(foreign_extinction_flow_sums, old = c('land_use', 'species_lost'), new = c('land_type', 'flow_outbound_foreign'))

# Here fix the countries with incorrect ISO codes by creating a manual lookup table
iso_lookup_correction <- data.table(country_name = c("Somaliland", "Norway", "Kosovo", "France", "Northern Cyprus",  "Indian Ocean Territories", "Ashmore and Cartier Islands", "Siachen Glacier"),
                                    ISO_A3 = c('SOM', 'NOR', 'XKX', 'FRA', 'XNCYPRUS', 'IOT', 'XASHMORE', 'XSIACHEN')
)
setDT(iso_lookup)
iso_lookup <- iso_lookup[!ISO_A3 %in% '-99']
iso_lookup <- rbind(iso_lookup, iso_lookup_correction)

foreign_land_flow_sums[, ISO_A3 := iso_lookup[match(foreign_land_flow_sums$country_name, iso_lookup$country_name), ISO_A3]]
foreign_extinction_flow_sums[, ISO_A3 := iso_lookup[match(foreign_extinction_flow_sums$country_name, iso_lookup$country_name), ISO_A3]]
global_country_map <- global_country_map %>%
  mutate(ISO_A3 = iso_lookup[match(global_country_map$NAME_LONG, iso_lookup$country_name), ISO_A3])

# Replace land type columns with all the correct names, filter away the totals for now to keep only the primary data rows (no duplicated total rows)

foreign_land_flow_sums <- foreign_land_flow_sums[!land_type %in% 'total', .(scenario_diet, scenario_waste, ISO_A3, land_type, flow_outbound_foreign)]
foreign_extinction_flow_sums <- foreign_extinction_flow_sums[!land_type %in% 'total' & !taxon %in% c('animals', 'total'), .(scenario_diet, scenario_waste, ISO_A3, land_type, taxon, flow_outbound_foreign)]

land_type_table <- data.frame(short = c('annual', 'pasture', 'permanent'), long = c('annual_cropland', 'pastureland', 'permanent_cropland'))

county_land_flow_sums[, land_type := land_type_table$short[match(land_type, land_type_table$long)]]

setnames(ag_names_lookup, old = 'BEA_389_code', new = 'BEA_code')

# Replace foreign goods country names with ISO
foreign_goods_flow_sums <- iso_lookup[foreign_goods_flow_sums, on = 'country_name']
foreign_goods_flow_sums <- foreign_goods_flow_sums[, .(scenario_diet, scenario_waste, ISO_A3, item, flow_outbound_foreign)]

bea_lookup <- as.data.table(ag_names_lookup)
setnames(bea_lookup, old = c('BEA_389_def', 'short_name'), new = c('ag_good_long_name', 'ag_good_short_name'))

global_country_map <- rename(global_country_map, country_name = NAME_LONG)


# Lookup table for county and state names ---------------------------------

data(fips_codes, package = 'tidycensus')

state_lookup <- unique(fips_codes[, c('state_code', 'state', 'state_name')])
names(state_lookup) <- c('fips_state', 'state_abbrev', 'state_name')

fips_codes$fips_county <- paste0(fips_codes$state_code, fips_codes$county_code)
county_lookup <- fips_codes[, c('fips_county', 'county')]

# Check presence of all counties in lookup table
unique_counties <- unique(county_land_flow_sums$county)
setdiff(unique_counties, county_lookup$fips_county)

# Add on the extra counties from the harmonization
fips_harmonization <- read_csv('data/crossreference_tables/fips_harmonization.csv', col_types = 'ccccc')

fips_harmonization_extra <- fips_harmonization %>%
  select(FIPS_data, name_data) %>%
  filter(!FIPS_data %in% county_lookup$fips_county) %>%
  rename(fips_county = FIPS_data, county = name_data) %>%
  mutate(county = gsub(',[^,]*$', '', county)) # Remove last comma and everything after

county_lookup <- rbind(county_lookup, fips_harmonization_extra)


# Filter to 10 scenarios --------------------------------------------------

# To reduce file size, get rid of the two waste scenarios not used in MS.
scens <- c('baseline', 'allavoidable')
setDT(county_goods_flow_sums)
county_goods_flow_sums <- county_goods_flow_sums[scenario_waste %in% scens]
county_land_flow_sums <- county_land_flow_sums[scenario_waste %in% scens]
county_extinction_flow_sums <- county_extinction_flow_sums[scenario_waste %in% scens]
foreign_goods_flow_sums <- foreign_goods_flow_sums[scenario_waste %in% scens]
foreign_land_flow_sums <- foreign_land_flow_sums[scenario_waste %in% scens]
foreign_extinction_flow_sums <- foreign_extinction_flow_sums[scenario_waste %in% scens]


# Additional preprocessing ------------------------------------------------

# Convert the goods and land flows to more sensible units
divide_numeric <- function(dt, n = 1e6) {
  cols <- names(dt)[sapply(dt, is.numeric)]
  dt[, (cols) := lapply(.SD, function(x) x/n), .SDcols = cols]
}
divide_numeric(county_goods_flow_sums)
divide_numeric(county_land_flow_sums)
divide_numeric(foreign_land_flow_sums)

# Remove -99 codes
foreign_extinction_flow_sums <- foreign_extinction_flow_sums[!ISO_A3 %in% '-99']
foreign_land_flow_sums <- foreign_land_flow_sums[!ISO_A3 %in% '-99']

# For presentation, simplify "mixed" land type in foreign land flow sums by assigning 50% to annual and 50% to permanent (simplest assumption)
foreign_land_flow_wide <- dcast(foreign_land_flow_sums, scenario_diet + scenario_waste + ISO_A3 ~ land_type)
replace_na_dt(foreign_land_flow_wide, replace_with = 0)
foreign_land_flow_wide[, annual := annual + mixed/2]
foreign_land_flow_wide[, permanent := permanent + mixed/2]
foreign_land_flow_wide[, mixed := NULL]
foreign_land_flow_sums <- melt(foreign_land_flow_wide, variable.name = 'land_type', value.name = 'flow_outbound_foreign')

# Reorder land types and taxa to ordered factors for plotting
land_options <- c('annual', 'permanent', 'pasture')
taxa_options <- c('plants', 'amphibians', 'birds', 'mammals', 'reptiles')
county_land_flow_sums[, land_type := factor(land_type, levels = land_options)]
foreign_land_flow_sums[, land_type := factor(land_type, levels = land_options)]
county_extinction_flow_sums[, taxon := factor(taxon, levels = taxa_options)]
county_extinction_flow_sums[, land_type := factor(land_type, levels = land_options)]
foreign_extinction_flow_sums[, taxon := factor(taxon, levels = taxa_options)]
foreign_extinction_flow_sums[, land_type := factor(land_type, levels = land_options)]

# Reorder scenario_diet and scenario_waste to ordered factors for plotting
reorder_scen <- function(dt) {
  dt[, scenario_diet := factor(scenario_diet, levels = c('baseline', 'usstyle', 'medstyle', 'vegetarian', 'planetaryhealth'))]
  dt[, scenario_waste := factor(scenario_waste, levels = c('baseline', 'allavoidable'))]
}

reorder_scen(county_land_flow_sums)
reorder_scen(foreign_land_flow_sums)
reorder_scen(county_extinction_flow_sums)
reorder_scen(foreign_extinction_flow_sums)
reorder_scen(county_goods_flow_sums)
reorder_scen(foreign_goods_flow_sums)

# Change encoding of e acute
foreign_goods_flow_sums[, item := gsub('\xe9', 'é', item)]


# Apply transformation to Alaska and Hawaii -------------------------------

# This will eliminate the need for inset maps because they will now appear as appropriately scaled 
# areas to the south and west of the L48
# Adapted from https://rstudio-pubs-static.s3.amazonaws.com/94122_462a1d171e4944f0a99c1f91fd5071d5.html#move-alaska-scaled-down-and-hawaii

rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

crs_lambert <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"

county_map_trans <- county_map %>%
  st_transform(crs = crs_lambert)

alaska <- county_map_trans %>% filter(fips_state %in% '02')
alaska_g <- st_geometry(alaska)
alaska_centroid <- st_centroid(st_union(alaska_g))

alaska_trans <- (alaska_g - alaska_centroid) * rot(-39 * pi/180) / 2.3 + alaska_centroid + c(1000000, -5000000)
alaska <- st_set_geometry(alaska, alaska_trans) %>% st_set_crs(st_crs(county_map_trans))


hi_crs <- '+proj=aea +lat_1=8 +lat_2=18 +lat_0=3 +lon_0=-157 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs'
hi_box <- c(xmin = -400000, ymin = 1761000, xmax = 230000, ymax = 2130000)

hawaii <- county_map %>% 
  filter(fips_state %in% '15') %>%
  st_transform(crs = hi_crs) %>%
  st_crop(hi_box) %>%
  st_transform(crs = crs_lambert)

hawaii_g <- st_geometry(hawaii)
hawaii_centroid <- st_centroid(st_union(hawaii_g))

hawaii_trans <- (hawaii_g - hawaii_centroid) * rot(-35 * pi/180) + hawaii_centroid + c(5200000, -1400000)
hawaii <- st_set_geometry(hawaii, hawaii_trans) %>% st_set_crs(st_crs(county_map_trans))

county_map <- county_map_trans %>%
  filter(!fips_state %in% c('02', '15')) %>%
  rbind(alaska) %>%
  rbind(hawaii)

# Write all to CSV --------------------------------------------------------

# We now have all the data in a consistent format that could be put in a database with common keys if needed.

objs <- c('bea_lookup', 'iso_lookup', 'state_lookup', 'county_lookup', 'county_map', 'global_country_map',
          'county_goods_flow_sums', 'county_land_flow_sums', 'county_extinction_flow_sums',
          'foreign_goods_flow_sums', 'foreign_land_flow_sums', 'foreign_extinction_flow_sums')

# Save all to .RData
save(list = objs, file = 'data/cfs_io_analysis/shinyapp_data/all_app_data.RData')

# Write the maps to GPKG
st_write(county_map, 'data/cfs_io_analysis/shinyapp_data/county_map.gpkg', driver = 'GPKG', append = FALSE)
st_write(global_country_map, 'data/cfs_io_analysis/shinyapp_data/global_country_map.gpkg', driver = 'GPKG', append = FALSE)

# Save to CSVs
walk(objs[-(5:6)], ~ fwrite(get(.), glue('data/cfs_io_analysis/shinyapp_data/{.}.csv')))

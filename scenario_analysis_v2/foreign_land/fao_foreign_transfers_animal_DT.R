# Virtual land transfers associated with imports of animal products from foreign countries into the USA
# QDR / Virtualland / 11 Mar 2021

# This will supersede the old method in fao_foreign_land_imports which overestimated pastureland and did not account for feed crops.

# Method
# 1. Join FAO food balance sheet with crop production + trade + area harvested data
# 2. Multiply percent of each crop going to feed * area harvested = land area of each crop used to feed animals in each country
# 3. Cartesian join the result with the livestock patterns data for each country (consistent biomass-based units per species).
# 4. Multiply land area to feed animals by relative share of livestock patterns for each species = land area of each crop used to feed each species in each country
# 5. Join the result with the pastureland totals for each country
# 6. Multiply the total pastureland for each country by the livestock patterns share of each species, divided only among grazer species = land area of pastureland used to feed each grazer species in each country
# 7.Cartesian join the result with livestock primary data that divide animal stocks into those used to produce meat vs. milk or eggs
# 8. Multiply land area per species by the proportion of stocks used to produce meat vs. milk or eggs = land area of each crop used to produce each type of product for each species in each country! (will have to find a rough estimate for how to break down dairy products further)
# 9. Join back with the original production+trade data frame for animals.
# 10. Apply scenario factors!


# Step 0. Initial processing ----------------------------------------------

# First source through line 154 of fao_foreign_land_imports_DT.R (we need the production + trade crop dataframe)

# We also need to load the FAO food balance sheet data.
fbs <- fread('data/cfs_io_analysis/fao_fbs/fbs_indiv_weights_wide.csv')
# Load the harmonization table for FBS "Aggregate" codes and the individual production codes
fbs_prod_crosswalk <- fread('data/crossreference_tables/fao_prodcodes_harmonized_fbs.csv')

# Next join the animal production and trade data frames. This is different than the old way which removed everything other than grazers.

# Concatenate three livestock production dataframes and remove doublecounting codes.
prod_animal_all <- rbindlist(list(production_livestock, production_livestockprimary, production_livestockprocessed))
prod_animal_all <- prod_animal_all[!grepl('17..$|18..$', item_code)]

# Get production by weight only.
prod_animal_wgt <- prod_animal_all[unit %in% 'tonnes']

# Get trade to usa by weight for the animal crops
trade_tousa_byweight_animal <- trade_tousa_qty[unit %in% 'tonnes']
trade_tousa_byweight_animal[, c('element_code', 'element', 'unit', 'partner_country_code', 'partner_country') := NULL]
setnames(trade_tousa_byweight_animal, old = 'value', new = 'export_qty')

# Sum up trade and production codes by type of animal and type of product.
# This is because the trade and production codes do not overlap in some cases.
prod_animal_wgt_bytype <- fao_codes_table[prod_animal_wgt, on = c('code' = 'item_code')]
prod_animal_wgt_bytype <- prod_animal_wgt_bytype[, .(production_qty = sum(value, na.rm = TRUE)), by = .(area_code, area, BEA_code, livestock_animal, livestock_product_type)]

trade_tousa_wgt_bytype <- fao_codes_table[trade_tousa_byweight_animal, on = c('code' = 'item_code')]
trade_tousa_wgt_bytype <- trade_tousa_wgt_bytype[, .(export_qty = sum(export_qty, na.rm = TRUE)), by = .(reporter_country_code, reporter_country, livestock_animal, livestock_product_type)]

# Join the two.
setnames(trade_tousa_wgt_bytype, old = c('reporter_country_code', 'reporter_country'), new = c('area_code', 'area'))
prod_animal_joined_trade <- trade_tousa_wgt_bytype[prod_animal_wgt_bytype, on = .(area_code, area, livestock_animal, livestock_product_type)]
prod_animal_joined_trade[is.na(export_qty), export_qty := 0]

# We can get rid of any animal species that the USA does not import any of.
animal_export_totals <- prod_animal_joined_trade[, .(total_export = sum(export_qty)), by = livestock_animal]
prod_animal_joined_trade <- prod_animal_joined_trade[livestock_animal %in% animal_export_totals$livestock_animal[animal_export_totals$total_export > 0]]


# Step 1-2. Join FBS and crop production/trade data -------------------------

# Step 1. Join.
# Select only the crops that have any use for animal feed, in any country
feed_proportion <- fbs[, .(feed = mean(feed, na.rm = TRUE)), by = .(item_code, item)][order(-feed)]
# There are 64 item codes that we will have to harmonize with the production codes.

# Check cross table of item codes in production_crops_trade and fbs. All in prod_crops_trade are hopefully in fbs
setdiff(x = production_crops_trade$item_code, y = fbs$item_code)
setdiff(y = production_crops_trade$item_code, x = fbs$item_code)
intersect(x = production_crops_trade$item_code, y = fbs$item_code)
# There is no overlap between the two sets of item codes, so I will have to figure out a new way to join.

# Write out the two sets of codes to CSVs and manually harmonize.
prod_item_codes <- production_crops_trade[, .(item, item_code)]
prod_item_codes[, any(!is.na(item)), by = item_code][(!V1)]

table(prod_item_codes$item_code %in% c(trade_tousa_byweight$item_code, production_crops$item_code))
item_codes_full <- unique(rbindlist(list(trade_tousa_byweight[, .(item, item_code)], production_crops[, .(item, item_code)])))

# Get the text item name corresponding to the item code then write to CSV
# Only need to harmonize the FBS codes that have any feed uses.
prod_item_code_lookup <- item_codes_full[item_code %in% prod_item_codes$item_code][order(item_code)]
fbs_item_code_lookup <- unique(fbs[, .(item, item_code)])[order(item_code)][item_code %in% feed_proportion$item_code[feed_proportion$feed > 0]]

# fwrite(prod_item_code_lookup, 'data/crossreference_tables/fao_prodcodes_to_harmonize_with_fbs.csv')
# fwrite(fbs_item_code_lookup, 'data/crossreference_tables/fao_fbscodes_to_harmonize_with_prod.csv')

# Use the mapping in the crosswalk table to join FBS with the production data.
# Keep only relevant columns of fbs
fbs_tojoin <- fbs[, .(area_code, area, item_code, item,  domestic_supply_quantity, feed)]
setnames(fbs_tojoin, old = c('item_code', 'item', 'area_code', 'area'), new = c('fbs_parent_group', 'fbs_item', 'country_code', 'country_name'))

# Join production_crops_trade with the fbs parent code crosswalk
production_crops_trade <- fbs_prod_crosswalk[production_crops_trade, on = 'item_code']
production_crops_trade[, i.item := NULL] # Remove the bad item column

# Join production_crops_trade with reduced fbs dataset
production_crops_fbs_joined <- production_crops_trade[fbs_tojoin, on = .NATURAL]

# Step 2. Calculate land footprint of each crop in each country going to feed.
# Where domestic supply quantity is less than feed, set them equal.
production_crops_fbs_joined[domestic_supply_quantity < feed, domestic_supply_quantity := feed]
production_crops_fbs_joined[, proportion_feed := feed / domestic_supply_quantity]
production_crops_fbs_joined[, land_footprint_feed := area_harvested * proportion_feed]

# Subset to only those with land footprint > 0
production_crops_fbs_joined <- production_crops_fbs_joined[!is.na(land_footprint_feed) & land_footprint_feed > 0]

# Step 3-4. Join with livestock patterns for each species -----------------

# Step 3. Join the feed footprint for each crop with the livestock patterns data

# N.B.: Livestock patterns excludes some of the less important livestock types, such as bees, turkey, ducks, rabbits.

# Clean up livestock patterns data (reshape the 3 variables wider so each country x species gets one row)
livestock_patterns[, c('unit', 'n', 'element_code') := NULL]
livestock_patterns[, element := fcase(
  element == 'Stocks', 'LSU_stocks',
  grepl('Share', element), 'LSU_percent',
  grepl('land', element), 'LSU_per_ha_ag_land'
)]

livestock_patterns_wide <- dcast(livestock_patterns, area_code + area + item_code + item ~ element)
setnames(livestock_patterns_wide, old = c('area_code', 'area', 'item_code', 'item'), new = c('country_code', 'country_name', 'livestock_item_code', 'livestock_name'))

# Filter out aggregated livestock codes to remove double counting
livestock_patterns_wide <- livestock_patterns_wide[!grepl('17..$|18..$', livestock_item_code)]

# Assign grazer (pasture using or ruminant) species in livestock_patterns_wide, and get the percent LSU share among grazers only. (i.e. remove pigs and chickens)
nongrazers <- c('Pigs', 'Chickens')
livestock_patterns_wide[, grazer := !livestock_name %in% nongrazers]
livestock_patterns_wide[, LSU_percent_grazers := 100 * LSU_stocks / sum(LSU_stocks), by = .(country_code, country_name, grazer)]
livestock_patterns_wide[(!grazer), LSU_percent_grazers := NA]

# Cartesian Join livestock patterns with production crops fbs joined.
land_feed_livestock <- production_crops_fbs_joined[livestock_patterns_wide, on = .NATURAL, allow.cartesian = TRUE]

# Step 4. Assign land footprint of feed to species, assuming that all species get the same mix of feed (naive)
land_feed_livestock[, land_footprint_feed_species := land_footprint_feed * LSU_percent / 100]

# Filter out countries that export no goods to USA (most of these are aggregated countries)
exporters <- trade_tousa_byweight[, sum(export_qty, na.rm = TRUE), by = .(country_code, country_name)][V1 > 0]
land_feed_livestock <- land_feed_livestock[country_code %in% exporters$country_code]

# Sum up the crop footprints by species x country (total annual and permanent cropland footprint of feed)
land_feed_livestock_byspecies <- land_feed_livestock[, .(cropland_footprint_feed_species = sum(land_footprint_feed_species, na.rm = TRUE),
                                                         LSU_percent = LSU_percent[1],
                                                         LSU_percent_grazers = LSU_percent_grazers[1]), 
                                                     by = .(country_code, country_name, livestock_item_code, livestock_name, annual, grazer)]

# Get rid of bad rows
land_feed_livestock_byspecies <- land_feed_livestock_byspecies[cropland_footprint_feed_species > 0]

# Widen so that annual and permanent crops have a separate column each.
land_feed_livestock_byspecies[, annual := ifelse(annual, 'annual_footprint_feed', 'permanent_footprint_feed')]
land_feed_livestock_byspecies <- dcast(land_feed_livestock_byspecies, ... ~ annual, value.var = 'cropland_footprint_feed_species', fill = 0)

# Step 5-6. Join with pastureland to get pasture footprint ----------------

# Step 5. Join pastureland totals (created from landuse_inputs code 6655 for pastureland)
# Convert pastureland to ha for comparison with cropland: multiply by 1000
pastureland_totals <- landuse_inputs[item_code %in% 6655, .(area_code, area, value)]
setnames(pastureland_totals, c('country_code', 'country_name', 'pastureland'))
pastureland_totals[, pastureland := pastureland * 1000]

land_feed_livestock_byspecies <- pastureland_totals[land_feed_livestock_byspecies, on = .NATURAL]

# Step 6. Multiply by the LSU percentage for grazers only to get the pastureland footprint per species
land_feed_livestock_byspecies[, pastureland_footprint := pastureland * LSU_percent_grazers /100]


# Step 7-8. Join with livestock primary to disaggregate by product --------

# Subset production_livestockprimary to only the relevant rows
# The same value is given for number of slaughtered animals for each meat product but a different number is given for milk products
unique(production_livestockprimary[unit %in% c('Head', '1000 Head'), .(item, element, element_code, unit)])
unique(production_livestockprimary[element %in% 'Milk Animals', .(item, element, unit)])

# get meat and milk number of head for cow, sheep, goat, camel, and buffalo, and meat and milk 1000 bird numbers for chickens
# remove a couple aggregated codes that got in.
livestockprimary_bytype <- production_livestockprimary[((grepl('Milk|Meat', item, ignore.case = TRUE) & grepl('cow|cattle|sheep|goat|buffalo|camel', item, ignore.case = TRUE)) |
                                                      (grepl('Eggs|Meat', item, ignore.case = TRUE) & grepl('chicken|hen', item, ignore.case = TRUE))) &
                                                      element_code %in% c(5318, 5320, 5321, 5313) & !item_code %in% c(1806, 1807, 1158)]

# Do some data manipulation on the livestockprimary_bytype dataframe.
livestockprimary_bytype[, livestock_name := fcase(
  grepl('cattle|cow', item), 'Cattle',
  grepl('sheep', item), 'Sheep',
  grepl('goat', item), 'Goats',
  grepl('chicken|hen', item), 'Chickens',
  grepl('camel', item), 'Camels',
  grepl('buffalo', item), 'Buffaloes'
)]
livestockprimary_bytype[, element := fcase(
  element == 'Milk Animals', 'milk',
  element == 'Laying', 'eggs',
  element == 'Producing Animals/Slaughtered', 'meat'
)]

# Multiply the 1000 head rows by 1000 to make it consistent, though in the end only the relative numbers will be used.
livestockprimary_bytype[unit == '1000 Head', value := value * 1000]

livestockprimary_bytype[, c('element_code', 'item_code', 'item', 'n', 'unit') := NULL]

setnames(livestockprimary_bytype, 'value', 'n_animals')
livestockprimary_bytype[, proportion_animals := n_animals / sum(n_animals), by = .(area_code, area, livestock_name)]

livestockprimary_bytype[, n_animals := NULL]
setnames(livestockprimary_bytype, old = c('area_code', 'area'), new = c('country_code', 'country_name'))

# Join this with land_feed_livestock_byspecies
land_feed_livestock_byspecies <- livestockprimary_bytype[land_feed_livestock_byspecies, on = .NATURAL]

# Fill in missing values
land_feed_livestock_byspecies[is.na(element), element := 'meat']
land_feed_livestock_byspecies[is.na(proportion_animals), proportion_animals := 1]

# step 8. Multiply the appropriate land footprints times the proportion used for milk or eggs.
footprint_cols <- grep('footprint', names(land_feed_livestock_byspecies), value = TRUE)
land_feed_livestock_byspecies[, (footprint_cols) := lapply(.SD, function(x) x * land_feed_livestock_byspecies$proportion_animals), .SDcols = footprint_cols]


# Step 9. Join back with original production+trade data -------------------

# Next join with the original prod_animal_joined_trade, 
# http://www.fao.org/3/T0045E/T0045E05.htm states that 100 L (~100 kg) milk at 40 g fat/L produces 11 kg cheese or 1.8 kg butter.
# yogurt is roughly 1:1 ratio, cream is similar to the ratio for cheese.

# Crude conversion factors:
dairy_conversion_factors <- c(cheese = 0.1, butter = 0.02, `other dairy` = 0.1)
# Add meat and fat together (representing biomass of animal)

# Clean up prod_animal_joined_trade by
# remove nonfood products (wool and hide)
# Remove honey (not accounting for land footprint of honey production)
# Convert butter and cheese (and other dairy) to milk equivalents, and convert fat to meat equivalent.
# Sum up by country x species x broader product type (meat, dairy, eggs)

prod_animal_joined_trade <- prod_animal_joined_trade[!livestock_product_type %in% c('wool', 'hide', 'honey', 'other')]
prod_animal_joined_trade[, production_qty := fcase(
    livestock_product_type %in% 'butter', production_qty * dairy_conversion_factors['butter'],
    livestock_product_type %in% 'cheese', production_qty * dairy_conversion_factors['cheese'],
    livestock_product_type %in% 'other dairy', production_qty * dairy_conversion_factors['other dairy'],
    !livestock_product_type %in% c('butter', 'cheese', 'other dairy'), production_qty
  )
]

prod_animal_joined_trade[, livestock_product_type_broad := fcase(
  livestock_product_type %in% c('meat', 'fat'), 'meat',
  livestock_product_type %in% c('butter', 'cheese', 'milk', 'other dairy'), 'milk',
  livestock_product_type %in% 'eggs', 'eggs'
)]

prod_trade_animal_agg <- prod_animal_joined_trade[, lapply(.SD, sum, na.rm = TRUE), by = .(area_code, area, livestock_animal, livestock_product_type_broad), .SDcols = c('export_qty', 'production_qty')]

# To join land_feed_livestock_byspecies with prod_trade_animal_agg, we need to match the livestock names up.

livestock_names_lookup <- data.frame(old = c("Cattle", "Buffaloes", "Sheep", "Goats", "Pigs", "Chickens", 
                                       "Horses", "Asses", "Mules", "Camels"),
                                     new = c('cattle', 'buffalo', 'sheep', 'goat', 'pig', 'chicken', 'horse', 'ass', 'mule', 'camel'))
land_feed_livestock_byspecies[, livestock_name := livestock_names_lookup$new[match(livestock_name, livestock_names_lookup$old)]]

land_feed_livestock_tojoin <- land_feed_livestock_byspecies[, .(country_code, country_name, element, livestock_name, proportion_animals, annual_footprint_feed, permanent_footprint_feed, pastureland_footprint)]
setnames(land_feed_livestock_tojoin, old = 'element', new = 'livestock_product_type_broad')

setnames(prod_trade_animal_agg, old = c('area_code', 'area', 'livestock_animal'), new = c('country_code', 'country', 'livestock_name'))
prod_trade_animal_agg[, country := NULL]

VLT_animal_baseline <- land_feed_livestock_tojoin[prod_trade_animal_agg, on = .NATURAL][
  !is.na(livestock_product_type_broad) & export_qty > 0 & (!is.na(annual_footprint_feed) | !is.na(permanent_footprint_feed) | !is.na(pastureland_footprint))]

# In joined data frame, multiply the VLT by the proportion sent to the USA. 
VLT_animal_baseline[, proportion_sent_to_usa := export_qty / production_qty]
VLT_animal_baseline[, VLT_annual := annual_footprint_feed * proportion_sent_to_usa]
VLT_animal_baseline[, VLT_permanent := permanent_footprint_feed * proportion_sent_to_usa]
VLT_animal_baseline[, VLT_pasture := pastureland_footprint * proportion_sent_to_usa]

# Step 10. Apply scenario factors -----------------------------------------

# calculate a coarsened scenario factor with meat, milk, and eggs aggregated.
# Use only meat, milk, and egg codes, and aggregate them
dairy_codes <- c('31151A', '311513', '311514')
meat_codes <- c('31161A', '311615', '112A00')
egg_codes <- c('112300')
scenario_factors_animal <- scenario_factors_long[BEA_389_code %in% c(dairy_codes, meat_codes, egg_codes)]
scenario_factors_animal[, livestock_product_type_broad := fcase(BEA_389_code %in% meat_codes, 'meat',
                                                                BEA_389_code %in% dairy_codes, 'milk',
                                                                BEA_389_code %in% egg_codes, 'eggs')]
scenario_factors_animal[, .(consumption_factor = mean(consumption_factor)), by = .(scenario, livestock_product_type_broad)]

# Cartesian join VLT_animal_baseline with coarsened scenario factors.
VLT_sums_animal <- scenario_factors_animal[VLT_animal_baseline, on = livestock_product_type_broad, allow.cartesian = TRUE]

# Multiply VLTs by consumption factors.
VLT_sums_animal[, VLT_annual := VLT_annual * consumption_factor]
VLT_sums_animal[, VLT_permanent := VLT_permanent * consumption_factor]
VLT_sums_animal[, VLT_pasture := VLT_pasture * consumption_factor]
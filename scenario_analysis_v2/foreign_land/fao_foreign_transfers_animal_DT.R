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
# 9. Apply scenario factors!


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
production_crops_fbs_joined[, proportion_feed := feed / domestic_supply_quantity]
production_crops_fbs_joined[, land_footprint_feed := area_harvested * proportion_feed]

# Subset to only those with land footprint > 0
production_crops_fbs_joined <- production_crops_fbs_joined[!is.na(land_footprint_feed) & land_footprint_feed > 0]

# Step 3-4. Join with livestock patterns for each species -----------------

# Step 3. Join the feed footprint for each crop with the livestock patterns data

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
                                                     by = .(country_code, country_name, livestock_item_code, livestock_name, grazer)]

# Step 5-6. Join with pastureland to get pasture footprint ----------------

# Step 5. Join pastureland totals (created from landuse_inputs code 6655 for pastureland)
# Convert pastureland to ha for comparison with cropland: multiply by 1000
pastureland_totals <- landuse_inputs[item_code %in% 6655, .(area_code, area, value)]
setnames(pastureland_totals, c('country_code', 'country_name', 'pastureland'))
pastureland_totals[, pastureland := pastureland * 1000]

land_feed_livestock_byspecies <- pastureland_totals[land_feed_livestock_byspecies, on = .NATURAL]

# Step 6. Multiply by the LSU percentage for grazers only to get the pastureland footprint per species
land_feed_livestock_byspecies[, pastureland_footprint_feed_species := pastureland * LSU_percent_grazers /100]


# Step 7-8. Join with livestock primary to disaggregate by product --------

# Subset production_livestockprimary

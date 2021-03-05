# Foreign virtual land transfers into United States, V2
# QDR / Virtualland / 16 Feb 2021

# Modified 18 Feb 2021: Get the goods transfers for each alternative scenario, before converting VLTs in each scenario.
# Modified 04 Mar 2021: Remove pre-consumer waste reduction factors for foreign (cannot be affected by USA policy decisions)

# Code copied and modified from scripts in the FAO directory
# Processing done in FAO/extract_faostat.R

library(data.table)
library(purrr)

fp_fao <- 'data/cfs_io_analysis/faostat2017'

# Read all processed FAOSTAT data for 2017
walk(dir(fp_fao), ~ assign(gsub('.csv', '', .), fread(file.path(fp_fao, .)), envir = .GlobalEnv))

# Read harmonization table with BEA codes.
fao_codes_table <- fread('data/crossreference_tables/faostat_all_codes_harmonized.csv')

# Read relative consumption factors vs. baseline by BEA code for each scenario
scenario_factors_bea <- fread('data/cfs_io_analysis/bea_consumption_factors_diet_waste_scenarios.csv')

# Modify preconsumer waste scenarios --------------------------------------

# Pre-consumer waste reduction converted to baseline.
# Pre-consumer & consumer waste reduction converted to consumer waste reduction only.
# Keep them all there so that the foreign results can be integrated with USA results.

precon_cols <- grep("WR_preconsumer", names(scenario_factors_bea), value = TRUE)
baseline_cols <- grep("WR_baseline", names(scenario_factors_bea), value = TRUE)
con_cols <- grep("WR_consumer", names(scenario_factors_bea), value = TRUE)
allav_cols <- grep("WR_allavoidable", names(scenario_factors_bea), value = TRUE)

scenario_factors_bea[, (precon_cols) := scenario_factors_bea[, ..baseline_cols], with = FALSE]
scenario_factors_bea[, (allav_cols) := scenario_factors_bea[, ..con_cols], with = FALSE]

# Get proportion of trade sent to USA -------------------------------------

# Find imports sent to United States for each item.
# Create a separate trade matrix for each item.

# # Example for one item.
# maize <- trade_matrix %>% filter(item_code == 56)
# 
# # US reported, China partner
# maize %>% filter(`Reporter Country Code` == 231, `Partner Country Code` == 41)
# maize %>% filter(`Reporter Country Code` == 41, `Partner Country Code` == 231)

trade_tousa <- trade_matrix[partner_country_code == 231]

trade_tousa_qty <- trade_tousa[element == 'Export Quantity'] # Mostly weight but a few in head and number
trade_tousa_value <- trade_tousa[element == 'Export Value'] # All in dollars

# Get total production of each item ---------------------------------------

# We will need to use the quantity data because the production is also in quantity.

# Get all the item codes from trade to USA.
codes_trade <- unique(trade_tousa_qty$item_code)

# Get all the item codes for production across the five production dataframes.
codes_production <- Reduce(union, list(production_crops$item_code, production_cropsprocessed$item_code, production_livestock$item_code, production_livestockprimary$item_code, production_livestockprocessed$item_code))

noprod_codes <- setdiff(codes_trade, codes_production)

# What proportion of the weight of imports are not included in the production
trade_tousa_qty[unit == 'tonnes', hasprod := !item_code %in% noprod_codes][, .(value = sum(value)), by = hasprod]
# About 1/3 of it does not have production value, so that's problematic.

# Use harmonization table to assign trade codes that are not in the production table to a different part of the production table

# Rows in the production DF but not in the trade DF get their production divided evenly among the production codes in that row.
# Do for crops:

fao_codes_toassign <- fao_codes_table[!is.na(trade_code_for_production)]

production_crops_joined_tradecodes <- fao_codes_table[production_crops, on = c('code' = 'item_code', 'name_production' = 'item')]
setnames(production_crops_joined_tradecodes, old = c('code', 'name_production'), new = c('item_code', 'item'))

# All codes either already have a valid trade category or can be assigned one!
production_crops_joined_tradecodes[, c('element_code', 'parent_code', 'unit') := NULL]
# Spread to wide
production_crops_withcodes_wide <- dcast(production_crops_joined_tradecodes[!is.na(element)],
                                         ... ~ element, value.var = 'value', fill = NA)

# Convert trade code to a list column
production_crops_withcodes_wide[, trade_code_for_production := strsplit(trade_code_for_production, ";") %>% map(as.numeric)]

# Expand out the rows with >1 entry in trade_code_for_production to more than one row.
production_crops_withcodes_wide[, n_codes := map_int(trade_code_for_production, length)]

production_crops_withcodes_unnest <- tidyr::unnest(production_crops_withcodes_wide, trade_code_for_production, keep_empty = TRUE)
setDT(production_crops_withcodes_unnest)

production_crops_withcodes_unnest[, trade_code_final := ifelse(is.na(trade_code_for_production), item_code, trade_code_for_production)]


# Divide area harvested and production evenly by the number of rows in each code.
production_crops_retotaled <- production_crops_withcodes_unnest[, .(`Area harvested` = `Area harvested`/.N,
                                                                    Production = Production/.N,
                                                                    Yield = Yield),
                                                                by = .(area_code, area, BEA_code, crop_type, item_code, item, trade_code_final)]

# Now, we have all crop production, in units of land, by country, in categories corresponding to trade.


# Join production qty with trade to USA qty -------------------------------

# We now need to get the ratio of goods exported to USA in tonnes / goods produced in tonnes
# Then multiply the area harvested by this ratio, to get the area virtually exported to the USA.

# Sum up by trade code.
production_crops_summed_tojoin <- production_crops_retotaled[!BEA_code %in% 'aggregate', 
                                                             .(Area_harvested = sum(`Area harvested`, na.rm = TRUE),
                                                               Yield = weighted.mean(Yield, Production, na.rm = TRUE),
                                                               Production = sum(Production, na.rm = TRUE)),
                                                             by = .(area_code, area, crop_type, BEA_code, trade_code_final)]

trade_tousa_byweight <- trade_tousa_qty[unit %in% 'tonnes']
trade_tousa_byweight[, c('element_code', 'element', 'unit', 'partner_country_code', 'partner_country', 'hasprod') := NULL]
setnames(trade_tousa_byweight, old = 'value', new = 'Export_Qty')


trade_tousa_byweight[!item_code %in% production_crops_summed_tojoin$trade_code_final]
production_crops_summed_tojoin[!trade_code_final %in% trade_tousa_byweight$item_code] # All are in there!

# Join it up to get the % of each production sent to the US.

# First correct the names.
names(trade_tousa_byweight) <- c('country_code', 'country_name', 'item_code', 'item', 'export_qty')
names(production_crops_summed_tojoin) <- c('country_code', 'country_name', 'crop_type', 'BEA_code', 'item_code', 'area_harvested', 'yield', 'production')

# Also correct for proportions greater than 1.
production_crops_trade <- trade_tousa_byweight[production_crops_summed_tojoin, on = .(country_code, country_name, item_code)]
production_crops_trade <- production_crops_trade[production > 0 & area_harvested > 0]
production_crops_trade[is.na(export_qty), export_qty := 0]
production_crops_trade[, proportion_sent_USA := export_qty/production]
production_crops_trade[, virtual_land_transfer := area_harvested * pmin(proportion_sent_USA, 1)]

#### Multiply the virtual land transfers by country x BEA code by the consumption factor for that BEA code in each scenario.
# Convert scenario_factors_bea to long, then do a Cartesian left join
# Multiply the baseline VLT times the consumption factor.
scenario_factors_long <- melt(scenario_factors_bea, id.vars = c('BEA_389_code', 'BEA_389_def'), variable.name = 'scenario', value.name = 'consumption_factor')
scenario_factors_long[, BEA_389_def := NULL]

production_crops_trade_by_scenario <- scenario_factors_long[production_crops_trade, on = c('BEA_389_code' = 'BEA_code'), allow.cartesian = TRUE]
production_crops_trade_by_scenario[, virtual_land_transfer := virtual_land_transfer * consumption_factor]

VLT_sums_crop <- production_crops_trade_by_scenario[, .(VLT_crop = sum(virtual_land_transfer, na.rm = TRUE)), by = .(scenario, country_code, country_name, crop_type)]
VLT_sums_crop <- VLT_sums_crop[!is.na(scenario)]

# Virtual pastureland transfers -------------------------------------------

# We also need to determine the proportion of pastureland exported from other countries to the USA
# This cannot be done by commodity since there is no separate yield per land data for each type of animal
# Instead, we will get the total amount of pastureland in each country, the ratio of grazing animal
# production value in the country that's exported to the US, and multiply them.

# Need
# 1. pastureland total
# 2. total grazing animal production value
# 3. grazing animal production exported to USA


# Get proportion production sent by weight --------------------------------


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
prod_animal_wgt_bytype <- prod_animal_wgt_bytype[livestock %in% 'grazer']
prod_animal_wgt_bytype <- prod_animal_wgt_bytype[, .(production_qty = sum(value, na.rm = TRUE)), by = .(area_code, area, BEA_code, livestock_animal, livestock_product_type)]

trade_tousa_wgt_bytype <- fao_codes_table[trade_tousa_byweight_animal, on = c('code' = 'item_code')]
trade_tousa_wgt_bytype <- trade_tousa_wgt_bytype[livestock %in% 'grazer']
trade_tousa_wgt_bytype <- trade_tousa_wgt_bytype[, .(export_qty = sum(export_qty, na.rm = TRUE)), by = .(reporter_country_code, reporter_country, livestock_animal, livestock_product_type)]

# Join the two.
setnames(trade_tousa_wgt_bytype, old = c('reporter_country_code', 'reporter_country'), new = c('area_code', 'area'))
prod_animal_joined_trade <- trade_tousa_wgt_bytype[prod_animal_wgt_bytype, on = .(area_code, area, livestock_animal, livestock_product_type)]
prod_animal_joined_trade[is.na(export_qty), export_qty := 0]

# Now calculate the proportion of the tonnage exported, if you sum up every single grazer product produced.
grazer_prod_trade_totals <- prod_animal_joined_trade[, .(production_qty = sum(production_qty), export_qty = sum(export_qty)), 
                                                     by = .(area_code, area, BEA_code)]
# Check whether production is listed as being less than export for any locations. If so set export to zero, they are all very small amounts.
grazer_prod_trade_totals[production_qty < export_qty, export_qty := 0]

grazer_prod_trade_totals[, proportion_sent_to_usa := export_qty/production_qty]

# Total land use for each country -----------------------------------------

# Sum up all just to see
landuse_inputs[unit %in% '1000 ha', .(value = sum(value)), by = .(item_code, item)][order(-value), ]

# Cropland; land under permanent meadows and pastures, added up gives agricultural land area.
# Note that there is permanent and temporary cropping separated.
landuse_inputs[item_code %in% c(6620, 6655)]

# Just pastureland
pastureland_totals <- landuse_inputs[item_code %in% 6655, .(area_code, area, value)]
setnames(pastureland_totals, c('country_code', 'country_name', 'pastureland'))

setnames(grazer_prod_trade_totals, old = c('area_code', 'area'), new = c('country_code', 'country_name'))

VLT_sums_pasture <- pastureland_totals[grazer_prod_trade_totals, on = .(country_code, country_name)]

# Do Cartesian join and multiply by the consumption factors for each scenario
VLT_sums_pasture <- scenario_factors_long[VLT_sums_pasture, on = c('BEA_389_code' = 'BEA_code'), allow.cartesian = TRUE]

# Calculate VLT and sum across scenarios and countries
VLT_sums_pasture[, VLT_pasture := 1000 * pastureland * proportion_sent_to_usa * consumption_factor] # Convert to hectares for comparison with cropland.
VLT_sums_pasture <- VLT_sums_pasture[!is.na(scenario), .(VLT_pasture = sum(VLT_pasture, na.rm = TRUE)), by = .(scenario, country_code, country_name)]

# Write outputs -----------------------------------------------------------

# Write the intermediate stuff to show the FAO flows by BEA code
# Foreign production and trade of crops
fwrite(production_crops_trade, '/nfs/qread-data/cfs_io_analysis/fao_production_trade_crops.csv')
fwrite(prod_animal_joined_trade, '/nfs/qread-data/cfs_io_analysis/fao_production_trade_animals.csv')

# Just write the very basic outputs
VLT_sums_crop[, crop_type := paste0('VLT_', crop_type)]
VLT_sums_crop <- dcast(VLT_sums_crop, scenario + country_code + country_name ~ crop_type, fill = 0)

VLT_all <- merge(VLT_sums_crop, VLT_sums_pasture, all = TRUE)

fwrite(VLT_all, '/nfs/qread-data/cfs_io_analysis/fao_VLT_provisional.csv')


# Calculation of land inputs to USA from other countries, based on FAOSTAT trade data

# 1. Find the proportion of agricultural production for each crop in each country that is sent to the USA
#   1a. total value sent to USA
#   1b. total value of production for the whole country
#   1c. Divide 1a by 1b.
# 2. Find the total land and other inputs that go into agricultural production of each crop in each country
# 3. Multiply the results of 1 and 2 to get the final result: land and other inputs virtually transferred to the USA


# Load most recent fao data -----------------------------------------------

library(tidyverse)

is_local <- dir.exists('Q:/')

fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_out <- ifelse(is_local, 'Q:/cfs_io_analysis', '/nfs/qread-data/cfs_io_analysis')
fp_fao <- file.path(fp_out, 'faostat2017')
fp_crosswalks <- file.path(fp, 'crossreference_tables')

# Read all CSVs in directory.
walk(dir(fp_fao), ~ assign(gsub('.csv', '', .), read_csv(file.path(fp_fao, .)), envir = .GlobalEnv))

# Read harmonization table with BEA codes.
fao_codes_table <- read_csv(file.path(fp_crosswalks, 'faostat_all_codes_harmonized.csv'))

# Get proportion of trade sent to USA -------------------------------------

# Find imports sent to United States for each item.
# Create a separate trade matrix for each item.

# # Example for one item.
# maize <- trade_matrix %>% filter(item_code == 56)
# 
# # US reported, China partner
# maize %>% filter(`Reporter Country Code` == 231, `Partner Country Code` == 41)
# maize %>% filter(`Reporter Country Code` == 41, `Partner Country Code` == 231)

# For each country, get the amount of exports they report sending to USA.

# trade_matrix %>% 
#   group_by(`Reporter Country Code`, `Reporter Countries`, item_code, Item, element, Element, Unit) %>%
  
trade_tousa <- trade_matrix %>%
  filter(partner_country_code == 231)

trade_tousa_qty <- trade_tousa %>% filter(element == 'Export Quantity') # Mostly weight but a few in head and number
trade_tousa_value <- trade_tousa %>% filter(element == 'Export Value') # All in dollars


# Get total production of each item ---------------------------------------

# We will need to use the quantity data because the production is also in quantity.

# Get all the item codes from trade to USA.
codes_trade <- unique(trade_tousa_qty$item_code)

# Get all the item codes for production across the five production dataframes.
codes_production <- Reduce(union, list(production_crops$item_code, production_cropsprocessed$item_code, production_livestock$item_code, production_livestockprimary$item_code, production_livestockprocessed$item_code))

noprod_codes <- setdiff(codes_trade, codes_production)

# What proportion of the weight of imports are not included in the production
trade_tousa_qty %>% 
  filter(unit == 'tonnes') %>%
  mutate(hasprod = !item_code %in% noprod_codes) %>%
  group_by(hasprod) %>%
  summarize(value = sum(value))
# About 1/3 of it does not have production value, so that's problematic.

# Use harmonization table to assign trade codes that are not in the production table to a different part of the production table

# Rows in the production DF but not in the trade DF get their production divided evenly among the production codes in that row.
# Do for crops:

fao_codes_toassign <- fao_codes_table %>% filter(!is.na(trade_code_for_production))

production_crops_joined_tradecodes <- production_crops %>%
  left_join(fao_codes_table, by = c('item_code' = 'code', 'item' = 'name_production'))

# All codes either already have a valid trade category or can be assigned one!
# Convert trade code to a list column
production_crops_joined_tradecodes <- production_crops_joined_tradecodes %>%
  mutate(trade_code_for_production = strsplit(trade_code_for_production, ";") %>% map(as.numeric))

# Spread to wide
production_crops_withcodes_wide <- production_crops_joined_tradecodes %>% 
  select(-element_code, -parent_code, -unit) %>%
  pivot_wider(names_from = element, values_from = value, values_fill = NA)

# Expand out the rows with >1 entry in trade_code_for_production to more than one row.

production_crops_withcodes_unnest <- production_crops_withcodes_wide %>%
  mutate(n_codes = map_int(trade_code_for_production, length)) %>%
  unnest(trade_code_for_production) %>%
  mutate(trade_code_final = if_else(is.na(trade_code_for_production), item_code, trade_code_for_production))

# Divide area harvested and production evenly by the number of rows in each code.
production_crops_retotaled <- production_crops_withcodes_unnest %>%
  group_by(area_code, area, BEA_code, item_code, item) %>%
  mutate(`Area harvested` = `Area harvested`/n(),
         Production = Production/n())

# Now, we have all crop production, in units of land, by country, in categories corresponding to trade.


# Join production qty with trade to USA qty -------------------------------

# We now need to get the ratio of goods exported to USA in tonnes / goods produced in tonnes
# Then multiply the area harvested by this ratio, to get the area virtually exported to the USA.

production_crops_tojoin <- production_crops_retotaled %>% ungroup %>%
  select(-item_code, -item, -name_trade, -trade_code_for_production) %>%
  filter(!BEA_code %in% "aggregate")

# Sum up by trade code.
production_crops_summed_tojoin <- production_crops_tojoin %>%
  group_by(area_code, area, BEA_code, trade_code_final) %>%
  summarize(Area_harvested = sum(`Area harvested`, na.rm = TRUE),
            Yield = weighted.mean(Yield, Production, na.rm = TRUE),
            Production = sum(Production, na.rm = TRUE))

trade_tousa_byweight <- trade_tousa_qty %>%
  filter(unit %in% 'tonnes') %>%
  select(-element_code, -element, -unit, -partner_country_code, -partner_country) %>%
  rename(Export_Qty = value)

trade_tousa_byweight %>% filter(!item_code %in% production_crops_tojoin$trade_code_final)
production_crops_summed_tojoin %>% filter(!trade_code_final %in% trade_tousa_byweight$item_code) # All are in there!

# Join it up to get the % of each production sent to the US.

# First correct the names.
names(trade_tousa_byweight) <- c('country_code', 'country_name', 'item_code', 'item', 'export_qty')
names(production_crops_summed_tojoin) <- c('country_code', 'country_name', 'BEA_code', 'item_code', 'area_harvested', 'yield', 'production')

# Also correct for proportions greater than 1.
production_crops_trade <- left_join(production_crops_summed_tojoin, trade_tousa_byweight) %>%
  filter(production > 0, area_harvested > 0) %>%
  mutate(export_qty = if_else(is.na(export_qty), 0, export_qty),
         proportion_sent_USA = export_qty/production,
         virtual_land_transfer = area_harvested * min(proportion_sent_USA, 1)) 

VLT_sums_crop <- production_crops_trade %>% 
  group_by(country_code, country_name) %>% 
  summarize(VLT_crop = sum(virtual_land_transfer, na.rm = TRUE))  


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
prod_animal_all <- bind_rows(production_livestock, production_livestockprimary, production_livestockprocessed) %>%
  filter(!grepl('17..$|18..$', item_code))

# Get production by weight only.
prod_animal_wgt <- prod_animal_all %>% filter(unit == 'tonnes')

# Get trade to usa by weight for the animal crops
trade_tousa_byweight_animal <- trade_tousa_qty %>%
  filter(unit %in% 'tonnes') %>%
  select(-element_code, -element, -unit, -partner_country_code, -partner_country) %>%
  rename(export_qty = value) 

# Modified 14 Sept. Sum up trade and production codes by type of animal and type of product.
# This is because the trade and production codes do not overlap in some cases.
prod_animal_wgt_bytype <- prod_animal_wgt %>%
  left_join(fao_codes_table, by = c('item_code' = 'code')) %>%
  filter(livestock == 'grazer') %>%
  group_by(area_code, area, BEA_code, livestock_animal, livestock_product_type) %>%
  summarize(production_qty = sum(value, na.rm = TRUE))

trade_tousa_wgt_bytype <- trade_tousa_byweight_animal %>%
  left_join(fao_codes_table, by = c('item_code' = 'code')) %>%
  filter(livestock == 'grazer') %>%
  group_by(reporter_country_code, reporter_country, livestock_animal, livestock_product_type) %>%
  summarize(export_qty = sum(export_qty, na.rm = TRUE))

# Join the two.
prod_animal_joined_trade <- left_join(prod_animal_wgt_bytype, trade_tousa_wgt_bytype, 
                                      by = c('area_code' = 'reporter_country_code', 
                                             'area' = 'reporter_country', 
                                             'livestock_animal' = 'livestock_animal',
                                             'livestock_product_type' = 'livestock_product_type')) %>%
  mutate(export_qty = if_else(is.na(export_qty), 0, export_qty))
         
# Now calculate the proportion of the tonnage exported, if you sum up every single grazer product produced.

grazer_prod_trade_totals <- prod_animal_joined_trade %>%
  group_by(area_code, area) %>%
  summarize(production_qty = sum(production_qty), export_qty = sum(export_qty)) %>%
  mutate(proportion_sent_to_usa = export_qty/production_qty)


# Get proportion production sent by value ---------------------------------

# Get production by value only.
prod_animal_value <- value_production %>% filter(element_code == 152) %>%
  left_join(fao_codes_table, by = c('item_code' = 'code')) %>%
  filter(livestock == 'grazer')

prod_animal_value_bytype <- prod_animal_value %>%
  group_by(area_code, area, livestock_animal, livestock_product_type) %>%
  summarize(production_value = sum(value, na.rm = TRUE))

# Get trade to usa by weight for the animal crops
trade_tousa_byvalue_animal <- trade_tousa_value %>%
  select(-element_code, -element, -unit, -partner_country_code, -partner_country) %>%
  rename(export_value = value) 

trade_tousa_value_bytype <- trade_tousa_byvalue_animal %>%
  left_join(fao_codes_table, by = c('item_code' = 'code')) %>%
  filter(livestock == 'grazer') %>%
  group_by(reporter_country_code, reporter_country, BEA_code, livestock_animal, livestock_product_type) %>%
  summarize(export_value = sum(export_value, na.rm = TRUE))

# Join the two.
prod_animal_value_joined_trade <- left_join(prod_animal_value_bytype, trade_tousa_value_bytype, 
                                      by = c('area_code' = 'reporter_country_code', 
                                             'area' = 'reporter_country', 
                                             'livestock_animal' = 'livestock_animal',
                                             'livestock_product_type' = 'livestock_product_type')) %>%
  mutate(export_value = if_else(is.na(export_value), 0, export_value))

# Now calculate the proportion of the value exported, if you sum up every single grazer product produced.
grazer_prod_value_trade_totals <- prod_animal_value_joined_trade %>%
  group_by(area_code, area) %>%
  summarize(production_value = sum(production_value), export_value = sum(export_value)) %>%
  mutate(proportion_sent_to_usa = export_value/production_value)
# This is really strange so we should probably just do it by weight.

# Total land use for each country -----------------------------------------

# Sum up all just to see
landuse_inputs %>% 
  filter(unit == "1000 ha") %>%
  group_by(item_code, item) %>%
  summarize(value = sum(value)) %>%
  arrange(-value)

# Cropland; land under permanent meadows and pastures, added up gives agricultural land area.
landuse_inputs %>% 
  filter(item_code %in% c(6620, 6655))
        
# Just pastureland
pastureland_totals <- landuse_inputs %>% 
  filter(item_code %in% 6655) %>%
  select(area_code, area, value) %>%
  setNames(c('country_code', 'country_name', 'pastureland'))

VLT_sums_pasture <- grazer_prod_trade_totals %>% 
  rename(country_code = area_code, country_name = area) %>%
  left_join(pastureland_totals) %>%
  mutate(VLT_pasture = 1000 * pastureland * proportion_sent_to_usa) # Convert to hectares for comparison with cropland.


# Write outputs -----------------------------------------------------------

# Write the intermediate stuff to show the FAO flows by BEA code
# Foreign production and trade of crops
write_csv(production_crops_trade, '/nfs/qread-data/cfs_io_analysis/fao_production_trade_crops.csv')
write_csv(prod_animal_joined_trade, '/nfs/qread-data/cfs_io_analysis/fao_production_trade_animals.csv')

# Just write the very basic outputs
VLT_all <- full_join(VLT_sums_crop, VLT_sums_pasture) %>%
  select(country_code, country_name, VLT_crop, VLT_pasture)

write_csv(VLT_all, '/nfs/qread-data/cfs_io_analysis/fao_VLT_provisional.csv')

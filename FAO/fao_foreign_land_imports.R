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
# maize <- trade_matrix %>% filter(`Item Code` == 56)
# 
# # US reported, China partner
# maize %>% filter(`Reporter Country Code` == 231, `Partner Country Code` == 41)
# maize %>% filter(`Reporter Country Code` == 41, `Partner Country Code` == 231)

# For each country, get the amount of exports they report sending to USA.

# trade_matrix %>% 
#   group_by(`Reporter Country Code`, `Reporter Countries`, `Item Code`, Item, `Element Code`, Element, Unit) %>%
  
trade_tousa <- trade_matrix %>%
  filter(`Partner Country Code` == 231)

trade_tousa_qty <- trade_tousa %>% filter(Element == 'Export Quantity') # Mostly weight but a few in head and number
trade_tousa_value <- trade_tousa %>% filter(Element == 'Export Value') # All in dollars


# Get total production of each item ---------------------------------------

# We will need to use the quantity data because the production is also in quantity.

# Get all the item codes from trade to USA.
codes_trade <- unique(trade_tousa_qty$`Item Code`)

# Get all the item codes for production across the five production dataframes.
codes_production <- Reduce(union, list(production_crops$`Item Code`, production_cropsprocessed$`Item Code`, production_livestock$`Item Code`, production_livestockprimary$`Item Code`, production_livestockprocessed$`Item Code`))

noprod_codes <- setdiff(codes_trade, codes_production)

# What proportion of the weight of imports are not included in the production
trade_tousa_qty %>% 
  filter(Unit == 'tonnes') %>%
  mutate(hasprod = !`Item Code` %in% noprod_codes) %>%
  group_by(hasprod) %>%
  summarize(Value = sum(Value))
# About 1/3 of it does not have production value, so that's problematic.

# Use harmonization table to assign trade codes that are not in the production table to a different part of the production table

# Rows in the production DF but not in the trade DF get their production divided evenly among the production codes in that row.
# Do for crops:

fao_codes_toassign <- fao_codes_table %>% filter(!is.na(trade_code_for_production))

production_crops_joined_tradecodes <- production_crops %>%
  left_join(fao_codes_table, by = c('Item Code' = 'code', 'Item' = 'name_production'))

# All codes either already have a valid trade category or can be assigned one!
# Convert trade code to a list column
production_crops_joined_tradecodes <- production_crops_joined_tradecodes %>%
  mutate(trade_code_for_production = strsplit(trade_code_for_production, ";") %>% map(as.numeric))

# Spread to wide
production_crops_withcodes_wide <- production_crops_joined_tradecodes %>% 
  select(-`Element Code`, -Flag, -Note, -parent_code, -Year, -Unit) %>%
  pivot_wider(names_from = Element, values_from = Value, values_fill = NA)

# Expand out the rows with >1 entry in trade_code_for_production to more than one row.

production_crops_withcodes_unnest <- production_crops_withcodes_wide %>%
  mutate(n_codes = map_int(trade_code_for_production, length)) %>%
  unnest(trade_code_for_production) %>%
  mutate(trade_code_final = if_else(is.na(trade_code_for_production), `Item Code`, trade_code_for_production))

# Divide area harvested and production evenly by the number of rows in each code.
production_crops_retotaled <- production_crops_withcodes_unnest %>%
  group_by(`Area Code`, Area, BEA_code, `Item Code`, Item) %>%
  mutate(`Area harvested` = `Area harvested`/n(),
         Production = Production/n())

# Now, we have all crop production, in units of land, by country, in categories corresponding to trade.


# Join production qty with trade to USA qty -------------------------------

# We now need to get the ratio of goods exported to USA in tonnes / goods produced in tonnes
# Then multiply the area harvested by this ratio, to get the area virtually exported to the USA.

production_crops_tojoin <- production_crops_retotaled %>% ungroup %>%
  select(-`Item Code`, -Item, -name_trade, -trade_code_for_production) %>%
  filter(!BEA_code %in% "aggregate")

# Sum up by trade code.
production_crops_summed_tojoin <- production_crops_tojoin %>%
  group_by(`Area Code`, Area, BEA_code, trade_code_final) %>%
  summarize(Area_harvested = sum(`Area harvested`, na.rm = TRUE),
            Yield = weighted.mean(Yield, Production, na.rm = TRUE),
            Production = sum(Production, na.rm = TRUE))

trade_tousa_byweight <- trade_tousa_qty %>%
  filter(Unit %in% 'tonnes') %>%
  select(-`Element Code`, -Element, -Year, -Unit, -Flag, -`Partner Country Code`, -`Partner Countries`) %>%
  rename(Export_Qty = Value)

trade_tousa_byweight %>% filter(!`Item Code` %in% production_crops_tojoin$trade_code_final)
production_crops_summed_tojoin %>% filter(!trade_code_final %in% trade_tousa_byweight$`Item Code`) # All are in there!

# Join it up to get the % of each production sent to the US.

# First correct the names.
names(trade_tousa_byweight) <- c('country_code', 'country_name', 'item_code', 'item', 'export_qty')
names(production_crops_summed_tojoin) <- c('country_code', 'country_name', 'BEA_code', 'item_code', 'area_harvested', 'yield', 'production')

production_crops_trade <- left_join(production_crops_summed_tojoin, trade_tousa_byweight) %>%
  filter(production > 0, area_harvested > 0) %>%
  mutate(export_qty = if_else(is.na(export_qty), 0, export_qty),
         proportion_sent_USA = export_qty/production,
         virtual_land_transfer = area_harvested * proportion_sent_USA) 

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

# Grazing animal production
# This will be the production of livestock that is pastured.

# gross production in constant 2004-06 1000 I$
grossprod <- valueprod_rawdata %>% 
  filter(`Element Code` %in% 152, Year == 2016) %>%
  left_join(fao_codes_table, by = c("Item Code" = "code"))

# Get grazer production in thousand dollars
grazerproduction_bycountry <- grossprod %>% 
  filter(livestock == 'grazer') %>%
  group_by(`Area Code`, Area) %>%
  summarize(Value = sum(Value, na.rm = TRUE)) %>%
  setNames(c('country_code', 'country_name', 'value'))

# Grazing production exported to USA
trade_tousa_byvalue <- trade_tousa %>% 
  filter(Element == 'Export Value') %>%
  select(-`Element Code`, -Element, -Year, -Unit, -Flag, -`Partner Country Code`, -`Partner Countries`) %>%
  rename(Export_Value = Value)

grazerproduction_exportedtousa <- trade_tousa_byvalue %>%
  left_join(fao_codes_table, by = c("Item Code" = "code")) %>%
  filter(livestock == "grazer") %>%
  rename(country_code = `Reporter Country Code`, country_name = `Reporter Countries`) %>%
  group_by(country_code, country_name) %>%
  summarize(export_value = sum(Export_Value, na.rm = TRUE))

grazerproduction_bycountry <- left_join(grazerproduction_bycountry, grazerproduction_exportedtousa) %>%
  mutate(export_value = if_else(is.na(export_value), 0, export_value),
         proportion_exported = export_value/value)
# This has issues because some are >1 and some have extremely high numbers. Maybe some production is excluded from some
# countries' total production.
# Fix later

# Total land use for each country -----------------------------------------

# Sum up all just to see
landuse_inputs %>% 
  filter(Unit == "1000 ha") %>%
  group_by(`Item Code`, Item) %>%
  summarize(Value = sum(Value)) %>%
  arrange(-Value)

# Cropland; land under permanent meadows and pastures, added up gives agricultural land area.
landuse_inputs %>% 
  filter(`Item Code` %in% c(6620, 6655))
        
# Just pastureland
pastureland_totals <- landuse_inputs %>% 
  filter(`Item Code` %in% 6655) %>%
  select(`Area Code`, Area, Value) %>%
  setNames(c('country_code', 'country_name', 'pastureland'))

VLT_sums_pasture <- grazerproduction_bycountry %>% 
  left_join(pastureland_totals) %>%
  mutate(VLT_pasture = pastureland * proportion_exported)


# Write outputs -----------------------------------------------------------

# Just write the very basic outputs
VLT_all <- full_join(VLT_sums_crop, VLT_sums_pasture) %>%
  select(country_code, country_name, VLT_crop, VLT_pasture)

write_csv(VLT_all, '/nfs/qread-data/cfs_io_analysis/fao_VLT_provisional.csv')

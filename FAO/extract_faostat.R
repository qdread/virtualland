# Extract and process FAOSTAT data
# All data downloaded 31 Aug 2020

library(tidyverse)
fp_fao <- '/nfs/qread-data/raw_data/FAOSTAT/31aug2020'

# We need the following data
# Production and yield data on crops, crops processed, livestock, livestock primary, and livestock processed.
# Emissions agriculture total, Emissions land use total
# Emissions by sector, emissions intensities
# Land use environment, land use inputs
# Trade: crops and livestock, live animals, and detailed trade matrix.

# Read the production CSVs
prod_crop_rawdata <- read_csv(file.path(fp_fao, 'Production_Crops_E_All_Data_(Normalized).csv'))
prod_cropsprocessed_rawdata <- read_csv(file.path(fp_fao, 'Production_CropsProcessed_E_All_Data_(Normalized).csv'))
prod_livestock_rawdata <- read_csv(file.path(fp_fao, 'Production_Livestock_E_All_Data_(Normalized).csv'))
prod_livestockprimary_rawdata <- read_csv(file.path(fp_fao, 'Production_LivestockPrimary_E_All_Data_(Normalized).csv'))
prod_livestockprocessed_rawdata <- read_csv(file.path(fp_fao, 'Production_LivestockProcessed_E_All_Data_(Normalized).csv'))

# Read the emissions CSVs
emission_ag_rawdata <- read_csv(file.path(fp_fao, 'Emissions_Agriculture_Agriculture_total_E_All_Data_(Normalized).csv'))
emission_lu_rawdata <- read_csv(file.path(fp_fao, 'Emissions_Land_Use_Land_Use_Total_E_All_Data_(Normalized).csv'))
emission_bysector_rawdata <- read_csv(file.path(fp_fao, 'Environment_Emissions_by_Sector_E_All_Data_(Normalized).csv'))
emission_intensity_rawdata <- read_csv(file.path(fp_fao, 'Environment_Emissions_intensities_E_All_Data_(Normalized).csv'))

# Read the land use CSVs
landuse_env_rawdata <- read_csv(file.path(fp_fao, 'Environment_LandUse_E_All_Data_(Normalized).csv'))
landuse_inputs_rawdata <- read_csv(file.path(fp_fao, 'Inputs_LandUse_E_All_Data_(Normalized).csv'))

# Read the trade CSVs
trade_crops_rawdata <- read_csv(file.path(fp_fao, 'Trade_Crops_Livestock_E_All_Data_(Normalized).csv'))
trade_animals_rawdata <- read_csv(file.path(fp_fao, 'Trade_LiveAnimals_E_All_Data_(Normalized).csv'))
trade_matrix_rawdata <- read_csv(file.path(fp_fao, 'Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv'))

# Read the production value CSV
valueprod_rawdata <- read_csv(file.path(fp_fao, 'Value_of_Production_E_All_Data_(Normalized).csv'))

# Process production data -------------------------------------------------

# Use only the most recent data:
prod_crop_2018 <- prod_crop_rawdata %>% 
  filter(Year == 2018) %>%
  select(-`Year Code`, -Note)

table(prod_crop_2018$Element, prod_crop_2018$Flag, useNA = "always")
table(prod_crop_2018$Element, prod_crop_2018$Unit, useNA = "always")

# Separate by element and then make a country x crop matrix
country_x_crop_2018 <- prod_crop_2018 %>% 
  select(-Item, -`Element Code`, -Flag, -Unit) %>%
  group_by(Element) %>%
  group_modify(~ pivot_wider(., id_cols = c(`Area Code`, Area, Year), names_from = `Item Code`, values_from = Value, values_fill = 0))

# Look at the other production and yield dataframes.
table(prod_cropsprocessed_rawdata$Element, prod_cropsprocessed_rawdata$Unit) # All production in tonnes.
table(prod_cropsprocessed_rawdata$Item) # These are all processed products, incl. beer, wine, oil, sugar, cotton.

table(prod_livestock_rawdata$Element, prod_livestock_rawdata$Unit) # Some head, some No, some 1000head
table(prod_livestock_rawdata$Item)

table(prod_livestockprimary_rawdata$Element, prod_livestockprimary_rawdata$Unit) # Many different units
table(prod_livestockprimary_rawdata$Item)

table(prod_livestockprocessed_rawdata$Element, prod_livestockprocessed_rawdata$Unit) # All production in tonnes.
table(prod_livestockprocessed_rawdata$Item) # Products incl. milk, cheese, butter, lard, silk

# Process trade data ------------------------------------------------------

# Crops and livestock trade.
# Use only the most recent data:
trade_crop_2018 <- trade_crops_rawdata %>% 
  filter(Year == 2018) %>%
  select(-`Year Code`)

table(trade_crop_2018$Element, trade_crop_2018$Flag, useNA = "always")
table(trade_crop_2018$Element, trade_crop_2018$Unit, useNA = "always")

# Separate by element and then make a country x crop matrix
country_x_croptrade_2018 <- trade_crop_2018 %>% 
  select(-Item, -`Element Code`, -Flag, -Unit) %>%
  group_by(Element) %>%
  group_modify(~ pivot_wider(., id_cols = c(`Area Code`, Area, Year), names_from = `Item Code`, values_from = Value, values_fill = 0))

# Detailed trade matrix (very large CSV)
# This has the reporter and partner nations

trade_matrix_2018 <- trade_matrix_rawdata %>% 
  filter(Year == 2018) %>%
  select(-`Year Code`)

table(trade_matrix_2018$Element, trade_matrix_2018$Flag, useNA = "always")
table(trade_matrix_2018$Element, trade_matrix_2018$Unit, useNA = "always") # Some are in 1000 head, some are in head.

# Correct the "head" discrepancy.
trade_matrix_2018 <- trade_matrix_2018 %>%
  mutate(Value = if_else(Unit == "1000 Head", Value * 1000, Value),
         Unit = if_else(Unit == "1000 Head", "Head", Unit))

# View rows with Unit == "No"

trade_matrix_2018 %>% filter(Unit == "No") %>% pull(Item) %>% table # It is all beehives. But this also exists in value.

# Separate by element and then make matrices for all trade relationships including the USA
usa_trade_2018 <- trade_matrix_2018 %>%
  filter(`Reporter Country Code` == 231 | `Partner Country Code` == 231) 


# Process land use data ---------------------------------------------------

# landuse_env_rawdata and landuse_inputs_rawdata
# Only goes up to 2017
landuse_env_2017 <- landuse_env_rawdata %>% 
  filter(Year == 2017) %>%
  select(-`Year Code`)

table(landuse_env_2017$Item, landuse_env_2017$Element)

landuse_inputs_2017 <- landuse_inputs_rawdata %>%
  filter(Year == 2017) %>%
  select(-`Year Code`)

table(landuse_inputs_2017$Item, landuse_inputs_2017$Element) # This appears to be the actual land area in hectares.


# Process emissions data --------------------------------------------------

table(emission_ag_rawdata$Item, emission_ag_rawdata$Element) # We have emissions in 3 main GHGs, by source.
table(emission_lu_rawdata$Item, emission_lu_rawdata$Element) # We have emissions in CO2 and N2O, by source.
table(emission_bysector_rawdata$Item, emission_bysector_rawdata$Element) # Share of emissions in ag vs other sectors (prob do not need)
table(emission_intensity_rawdata$Item, emission_intensity_rawdata$Element) # CO2 per unit production for livestock, dairy, cereals, and rice (prob do not need)


# Process value data ------------------------------------------------------

table(valueprod_rawdata$Item)
codes_valueprod_names <- unique(valueprod_rawdata[,c('Item Code', 'Item')])

# Write all 2017 data to CSVs ---------------------------------------------

# For ease of processing, save 2017 data only to CSVs.
# Production: crop, cropsprocessed, livestock, livestockprimary, livestockprocessed
# Emissions: ag, land use
# Land use: Land use Env, Inputs
# Trade: crops, animals, detailed trade matrix.

oneyr <- function(dat, file, yr = 2017) {
  datfilter <- dat %>%
    filter(Year %in% yr) %>%
    select(-`Year Code`)
  write_csv(datfilter, file.path(fp_out, paste0(file, '.csv')))
}

fp_out <- '/nfs/qread-data/cfs_io_analysis/faostat2017'

oneyr(prod_crop_rawdata, 'production_crops')
oneyr(prod_cropsprocessed_rawdata, 'production_cropsprocessed', yr = 2014)
oneyr(prod_livestock_rawdata, 'production_livestock')
oneyr(prod_livestockprimary_rawdata, 'production_livestockprimary')
oneyr(prod_livestockprocessed_rawdata, 'production_livestockprocessed', yr = 2014)
oneyr(emission_ag_rawdata, 'emissions_agriculture')
oneyr(emission_lu_rawdata, 'emissions_landuse')
oneyr(landuse_env_rawdata, 'landuse_environmental')
oneyr(landuse_inputs_rawdata, 'landuse_inputs')
oneyr(trade_crops_rawdata, 'trade_crops')
oneyr(trade_animals_rawdata, 'trade_animals')
oneyr(trade_matrix_rawdata, 'trade_matrix')
oneyr(valueprod_rawdata, 'value_production', yr = 2016)

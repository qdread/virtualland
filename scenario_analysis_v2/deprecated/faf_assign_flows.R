# Use FAF to assign consumption in one FAF region to production in another
# QDR / Virtualland / 18 December 2020


# Load data ---------------------------------------------------------------

library(tidyverse)

is_local <- dir.exists('Q:/')

fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_out <- file.path(fp, 'cfs_io_analysis')

# FAF-level consumption in BEA categories
# FAF-level production in BEA categories
# FAF-level flows in BEA categories

faf_consumption <- read_csv(file.path(fp_out, 'faf_demand2012.csv'))
faf_production <- read_csv(file.path(fp_out, 'faf_production2012.csv'))
faf_flows <- read_csv(file.path(fp_out, 'FAF_all_flows_x_BEA.csv'))

# Lookup table of region names
faf_region_lookup <- read_csv(file.path(fp_out, 'faf_region_lookup.csv'))

# Ready data for joining --------------------------------------------------

faf_cons_prod <- full_join(faf_consumption, faf_production) %>%
  mutate(demand = demand * 1e6,
         CFS12_NAME = gsub('  ', ' ', CFS12_NAME))

# Fix region names
setdiff(faf_cons_prod$CFS12_NAME, faf_region_lookup$FAF_Region) # Laredo, Remainder of Alaska, Remainder of Idaho

faf_cons_prod <- faf_cons_prod %>%
  rename(FAF_Region = CFS12_NAME) %>%
  mutate(FAF_Region = case_when(FAF_Region == 'Remainder of Alaska' ~ 'Alaska',
                                FAF_Region == 'Remainder of Idaho'~ 'Idaho',
                                grepl('Laredo', FAF_Region) ~ 'Laredo, TX CFS Area',
                                TRUE ~ FAF_Region))

setdiff(faf_cons_prod$FAF_Region, faf_region_lookup$FAF_Region) # All are accounted for.

faf_cons_prod <- faf_cons_prod %>%
  left_join(faf_region_lookup[,c('Code','FAF_Region')])


# Proportionally assign flows ---------------------------------------------

# Each FAF region now has demand, in dollars, for goods (and production of goods).
# We need to determine the likely origin of all those goods to proportionally assign the land footprint of each region to a destination.

faf_cons_prod_ag <- faf_cons_prod %>% filter(BEA_code %in% faf_flows$BEA_Code)

# For each consuming region, divide its demand for each ag good proportionally among the producing regions that ship to it,
# based on the proportion of dollars coming in from those regions.
# This ignores a bit that things are processed and shipped. 

faf_flows_joined <- faf_flows %>% left_join(faf_cons_prod %>% select(-FAF_Region), by = c('dms_dest' = 'Code', 'BEA_Code' = 'BEA_code'))

faf_flows_assigned <- faf_flows_joined %>%
  filter(trade_type == 1) %>%
  group_by(dms_dest, BEA_Code) %>%
  mutate(demand_scaled = demand * (value_2012/sum(value_2012)))

faf_flows_assigned %>% select(dms_orig, dms_dest, dms_mode, BEA_Code, demand_scaled) %>% filter(dms_dest=="242",dms_mode==1,BEA_Code=="1111A0") %>% arrange(-demand_scaled) # This also does not work.

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


# Ready data for joining --------------------------------------------------

faf_cons_prod <- full_join(faf_consumption, faf_production) %>%
  mutate(demand = demand * 1e6,
         CFS12_NAME = gsub('  ', ' ', CFS12_NAME))

# FIXME The units are not the same and need to be fixed. Demand is probably in millions while production is in dollars.
# FIXME The consumption and production data just have text names while the FAF flows data have numerical codes
# But the text names are likely badly formatted. Need to fix this.
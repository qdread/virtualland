# Figures and summary statistics for "Scenario Analysis V2"
# QDR / Virtualland / 14 Jan 2021

# Proposed outline of figures

# 0. Consumption differences among scenarios, or basic figure showing how the scenarios differ among one another
# 1. Maps showing the trade in different agricultural products, in baseline and alternative scenarios.
# 2. Boxplot or similar summarizing the maps in fig1.
# 3. Maps showing the land flows associated with the trade in fig1.
# 4. Boxplot or similar summarizing the maps in fig3.
# 5. Maps showing the extinction threat exports/imports associated with the land flows in fig3.
# 6. Boxplot or similar summarizing the maps in fig5.


# Load data ---------------------------------------------------------------

library(tidyverse)

### Diet
diet_lancet <- read_csv('data/cfs_io_analysis/proportion_diet_lancet.csv')
diet_usa <- read_csv('data/cfs_io_analysis/proportion_diet_usaguidelines.csv')
lafa_joined <- read_csv('data/cfs_io_analysis/lafa_joined_with_diet_proportions.csv')

### County-level consumption and production
county_production <- read_csv('data/cfs_io_analysis/county_production2012.csv')
county_consumption <- read_csv('data/cfs_io_analysis/county_consumption2012_allscenarios.csv') # 300 MB

### Scenario factors by BEA category
bea_scenario_factors <- read_csv('data/cfs_io_analysis/bea_consumption_factors_diet_waste_scenarios.csv')

### State land exchange tables
load('data/cfs_io_analysis/state_land_exchange_tables.RData')

# Flows of goods and land between counties are loaded below (individually by scenario)

# Flows of land between ecoregions
tnc_landflows <- read_csv('data/cfs_io_analysis/scenarios/landflows_tnc_x_tnc_all_scenarios.csv')

# Flows of species 


# Food consumption differences among scenarios ----------------------------



# Flows of goods between counties -----------------------------------------

fp_goods <- 'data/cfs_io_analysis/county_consumption_csvs'


# Flows of land between counties ------------------------------------------

fp_landcounties <- 'data/cfs_io_analysis/county_landconsumption_csvs'


# Flows of land between ecoregions ----------------------------------------


# Flows of extinctions between ecoregions ---------------------------------



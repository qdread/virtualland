# Harmonize LAFA food production baseline and scenario values with BEA and supplement missing categories with FAO waste rates.
# QDR / Virtualland / 05 Jan 2021

library(tidyverse)

fp_diet <- 'data/raw_data/food_consumption/diet_guidelines'
fp_crosswalk <- 'data/crossreference_tables'
fp_out <- 'data/cfs_io_analysis'

lafa_df <- read_csv(file.path(fp_out, 'lafa_with_production_factors_diet_x_waste.csv'))

# 1. Get produced food weights for all the scenarios by first converting all to same units and then reshaping

# Primary weight lb/y is the produced weight in baseline case.
# For diet shifts only, just multiply primary weight by appropriate production factor.
# Then for waste reduction, multiply primary weight by the new total waste reduction change

# FIXME Harmonize with BEA categories

# FIXME Add FAO waste rates for the categories present in BEA but not LAFA (beverages)
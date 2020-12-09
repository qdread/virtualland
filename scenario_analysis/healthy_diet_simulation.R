# Diet shift simulation version 2
# QDR / Virtualland / 07 Dec 2020

# The original way of simulating this was naive, just getting rid of 50% of meat and replacing with evenly divided veg food
# Instead of doing this, and instead of manually doing the linear programming, 
# let's just get the lancet planetary health diet and USA healthy diet guidelines diets.
# See Lancet report (summary), Dietary Guidelines report, and Blackstone et al. which gave me the idea of the approach.


# Read dietary guidelines -----------------------------------

library(tidyverse)

is_local <- dir.exists('Q:/')

fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_diet <- file.path(fp, 'raw_data/food_consumption/diet_guidelines')
fp_crosswalk <- file.path(fp, 'crossreference_tables')
fp_out <- file.path(fp, 'cfs_io_analysis')

# Read Lancet dietary guidelines
diet_lancet <- read_csv(file.path(fp_diet, 'lancet_planetary_health_diet.csv'))
# Read USA dietary guidelines
diet_usa <- read_csv(file.path(fp_diet, 'us_dietary_guidelines_long.csv'))

# Harmonization of LAFA category names with dietary guidelines food pattern equivalent, and Lancet food categories
# Created from lafa_category_structure.csv
# Note that LAFA does not distinguish between whole and refined grains. So it is listed as just grains for both lancet and usa.
# Also note that we are not going to distinguish between saturated and unsaturated oil/fat because LAFA is by food category not fat type.
lafa_cat_lookup <- read_csv(file.path(fp_crosswalk, 'lafa_dietary_guidelines_crosswalk.csv'))

# Read LAFA
load(file.path(fp_out, 'lafa_processed.RData'))
lafa <- list(dairy, fat, fruit, grain, meat, sugar, veg)


# Process LAFA to single year and no aggregates ---------------------------

# We only want the most recent year with complete data, and only the primary (not aggregated) categories

lafa_agg_groups <- lafa_cat_lookup %>% select(starts_with('subgroup')) %>% unlist %>% unique

lafa_df <- bind_rows(lafa) %>%
  filter(!Category %in% lafa_agg_groups) %>%
  group_by(Category) %>%
  filter(Year == max(Year))

# Diet shift scenario analysis
# First, load the calories per weight for each of the LAFA categories.
# If all animal products have their calories reduced by 50%, we can replace their calories with (naively) an equal mix
# of all other foods.
# Meat shipments by weight should decrease by 50%
# Calculate the proportion of weight that other food needs to increase by.

# FIXME for now we will assume that the calories are allocated evenly across all the other food groups

# We also need to decrease the shipments of oilcrops and grains (feedstock) to reflect decreased meat consumption


# Load data ---------------------------------------------------------------


library(tidyverse)

is_local <- dir.exists('Q:/')

fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_cfs <- file.path(fp, 'raw_data/commodity_flows/CFS')
fp_faf <- file.path(fp, 'raw_data/commodity_flows/FAF')
fp_crosswalk <- file.path(ifelse(is_local, 'Q:', '/nfs/qread-data'), 'crossreference_tables')
fp_out <- file.path(ifelse(is_local, 'Q:', '/nfs/qread-data'), 'cfs_io_analysis')
fp_chaud <- file.path(fp, 'raw_data/biodiversity/chaudhary2015SI')
fp_github <- ifelse(is_local, '~/Documents/GitHub/foodwaste/virtualland', '~/virtualland')

# Load LAFA calories by weight
lafa_calories_weight <- read_csv(file.path(fp_out, 'lafa_calories_weight.csv'))

# Load FAF flows (domestic only)
# FIXME later add foreign
faf_flows <- read_csv(file.path(fp_out, 'FAF_all_flows_x_BEA.csv'))

# Do calorie conversion ---------------------------------------------------

# A few aggregated groups are included. All say total. Remove.
lafa_calories_weight <- lafa_calories_weight %>%
  mutate(grams_available = calories_available/calories_per_gram) %>%
  filter(!grepl('Total', category, ignore.case = TRUE))

# Sum up calories by group
cal_group_sums <- lafa_calories_weight %>% 
  group_by(animal_product) %>%
  summarize(calories_available = sum(calories_available))
veg_calories <- cal_group_sums$calories_available[cal_group_sums$animal_product %in% 'no']

lafa_calories_weight <- lafa_calories_weight %>%
  mutate(calories_reduced = if_else(animal_product %in% 'no', 0, calories_available * 0.5),
         grams_reduced = calories_reduced/calories_per_gram) 

calories_reduced_total <- sum(lafa_calories_weight$calories_reduced) # about 600 calories per day decreased, as ~1200 come from animals.

# Divide up the increased calories assuming all non animal foods are increased by a proportional amount
lafa_calories_weight <- lafa_calories_weight %>%
  mutate(calories_increased = if_else(animal_product %in% 'no', calories_available * calories_reduced_total/veg_calories, 0),
         grams_increased = calories_increased/calories_per_gram)

# Feed production must decrease by 50% so that meat production can decrease by the same rate
# FIXME this might not be exact because not all feed goes to animals producing meat people eat.

# Use weight changes to retotal shipments ---------------------------------

# Assign each of the 10 BEA codes to animal product to determine whether they will be increased or decreased.
# Basically the animal codes decrease by 50% and the plant codes increase by 37% (this equalizes calories)
# Then the feedstock codes decrease such that 43% increase by 37% and 57% decrease by 50% (net 12% decrease)
animal_product_codes <- c('112120', '1121A0', '112300', '112A00')
plant_product_codes <- c('111200', '111300', '111400', '111900')
feed_codes <- c('1111A0', '1111B0')

# We can multiply each shipment times the modified waste rate to get the reduced ones
flows_dietshift_domestic <- faf_flows %>%
  mutate(tons_reduced = case_when(BEA_Code %in% animal_product_codes ~ tons_2012 * 0.5,
                                  BEA_Code %in% plant_product_codes ~ tons_2012 * (1 + calories_reduced_total/veg_calories),
                                  BEA_Code %in% feed_codes ~ tons_2012 * (((1-0.57) * (1+calories_reduced_total/veg_calories)) + (0.57 * 0.5))))

write_csv(flows_dietshift_domestic, file.path(fp_out, 'scenarios/flows_dietshift_domestic_provisional.csv'))

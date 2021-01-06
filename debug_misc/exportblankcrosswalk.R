# Export crosswalks to fill out later
# LAFA to map to BEA
lafa_df %>% select(Category) %>% setNames('lafa') %>% write_csv('data/crossreference_tables/lafa_bea_crosswalk.csv')

# BEA table which will be mapped to LAFA (need to go the other way)
bea_all <- read_csv('data/crossreference_tables/naics_crosswalk_final.csv')

# Only get the rows that will be needed for mapping to LAFA.
# And the ones where personal consumption expenditure is greater than zero.

use2012 <- read_csv('data/raw_data/BEA/formatted/use2012.csv')
# Get the PCE for the 389 BEA goods from this table.
# F01000 is the code for personal consumption expenditures.
pce2012 <- setNames(use2012$F01000[1:389], use2012$X1[1:389])
nonzerocodes <- names(pce2012)[pce2012 > 0]

bea_food <- bea_all %>% filter(food_system %in% c('y', 'partial'), BEA_389_code %in% nonzerocodes) %>%
  select(BEA_389_code, BEA_389_def, food_system, stage, proportion_food)

write_csv(bea_food, 'data/crossreference_tables/bea_lafa_crosswalk.csv')

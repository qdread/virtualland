# Annual and permanent cropland.

source('FAF/combine_nass_susb_weightings.r') # To produce nass_naics_edited

nass_naics_join_bea <- nass_naics_edited %>%
  filter(NAICS %in% nass_naics_notredundant_modified) %>%
  left_join(nass_bea_lookup) 

permanent_codes <- c('1113', '11193') # All others are annual codes except for 1114 which is assumed as 50% annual, 50% permanent cropland.

nass_naics_join_bea <- nass_naics_join_bea %>%
  mutate(annual_cropland = case_when(NAICS %in% permanent_codes ~ 0,
                                     NAICS %in% '1114' ~ cropland/2,
                                     TRUE ~ cropland),
         permanent_cropland = case_when(NAICS %in% permanent_codes ~ cropland,
                                        NAICS %in% '1114' ~ cropland/2,
                                        TRUE ~ 0))

nass_bea_receipts_land <- nass_naics_join_bea %>%
  group_by(state_fips, state_abbrev, state_name, BEA_code) %>%
  summarize(across(c(n_operations, labor_hired_expense, labor_contract_expense, receipts, n_workers, annual_cropland, permanent_cropland, pastureland), sum))

write_csv(nass_bea_receipts_land, 'data/cfs_io_analysis/nass_workers_receipts_3landtypes_bea.csv')

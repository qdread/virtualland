# Get the totals of cropland and pastureland by ecoregion-county intersection
# Copied and modified from tabulate_nlcd_x_faf_x_tnc.R
# QDR/Virtualland/12 Jan 2021

# Modified 04 Feb 2021: add Alaska (2016) and Hawaii (2001)

library(tidyverse)
fp_out <- 'data/cfs_io_analysis'

# Use NLCD weighting for domestic, global crop/pasture datalayers weighting for foreign
nlcd_county_tnc <- read_csv('data/raw_data/landuse/output_csvs/NLCD_2016_countyTNC.csv', col_types = "ncccccccnncnccncncccnccccccncnnnnnnnnnnnnnnnnn")
nlcd_county_tnc_hi <- read_csv('data/raw_data/landuse/output_csvs/HI_NLCD_2001_countyTNC.csv', col_types = "ncccccccnncnccncncccnccccccncnnnnnnnnnnnnnn")
nlcd_county_tnc_ak <- read_csv('data/raw_data/landuse/output_csvs/AK_NLCD_2016_countyTNC.csv', col_types = "ncccccccnncnccncncccnccccccncnnnnnnnnnnnnnnnnnnnn")

# Fill in the Alaska and Hawaii rows: get rid of AK and HI from main df, retain only AK and HI from smaller ones,
# and then bind rows together.
nlcd_county_tnc <- nlcd_county_tnc %>% filter(!STATEFP %in% c('02', '15'))
nlcd_county_tnc_hi <- nlcd_county_tnc_hi %>% filter(STATEFP %in% c('15'))
nlcd_county_tnc_ak <- nlcd_county_tnc_ak %>% filter(STATEFP %in% c('02'))

nlcd_county_tnc <- bind_rows(nlcd_county_tnc, nlcd_county_tnc_hi, nlcd_county_tnc_ak)

# Reshape to long
# Aggregate by cropland, pastureland, and other codes. Pasture 81, Crop 82, Water 11, Other Land = all others.
nlcd_county_tnc_long <- nlcd_county_tnc %>%
  select(county, ECO_CODE, matches('^[0-9]{2}$')) %>%
  pivot_longer(-c(county, ECO_CODE), names_to = 'NLCD', values_to = 'n_pixels') %>%
  mutate(cover_class = case_when(NLCD == '81' ~ 'pasture',
                                 NLCD == '82' ~ 'crop',
                                 NLCD == '11' ~ 'water',
                                 TRUE ~ 'other'))

nlcd_county_tnc_ag <- nlcd_county_tnc_long %>%
  group_by(county, ECO_CODE, cover_class) %>%
  summarize(n_pixels = sum(n_pixels, na.rm = TRUE))

nlcd_county_tnc_ag_wide <- nlcd_county_tnc_ag %>%
  pivot_wider(id_cols = c(county, ECO_CODE), names_from = cover_class, values_from = n_pixels)

# # Test to see if crop pct makes sense 
# nlcd_county_tnc_ag_wide %>% 
#   mutate(croppct = crop / (crop+other+pasture)) %>%
#   ggplot() +
#   geom_sf(aes(fill = croppct), color = NA)

# Write intersected table to CSV
write_csv(nlcd_county_tnc_ag_wide, file.path(fp_out, 'NLCDcrop_county_x_TNC.csv'))

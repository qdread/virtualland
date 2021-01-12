# Get the totals of cropland and pastureland by ecoregion-county intersection
# Copied and modified from tabulate_nlcd_x_faf_x_tnc.R
# QDR/Virtualland/12 Jan 2021

library(tidyverse)
library(sf)

# Use NLCD weighting for domestic, global crop/pasture datalayers weighting for foreign
nlcd_county_tnc <- read_csv('data/raw_data/landuse/output_csvs/NLCD_2016_countyTNC.csv')

# Reshape to long
# Aggregate by cropland, pastureland, and other codes. Pasture 81, Crop 82, Water 11, Other Land = all others.
nlcd_county_tnc_long <- nlcd_county_tnc %>%
  pivot_longer(-X1, names_to = 'NLCD', values_to = 'n_pixels') %>%
  mutate(cover_class = case_when(NLCD == '81' ~ 'pasture',
                                 NLCD == '82' ~ 'crop',
                                 NLCD == '11' ~ 'water',
                                 TRUE ~ 'other'))

nlcd_county_tnc_ag <- nlcd_county_tnc_long %>%
  group_by(X1, cover_class) %>%
  summarize(n_pixels = sum(n_pixels, na.rm = TRUE))

nlcd_county_tnc_ag_wide <- nlcd_county_tnc_ag %>%
  pivot_wider(id_cols = X1, names_from = cover_class, values_from = n_pixels)

# Load the intersected polygon shapefile to get the codes that go with the regions
county_tnc_geo <- st_read('data/cfs_io_analysis/county_tnc_aea_intersect.gpkg')

county_tnc_geo <- county_tnc_geo %>%
  mutate(X1 = 0:(nrow(.)- 1)) %>%
  left_join(nlcd_county_tnc_ag_wide)

# Test to see if crop pct makes sense 
county_tnc_geo %>% 
  mutate(croppct = crop / (crop+other+pasture)) %>%
  ggplot() +
  geom_sf(aes(fill = croppct), color = NA)

# Write intersected table to CSV
write_csv(county_tnc_geo %>% st_drop_geometry, file.path(fp_out, 'NLCDcrop_county_x_TNC.csv'))

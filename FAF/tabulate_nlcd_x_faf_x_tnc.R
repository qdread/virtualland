# Get the totals of cropland and pastureland by ecoregion-FAF intersection
# Moved this bit of code from faf_land_transfer_to_tnc.R
# QDR/Virtualland/1 Oct 2020

library(tidyverse)
library(units)
library(sf)

is_local <- dir.exists('Q:/')

fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_cfs <- file.path(fp, 'raw_data/commodity_flows/CFS')
fp_faf <- file.path(fp, 'raw_data/commodity_flows/FAF')
fp_crosswalk <- file.path(ifelse(is_local, 'Q:', '/nfs/qread-data'), 'crossreference_tables')
fp_out <- file.path(ifelse(is_local, 'Q:', '/nfs/qread-data'), 'cfs_io_analysis')
fp_github <- ifelse(is_local, '~/Documents/GitHub/foodwaste/virtualland', '~/virtualland')

# Use NLCD weighting for domestic, global crop/pasture datalayers weighting for foreign
nlcd_cfs_tnc <- read_csv(file.path(fp, 'raw_data/landuse/output_csvs/NLCD_2016_CFSTNC.csv'))

# Reshape to long
# Aggregate by cropland, pastureland, and other codes. Pasture 81, Crop 82, Water 11, Other Land = all others.
nlcd_cfs_tnc_long <- nlcd_cfs_tnc %>%
  pivot_longer(-X1, names_to = 'NLCD', values_to = 'n_pixels') %>%
  mutate(cover_class = case_when(NLCD == '81' ~ 'pasture',
                                 NLCD == '82' ~ 'crop',
                                 NLCD == '11' ~ 'water',
                                 TRUE ~ 'other'))

nlcd_cfs_tnc_ag <- nlcd_cfs_tnc_long %>%
  group_by(X1, cover_class) %>%
  summarize(n_pixels = sum(n_pixels, na.rm = TRUE))

nlcd_cfs_tnc_ag_wide <- nlcd_cfs_tnc_ag %>%
  pivot_wider(id_cols = X1, names_from = cover_class, values_from = n_pixels)

# Load the intersected polygon shapefile to get the codes that go with the regions
library(sf)
cfs_tnc_geo <- st_read(file.path(fp_out, 'cfs_tnc_aea_intersect.gpkg'))

cfs_tnc_geo <- cfs_tnc_geo %>%
  mutate(X1 = 0:452) %>%
  left_join(nlcd_cfs_tnc_ag_wide)

# Test to see if crop pct makes sense 
cfs_tnc_geo %>% 
  mutate(croppct = crop / (crop+other+pasture)) %>%
  ggplot() +
  geom_sf(aes(fill = croppct))

st_geometry(cfs_tnc_geo) <- NULL

# Write intersected table to CSV
write_csv(cfs_tnc_geo, file.path(fp_out, 'NLCDcrop_FAF_x_TNC.csv'))
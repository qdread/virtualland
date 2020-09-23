# Example of transport optimization for a single agricultural good
# Assumes that cost to transport between FAF regions is exactly equivalent to the distance between their centroids

# Need to do
# ==========

# 1. Get distance between centroids of FAF regions
# 2. Instead of production and consumption, just use the total amount of a given good summed by destination and by origin from FAF
#   - This can either be the raw SCTG code or a BEA code
# 3. Use the values created from 1 and 2 to define a, b, and costm for the transport() function
# 4. Run the transport() function, maybe with other control parameters


library(tidyverse)
library(sf)
library(transport)
library(units)

is_local <- dir.exists('Q:/')

fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_cfs <- file.path(fp, 'raw_data/commodity_flows/CFS')
fp_faf <- file.path(fp, 'raw_data/commodity_flows/FAF')
fp_crosswalk <- file.path(ifelse(is_local, 'Q:', '/nfs/qread-data'), 'crossreference_tables')
fp_out <- file.path(ifelse(is_local, 'Q:', '/nfs/qread-data'), 'cfs_io_analysis')
fp_github <- ifelse(is_local, '~/Documents/GitHub/foodwaste/virtualland', '~/virtualland')

cfsmap <- st_read(dsn = file.path(fp_faf, 'Freight_Analysis_Framework_Regions/cfs_aea.gpkg'))

# Sort cfsmap by the code
cfsmap <- cfsmap %>% arrange(Code)

# Centroids of CFS map
cfs_centroid <- st_centroid(cfsmap)

# Distance between pairs
cfs_centroid_dist <- st_distance(cfs_centroid, by_element = FALSE)
# Coerce distance matrix to plain numeric matrix
cfs_centroid_distmat <- drop_units(cfs_centroid_dist)

# Load FAF data to get the "production and consumption" values
faf_flows <- read_csv(file.path(fp_out, 'FAF_all_flows_x_BEA.csv'))

# Sum for all BEA_codes
# Sum tonnage by all origins and all destinations
# Only use domestic for now

faf_sums_origin <- faf_flows %>%
  filter(trade_type == 1) %>%
  group_by(dms_orig, BEA_Code) %>%
  summarize(tons = sum(tons_2012, na.rm = TRUE))

faf_sums_destination <- faf_flows %>%  
  filter(trade_type == 1) %>%
  group_by(dms_dest, BEA_Code) %>%
  summarize(tons = sum(tons_2012, na.rm = TRUE))

faf_sums_origin_wide <- faf_sums_origin %>%
  pivot_wider(names_from = BEA_Code, values_from = tons, values_fill = 0)

faf_sums_destination_wide <- faf_sums_destination %>%
  pivot_wider(names_from = BEA_Code, values_from = tons, values_fill = 0)

# Washington DC has no origin row so add that into the origin data with all zeroes
setdiff(faf_sums_destination_wide$dms_dest, faf_sums_origin_wide$dms_orig)

faf_sums_origin_wide <- faf_sums_origin_wide %>%
  ungroup %>%
  add_row(dms_orig = '111') %>%
  arrange(dms_orig) %>%
  mutate_if(is.numeric, ~ if_else(is.na(.), 0, .))

colSums(faf_sums_origin_wide[,-1]) == colSums(faf_sums_destination_wide[,-1]) # Yes.

# Check all orders are the same
all.equal(cfsmap$Code, faf_sums_origin_wide$dms_orig) # Yes
all.equal(cfsmap$Code, faf_sums_destination_wide$dms_dest) # Yes

# Get the observed transport between each pair
faf_sums_paired_x_bea <- faf_flows %>%
  filter(trade_type == 1) %>%
  group_by(dms_orig, dms_dest, BEA_Code) %>%
  summarize(tons = sum(tons_2012, na.rm = TRUE),
            tmiles = sum(tmiles_2012, na.rm = TRUE))

# Test transport out using first code: oilseeds
oilseed_production <- faf_sums_origin_wide %>% pull(`1111A0`)
oilseed_consumption <- faf_sums_destination_wide %>% pull(`1111A0`)

# Diagnostic map to see if this is correct
cfsmap <- cfsmap %>% mutate(oilseed_production = oilseed_production, oilseed_consumption = oilseed_consumption)

ggplot(cfsmap, aes(fill = oilseed_production)) + geom_sf()
ggplot(cfsmap, aes(fill = oilseed_consumption)) + geom_sf()

oilseed_opt <- transport(a = oilseed_production,
                         b = oilseed_consumption,
                         costm = cfs_centroid_distmat,
                         method = 'revsimplex',
                         fullreturn = TRUE)

# Compare the observed transport distances with the optimal.
# Get optimal transport in the same format as observed.
oilseed_observed <- faf_sums_paired_x_bea %>%
  filter(BEA_Code == '1111A0') %>%
  mutate(from = match(dms_orig, faf_sums_origin_wide$dms_orig),
         to = match(dms_dest, faf_sums_origin_wide$dms_orig))

# Convert the distance in meters to miles and then multiply by the mass to get the tmiles
oilseed_optimal <- oilseed_opt %>%
  mutate(dms_orig = faf_sums_origin_wide$dms_orig[from],
         dms_dest = faf_sums_origin_wide$dms_orig[to])

oilseed_obs_opt <- full_join(oilseed_observed, oilseed_optimal) %>%
  mutate(distance = map2_dbl(from, to, ~ cfs_centroid_dist[.x, .y]),
         distance_miles = distance %>% set_units(m) %>% set_units(miles),
         tmiles_observed = tons * distance_miles,
         tmiles_optimal = mass * distance_miles) 

oilseed_obs_opt %>%
  ungroup %>%
  summarize(sum_obs = sum(tmiles_observed, na.rm = TRUE),
            sum_opt = sum(tmiles_optimal, na.rm = TRUE))

oilseed_obs_opt_toplot <- oilseed_obs_opt %>%
  select(dms_orig, dms_dest, from, to, tmiles_observed, tmiles_optimal) %>%
  pivot_longer(c(tmiles_observed, tmiles_optimal), values_to = 'tmiles') %>%
  mutate(name = gsub('tmiles_', '', name), tmiles = drop_units(tmiles)) %>%
  mutate(tmiles = if_else(is.na(tmiles), 0, tmiles))

# Log scale
ggplot(oilseed_obs_opt_toplot, aes(x = tmiles, fill = name)) +
  scale_x_log10() +
  geom_density(alpha = 0.5) +
  theme_minimal()

# Truncated arithmetic scale
ggplot(oilseed_obs_opt_toplot, aes(x = tmiles, fill = name)) +
  scale_x_continuous(limits = c(0, 25000)) +
  geom_density(alpha = 0.5) +
  theme_minimal()

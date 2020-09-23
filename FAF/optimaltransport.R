# Optimal transport plans for each BEA good.
# Assumes that cost to transport between FAF regions is exactly equivalent to the distance between their centroids

# Need to do
# ==========

# 1. Get distance between centroids of FAF regions
# 2. Instead of production and consumption, just use the total amount of a given good summed by destination and by origin from FAF
#   - This can either be the raw SCTG code or a BEA code
# 3. Use the values created from 1 and 2 to define a, b, and costm for the transport() function
# 4. Run the transport() function


# Load data ---------------------------------------------------------------


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

# Load crosswalk to replace BEA codes with informative names
bea_codes <- read_csv(file.path(fp_crosswalk, 'all_codes.csv')) %>%
  select(sector_code_uppercase, sector_desc_demand) %>%
  setNames(c('BEA_Code', 'BEA_Name')) %>%
  filter(BEA_Code %in% faf_flows$BEA_Code)

# Use shorter names
bea_codes$BEA_Name <- c('soybeans & oilseeds', 'grains', 'vegetables & potatoes', 'fruits & nuts', 'greenhouse crops',
                        'tobacco, cotton, sugar, peanuts, etc.', 'dairy', 'cattle', 'poultry', 'other animals')

# Process data for transport() --------------------------------------------

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


# Calculate transport plan for each good ----------------------------------

goods <- unique(faf_sums_origin$BEA_Code)

# Optimal transport plan for each of the 10 goods
transport_plans <- imap_dfr(goods, ~ data.frame(BEA_Code = .,
                                                transport(a = deframe(faf_sums_origin_wide[,.]),
                                                          b = deframe(faf_sums_destination_wide[,.]),
                                                          costm = cfs_centroid_distmat,
                                                          method = 'revsimplex')))


# Process output data -----------------------------------------------------

# Compare the observed transport distances with the optimal.
# Get optimal transport in the same format as observed.
transport_observed <- faf_sums_paired_x_bea %>%
  mutate(from = match(dms_orig, faf_sums_origin_wide$dms_orig),
         to = match(dms_dest, faf_sums_origin_wide$dms_orig)) %>%
  rename(mass_observed = tons) %>%
  select(-tmiles)

transport_optimal <- transport_plans %>%
  mutate(dms_orig = faf_sums_origin_wide$dms_orig[from],
         dms_dest = faf_sums_origin_wide$dms_orig[to]) %>%
  rename(mass_optimal = mass)

# Convert the distance in meters to miles and then multiply by the mass to get the tmiles
transport_obs_opt <- full_join(transport_observed, transport_optimal) %>%
  mutate(distance = map2_dbl(from, to, ~ cfs_centroid_dist[.x, .y]),
         distance_miles = distance %>% set_units(m) %>% set_units(miles),
         tmiles_observed = mass_observed * distance_miles,
         tmiles_optimal = mass_optimal * distance_miles) 

# Summary of total distances for observed and optimal transport
transport_dists <- transport_obs_opt %>%
  group_by(BEA_Code) %>%
  summarize(sum_obs = sum(tmiles_observed, na.rm = TRUE),
            sum_opt = sum(tmiles_optimal, na.rm = TRUE),
            diff = sum_obs - sum_opt,
            improvement = diff/sum_obs)


# Plot results ------------------------------------------------------------

transport_obs_opt_toplot <- transport_obs_opt %>%
  select(BEA_Code, dms_orig, dms_dest, from, to, tmiles_observed, tmiles_optimal) %>%
  pivot_longer(c(tmiles_observed, tmiles_optimal), values_to = 'tmiles') %>%
  mutate(name = gsub('tmiles_', '', name), tmiles = drop_units(tmiles)) %>%
  mutate(tmiles = if_else(is.na(tmiles), 0, tmiles)) %>%
  left_join(bea_codes)

# Log scale
p_logdists <- ggplot(transport_obs_opt_toplot, aes(x = tmiles, fill = name)) +
  scale_x_log10(name = 'ton-miles') +
  geom_density(alpha = 0.8) +
  facet_wrap(~ BEA_Name) +
  scale_fill_brewer(palette = 'Set3') +
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = c(0.8, 0.2))

# Truncated arithmetic scale
ggplot(transport_obs_opt_toplot, aes(x = tmiles, fill = name)) +
  scale_x_continuous(name = 'ton-miles', limits = c(0, 25000)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ BEA_Name) +
  scale_fill_manual(values = c('red', 'blue')) +
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = c(0.8, 0.2))

# Just the totals
p_bars <- transport_dists %>%
  left_join(bea_codes) %>%
  select(-diff, -improvement) %>%
  pivot_longer(-c(BEA_Code, BEA_Name), values_to = 'distance') %>%
  ggplot(aes(x = BEA_Name, y = drop_units(distance), fill = name)) +
  geom_col(position = 'dodge', color = 'black', lwd = 0.2) +
  theme_minimal() +
  scale_y_continuous(name = 'ton-miles') +
  scale_x_discrete(name = 'BEA category') +
  scale_fill_brewer(palette = 'Set3', labels = c('observed', 'optimal')) +
  coord_flip() +
  theme(legend.position = c(0.8, 0.8), legend.title = element_blank())

# Write results to file, and save figures
save(faf_sums_paired_x_bea, faf_sums_destination_wide, faf_sums_origin_wide, transport_obs_opt, transport_obs_opt_toplot,
     file = file.path(fp_out, 'optimal_transport_results.RData'))

ggsave(file.path(fp_out, 'plots/optimal_transport_log_density.png'), p_logdists)
ggsave(file.path(fp_out, 'plots/optimal_transport_bar_plots.png'), p_bars)

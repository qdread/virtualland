# Visualize preliminary biodiversity results

# Load data ---------------------------------------------------------------

library(tidyverse)

is_local <- dir.exists('Q:/')

fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_cfs <- file.path(fp, 'raw_data/commodity_flows/CFS')
fp_faf <- file.path(fp, 'raw_data/commodity_flows/FAF')
fp_crosswalk <- file.path(fp, 'crossreference_tables')
fp_out <- file.path(fp, 'cfs_io_analysis')
fp_chaud <- file.path(fp, 'raw_data/biodiversity/chaudhary2015SI')
fp_github <- ifelse(is_local, '~/Documents/GitHub/foodwaste/virtualland', '~/virtualland')
fp_scen <- file.path(fp_out, 'scenarios')

splost <- read_csv(file.path(fp_scen, 'species_lost_scenarios_provisional.csv'))

# Sum biodiversity footprints ---------------------------------------------

# Sum incoming biodiversity threats by region, land use type, and taxon (for each CF)
bio_import_sums <- splost %>%
  group_by(scenario, TNC_dest, landuse, CF, taxon, stat) %>%
  summarize(species_lost = sum(species_lost, na.rm = TRUE)) %>%
  ungroup %>%
  filter(complete.cases(.))

# Aggregate to a single value for each region x CF
bio_import_sums_agg <- bio_import_sums %>%
  group_by(scenario, TNC_dest, CF, stat) %>%
  summarize(species_lost = sum(species_lost)) %>%
  pivot_wider(names_from = stat, values_from = species_lost) 

# Visualize summary totals ------------------------------------------------

ggplot(bio_import_sums_agg, aes(x = median, fill = scenario)) +
  facet_wrap(~ CF, scales = 'free_x') +
  geom_histogram(bins = 20, alpha = 0.5, position = 'identity', color = 'black', lwd = 0.2) +
  scale_fill_brewer(palette = 'Dark2') +
  theme_minimal() +
  ggtitle('Virtual biodiversity transfers: USA')

ggplot(bio_import_sums_agg, aes(x = scenario, y = median, fill = scenario)) +
  geom_boxplot() +
  facet_wrap(~ CF, scales = 'free_y') +
  scale_fill_brewer(palette = 'Dark2') +
  theme_minimal() +
  ggtitle('Virtual biodiversity transfers: USA')

grandtotals <- bio_import_sums_agg %>% group_by(scenario, CF) %>% summarize(sp = sum(median)) %>% pivot_wider(names_from=scenario,values_from=sp)
# Domestic extinction bar plots by scenario x land type
# QDR / Virtualland / 25 Mar 2021

# Modified 31 Mar 2021: use the precomputed domestic and foreign total extinctions from figs_v2_panelmaps.R

# Total extinctions across land types, domestic only
extinction_sums_byscenario <- county_extinction_flow_sums %>%
  filter(!land_use %in% 'total') %>%
  mutate(scenario_diet = factor(scenario_diet, levels = diet_levels_ordered),
         scenario_waste = factor(scenario_waste, levels = waste_levels_ordered),
         land_use = factor(land_use, levels = land_levels_ordered)) %>%
  group_by(scenario_diet, scenario_waste, land_use) %>%
  summarize(extinction_domestic = sum(extinction_inbound),
            extinction_foreign = sum(extinction_inbound_foreign),
            extinction_total = sum(extinction_inbound_total))

# Plot
diet_medium_names <- c('baseline', 'healthy\nUS-style', 'healthy\nMediterranean', 'vegetarian', 'planetary\nhealth')

# Fixed y axis
# Domestic only
p_extinction_sums_fixed <- ggplot(extinction_sums_byscenario, aes(y = extinction_domestic, x = scenario_diet, group = scenario_waste, fill = scenario_waste)) +
  geom_col(position = 'dodge') +
  facet_wrap(~ land_use, nrow = 3) +
  scale_x_discrete(name = 'diet scenario', labels = diet_medium_names) +
  scale_fill_brewer(name = 'waste scenario', palette = 'Dark2', labels = waste_long_names$long_name) +
  scale_y_continuous(name = 'species committed to extinction', expand = expansion(mult = c(0, 0.03))) +
  theme(legend.position = 'bottom', 
        legend.text = element_text(size = rel(.7))) +
  guides(fill=guide_legend(nrow = 2, byrow = FALSE))

# Domestic and foreign combined
p_extinction_sums_fixed_total <- ggplot(extinction_sums_byscenario, aes(y = extinction_total, x = scenario_diet, group = scenario_waste, fill = scenario_waste)) +
  geom_col(position = 'dodge') +
  facet_wrap(~ land_use, nrow = 3) +
  scale_x_discrete(name = 'diet scenario', labels = diet_medium_names) +
  scale_fill_brewer(name = 'waste scenario', palette = 'Dark2', labels = waste_long_names$long_name) +
  scale_y_continuous(name = 'species committed to extinction', expand = expansion(mult = c(0, 0.03))) +
  theme(legend.position = 'bottom', 
        legend.text = element_text(size = rel(.7))) +
  guides(fill=guide_legend(nrow = 2, byrow = FALSE))

# Free y axis
# Domestic only
p_extinction_sums_free <- ggplot(extinction_sums_byscenario, aes(y = extinction_domestic, x = scenario_diet, group = scenario_waste, fill = scenario_waste)) +
  geom_col(position = 'dodge') +
  facet_wrap(~ land_use, nrow = 3, scales = 'free_y') +
  scale_x_discrete(name = 'diet scenario', labels = diet_medium_names) +
  scale_fill_brewer(name = 'waste scenario', palette = 'Dark2', labels = waste_long_names$long_name) +
  scale_y_continuous(name = 'species committed to extinction', expand = expansion(mult = c(0, 0.03))) +
  theme(legend.position = 'bottom', 
        legend.text = element_text(size = rel(.7))) +
  guides(fill=guide_legend(nrow = 2, byrow = FALSE))

p_extinction_sums_free_total <- ggplot(extinction_sums_byscenario, aes(y = extinction_total, x = scenario_diet, group = scenario_waste, fill = scenario_waste)) +
  geom_col(position = 'dodge') +
  facet_wrap(~ land_use, nrow = 3, scales = 'free_y') +
  scale_x_discrete(name = 'diet scenario', labels = diet_medium_names) +
  scale_fill_brewer(name = 'waste scenario', palette = 'Dark2', labels = waste_long_names$long_name) +
  scale_y_continuous(name = 'species committed to extinction', expand = expansion(mult = c(0, 0.03))) +
  theme(legend.position = 'bottom', 
        legend.text = element_text(size = rel(.7))) +
  guides(fill=guide_legend(nrow = 2, byrow = FALSE))

ggsave(file.path(fp_fig, 'extinction_sums_fixed_y.png'), p_extinction_sums_fixed, height = 7, width = 5, dpi = 400)
ggsave(file.path(fp_fig, 'extinction_sums_free_y.png'), p_extinction_sums_free, height = 7, width = 5, dpi = 400)
ggsave(file.path(fp_fig, 'extinction_sums_fixed_y_total.png'), p_extinction_sums_fixed_total, height = 7, width = 5, dpi = 400)
ggsave(file.path(fp_fig, 'extinction_sums_free_y_total.png'), p_extinction_sums_free_total, height = 7, width = 5, dpi = 400)


# Versions with only 10 scenarios -----------------------------------------

extinction_sums_10scen <- extinction_sums_byscenario %>% filter(scenario_waste %in% c('baseline', 'allavoidable'))
waste_names2 <- waste_long_names$long_name[c(1, 4)]

# Fixed y axis
# Domestic only
p_extinction_sums_fixed <- ggplot(extinction_sums_10scen, aes(y = extinction_domestic, x = scenario_diet, group = scenario_waste, fill = scenario_waste)) +
  geom_col(position = 'dodge') +
  facet_wrap(~ land_use, nrow = 3) +
  scale_x_discrete(name = 'diet scenario', labels = diet_medium_names) +
  scale_fill_brewer(name = 'waste scenario', palette = 'Dark2', labels = waste_names2) +
  scale_y_continuous(name = 'species committed to extinction', expand = expansion(mult = c(0, 0.03))) +
  theme(legend.position = 'bottom', 
        legend.text = element_text(size = rel(.7))) +
  guides(fill=guide_legend(nrow = 2, byrow = FALSE))

# Domestic and foreign combined
p_extinction_sums_fixed_total <- ggplot(extinction_sums_10scen, aes(y = extinction_total, x = scenario_diet, group = scenario_waste, fill = scenario_waste)) +
  geom_col(position = 'dodge') +
  facet_wrap(~ land_use, nrow = 3) +
  scale_x_discrete(name = 'diet scenario', labels = diet_medium_names) +
  scale_fill_brewer(name = 'waste scenario', palette = 'Dark2', labels = waste_names2) +
  scale_y_continuous(name = 'species committed to extinction', expand = expansion(mult = c(0, 0.03))) +
  theme(legend.position = 'bottom', 
        legend.text = element_text(size = rel(.7))) +
  guides(fill=guide_legend(nrow = 2, byrow = FALSE))

# Free y axis
# Domestic only
p_extinction_sums_free <- ggplot(extinction_sums_10scen, aes(y = extinction_domestic, x = scenario_diet, group = scenario_waste, fill = scenario_waste)) +
  geom_col(position = 'dodge') +
  facet_wrap(~ land_use, nrow = 3, scales = 'free_y') +
  scale_x_discrete(name = 'diet scenario', labels = diet_medium_names) +
  scale_fill_brewer(name = 'waste scenario', palette = 'Dark2', labels = waste_names2) +
  scale_y_continuous(name = 'species committed to extinction', expand = expansion(mult = c(0, 0.03))) +
  theme(legend.position = 'bottom', 
        legend.text = element_text(size = rel(.7))) +
  guides(fill=guide_legend(nrow = 2, byrow = FALSE))

p_extinction_sums_free_total <- ggplot(extinction_sums_10scen, aes(y = extinction_total, x = scenario_diet, group = scenario_waste, fill = scenario_waste)) +
  geom_col(position = 'dodge') +
  facet_wrap(~ land_use, nrow = 3, scales = 'free_y') +
  scale_x_discrete(name = 'diet scenario', labels = diet_medium_names) +
  scale_fill_brewer(name = 'waste scenario', palette = 'Dark2', labels = waste_names2) +
  scale_y_continuous(name = 'species committed to extinction', expand = expansion(mult = c(0, 0.03))) +
  theme(legend.position = 'bottom', 
        legend.text = element_text(size = rel(.7))) +
  guides(fill=guide_legend(nrow = 2, byrow = FALSE))

ggsave(file.path(fp_fig, 'extinction_sums_fixed_y_10scen.png'), p_extinction_sums_fixed, height = 7, width = 5, dpi = 400)
ggsave(file.path(fp_fig, 'extinction_sums_free_y_10scen.png'), p_extinction_sums_free, height = 7, width = 5, dpi = 400)
ggsave(file.path(fp_fig, 'extinction_sums_fixed_y_total_10scen.png'), p_extinction_sums_fixed_total, height = 7, width = 5, dpi = 400)
ggsave(file.path(fp_fig, 'extinction_sums_free_y_total_10scen.png'), p_extinction_sums_free_total, height = 7, width = 5, dpi = 400)

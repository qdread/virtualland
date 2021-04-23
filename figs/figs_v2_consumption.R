# Figures and summary statistics for "Scenario Analysis V2"
# QDR / Virtualland / 14 Jan 2021

# Proposed outline of figures

# 0. Consumption differences among scenarios, or basic figure showing how the scenarios differ among one another
# 1. Maps showing the trade in different agricultural products, in baseline and alternative scenarios.
# 2. Boxplot or similar summarizing the maps in fig1.
# 3. Maps showing the land flows associated with the trade in fig1.
# 4. Boxplot or similar summarizing the maps in fig3.
# 5. Maps showing the extinction threat exports/imports associated with the land flows in fig3.
# 6. Boxplot or similar summarizing the maps in fig5.


source('figs/figs_v2_loaddata.R')

# Diet differences among scenarios ----------------------------------------

# Show by the "Group" column in lafa_joined
# Need to assign the missing ones to groups.
missing_groups <- data.frame(Category = c("White and whole wheat flour", "Durum flour", "Fresh brussels sprouts", "Other processed vegetables"),
                    Group = c('grain', 'grain', 'veg', 'veg'))
idx <- match(missing_groups$Category, lafa_joined$Category)
lafa_joined$Group[idx] <- missing_groups$Group

lafa_cal_by_diet <- lafa_joined %>%
  select(Category, Group, calories_available_cal_day, planetary_health, us_style, med_style, vegetarian) %>%
  mutate(baseline = rep(1, nrow(.))) %>%
  pivot_longer(planetary_health:baseline, names_to = 'diet', values_to = 'factor') %>%
  mutate(calories_available_cal_day = calories_available_cal_day * factor) %>%
  rename(food = Category, food_group = Group)

# Split up meat, fish, eggs, and nuts
fish_names <- c("Fresh and frozen fish", "Fresh and frozen shellfish", "Canned Salmon", "Canned Sardines", "Canned Tuna", "Canned shellfish", "Other canned fish", "Cured fish")
nut_names <- c("Peanuts", "Almonds", "Hazelnuts", "Pecans", "Walnuts", "Macadamia", "Pistachios", "Other tree nuts", "Coconut")

lafa_cal_summ <- lafa_cal_by_diet %>%
  mutate(food_group = case_when(food_group == 'veg' ~ 'vegetables',
                                food %in% nut_names ~ 'nuts',
                                food %in% fish_names ~ 'fish',
                                food_group == 'meat' ~ 'meat/eggs',
                                TRUE ~ food_group)) %>%
  group_by(diet, food_group) %>%
  summarize(calories_day = sum(calories_available_cal_day)) %>%
  ungroup %>%
  mutate(diet = factor(gsub('_', '', diet), levels = diet_levels_ordered))

p_diet_foodgroups <- ggplot(lafa_cal_summ %>% mutate(), aes(y = calories_day, x = food_group, color = diet, fill = diet, group = diet)) +
  geom_col(position = 'dodge', color = 'black', size = 0.25) +
  scale_fill_manual(values = okabe_colors[c(1,7,3,4,5)] %>% setNames(NA), labels = diet_long_names$medium_name) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.03)), name = 'calories per person per day') +
  scale_x_discrete(name = 'food group') +
  theme(legend.position = 'bottom',
        legend.text = element_text(size = rel(.75))) +
  guides(fill=guide_legend(nrow = 2, byrow = FALSE))

ggsave(file.path(fp_fig, 'foodgroup_consumption_by_diet.png'), p_diet_foodgroups, height = 5, width = 6, dpi = 400)

# Goods consumption differences among scenarios ---------------------------

# This is by BEA code across diet change and waste change scenarios.
# It also accounts for the "footprint" of consumption, so for instance meat consumption would reflect the consumption of feed (need to check this)

# Reorder factors in loaded CSV.
totaldemand_sums <- totaldemand_sums %>%
  mutate(short_name = factor(short_name, levels = unique(short_name)),
         scenario_diet = factor(scenario_diet, levels = unique(scenario_diet)),
         scenario_waste = factor(scenario_waste, levels = unique(scenario_waste)))

# Absolute values
p_totaldemand_sums <- ggplot(totaldemand_sums, aes(x = short_name, y = demand)) +
  geom_col(aes(fill = kingdom), color = 'black', size = 0.25) +
  facet_grid(scenario_waste ~ scenario_diet, labeller = scenario_labeller) +
  scale_x_discrete(name = 'food category') +
  scale_y_continuous(expand = expansion(mult = c(0, 0.03)), name = 'consumption (million USD)') +
  scale_fill_manual(values = setNames(okabe_colors[c(8, 4)], c('animal', 'plant'))) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'none')

p_totaldemand_sums <- ggdraw(p_totaldemand_sums + theme(plot.margin = unit(c(25, 25, 5.5, 5.5), 'points'))) +
  draw_label(label = 'diet scenario', x = 0.5, y = 0.97) +
  draw_label(label = 'waste scenario', x = 0.99, y = 0.5, angle = -90)

ggsave(file.path(fp_fig, 'total_consumption_all_scenarios.png'), p_totaldemand_sums, height = 9, width = 12, dpi = 400)

# Relative to baseline. Must reconstruct the darn scenarios again.
totaldemand_relative <- totaldemand_sums %>%
  pivot_wider(names_from = c(scenario_diet, scenario_waste), values_from = demand) %>%
  mutate(across(where(is.numeric), ~ . / baseline_baseline)) %>%
  pivot_longer(-c(BEA_code, short_name, kingdom), names_to = 'scenario', values_to = 'demand') %>%
  separate(scenario, into = c('scenario_diet', 'scenario_waste'), sep = '_') %>%
  mutate(scenario_diet = factor(scenario_diet, levels = diet_levels_ordered),
         scenario_waste = factor(scenario_waste, levels = waste_levels_ordered))

p_totaldemand_relative <- ggplot(totaldemand_relative, aes(x = short_name, y = demand)) +
  geom_col(aes(fill = kingdom), color = 'black', size = 0.25) +
  geom_hline(yintercept = 1, linetype = 'dotted', color = 'black', size = 0.5) +
  facet_grid(scenario_waste ~ scenario_diet, labeller = scenario_labeller_medium) +
  scale_x_discrete(name = 'food category') +
  scale_y_continuous(expand = expansion(mult = c(0, 0.03)), name = 'consumption relative to baseline') +
  scale_fill_manual(values = setNames(okabe_colors[c(8, 4)], c('animal', 'plant'))) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = rel(0.9)),
        legend.position = 'none')

p_totaldemand_relative <- ggdraw(p_totaldemand_relative + theme(plot.margin = unit(c(25, 25, 5.5, 5.5), 'points'))) +
  draw_label(label = 'diet scenario', x = 0.5, y = 0.97) +
  draw_label(label = 'waste scenario', x = 0.99, y = 0.5, angle = -90)

ggsave(file.path(fp_fig, 'total_consumption_relative_all_scenarios.png'), p_totaldemand_relative, height = 9*.75, width = 12*.75, dpi = 400)


# Alternative figs with 10 scenarios --------------------------------------

# With only no waste and all waste.

# Absolute values
p_totaldemand_sums <- ggplot(totaldemand_sums %>% filter(scenario_waste %in% c('baseline', 'allavoidable')), aes(x = short_name, y = demand)) +
  geom_col(aes(fill = kingdom), color = 'black', size = 0.25) +
  facet_grid(scenario_waste ~ scenario_diet, labeller = scenario_labeller) +
  scale_x_discrete(name = 'food category') +
  scale_y_continuous(expand = expansion(mult = c(0, 0.03)), name = 'consumption (million USD)') +
  scale_fill_manual(values = setNames(okabe_colors[c(8, 4)], c('animal', 'plant'))) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'none')

p_totaldemand_sums <- ggdraw(p_totaldemand_sums + theme(plot.margin = unit(c(25, 25, 5.5, 5.5), 'points'))) +
  draw_label(label = 'diet scenario', x = 0.5, y = 0.97) +
  draw_label(label = 'waste scenario', x = 0.99, y = 0.5, angle = -90)

ggsave(file.path(fp_fig, 'total_consumption_10_scenarios.png'), p_totaldemand_sums, height = 9*.75, width = 12, dpi = 400)

# Relative to baseline
p_totaldemand_relative <- ggplot(totaldemand_relative %>% filter(scenario_waste %in% c('baseline', 'allavoidable')), aes(x = short_name, y = demand)) +
  geom_col(aes(fill = kingdom), color = 'black', size = 0.25) +
  geom_hline(yintercept = 1, linetype = 'dotted', color = 'black', size = 0.5) +
  facet_grid(scenario_waste ~ scenario_diet, labeller = scenario_labeller_fn(diet = 'medium', waste = 'long')) +
  scale_x_discrete(name = 'food category') +
  scale_y_continuous(expand = expansion(mult = c(0, 0.03)), name = 'consumption relative to baseline') +
  scale_fill_manual(values = setNames(okabe_colors[c(8, 4)], c('animal', 'plant'))) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = rel(0.9)),
        legend.position = 'none')

p_totaldemand_relative <- ggdraw(p_totaldemand_relative + theme(plot.margin = unit(c(25, 25, 5.5, 5.5), 'points'))) +
  draw_label(label = 'diet scenario', x = 0.5, y = 0.97) +
  draw_label(label = 'waste scenario', x = 0.99, y = 0.5, angle = -90)

ggsave(file.path(fp_fig, 'total_consumption_relative_10_scenarios.png'), p_totaldemand_relative, height = 9*.75*.75, width = 12*.75, dpi = 400)

# Black on white figures for presentation
# Updated to use new data and to be able to run standalone.
# QDR / virtualland / 25 March 2022

source('figs/figs_v2_loaddata.R')

library(Rutilitybelt)

fp_bw <- 'data/cfs_io_analysis/scenario_v2_figs/bwfigs'

# Figs 1 and 2: consumption barplots


#### Fig 1 setup
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

lafa_cal_summ <- lafa_cal_summ %>% mutate(food_group = if_else(food_group == 'fat', 'added fats', food_group))
foodgroups_ordered <- c('grain', 'fruit', 'nuts', 'vegetables', 'sugar', 'meat/eggs', 'dairy', 'fish', 'added fats')

#### Drawing Fig 1
p_consprod_top <- ggplot(lafa_cal_summ %>% mutate(food_group = factor(food_group, levels = foodgroups_ordered)) %>% filter(!diet %in% 'baseline') %>% mutate(diet = factor(diet, levels = levels(diet)[-1])), aes(y = calories_day, x = food_group)) +
  geom_col(position = 'dodge', color = 'black', size = 0.1, aes(fill = diet, group = diet)) +
  ungeviz::geom_hpline(data = lafa_cal_summ %>% mutate(food_group = factor(food_group, levels = foodgroups_ordered)) %>% filter(diet == 'baseline'), color = 'white', width = 1) +
  #scale_fill_manual(values = unname(okabe_colors[c(1,7,3,4,5)]), labels = diet_long_names$medium_name) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.03)), name = 'calories per person per day') +
  scale_x_discrete(name = 'food group') +
  theme_classic() +
  theme(legend.position = c(0.65, 0.85),
        legend.text = element_text(size = rel(.65)),
        legend.background = element_blank(),
        legend.key.size = unit(0.5, 'cm'),
        legend.margin=margin(t=0, r=1, b=1, l=1, unit="mm")) +
  guides(fill = guide_legend(ncol = 2, title = NULL))

(fig1bw <- p_consprod_top + 
  theme_black() +
  scale_fill_manual(values = unname(c(okabe_colors[c(7,3,4,5)])), labels = diet_long_names$medium_name[-1]) +
  theme(legend.position = c(0.55, 0.85), legend.key = element_rect(color = 'black'), 
        legend.title = element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.text = element_text(size = rel(.75)),
        legend.background = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  guides(fill=guide_legend(nrow = 2, byrow = FALSE))
)
ggsave(file.path(fp_bw, 'bw_fig1.png'), fig1bw, height = 5*.85, width = 6*.85, dpi = 400, device = png)

#### Fig 2 Setup
# Reorder factors in loaded CSV.
totaldemand_sums <- totaldemand_sums %>%
  mutate(short_name = factor(short_name, levels = unique(short_name)),
         scenario_diet = factor(scenario_diet, levels = unique(scenario_diet)),
         scenario_waste = factor(scenario_waste, levels = unique(scenario_waste)))

# Relative to baseline. Must reconstruct the scenarios again.
totaldemand_relative <- totaldemand_sums %>%
  pivot_wider(names_from = c(scenario_diet, scenario_waste), values_from = demand) %>%
  mutate(across(where(is.numeric), ~ . / baseline_baseline)) %>%
  pivot_longer(-c(BEA_code, short_name, kingdom), names_to = 'scenario', values_to = 'demand') %>%
  separate(scenario, into = c('scenario_diet', 'scenario_waste'), sep = '_') %>%
  mutate(scenario_diet = factor(scenario_diet, levels = diet_levels_ordered),
         scenario_waste = factor(scenario_waste, levels = waste_levels_ordered))

#### Drawing Fig2
fig2bw <- ggplot(totaldemand_relative %>% filter(scenario_waste %in% c('baseline', 'allavoidable')), aes(x = short_name, y = demand)) +
  geom_col(aes(fill = kingdom), color = 'black', size = 0.25) +
  geom_hline(yintercept = 1, linetype = 'dotted', color = 'white', size = 0.5) +
  facet_grid(scenario_waste ~ scenario_diet, labeller = scenario_labeller_fn(diet = 'medium', waste = 'long')) +
  scale_x_discrete(name = 'food category') +
  scale_y_continuous(expand = expansion(mult = c(0, 0.03)), name = 'production relative to baseline') +
  scale_fill_manual(values = setNames(okabe_colors[c(8, 4)], c('animal', 'plant'))) +
  theme_black() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1), 
        legend.position = 'none')

fig2bw <- ggdraw(fig2bw + theme(plot.margin = unit(c(25, 25, 5.5, 5.5), 'points'))) +
  draw_label(label = 'diet scenario', x = 0.5, y = 0.97, color = 'white') +
  draw_label(label = 'waste scenario', x = 0.99, y = 0.5, angle = -90, color = 'white')

ggsave(file.path(fp_bw, 'bw_fig2.png'), fig2bw, height = 9*.75*.75, width = 12*.8, dpi = 400, device = png)



# Fig 3: In & Out map.

#### Fig 3 Setup
library(data.table)
load('~/biodiversity-farm2fork/data/all_app_data.RData')

domestic_land_toplot <- county_land_flow_sums[scenario_diet == 'baseline' & scenario_waste == 'baseline',
                                              .(VLT = 100*sum(flow_outbound_domestic, na.rm = TRUE)),
                                              by = .(county)]
domestic_ext_toplot <- county_extinction_flow_sums[scenario_diet == 'baseline' & scenario_waste == 'baseline',
                                                   .(VBT = sum(flow_outbound_domestic, na.rm = TRUE)),
                                                   by = .(county)]
foreign_land_toplot <- foreign_land_flow_sums[scenario_diet == 'baseline' & scenario_waste == 'baseline',
                                              .(VLT = 100*sum(flow_outbound_foreign, na.rm = TRUE)),
                                              by = .(ISO_A3)]
foreign_ext_toplot <- foreign_extinction_flow_sums[scenario_diet == 'baseline' & scenario_waste == 'baseline',
                                                   .(VBT = sum(flow_outbound_foreign, na.rm = TRUE)),
                                                   by = .(ISO_A3)]


# Set up world map projection ---------------------------------------------

# Filter country map to get rid of very southern latitudes
extent_countries <- st_bbox(c(xmin = -180, ymin = -58, xmax = 180, ymax = 84), crs = "+proj=longlat +ellps=WGS84") %>%
  st_as_sfc
poly_countries <- geosphere::makePoly(st_coordinates(extent_countries)[, c('X', 'Y')])
poly_countries <- st_polygon(list(poly_countries)) %>% st_sfc
st_crs(poly_countries) <- st_crs(extent_countries)
poly_countries <- st_transform(poly_countries, crs = "+proj=robin")

country_map_toplot <- global_country_map %>%
  st_transform(crs = "+proj=robin") %>%
  filter(!country_name %in% "Antarctica")

# Join data with maps and draw maps ---------------------------------------

# Note: land flows are already in units of ha for both.

scale_breaks = scales::trans_breaks("log10", function(x) 10^x)
scale_labels = scales::trans_format("log10", scales::math_format(10^.x))

leg_longbottom_theme <- theme_void() + theme(legend.position = 'bottom',
                                             legend.title = element_text(size = rel(1)),
                                             legend.key.width = unit(0.3, 'in'))

leg_long_bw <- theme_void() + theme(legend.position = 'bottom',
                                    legend.title = element_text(size = rel(1), color = 'white'),
                                    legend.key.width = unit(0.3, 'in'),
                                    legend.text = element_text(color = 'white'),
                                    plot.background = element_rect(color = 'black', fill = 'black'),
                                    panel.background = element_rect(color = 'black', fill = 'black')
)

p_dland <- ggplot(left_join(county_map, domestic_land_toplot)) +
  geom_sf(aes(fill = VLT), color = NA) +
  scale_fill_viridis_c(name = 'Domestic land\nfootprint (ha)', trans = 'log10', breaks = scale_breaks, labels = scale_labels, na.value = 'gray80') +
  leg_long_bw

p_fland <- ggplot(left_join(country_map_toplot, foreign_land_toplot)) +
  geom_sf(aes(fill = VLT), color = NA) +
  geom_sf(data = st_geometry(poly_countries), fill = NA) +
  scale_fill_viridis_c(name = 'Foreign land\nfootprint (ha)', trans = 'log10', breaks = scale_breaks, labels = scale_labels, na.value = 'gray80') +
  leg_long_bw

p_dext <- ggplot(left_join(county_map, domestic_ext_toplot)) +
  geom_sf(aes(fill = VBT), color = NA) +
  scale_fill_viridis_c(name = 'Domestic biodiversity\nfootprint (extinctions)', trans = 'log10', breaks = scale_breaks, labels = scale_labels, na.value = 'gray80') +
  leg_long_bw

p_fext <- ggplot(left_join(country_map_toplot, foreign_ext_toplot)) +
  geom_sf(aes(fill = VBT), color = NA) +
  geom_sf(data = st_geometry(poly_countries), fill = NA) +
  scale_fill_viridis_c(name = 'Foreign biodiversity\nfootprint (extinctions) ', trans = 'log10', breaks = scale_breaks, labels = scale_labels, na.value = 'gray80') +
  leg_long_bw

png(glue('{fp_bw}/bw_fig3.png'), height=8+90+90,width=8+120+120,res=400,units='mm')
grid.newpage()
grid.draw(gtable_cbind(
  gtable_rbind(ggplotGrob(p_dland), ggplotGrob(p_fland)),
  gtable_rbind(ggplotGrob(p_dext), ggplotGrob(p_fext))))
dev.off()

### FIXME ANYTHING BELOW THIS MUST BE CHANGED.

# Fig.4 not going to include.

# Fig.5: goofy stacked bar plot.

fig5abw <- p_ext_grandtotals_10 + 
  theme_black() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        strip.text = element_text(size = rel(0.7)),
        legend.title = element_text(size = rel(0.8)),
        legend.position = c(0.93, 0.78),
        legend.background = element_blank(),
        legend.spacing.y = unit(0.1, 'cm')) 

fig5bbw <- p_land_grandtotals_10 +
  theme_black() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        strip.text = element_text(size = rel(0.7)),
        legend.title = element_text(size = rel(0.8)),
        legend.position = c(0.93, 0.71),
        legend.background = element_blank(),
        legend.spacing.y = unit(0.1, 'cm'))

ggsave(file.path(fp_bw, 'bw_fig5a.png'), fig5abw, height = 6, width = 8, dpi = 400)
ggsave(file.path(fp_bw, 'bw_fig5b.png'), fig5bbw, height = 6, width = 8, dpi = 400)

png(file.path(fp_fig, 'foreign_vs_domestic_10scenarios_grandtotals.png'), height = 7, width = 7, res = 400, units = 'in')
grid.draw(gridExtra::gtable_rbind(ggplotGrob(p_top), ggplotGrob(p_bottom)))
dev.off()
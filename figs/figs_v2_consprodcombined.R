# Combine diet consumption with scenario production in a single figure (was formerly Figs 1 and 2)
# data generated in figs_v2_consumption.R

foodgroups_ordered <- c('grain', 'fruit', 'nuts', 'vegetables', 'sugar', 'meat/eggs', 'dairy', 'fish', 'fat')
p_consprod_top <- ggplot(lafa_cal_summ %>% mutate(food_group = factor(food_group, levels = foodgroups_ordered)), aes(y = calories_day, x = food_group, color = diet, fill = diet, group = diet)) +
  geom_col(position = 'dodge', color = 'black', size = 0.25) +
  scale_fill_manual(values = okabe_colors[c(1,7,3,4,5)] %>% setNames(NA), labels = diet_long_names$medium_name) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.03)), name = 'calories per person per day') +
  scale_x_discrete(name = 'food group') +
  theme(legend.position = 'none') 

p_consprod_bottom <- ggplot(totaldemand_relative %>% filter(scenario_waste %in% c('baseline', 'allavoidable'))) +
  geom_col(aes(x = short_name, y = demand, color = scenario_diet, fill = scenario_diet, group = scenario_diet), position = 'dodge', color = 'black', size = 0.25) +
  geom_hline(yintercept = 1, linetype = 'dotted', color = 'black', size = 0.5) +
  geom_text(data = data.frame(scenario_waste = factor(c('baseline', 'allavoidable'), levels = c('baseline', 'allavoidable')), label = paste('waste scenario:', waste_long_names$long_name[c(1,4)])), aes(label = label), x = Inf, y = Inf, hjust = 1.01, vjust = 1.3) +
  scale_fill_manual(name = 'diet', values = okabe_colors[c(1,7,3,4,5)] %>% setNames(NA), labels = diet_long_names$medium_name) +
  facet_wrap(~ scenario_waste, nrow = 2) +
  scale_x_discrete(name = 'primary agricultural product') +
  scale_y_continuous(expand = expansion(mult = c(0, 0.03)), name = 'production relative to baseline') +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = rel(0.9)),
        legend.position = 'bottom',
        legend.text = element_text(size = rel(.75)),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  guides(fill=guide_legend(nrow = 2, byrow = FALSE))

png(file.path(fp_fig, 'cons_prod_fig1_combined.png'), height = 9, width = 6, res = 400, units = 'in')
  grid.arrange(p_consprod_top, p_consprod_bottom, nrow = 2, heights = c(1, 3))
dev.off()

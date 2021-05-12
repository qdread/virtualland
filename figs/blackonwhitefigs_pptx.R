# Black on white figures for presentation
# QDR / virtualland / 12 May 2021

library(Rutilitybelt)

fp_bw <- 'data/cfs_io_analysis/scenario_v2_figs/bwfigs'

# Figs 1 and 2: consumption barplots

fig1bw <- p_diet_foodgroups + 
  theme_black() +
  scale_fill_manual(values = c('white', okabe_colors[c(7,3,4,5)]) %>% setNames(NA), labels = diet_long_names$medium_name) +
  theme(legend.position = 'bottom', legend.key = element_rect(color = 'black')) +
  guides(fill=guide_legend(nrow = 2, byrow = FALSE))

ggsave(file.path(fp_bw, 'bw_fig1.png'), fig1bw, height = 5, width = 6, dpi = 400)

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

ggsave(file.path(fp_bw, 'bw_fig2.png'), fig2bw, height = 9*.75*.75, width = 12*.8, dpi = 400)

leg_long_bw <- theme_void() + theme(legend.position = 'bottom',
                                    legend.title = element_text(size = rel(1), color = 'white'),
                                    legend.key.width = unit(0.3, 'in'),
                                    legend.text = element_text(color = 'white'),
                                    plot.background = element_rect(color = 'black', fill = 'black'),
                                    panel.background = element_rect(color = 'black', fill = 'black')
)

# Fig 3: In & Out map.
land_inbound_dat <- county_land_map_base[land_range, on = .NATURAL][land_type == 'total'] 
map_land_inbound <- draw_usmap_with_insets(map_data = left_join(county_map, land_inbound_dat$data[[1]]),
                                           ak_idx = county_ak_idx,
                                           hi_idx = county_hi_idx,
                                           variable = flow_inbound,
                                           scale_name = 'Virtual land\nimport (ha)',
                                           scale_factor = 10000,
                                           scale_trans = 'log10',
                                           scale_palette = seq_pal,
                                           scale_range = with(land_inbound_dat, c(min, max)),
                                           scale_breaks = scales::trans_breaks("log10", function(x) 10^x),
                                           scale_labels = scales::trans_format("log10", scales::math_format(10^.x)),
                                           add_theme = leg_long_bw)

# Inbound extinctions
ext_inbound_dat <- county_extinction_map_base[extinction_range, on = .NATURAL][taxon %in% 'total' & land_use %in% 'total']
map_ext_inbound <- draw_usmap_with_insets(map_data = left_join(county_map, ext_inbound_dat$data[[1]]),
                                          ak_idx = county_ak_idx,
                                          hi_idx = county_hi_idx,
                                          variable = extinction_inbound,
                                          scale_name = 'Extinction\nimport',
                                          scale_factor = 1,
                                          scale_trans = 'log10',
                                          scale_palette = seq_pal,
                                          scale_range = with(ext_inbound_dat, c(min, max)),
                                          scale_breaks = scales::trans_breaks("log10", function(x) 10^x),
                                          scale_labels = scales::trans_format("log10", scales::math_format(10^.x)),
                                          add_theme = leg_long_bw)

# Outbound land
land_outbound_dat <- county_land_map_base[outbound_land_range, on = .NATURAL][land_type %in% 'total']
map_land_outbound <-  draw_usmap_with_insets(map_data = left_join(county_map, land_outbound_dat$data[[1]]),
                                             ak_idx = county_ak_idx,
                                             hi_idx = county_hi_idx,
                                             variable = flow_outbound,
                                             scale_name = 'Virtual land\nexport (ha)',
                                             scale_factor = 10000,
                                             scale_trans = 'log10',
                                             scale_palette = seq_pal,
                                             scale_range = with(land_outbound_dat, c(min, max)),
                                             scale_breaks = scales::trans_breaks("log10", function(x) 10^x),
                                             scale_labels = scales::trans_format("log10", scales::math_format(10^.x)),
                                             add_theme = leg_long_bw)

# Outbound extinctions
ext_outbound_dat <- county_extinction_map_base[outbound_extinction_range, on = .NATURAL][land_use %in% 'total' & taxon %in% 'total']
map_ext_outbound <- draw_usmap_with_insets(map_data = left_join(county_map, ext_outbound_dat$data[[1]]),
                                           ak_idx = county_ak_idx,
                                           hi_idx = county_hi_idx,
                                           variable = extinction_outbound,
                                           scale_name = 'Extinction\nexport',
                                           scale_factor = 1,
                                           scale_trans = 'log10',
                                           scale_palette = seq_pal,
                                           scale_range = with(ext_outbound_dat, c(min, max)),
                                           scale_breaks = scales::trans_breaks("log10", function(x) 10^x),
                                           scale_labels = scales::trans_format("log10", scales::math_format(10^.x)),
                                           add_theme = leg_long_bw)

# Combine the four into a single figure with labeled columns and rows

x_label_grobs <- map(c('imports (consumption)', 'exports (production)'), ~ textGrob(., gp = gpar(fontsize = 15, col = 'white')))
y_label_grobs <- map(c('land', 'biodiversity'), ~ textGrob(., gp = gpar(fontsize = 15, col = 'white'), rot = 270))

mat <- rbind(c(1,2,NA), c(5,6,3), c(7,8,4))

panel_width <- 120
panel_height <- 90
label_width <- 8

fourmaps <- arrangeGrob(grobs = c(x_label_grobs, y_label_grobs, list(map_land_inbound, map_land_outbound, map_ext_inbound, map_ext_outbound)),
                        layout_matrix = mat,
                        widths = unit(c(rep(panel_width, 2), label_width), 'mm'),
                        heights = unit(c(label_width, rep(panel_height, 2)), 'mm'),
                        padding = unit(0, "line"))

png(glue('{fp_bw}/bw_fig3.png'), height=8+90+90,width=8+120+120,res=300,units='mm')
  grid.draw(grobTree(
    rectGrob(gp = gpar(fill = 'black', lwd = 0)),
    fourmaps))
dev.off()

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
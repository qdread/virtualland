# Diagnostic plots to check the proportions of production sent to USA

# Crops
production_trade_totals <- production_crops_trade %>% 
  group_by(country_name) %>%
  summarize(production_qty=sum(production,na.rm=T),
            export_qty=sum(export_qty, na.rm=T)) %>%
  mutate(proportion_sent_to_usa = export_qty/production_qty)

# Diagnostic plot to make sure all values are realistic.
ggplot(production_trade_totals, aes(x=production_qty,y=export_qty)) +
  geom_point() +
  geom_abline(slope=1) +
  scale_x_log10() + scale_y_log10()

# Grazers
# Diagnostic plot to make sure all values are realistic.
ggplot(grazer_prod_trade_totals, aes(x=production_qty,y=export_qty)) +
  geom_point() +
  geom_abline(slope=1) +
  scale_x_log10() + scale_y_log10()

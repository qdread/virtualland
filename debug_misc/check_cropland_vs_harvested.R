# Compare total production area with total land use

# Cropland by country
cropland_by_country <- landuse_inputs %>% filter(Item == 'Cropland')
agland_by_country <- landuse_inputs %>% filter(Item == 'Agricultural land')

# Summed crop production areas
# Remove all codes with 4 characters and 17 and 18
harvestedarea_by_country <- production_crops %>% 
  filter(Element == 'Area harvested', !grepl('17..$|18..$', `Item Code`)) %>%
  group_by(`Area Code`, Area) %>%
  summarize(area = sum(Value, na.rm = TRUE)/1000)

cropland_by_country <- full_join(cropland_by_country, harvestedarea_by_country)

ggplot(cropland_by_country, aes(x=Value,y=area)) +
  geom_point() + geom_abline(slope=1,intercept=0) +
  scale_x_log10(limits = c(1, 2e6)) + scale_y_log10(limits = c(1, 2e6)) +
  labs(x = 'cropland', y = 'sum of harvested area by crop') +
  theme(aspect.ratio = 1)
# Once double count codes are removed, this looks fine.


# Find countries that are the largest exporters of biodiversity threats from different sources to the USA
# Using tidyverse :-O

source('figs/figs_v2_loaddata.R')

# Sum by country, land use, and taxon
VET_countries <- foreign_extinction_export %>%
  filter(scenario_diet %in% 'baseline', scenario_waste %in% 'baseline') %>%
  group_by(country_name, ISO_A3, land_use, taxon) %>%
  summarize(VET = sum(species_lost, na.rm = TRUE))

# Also sum across taxa for land use
VET_countries_alltaxa <- VET_countries %>%
  group_by(country_name, ISO_A3, land_use) %>%
  summarize(VET = sum(VET))

# Make a plot of the top ten in each combination of land use and taxon.

# Make a plot of the top ten just by land use
VET_countries_alltaxa %>%
  arrange(land_use, -VET) %>%
  group_by(land_use) %>%
  slice(1:10) %>%
  ggplot(aes(x = ISO_A3, y = VET)) +
    geom_col() +
    facet_wrap(~ land_use, scales = "free_x")

# Also look at it by crop or animal products
foreign_animal_export %>%
  arrange(livestock_animal, livestock_product_type, -export_qty) %>%
  group_by(livestock_animal, livestock_product_type) %>% 
  slice(1:5) %>%
  print(n=135)

topcrops <- foreign_crop_export %>%
  arrange(item, -export_qty) %>%
  group_by(item) %>%
  slice(1:5)

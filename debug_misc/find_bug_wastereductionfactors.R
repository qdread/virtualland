# Debug the waste consumption factors (why is all-avoidable waste reduction impact less than waste reduction impact at individual stages)?

# Check whether it occurs at script 7 (Waste reduction simulation)

lafa_joined <- read_csv('data/cfs_io_analysis/lafa_with_production_factors_diet_x_waste.csv')

lafa_joined %>%
  select(Category, contains('prod_factor')) %>%
  pivot_longer(-Category) %>%
  group_by(name) %>%
  summarize(value=mean(value))

# Yes. The production factor is higher for allavoidable compared to consumer and preconsumer.
# Ensure that old and new lafa df with production factors have the same ordering, for crosswalk.

lafa_df_old <- read_csv('data/cfs_io_analysis/old/lafa_with_production_factors_diet_x_waste.csv')
lafa_df_foreign_old <- read_csv('data/cfs_io_analysis/old/lafa_with_production_factors_diet_x_waste_foreign.csv')
lafa_df_new <- read_csv('data/cfs_io_analysis/lafa_with_production_factors_diet_x_waste.csv')
lafa_df_foreign_new <- read_csv('data/cfs_io_analysis/lafa_with_production_factors_diet_x_waste_foreign.csv')

all(lafa_df_old$Category == lafa_df_new$Category)
all(lafa_df_foreign_old$Category == lafa_df_foreign_new$Category)
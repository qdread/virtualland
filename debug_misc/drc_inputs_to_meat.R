# Check the quantities of input required to produce meat outputs in the DRC table

fp_useeio <- file.path(ifelse(dir.exists('Q:/'), '~/Documents/GitHub/foodwaste', '~'), 'USEEIO/useeiopy/Model Builds/USEEIO2012')

drc2012 <- read.csv(file.path(fp_useeio, 'USEEIO2012_DRC.csv'), row.names = 1, check.names = FALSE) # Use this to get row names
drc2012_mat <- as.matrix(drc2012)
leontief_inverse2012 <- solve(diag(nrow(drc2012_mat)) - drc2012_mat)

# Inputs to meat sectors found in the columns for those sectors

primary_animal_products <- c("112120/dairies/us", "1121a0/cattle ranches and feedlots/us", 
  "112300/poultry farms/us", "112a00/animal farms and aquaculture ponds (except cattle and poultry)/us"
)
secondary_animal_products <- c("311513/cheese/us", "311514/dry, condensed, and evaporated dairy/us", 
                               "31151a/fluid milk and butter/us", "311520/ice cream and frozen desserts/us", 
                               "311615/packaged poultry/us", "31161a/packaged meat (except poultry)/us"
)

input_to_meat <- drc2012[, c(primary_animal_products, secondary_animal_products)]

input_from_soy <- unlist(drc2012[1,])
input_from_wheat <- unlist(drc2012[2,])
head(sort(input_from_soy, decreasing = T), 20)
head(sort(input_from_wheat, decreasing = T), 20)

# For each meat industry, which industries supply it the most?

input_to_meat$supplier <- row.names(input_to_meat)

library(tidyverse)

top_suppliers <- input_to_meat %>%
  pivot_longer(-supplier, names_to = 'buyer') %>%
  group_by(buyer) %>%
  arrange(-value) %>%
  slice(1:10)


# Check consumption factors -----------------------------------------------

consfactors <- read_csv('data/cfs_io_analysis/bea_consumption_factors_diet_waste_scenarios.csv')

consfactors %>% select(starts_with("BEA"), starts_with("D_veg"))

consall <- read_csv('data/cfs_io_analysis/county_totaldemand2012_allscenarios.csv')

consall <- consall %>%
  filter(scenario == 'D_vegetarian_WR_baseline')

conssums <- rowSums(consall[,-(1:2)])

cbind(consall[,1:2], conssums)

# Lookup tables/vectors and ggplot themes/color schemes for Scenario V2 figures.
# QDR / Virtualland / 26 Jan 2021

# Order levels for the different scenarios
diet_levels_ordered <- c('baseline', 'usstyle', 'medstyle', 'vegetarian', 'planetaryhealth')
waste_levels_ordered <- c('baseline', 'preconsumer', 'consumer', 'allavoidable')
land_levels_ordered <- c('annual', 'permanent', 'pasture')

# Lookup tables for longer legend names
diet_long_names <- data.frame(scenario_diet = diet_levels_ordered,
                              long_name = c('baseline diet', 'healthy US-style (USDA)', 'healthy Mediterranean-style (USDA)', 'healthy vegetarian (USDA)', 'planetary health (Lancet)'),
                              medium_name = c('baseline diet', 'USDA US-style', 'USDA Mediterranean-style', 'USDA vegetarian', 'planetary health'))
waste_long_names <- data.frame(scenario_waste = waste_levels_ordered,
                               long_name = c('no waste reduction', 'pre-consumer waste cut 50%', 'consumer waste cut 50%', 'all waste cut 50%'),
                               medium_name = c('no reduction', 'pre-consumer -50%', 'consumer -50%', 'all -50%'))

# Labeller function with character vector lookup tables for 2x2 scenarios
scenario_labeller <- labeller(scenario_diet = setNames(diet_long_names$long_name, diet_long_names$scenario_diet),
                              scenario_waste = setNames(waste_long_names$long_name, waste_long_names$scenario_waste))

# Shorter labeller
scenario_labeller_medium <- labeller(scenario_diet = setNames(diet_long_names$medium_name, diet_long_names$scenario_diet),
                              scenario_waste = setNames(waste_long_names$medium_name, waste_long_names$scenario_waste))

# Labeller that can specify long or medium name for each one.
scenario_labeller_fn <- function(diet, waste) {
  if (diet == 'long') diet_names_use <- diet_long_names$long_name
  if (diet == 'medium') diet_names_use <- diet_long_names$medium_name
  if (waste == 'long') waste_names_use <- waste_long_names$long_name
  if (waste == 'medium') waste_names_use <- waste_long_names$medium_name
  labeller(scenario_diet = setNames(diet_names_use, diet_long_names$scenario_diet),
           scenario_waste = setNames(waste_names_use, waste_long_names$scenario_waste))
}

# Short names of the ten agricultural goods in BEA, plus wild-caught fish
ag_names_lookup <- data.frame(
  BEA_389_code = c("1111A0", "1111B0", "111200", "111300", "111400", "111900", "112120", "1121A0", "112300", "112A00", "114000"
  ), 
  BEA_389_def = c("Fresh soybeans, canola, flaxseeds, and other oilseeds",
                  "Fresh wheat, corn, rice, and other grains", 
                  "Fresh vegetables, melons, and potatoes", 
                  "Fresh fruits and tree nuts", 
                  "Greenhouse crops, mushrooms, nurseries, and flowers", 
                  "Tobacco, cotton, sugarcane, peanuts, sugar beets, herbs and spices, and other crops", 
                  "Dairies", 
                  "Cattle ranches and feedlots", 
                  "Poultry farms", 
                  "Animal farms and aquaculture ponds (except cattle and poultry)", 
                  "Wild-caught fish and game"),
  short_name = c('oilseeds & soybeans', 'grains', 'vegetables & potatoes', 'fruits & nuts', 'greenhouse crops', 'peanuts, sugar, etc.', 'dairy', 'beef cattle', 'poultry & eggs', 'other meat incl. aquaculture', 'wild-caught seafood'),
  kingdom = rep(c('plant', 'animal'), c(6, 5))) %>%
  mutate(short_name = factor(short_name, levels = unique(short_name)))

theme_set(theme_bw() + theme(strip.background = element_blank()))
fill_dark <- scale_fill_brewer(palette = 'Dark2')
okabe_colors <- palette.colors(n = 9, palette = 'Okabe-Ito')

# Function to make a "dummy axis" so I can label the secondary axis.
dummy_axis <- function(label) sec_axis(~ . , name = label, labels = NULL, breaks = NULL)

# function to make category labels for the two facets
label_scenario_categories <- function(p) {
  ggdraw(p + theme(plot.margin = unit(c(25, 25, 5.5, 5.5), 'points'))) +
    draw_label(label = 'diet scenario', x = 0.5, y = 0.97) +
    draw_label(label = 'waste scenario', x = 0.99, y = 0.5, angle = -90)
}


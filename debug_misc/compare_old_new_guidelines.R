# Compare the 2015-2020 to 2020-2025 dietary guidelines

library(tidyverse)
fp_diet <- 'data/raw_data/food_consumption/diet_guidelines'

# Load old and new
guidelines_old <- read_csv(file.path(fp_diet, 'us_dietary_guidelines_long.csv')) %>% rename(value_old = value)
guidelines_new <- read_csv(file.path(fp_diet, 'us_dietary_guidelines2020-2025_long.csv')) %>% rename(value_new = value)

# Join to compare
guidelines <- full_join(guidelines_old, guidelines_new)

# Look at difference between the two
guidelines <- guidelines %>% mutate(diff = value_new - value_old)

table(guidelines$diff != 0)

guidelines %>% mutate(absdiff = abs(diff)) %>% arrange(-absdiff) %>% print(n=100)

# After fixing the issue, seafood has gone down slightly in the USA diet.
# "Other" has decreased, which I think is mainly due to the rounding errors.

# Rows not present in old but present in new
guidelines_bind <- rbind(guidelines_old %>% select(-value_old), guidelines_new %>% select(-value_new))
guidelines_bind[!(duplicated(guidelines_bind) | duplicated(guidelines_bind, fromLast = TRUE)), ]

# Nuts-seeds-soy in the old, but separated in the new.
# NASS extract county-level data
# QDR / Virtualland / 17 Dec. 2020

# Modified 24 March 2021: new version updated for compatibility with USEEIOv2.0: use new NAICS-BEA crosswalk.

# Some code modified from combine_nass_susb_weightings.R

library(tidyverse)

fp_out <- 'data/cfs_io_analysis'

# Read crosswalk from USEEIO that maps NAICS 12 to the BEA codes
bea_naics <- read_csv(file.path(fp_out, 'useeio2012v2.0_NAICS_BEA_crosswalk.csv'))

# Read text file ---------------------------------------------------

cdqt_file <- 'data/raw_data/USDA/2012_cdqt_data.txt'

# Columns are delimited by tabs? Probably
# Read all as characters to begin with.
cdqt <- read_delim(cdqt_file, delim = '\t', col_names = FALSE, col_types = strrep('c', 15))


# NAICS by county if possible ---------------------------------------------

cdqt_naics_county <- cdqt %>%
  filter(grepl('^NAICS', X14), X8 == 'COUNTY')

# Number of farm operations by county can be used to downscale the NAICS production by value from state to county.
cdqt_county_noperations <- cdqt_naics_county %>%
  filter(X6 == 'FARM OPERATIONS - NUMBER OF OPERATIONS')

# Clean the data frame
cdqt_county_noperations <- cdqt_county_noperations %>%
  select(X9, X10, X12, X13, X14, X15) %>%
  setNames(c('state_fips', 'state_abbrev', 'county_fips', 'county_name', 'NAICS', 'value')) %>%
  mutate(NAICS = gsub('NAICS CLASSIFICATION: ', '', NAICS),
         NAICS = gsub('\\(|\\)', '', NAICS))

write_csv(cdqt_county_noperations, file.path(fp_out, 'forcountydownscale_nass_n_operations.csv'))

# CBP county level data ---------------------------------------------------

# Use this for non-agricultural codes.

cbp12co <- read_csv('data/raw_data/Census/CBP/cbp12co.txt')

### Use CBP for the other non-agricultural codes
# Variables: empflag shows if data are withheld, emp_nf is employee noise flag, emp is total employees, qp1 is quarter 1 payroll. ap is annual payroll, est is number of establishments and then the number of establishments with different numbers of employees.

# We have CBP in NAICS codes so that would have to be converted to BEA
# Use number of establishments as a proxy? This is most similar to the data being used for the ag codes. 
# Or use number of employees which is likely better.

# Map CBP to BEA (codes 113 and above) -----------------------------------

# Unique codes in the CBP dataset
cbp_naics <- unique(gsub('-|/','', cbp12co$naics))

# First get rid of any redundant ones in the CBP that have a longer and more specific code.
cbp_redundant <- map_lgl(cbp_naics, ~ nchar(.) < max(nchar(grep(paste0('^', .), cbp_naics, value = TRUE))))
cbp_naics_notredundant <- cbp_naics[!cbp_redundant] # All are six characters except 99!

shortcodes <- cbp_naics_notredundant[nchar(cbp_naics_notredundant) < 6] # Just 99.
cbp_naics_notredundant <- sort(cbp_naics_notredundant[nchar(cbp_naics_notredundant) == 6] )

# Check which codes are in the crosswalk and which aren't
intersect(cbp_naics_notredundant, bea_naics$NAICS)
setdiff(cbp_naics_notredundant, bea_naics$NAICS) # All are present in crosswalk
crosswalk_not_cbp <- setdiff(bea_naics$NAICS, cbp_naics_notredundant) 
crosswalk_not_cbp <- crosswalk_not_cbp[nchar(crosswalk_not_cbp) == 6] # These are the primary ag codes and some codes beginning with 5 and 9 (a few services and government, likely not relevant)

# Get BEA codes corresponding to the NAICS codes in CBP
# Subset crosswalk to 6 digit
bea_naics6 <- bea_naics %>% filter(nchar(NAICS) == 6)

cbp_bea_lookup <- data.frame(NAICS = cbp_naics_notredundant, 
                              BEA_code = bea_naics6$BEA_Detail[match(cbp_naics_notredundant, bea_naics6$NAICS)])

# Remove redundant rows from the SUSB dataset and add column for BEA code
cbp_bea <- cbp12co %>%
  filter(naics %in% cbp_naics_notredundant) %>%
  left_join(cbp_bea_lookup, by = c('naics' = 'NAICS')) %>%
  group_by(fipstate, fipscty, BEA_code) %>%
  summarize_at(vars(emp, qp1, ap, est), sum)

# Number of establishments looks like the thing to use because it's the only one not suppressed.

# Map NASS to BEA (codes 111 and 112) -------------------------------------
  
# We have a complication where 11193,11194,11199 are in a single classification, as well as 1125 and 1129.
# Check whether these are included in the same BEA codes. If so we can just lump them under one code.

bea_naics %>% filter(grepl('^1119', NAICS)) # Only 1 code. "other crops" In fact all 1119 are included under this.
bea_naics %>% filter(grepl('^1125|^1129', NAICS)) # Only 1 BEA code. It's all included under other animal production (all except cows and chickens)

# Take only the first string before the first space character in the NASS NAICS codes.
# Also convert value to numeric but remove the comma first
nass_county_naics <- cdqt_county_noperations %>%
  mutate(NAICS = map_chr(strsplit(NAICS, split = ' '), 1)) %>%
  mutate(value = as.numeric(gsub(',', '', value)))

nass_uniquenaics <- unique(nass_county_naics$NAICS)

# First get rid of any redundant ones in the NASS that have a longer and more specific code.
nass_redundant <- map_lgl(nass_uniquenaics, ~ nchar(.) < max(nchar(grep(paste0('^', .), nass_uniquenaics, value = TRUE))))
nass_naics_notredundant <- nass_uniquenaics[!nass_redundant] # Some of these are actually less than 6 characters.

# Check and make sure the 1119 is in fact redundant
nass_county_naics %>% filter(state_fips %in% '01', county_fips %in% '001', grepl('^1119', NAICS)) # Yes, it is. Row 1 is equal to the sums of rows 2-3. 

# However we cannot ignore the less than 6 digit NAICS codes because most of it is actually less than 6 digits.
# Some of the four digit NAICS codes in the NASS data actually correspond to multiple BEA codes, so that's unfortunate.

# Check overlaps
intersect(nass_naics_notredundant, bea_naics$NAICS)
setdiff(nass_naics_notredundant, bea_naics$NAICS) # None.
setdiff(bea_naics$NAICS, nass_naics_notredundant)

# Number of BEA codes associated with each non redundant NAICS code in NASS
map_int(nass_naics_notredundant, ~ length(unique(bea_naics$BEA_Detail[grepl(paste0('^', .), bea_naics$NAICS)])))
# None are a problem except that oilseeds and grains are lumped into a single NASS NAICS code.

# Use NASS to find ratios of oilseed and grain production within each state to disaggregate code 1111 into 1111A and 1111B.
# That is done in another script, disaggregate_oilseed_and_grain.r. Read in the result of that script and use to disaggregate.
oilseed_grain_proportions <- read_csv(file.path(fp_out, 'oilseed_grain_proportions.csv'))
# This assumes the proportion of oilseed and grain are the same across all counties in a state.

# Create disaggregated grain and oilseed data.
nass1111_by_county <- nass_county_naics %>% 
  filter(NAICS %in% '1111') %>%
  left_join(oilseed_grain_proportions) %>%
  select(-grain, -oilseed) %>%
  pivot_longer(cols = c(proportion_grain, proportion_oilseed), names_to = 'crop', values_to = 'proportion') %>%
  mutate(value = round(value * proportion))

nass1111_by_county_edited <- nass1111_by_county %>%
  mutate(NAICS = if_else(crop == 'proportion_grain', '111130', '111110')) %>% # These are just one of the naics codes we could use.
  select(-crop, -proportion)

nass_county_naics_edited <- rbind(nass1111_by_county_edited %>% select(-state_name), nass_county_naics %>% filter(!NAICS %in% '1111'))


# Transform nass naics to nass bea ----------------------------------------

# combine them so that any "more specific" one is matched to its less specific parent code.
nass_naics_notredundant_modified <- c('111110','111130', nass_naics_notredundant[-1])

nass_county_naics_matchidx <- map_int(nass_naics_notredundant_modified, function(code) {
  subcodes <- map(2:nchar(code), ~ substr(code, 1, .)) # all possible subcodes
  match_idx <- map(subcodes, ~ grep(paste0('^', .), bea_naics$NAICS))
  # Find the longest matching code
  longest_match <- max(which(map_int(match_idx, length) > 0))
  ifelse(longest_match > 0, match_idx[[longest_match]], NA)
})

# Get BEA codes corresponding to the matches
nass_bea_lookup <- data.frame(NAICS = nass_naics_notredundant_modified, 
                              BEA_code = bea_naics$BEA_Detail[nass_county_naics_matchidx])

# Remove redundant rows from the NASS dataset and add column for BEA code
nass_county_bea <- nass_county_naics_edited %>%
  filter(NAICS %in% nass_naics_notredundant_modified) %>%
  left_join(nass_bea_lookup) %>%
  select(-NAICS) %>%
  group_by(state_fips, state_abbrev, county_fips, county_name, BEA_code) %>%
  summarize_all(sum)

# Combine SUSB and NASS data ----------------------------------------------

# Combine SUSB and NASS so that SUSB covers code 113*** and above, and NASS covers 111*** and 112***.
# We will only have establishments for the NASS codes, we will also have some other fields for the CBP codes but likely won't use them.

nass_county_bea <- nass_county_bea %>%
  ungroup %>%
  rename(n_establishments = value) %>%
  select(-state_abbrev)

cbp_county_bea <- cbp_bea %>%
  setNames(c('state_fips', 'county_fips', 'BEA_code', 'n_employees', 'q1_payroll', 'annual_payroll', 'n_establishments'))


cbp_nass_county_bea <- bind_rows(nass_county_bea, ungroup(cbp_county_bea)) 


# Correct Bedford City VA -------------------------------------------------

# This modification added 15 Jan 2021. Bedford City (51515) was added to Bedford County (51019), VA.
bedford_city <- cbp_nass_county_bea %>% filter(state_fips %in% "51", county_fips %in% "515")
bedford_county <- cbp_nass_county_bea %>% filter(state_fips %in% "51", county_fips %in% "019")
bedford_joined <- full_join(bedford_city, bedford_county, by = c('state_fips', 'BEA_code')) %>%
  mutate(n_establishments = pmap_dbl(., function(n_establishments.x,n_establishments.y,...) sum(n_establishments.x,n_establishments.y,na.rm=TRUE)),
         n_employees = pmap_dbl(., function(n_employees.x,n_employees.y,...) sum(n_employees.x,n_employees.y,na.rm=TRUE)),
         q1_payroll = pmap_dbl(., function(q1_payroll.x,q1_payroll.y,...) sum(q1_payroll.x,q1_payroll.y,na.rm=TRUE)),
         annual_payroll = pmap_dbl(., function(annual_payroll.x,annual_payroll.y,...) sum(annual_payroll.x,annual_payroll.y,na.rm=TRUE)),
         county_fips = rep("019", nrow(.)),
         county_name = rep("BEDFORD", nrow(.))) %>%
  select(state_fips, county_fips, county_name, BEA_code, n_establishments, n_employees, q1_payroll, annual_payroll)

# Remove old Bedford City and County data from the weighting dataframe and add the new summed County data to it.
cbp_nass_county_bea <- cbp_nass_county_bea %>%
  filter(!(state_fips %in% '51' & county_fips %in% c('019', '515'))) %>%
  bind_rows(bedford_joined)

write_csv(cbp_nass_county_bea, file.path(fp_out, 'county_weightings_for_downscale.csv'))


# NASS and SUSB by state --------------------------------------------------

# The following aggregates NASS variables by state instead of county.
# SUSB is used for state-level non-agricultural codes.

# This code is combined and modified from nass_extract_impute.R and combine_nass_susb_weightings.R

# All variables that are classified by state x NAICS code under operations, labor, and income
# Get rid of duplicates, set row names, convert NAICS into a short character
# Convert value column to numeric, setting suppressed rows to NA.
cdqt_naics_state <- cdqt %>%
  filter(grepl('^NAICS', X14)) %>%
  filter(!grepl('AND', X14)) %>%
  filter(grepl('FARM OPERATIONS|LABOR|INCOME|ACRES|COMMODITY TOTALS', X6)) %>%
  filter(!grepl('ENVIRONMENTAL', X5)) %>%
  filter(!grepl('PRACTICES', X6)) %>%
  filter(!grepl('FARM OPERATIONS, ORGANIZATION', X6, fixed = TRUE)) %>% # this seems fairly unnecessary.
  filter(!grepl('INCOME, NET CASH FARM', X6, fixed = TRUE)) %>% # also not necessary I think.
  filter(X8 %in% c('STATE', 'NATIONAL')) %>%
  select(X6, X7, X8, X9, X10, X11, X14, X15) %>%
  setNames(c('variable','category','level', 'state_fips', 'state_abbrev', 'state_name', 'NAICS', 'value')) %>%
  filter(!duplicated(.)) %>%
  mutate(NAICS = gsub('NAICS CLASSIFICATION: ', '', NAICS),
         NAICS = gsub('\\(|\\)', '', NAICS))

tail(sort(unique(cdqt_naics_state$value))) # Both D and Z codes are included here. 
sum(cdqt_naics_state$value == '(D)') # about 10K values. Not disclosed for confidentiality. Should be imputed. 
sum(cdqt_naics_state$value == '(Z)') # 147 values. These are effectively zero values. -- also impute these.

cdqt_naics_state <- cdqt_naics_state %>%
  mutate(
    suppressed = value %in% c('(D)', '(Z)'),
    value = case_when(
      value %in% c('(D)', '(Z)') ~ as.numeric(NA),
      TRUE ~ as.numeric(gsub(',', '', value))
    ))

unique(cdqt_naics_state$variable) # 34 variables. Some are sums of other variables. (now >100 with land acreage)
unique(cdqt_naics_state$NAICS) # this has all the ag naics codes.

# Reduce the number of variables to a final list of a few important ones.
vars_to_use <- c('FARM OPERATIONS - NUMBER OF OPERATIONS', 
                 'LABOR, HIRED - EXPENSE, MEASURED IN $',
                 'LABOR, CONTRACT - EXPENSE, MEASURED IN $',
                 'INCOME, FARM-RELATED - RECEIPTS, MEASURED IN $',
                 'COMMODITY TOTALS, INCL GOVT PROGRAMS - RECEIPTS, MEASURED IN $',
                 'LABOR, HIRED - NUMBER OF WORKERS',
                 'AG LAND, CROPLAND - ACRES',
                 'AG LAND, PASTURELAND, (EXCL CROPLAND & WOODLAND) - ACRES')
vars_shortnames <- c('n_operations', 'labor_hired_expense', 'labor_contract_expense', 'income', 'receipts', 'n_workers', 'cropland', 'pastureland')


cdqt_naics_state <- cdqt_naics_state %>%
  filter(variable %in% vars_to_use) %>%
  mutate(variable = vars_shortnames[match(variable, vars_to_use)])

# Widen this data frame by state so that each variable has a column within state and NAICS

cdqt_naics_state_wide <- cdqt_naics_state %>%
  select(-category, -suppressed) %>%
  pivot_wider(names_from = variable, values_from = value, values_fill = list(value = 0))

# Get rid of the NAICS codes entirely that have no data other than number of operations.
naicstoremove <- cdqt_naics_state_wide %>% group_by(NAICS) %>%
  summarize(no_data = all(labor_hired_expense == 0 & labor_contract_expense == 0 & receipts == 0 & n_workers == 0))

cdqt_naics_state_wide <- cdqt_naics_state_wide %>%
  filter(!NAICS %in% naicstoremove$NAICS[naicstoremove$no_data]) # There are 374 missing values out of 5552 (694*8) so can be imputed.

# If there are spurious zeroes, convert them to NA.
map(cdqt_naics_state_wide, ~ sum(. == 0, na.rm = TRUE))

cdqt_naics_state_wide <- cdqt_naics_state_wide %>%
  mutate_at(vars(labor_hired_expense, income, receipts, cropland, pastureland, n_workers), ~ if_else(. == 0, as.numeric(NA), .))

# Repivot to long form.
cdqt_naics_state <- cdqt_naics_state_wide %>%
  pivot_longer(n_operations:n_workers, names_to = 'variable')

# We cannot impute when done by state row because there are too many variables.

# Use MICE to create some imputations of this dataset ---------------------

cdqt_to_impute <- cdqt_naics_state_wide %>%
  filter(!level %in% 'NATIONAL') %>%
  select(state_abbrev, NAICS, n_operations:n_workers) %>%
  mutate(state_abbrev = factor(state_abbrev), NAICS = factor(NAICS)) %>%
  mutate_at(vars(n_operations:n_workers), ~ log(. + 1))

cdqt_imputed <- mice::mice(data = as.data.frame(cdqt_to_impute), m = 10, seed = 222, method = 'rf')  # Random forest imputation.

cdqt_imputed_complete <- mice::complete(cdqt_imputed) %>%
  mutate_at(vars(n_operations:n_workers), ~ round(exp(.) - 1)) %>%
  rename_at(vars(receipts:n_workers), ~ paste(., 'imputed', sep = '_'))


# Join imputed with real data and replace ---------------------------------

# Only replace the ones that are needed to replace

cdqt_naics_wide_withimp <- cdqt_naics_state_wide %>%
  left_join(cdqt_imputed_complete %>% select(-n_operations))

# Manipulate this wide DF to long form to put the imputed column side by side with the non imputed column.
cdqt_naics_withimp <- cdqt_naics_wide_withimp %>%
  pivot_longer(n_operations:n_workers_imputed) %>%
  mutate(imputed = if_else(grepl('imputed', name), 'imputed', 'original'), name = gsub('_imputed', '', name)) %>%
  pivot_wider(names_from = imputed)

# If there is no original value, replace it with the imputed value
cdqt_naics_withimp <- cdqt_naics_withimp %>%
  mutate(value = if_else(is.na(original), imputed, original)) %>%
  select(-original, -imputed) %>%
  pivot_wider()


# Export imputed dataframe ------------------------------------------------

nass_naics <- cdqt_naics_withimp %>%
  arrange(NAICS, state_fips)

write_csv(nass_naics, '/nfs/qread-data/cfs_io_analysis/NASS2012_receipts_workers_land_NAICS_imputed.csv')


# Map state NAICS NASS and SUSB to BEA ------------------------------------

# Read SUSB data for all other NAICS codes besides primary agg.
susb12 <- read_csv('data/raw_data/Census/SUSB/us_state_6digitnaics_2012.txt', col_types = 'fffnnnffnfnfccc') 
susb_total <- susb12 %>% 
  filter(ENTRSIZEDSCR %in% 'Total')

# Summarize n of employees for SUSB ---------------------------------------

# For each SCTG code, find the numbers of employees in each of the NAICS codes that go with it in each state.
# Also use receipts.

# Total employees and receipts by NAICS code by state in SUSB - there is no need to group sum them because they already represent totals.
# However we have a lot of different levels of NAICS codes.
susb_total <- susb_total %>% 
  mutate(RCPT_N = RCPT_N * 1000) %>%
  select(STATE, STATEDSCR, NAICS, NAICSDSCR, EMPL_N, RCPT_N) %>%
  setNames(c('state_fips', 'state_name', 'NAICS', 'NAICS_description', 'employees', 'receipts'))

# Map SUSB to BEA (codes 113 and above) -----------------------------------

# Unique codes in the SUSB dataset
susb_naics <- unique(gsub('-','', susb_total$NAICS))

# First get rid of any redundant ones in the SUSB that have a longer and more specific code.
susb_redundant <- map_lgl(susb_naics, ~ nchar(.) < max(nchar(grep(paste0('^', .), susb_naics, value = TRUE))))
susb_naics_notredundant <- susb_naics[!susb_redundant] # Some of these are actually less than 6 characters.

# See if the <6 characters are in FSC.
shortcodes <- susb_naics_notredundant[nchar(susb_naics_notredundant) < 6]
susb_total %>% filter(NAICS %in% c('44-45','48-49')) # We can ignore these.

susb_naics_notredundant <- susb_naics_notredundant[nchar(susb_naics_notredundant) == 6] # This begins at 113110 so that's good.

# Check which codes are in the crosswalk and which aren't
intersect(susb_naics_notredundant, bea_naics$NAICS)
setdiff(susb_naics_notredundant, bea_naics$NAICS) # None.
crosswalk_not_susb <- setdiff(bea_naics$NAICS, susb_naics_notredundant)
crosswalk_not_susb[nchar(crosswalk_not_susb) == 6] # Same codes as CBP above.

# combine them so that any "more specific" one is matched to its less specific parent code.
susb_naics_matchidx <- map_int(susb_naics_notredundant, function(code) {
  subcodes <- map(2:nchar(code), ~ substr(code, 1, .)) # all possible subcodes
  match_idx <- map(subcodes, ~ grep(paste0('^', .), bea_naics$NAICS))
  # Find the longest matching code
  longest_match <- max(which(map_int(match_idx, length) > 0))
  ifelse(longest_match > 0, match_idx[[longest_match]], NA)
})

# Get BEA codes corresponding to the matches
susb_bea_lookup <- data.frame(NAICS = susb_naics_notredundant, 
                              BEA_code = bea_naics$BEA_Detail[susb_naics_matchidx])

# Remove redundant rows from the SUSB dataset and add column for BEA code
susb_bea <- susb_total %>%
  filter(NAICS %in% susb_naics_notredundant) %>%
  left_join(susb_bea_lookup) %>%
  select(-NAICS, -NAICS_description) %>%
  group_by(state_fips, state_name, BEA_code) %>%
  summarize_all(sum)

# Map NASS to BEA (codes 111 and 112) -------------------------------------

# We have a complication where 11193,11194,11199 are in a single classification, as well as 1125 and 1129.

# Take only the first string before the first space character in the NASS NAICS codes.
nass_naics <- nass_naics %>%
  mutate(NAICS = map_chr(strsplit(NAICS, split = ' '), 1))

nass_uniquenaics <- unique(nass_naics$NAICS)

# First get rid of any redundant ones in the NASS that have a longer and more specific code.
nass_redundant <- map_lgl(nass_uniquenaics, ~ nchar(.) < max(nchar(grep(paste0('^', .), nass_uniquenaics, value = TRUE))))
nass_naics_notredundant <- nass_uniquenaics[!nass_redundant] # Some of these are actually less than 6 characters.

# Check and make sure the 1119 is in fact redundant
nass_naics %>% filter(state_fips %in% '99', grepl('^1119', NAICS)) # Yes, it is. Row 1 is equal to the sums of rows 2-4. 

# However we cannot ignore the less than 6 digit NAICS codes because most of it is actually less than 6 digits.
# Some of the four digit NAICS codes in the NASS data actually correspond to multiple BEA codes, so that's unfortunate.

# Check overlaps
intersect(nass_naics_notredundant, bea_naics$NAICS)
setdiff(nass_naics_notredundant, bea_naics$NAICS) # None
setdiff(bea_naics$NAICS, nass_naics_notredundant)

# Number of BEA codes associated with each non redundant NAICS code in NASS
map_int(nass_naics_notredundant, ~ length(unique(bea_naics$BEA_Detail[grepl(paste0('^', .), bea_naics$NAICS)])))
# None are a problem except that oilseeds and grains are lumped into a single NASS NAICS code.

# Use NASS to find ratios of oilseed and grain production within each state to disaggregate code 1111 into 1111A and 1111B.

# Create disaggregated grain and oilseed data.
nass1111 <- nass_naics %>% 
  filter(NAICS %in% '1111') %>%
  left_join(oilseed_grain_proportions) %>%
  select(-grain, -oilseed) %>%
  pivot_longer(cols = c(proportion_grain, proportion_oilseed), names_to = 'crop', values_to = 'proportion') %>%
  mutate(receipts = round(receipts * proportion),
         income = round(income * proportion),
         n_workers = round(n_workers * proportion),
         cropland = round(cropland * proportion),
         pastureland = round(pastureland * proportion))

nass1111_edited <- nass1111 %>%
  mutate(NAICS = if_else(crop == 'proportion_grain', '111130', '111110')) %>% # These are just one of the naics codes we could use.
  select(-crop, -proportion)

nass_naics_edited <- rbind(nass1111_edited, nass_naics %>% filter(!NAICS %in% '1111'))

# Transform nass naics to nass bea ----------------------------------------

# combine them so that any "more specific" one is matched to its less specific parent code.
nass_naics_notredundant_modified <- c('111110','111130', nass_naics_notredundant[-1])

nass_naics_matchidx <- map_int(nass_naics_notredundant_modified, function(code) {
  subcodes <- map(2:nchar(code), ~ substr(code, 1, .)) # all possible subcodes
  match_idx <- map(subcodes, ~ grep(paste0('^', .), bea_naics$NAICS))
  # Find the longest matching code
  longest_match <- max(which(map_int(match_idx, length) > 0))
  ifelse(longest_match > 0, match_idx[[longest_match]], NA)
})

# Get BEA codes corresponding to the matches
nass_bea_lookup <- data.frame(NAICS = nass_naics_notredundant_modified, 
                              BEA_code = bea_naics$BEA_Detail[nass_naics_matchidx])

# Remove redundant rows from the NASS dataset and add column for BEA code
nass_bea <- nass_naics_edited %>%
  filter(NAICS %in% nass_naics_notredundant_modified) %>%
  left_join(nass_bea_lookup) %>%
  select(-NAICS) %>%
  group_by(level, state_fips, state_abbrev, state_name, BEA_code) %>%
  summarize_all(sum)


# Combine SUSB and NASS data ----------------------------------------------

# Combine SUSB and NASS so that SUSB covers code 113*** and above, and NASS covers 111*** and 112***.

nass_bea_edited <- nass_bea %>%
  ungroup %>%
  select(state_fips, state_name, BEA_code, n_workers, receipts, cropland, pastureland) %>%
  setNames(c(names(susb_bea), 'cropland', 'pastureland'))

susb_nass_bea <- bind_rows(nass_bea_edited, ungroup(susb_bea)) %>%
  mutate(state_name = if_else(state_name == 'United States', 'US TOTAL', state_name),
         state_name = toupper(state_name))

write_csv(susb_nass_bea, file.path(fp_out, 'susb_nass_workers_receipts_land_bea.csv'))


# Tabulate annual and permanent cropland ----------------------------------

nass_naics_join_bea <- nass_naics_edited %>%
  filter(NAICS %in% nass_naics_notredundant_modified) %>%
  left_join(nass_bea_lookup) 

permanent_codes <- c('1113', '11193') # All others are annual codes except for 1114 which is assumed as 50% annual, 50% permanent cropland.

nass_naics_join_bea <- nass_naics_join_bea %>%
  mutate(annual_cropland = case_when(NAICS %in% permanent_codes ~ 0,
                                     NAICS %in% '1114' ~ cropland/2,
                                     TRUE ~ cropland),
         permanent_cropland = case_when(NAICS %in% permanent_codes ~ cropland,
                                        NAICS %in% '1114' ~ cropland/2,
                                        TRUE ~ 0))

nass_bea_receipts_land <- nass_naics_join_bea %>%
  group_by(state_fips, state_abbrev, state_name, BEA_code) %>%
  summarize(across(c(n_operations, labor_hired_expense, labor_contract_expense, income, receipts, n_workers, annual_cropland, permanent_cropland, pastureland), sum))

write_csv(nass_bea_receipts_land, 'data/cfs_io_analysis/nass_workers_receipts_3landtypes_bea.csv')


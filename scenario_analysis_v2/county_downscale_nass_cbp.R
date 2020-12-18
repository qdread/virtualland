# NASS extract county-level data
# QDR / Virtualland / 17 Dec. 2020

# Some code modified from combine_nass_susb_weightings.R

library(tidyverse)

is_local <- dir.exists('Q:/')

fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_out <- file.path(fp, 'cfs_io_analysis')
fp_crosswalk <- file.path(fp, 'crossreference_tables')

# Read crosswalk that maps NAICS 07 and NAICS 12 to the BEA codes
bea_naics <- read_csv(file.path(fp_crosswalk, 'BEA_NAICS07_NAICS12_crosswalk.csv'))

# Read text file ---------------------------------------------------

cdqt_file <- file.path(fp, 'raw_data/USDA/2012_cdqt_data.txt')

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

cbp12co <- read_csv(file.path(fp, 'raw_data/Census/CBP/cbp12co.txt'))

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
intersect(cbp_naics_notredundant, bea_naics$related_2012_NAICS_6digit)
setdiff(cbp_naics_notredundant, bea_naics$related_2012_NAICS_6digit)
setdiff(bea_naics$related_2012_NAICS_6digit, cbp_naics_notredundant) # The primary ag codes and a few other ones that are likely not relevant.
cbp12co %>% filter(naics %in% c('517911','517919')) # Not relevant

# Get BEA codes corresponding to the NAICS codes in CBP
cbp_bea_lookup <- data.frame(NAICS = cbp_naics_notredundant, 
                              BEA_code = bea_naics$BEA_Code[match(cbp_naics_notredundant, bea_naics$related_2012_NAICS_6digit)])

# Remove redundant rows from the SUSB dataset and add column for BEA code
cbp_bea <- cbp12co %>%
  filter(naics %in% cbp_naics_notredundant) %>%
  left_join(cbp_bea_lookup, by = c('naics' = 'NAICS')) %>%
  group_by(fipstate, fipscty, BEA_code) %>%
  summarize_at(vars(emp, qp1, ap, est), sum)

# Number of establishments looks like the thing to use because it's the only one not suppressed.

### FIXME all below here is not modified from the other script yet.
# Map NASS to BEA (codes 111 and 112) -------------------------------------
  
# We have a complication where 11193,11194,11199 are in a single classification, as well as 1125 and 1129.
# Check whether these are included in the same BEA codes. If so we can just lump them under one code.

bea_naics %>% filter(grepl('^1119', related_2012_NAICS_6digit)) # Only 1 code. "other crops" In fact all 1119 are included under this.
bea_naics %>% filter(grepl('^1125|^1129', related_2012_NAICS_6digit)) # Only 1 BEA code. It's all included under other animal production (all except cows and chickens)

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
intersect(nass_naics_notredundant, bea_naics$related_2012_NAICS_6digit)
setdiff(nass_naics_notredundant, bea_naics$related_2012_NAICS_6digit) # These are all 4 and 5 digit codes.
setdiff(bea_naics$related_2012_NAICS_6digit, nass_naics_notredundant)

# Number of BEA codes associated with each non redundant NAICS code in NASS
map_int(nass_naics_notredundant, ~ length(unique(bea_naics$BEA_Code[grepl(paste0('^', .), bea_naics$related_2012_NAICS_6digit)])))
# None are a problem except that oilseeds and grains are lumped into a single NASS NAICS code.

# Use NASS to find ratios of oilseed and grain production within each state to disaggregate code 1111 into 1111A and 1111B.
# That is done in another script, disaggregate_oilseed_and_grain.r. Read in the result of that script and use to disaggregate.
oilseed_grain_proportions <- read_csv(file.path(fp_out, 'oilseed_grain_proportions.csv') )
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
  match_idx <- map(subcodes, ~ grep(paste0('^', .), bea_naics$related_2012_NAICS_6digit))
  # Find the longest matching code
  longest_match <- max(which(map_int(match_idx, length) > 0))
  ifelse(longest_match > 0, match_idx[[longest_match]], NA)
})

# Get BEA codes corresponding to the matches
nass_bea_lookup <- data.frame(NAICS = nass_naics_notredundant_modified, 
                              BEA_code = bea_naics$BEA_Code[nass_county_naics_matchidx])

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


write_csv(cbp_nass_county_bea, file.path(fp_out, 'county_weightings_for_downscale.csv'))

# Test for double counting in NASS 2012 land data

cdqt_agland <- cdqt %>% filter(X6 == "AG LAND, CROPLAND - ACRES")

# Look at a particular state
cdqt_agland %>% filter(X11 == 'MARYLAND', is.na(X14))
#1396144

naicsacresmd <- cdqt_agland %>% filter(X11 == 'MARYLAND', grepl('NAICS', X14)) %>% pull(X15)
naicsacresmd <- sub(',', '', naicsacresmd) %>% as.numeric
sum(naicsacresmd) # Slightly greater but most likely not a huge issue.

cdqt_agland_totals <- cdqt_agland %>% filter(is.na(X14), X1 == "1", X2 == "1", X3 == "16", X4 == "1")
cdqt_agland_naics <- cdqt_agland %>% filter(grepl('NAICS', X14))

# compare sums
cdqt_agland_totals <- cdqt_agland_totals %>% select(X11, X15) %>%
  rename('state'=X11, 'land'=X15) %>%
  mutate(land = gsub(',', '', land) %>% as.numeric)

cdqt_agland_naics <- cdqt_agland_naics %>% select(X11, X14, X15) %>%
  rename('state'=X11, 'naics'=X14, 'land'=X15) %>%
  mutate(land = gsub(',', '', land) %>% as.numeric)

cdqt_agland_naics_totals <- cdqt_agland_naics %>%
  group_by(state) %>%
  summarize(land_naics=sum(land, na.rm=TRUE))

left_join(cdqt_agland_totals, cdqt_agland_naics_totals) %>% print(n=nrow(.)) # For many states, there is some double counting but not enough to make a big difference.

# Check the receipts
usnaicstotals <- cdqt %>% filter(X5 == "ECONOMICS", grepl("NAICS", X14), X11 == "US TOTAL", grepl("$", X6, fixed = TRUE))
unique(usnaicstotals$X6)

usreceipts <- usnaicstotals %>% filter(grepl('RECEIPTS', X6)) %>% select(X6,X14,X15)

usreceipts %>% filter(X6 == "INCOME, FARM-RELATED - RECEIPTS, MEASURED IN $")

# Commodity receipts for naics x state
naicsreceipts <- cdqt %>% filter(grepl("NAICS", X14), X6 == "COMMODITY TOTALS, INCL GOVT PROGRAMS - RECEIPTS, MEASURED IN $")
# Some will require imputation but a lot of them are there.
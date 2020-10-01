# Reduce faf_by_bea to agriculture only.

library(tidyverse)

fp_out <- file.path(ifelse(is_local, 'Q:', '/nfs/qread-data'), 'cfs_io_analysis')
fp_github <- ifelse(is_local, '~/Documents/GitHub/foodwaste/virtualland', '~/virtualland')

# Load FAF data
load(file.path(fp_out, 'faf_by_bea.RData'))

crop_codes <- sprintf('%02d', c(2,3,6,7,8,9))
pasture_codes <- sprintf('%02d', c(1,4,5))

faf_by_bea_ag <- faf_by_bea %>% filter(SCTG_Code %in% c(crop_codes, pasture_codes))

write_csv(faf_by_bea_ag, file.path(fp_out, 'scenarios/flows_baseline.csv'))

faf_by_bea_domestic <- faf_by_bea_ag %>% filter(trade_type != '2')

write.csv(faf_by_bea_domestic, file.path(fp_out, 'scenarios/flows_baseline_domestic.csv'))

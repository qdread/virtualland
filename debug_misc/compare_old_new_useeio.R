# Compare old DRC table and PCE vector (v1.1 which I manually modified) with the new one (v2.0) to ensure consistency
# QDR / Virtualland / 23 Mar 2021

# Old
library(tidyverse)

fp_bea <- 'data/raw_data/BEA/formatted'
fp_useeio <- file.path(ifelse(dir.exists('Q:/'), '~/Documents/GitHub/foodwaste', '~'), 'USEEIO/useeiopy/Model Builds/USEEIO2012')

use2012 <- read_csv(file.path(fp_bea, 'use2012.csv'))
pce2012_old <- setNames(use2012$F01000[1:389], use2012$X1[1:389])
drc2012_old <- read.csv(file.path(fp_useeio, 'USEEIO2012_DRC.csv'), row.names = 1, check.names = FALSE) 

# New
load('data/cfs_io_analysis/useeio2012v2.0_pce_drc.RData')

setdiff(names(pce2012), names(pce2012_old))
setdiff(names(pce2012_old), names(pce2012))

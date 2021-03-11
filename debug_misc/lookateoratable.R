# Attempt to extract relevant information from one or more of the EORA I-O tables

file <- 'data/cfs_io_analysis/eora/country_io_tables/IO_ZAF_2012_PurchasersPrice.txt'

zafraw <- read.table(file, sep = '\t')

zafraw[zafraw[,2] %in% "FAO-LANDINPUTS", ]

zaf_land_table <- zafraw[c(1:4, which(zafraw[,2] %in% 'FAO-LANDINPUTS')), ]

zaf_land_table[, c(1:4, which(zaf_land_table[4,] %in% c('Meat','Dairy')))]

inputs_meatdairy <- zafraw[, c(1:4, which(zaf_land_table[4,] %in% c('Meat','Dairy')))]

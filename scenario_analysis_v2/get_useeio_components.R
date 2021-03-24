# Script to obtain the two components of the USEEIO v 2.0 alpha model needed for the analysis
# QDR / Virtualland / 23 Mar 2021

library(useeior)
useeio <- buildModel('USEEIOv2.0') # This is the default specification of the commodity-by-commodity 2012 model.

# Extract the personal consumption expenditure vector

pce2012 <- setNames(useeio$FinalDemand[, 'F01000/US'], dimnames(useeio$FinalDemand)[[1]])

# Extract the DRC table
# Domestic direct requirements coefficients and total DRC
drc2012 <- useeio$A
drc2012_domestic <- useeio$A_d

# Remove the /US from all the names of the objects.
f <- function(n) gsub('\\/.*', '', n)
names(pce2012) <- f(names(pce2012))
dimnames(drc2012) <- lapply(dimnames(drc2012), f)
dimnames(drc2012_domestic) <- lapply(dimnames(drc2012_domestic), f)

save(pce2012, drc2012, drc2012_domestic, file = 'data/cfs_io_analysis/useeio2012v2.0_pce_drc.RData')

# Write crosswalk that can be used to map the NAICS production categories to BEA codes
write_csv(useeio$crosswalk, 'data/cfs_io_analysis/useeio2012v2.0_NAICS_BEA_crosswalk.csv')

# Write PCE and DRCs to CSVs also
pce2012_df <- useeio$FinalDemand[, 'F01000/US', drop = FALSE]
dimnames(pce2012_df)[[1]] <- f(dimnames(pce2012_df)[[1]])
write.csv(pce2012_df, 'data/cfs_io_analysis/useeio2012v2.0_PCE.csv', row.names = TRUE)

write.csv(drc2012, 'data/cfs_io_analysis/useeio2012v2.0_DRC.csv', row.names = TRUE)
write.csv(drc2012_domestic, 'data/cfs_io_analysis/useeio2012v2.0_DRCdomestic.csv', row.names = TRUE)

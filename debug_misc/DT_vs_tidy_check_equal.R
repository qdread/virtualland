# Compare dttest output with output from old one

library(data.table)

landold <- fread('/nfs/qread-data/cfs_io_analysis/county_land_consumption_csvs/D_baseline_WR_baseline_landconsumption.csv')
landnew <- fread('/nfs/qread-data/cfs_io_analysis/dttest/D_baseline_WR_baseline_landconsumption.csv')

tncold <- fread('/nfs/qread-data/cfs_io_analysis/ecoregion_landflow_csvs/D_baseline_WR_baseline_landflows_tnc_to_tnc.csv')
tncnew <- fread('/nfs/qread-data/cfs_io_analysis/dttest/D_baseline_WR_baseline_landflows_tnc_to_tnc.csv')

all.equal(landold[order(scenario, county_to, county_from, land_type)], landnew[order(scenario, county_to, county_from, land_type)])
all.equal(tncold[order(scenario, TNC_from, TNC_to)], tncnew[order(scenario, TNC_from, TNC_to)])

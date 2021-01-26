# Which gave errors?

fileInfo <- pmap_dfr(scenario_combos, function(diet, waste) glue::glue('data/cfs_io_analysis/county_land_consumption_csvs/D_{diet}_WR_{waste}_landconsumption.csv') %>% file.info)

# Files last modified before Jan 11 16:00 must be rerun
lubridate::day(fileInfo$ctime)

jobstorun <- which(fileInfo$ctime < as.POSIXct("2021-01-20 16:00:00"))

sjob2 <- slurm_apply(land_consumption_by_scenario, scenario_combos[jobstorun, ], 
                    jobname = 'county_land2', nodes = 5, cpus_per_node = 2, 
                    global_objects = c('land_exch_tables'),
                    slurm_options = list(partition = 'sesync'))

sjob3 <- slurm_apply(land_consumption_by_scenario, scenario_combos[5, ], 
                     jobname = 'county_land3', nodes = 1, cpus_per_node = 1, 
                     global_objects = c('land_exch_tables'),
                     slurm_options = list(partition = 'sesync'))

# Read one and see if it is OK.
x <- read_csv('data/cfs_io_analysis/county_land_consumption_csvs/D_baseline_WR_baseline_landconsumption.csv', n_max = 10)


fileInfo <- pmap_dfr(scenario_combos, function(diet, waste) glue::glue('data/cfs_io_analysis/ecoregion_landflow_csvs/D_{diet}_WR_{waste}_landflows_tnc_to_tnc.csv') %>% file.info)

jobstorun <- which(fileInfo$ctime < as.POSIXct("2021-01-12 21:00:00"))

sjob_convertflows <- slurm_apply(county_flows_to_tnc_flows, scenario_combos[jobstorun, ], 
                                 jobname = 'convert_flows', nodes = 1, cpus_per_node = 4, preschedule_cores = FALSE,
                                 global_objects = c('county_tnc_weights'),
                                 slurm_options = list(partition = 'sesync'))

sjob_convertflows <- slurm_apply(county_flows_to_tnc_flows, scenario_combos[5, ], 
                                 jobname = 'convert_flows', nodes = 1, cpus_per_node = 1, preschedule_cores = FALSE,
                                 global_objects = c('county_tnc_weights'),
                                 slurm_options = list(partition = 'sesync'))

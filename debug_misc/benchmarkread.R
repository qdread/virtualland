library(microbenchmark)
library(data.table)
library(vroom)
library(readr)

nr <- 10000
f <- '/nfs/qread-data/cfs_io_analysis/county_consumption_csvs/D_baseline_WR_baseline_wide.csv'
f2 <- '/nfs/qread-data/cfs_io_analysis/county_land_consumption_csvs/D_baseline_WR_baseline_landconsumption.csv'

readbm <- microbenchmark(
  list = list(
  fread = fread(f, colClasses = rep(c('character','double'), c(3, 3141)), nrows = nr),
  vroom = vroom(f, col_types = paste0(strrep('c',3), strrep('d',3141)), n_max = nr),
  read_csv = read_csv(f, col_types = paste0(strrep('c',3), strrep('d',3141)), n_max = nr)
  ),
  times = 5
)

readbmwide <- microbenchmark(
  list = list(
    fread = fread(f, colClasses = rep(c('character','double'), c(3, 3141))),
    vroom = vroom(f, col_types = paste0(strrep('c',3), strrep('d',3141))),
    read_csv = read_csv(f, col_types = paste0(strrep('c',3), strrep('d',3141)))
  ),
  times = 5
)

readbmlong <- microbenchmark(
  list = list(
    fread = fread(f2, colClasses = rep(c('character', 'double'), c(5, 1))),
    vroom = vroom(f2, col_types = 'cccccd'),
    read_csv = read_csv(f2, col_types = 'cccccd')
  ),
  times = 5
)

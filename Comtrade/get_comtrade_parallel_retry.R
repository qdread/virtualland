# Get data from UN Comtrade: script to go back through and retry ones that returned errors
# QDR / virtualland / 1 June 2020

library(httr)
library(jsonlite)
library(tidyverse)

base_url <- 'https://comtrade.un.org/api/get?'

years <- 2010:2019

load('/nfs/qread-data/cfs_io_analysis/comtrade_rawdata.RData')

# Find all the country IDs and loop through them, getting data for each one for each year.
all_countries <- read_csv('/nfs/qread-data/cfs_io_analysis/comtrade_country_lookup.csv')
reporting_countries <- fromJSON('/nfs/qread-data/cfs_io_analysis/reporterAreas.json', simplifyDataFrame = TRUE)
partner_countries <- fromJSON('/nfs/qread-data/cfs_io_analysis/partnerAreas.json', simplifyDataFrame = TRUE)


reporting_x_year <- expand_grid(year = years, country = reporting_countries$results$id) %>%
  filter(!country %in% c('all', '841', '842'))
partner_x_year <- expand_grid(year = years, country = partner_countries$results$id) %>%
  filter(!country %in% c('all', '841', '842'))

get_one_year <- function(year, detail, reporting, partner) {
  Sys.sleep(5) # Slow things down to not overwhelm the API
  message('downloading data reported by ', reporting, ' from ', partner, ' in ', year)
  req <- try(GET(paste0(base_url, 'fmt=json&px=HS&freq=A&max=100000&r=', reporting, '&p=', partner, '&cc=AG', detail, '&ps=', year),
                 config(ssl_verifypeer = FALSE)), TRUE)
  if (inherits(req, 'try-error')) return('error in get')
  dat <- try(fromJSON(content(req, as = 'text'), simplifyDataFrame = TRUE), TRUE)
  if (inherits(dat, 'try-error')) return('error in json')
  return(dat$dataset)
}

all_partner <- map(1:length(all_partner), function(i) {
  if (!class(all_partner[[i]]) %in% 'character') return(all_partner[[i]])
  get_one_year(year = reporting_x_year$year[i], detail = 4, reporting = reporting_x_year$country[i], partner = 842)
})

all_reporting <- map(1:length(all_reporting), function(i) {
  if (!class(all_reporting[[i]]) %in% 'character') return(all_reporting[[i]])
  get_one_year(year = partner_x_year$year[i], detail = 4, reporting = 842, partner = partner_x_year$country[i])
})

save(all_partner, all_reporting, file = paste0('/nfs/qread-data/cfs_io_analysis/comtrade_rawdata', Sys.Date(), '.RData'))
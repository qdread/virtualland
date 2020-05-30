# Get data from UN Comtrade
# QDR / virtualland / 29 May 2020

library(httr)
library(jsonlite)
library(tidyverse)

base_url <- 'http://comtrade.un.org/api/get?'

years <- 2010:2019

# Find all the country IDs and loop through them, getting data for each one for each year.
reporting_countries <- fromJSON('https://comtrade.un.org/Data/cache/reporterAreas.json', simplifyDataFrame = TRUE)
partner_countries <- fromJSON('https://comtrade.un.org/Data/cache/partnerAreas.json', simplifyDataFrame = TRUE)


reporting_x_year <- expand_grid(year = years, country = reporting_countries$results$id) %>%
  filter(!country %in% c('all', '841', '842'))
partner_x_year <- expand_grid(year = years, country = partner_countries$results$id) %>%
  filter(!country %in% c('all', '841', '842'))

get_one_year <- function(year, detail, reporting, partner) {
  Sys.sleep(5) # Slow things down to not overwhelm the API
  message('downloading data reported by ', reporting, ' from ', partner, ' in ', year)
  req <- try(GET(paste0(base_url, 'fmt=json&px=HS&freq=A&max=100000&r=', reporting, '&p=', partner, '&cc=AG', detail, '&ps=', year)), TRUE)
  if (inherits(req, 'try-error')) return('error in get')
  dat <- try(fromJSON(content(req, as = 'text'), simplifyDataFrame = TRUE), TRUE)
  if (inherits(dat, 'try-error')) return('error in json')
  return(dat$dataset)
}

all_partner <- pmap(reporting_x_year, function(year, country) get_one_year(year = year, detail = 4, reporting = country, partner = 842))
all_reporting <- pmap(partner_x_year, function(year, country) get_one_year(year = year, detail = 4, reporting = 842, partner = country))

save(all_partner, all_reporting, file = '/nfs/qread-data/cfs_io_analysis/comtrade_rawdata.RData')
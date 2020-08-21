# Get data from UN Comtrade: script to go back through and retry ones that returned errors
# QDR / virtualland / 1 June 2020

library(httr)
library(jsonlite)
library(tidyverse)

fp_comtrade <- '/nfs/qread-data/raw_data/commodity_flows/Comtrade'
base_url <- 'https://comtrade.un.org/api/get?'

years <- 2010:2019

load(file.path(fp_comtrade, 'comtrade_rawdata2020-06-04.RData'))
# Find all the country IDs and loop through them, getting data for each one for each year.
all_countries <- read_csv(file.path(fp_comtrade, 'comtrade_country_lookup.csv'))
reporting_countries <- fromJSON(file.path(fp_comtrade, 'reporterAreas.json'), simplifyDataFrame = TRUE)
partner_countries <- fromJSON(file.path(fp_comtrade, 'partnerAreas.json'), simplifyDataFrame = TRUE)


reporting_x_year <- expand_grid(year = years, country = reporting_countries$results$id) %>%
  filter(!country %in% c('all', '841', '842'))
partner_x_year <- expand_grid(year = years, country = partner_countries$results$id) %>%
  filter(!country %in% c('all', '841', '842'))

get_one_year <- function(year, detail, reporting, partner) {
  Sys.sleep(30) # Slow things down to not overwhelm the API -- now do 30 seconds to limit to around 120 requests per hour.
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

save(all_partner, all_reporting, file = paste0(fp_comtrade, 'comtrade_rawdata', Sys.Date(), '.RData'))
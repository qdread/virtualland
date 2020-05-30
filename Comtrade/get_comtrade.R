# Get data from UN Comtrade
# QDR / virtualland / 29 May 2020

library(httr)
library(jsonlite)
library(tidyverse)

base_url <- 'http://comtrade.un.org/api/get?'

# 100000 rows, 2 digit codes, year 2012, reporting area 842 means USA
# px: Classification HS is harmonized system which looks like the most updated and best classification system
# cc: AG(number) means how many digit code level you want to get detail for, up to 6
# freq: A is annual, M would be monthly
# 
usreporting2012 <- GET(paste0(base_url, 'fmt=json&px=HS&freq=A&ps=2012&r=842&p=all&cc=AG2&max=100000'))
usreporting2012_df <- fromJSON(content(usreporting2012, as = 'text'), simplifyDataFrame = TRUE)
head(usreporting2012_df$dataset)

uspartner2012 <- GET(paste0(base_url, 'fmt=json&px=HS&freq=A&ps=2012&r=all&p=842&cc=AG2&max=100000'))
uspartner2012_df <- fromJSON(content(uspartner2012, as = 'text'), simplifyDataFrame = TRUE)

# Get data for each year 2010-present
# Try to get a finer level of detail
years <- 2010:2019

get_year_dat <- function(year, detail) {
  req_year_reporting <- GET(paste0(base_url, 'fmt=json&px=HS&freq=A&r=842&p=all&max=100000&cc=AG', detail, '&ps=', year))
  year_reporting <- fromJSON(content(req_year, as = 'text'), simplifyDataFrame = TRUE)
  req_year_partner <- GET(paste0(base_url, 'fmt=json&px=HS&freq=A&r=all&p=842&max=100000&cc=AG', detail, '&ps=', year))
  year_partner <- fromJSON(content(req_year, as = 'text'), simplifyDataFrame = TRUE)
  message(year, ' done')
  return(list(reporting = year_reporting, partner = year_partner))
}

all_year_dat <- map(years, get_year_dat, detail = 4)

usa_all_year_dat <- map_dfr(all_year_dat, 'dataset') 
# Can't do this all at once because there is too much to download.

# Find all the country IDs and loop through them, getting data for each one. Hopefully the API will allow.

reporting_countries <- fromJSON('https://comtrade.un.org/Data/cache/reporterAreas.json', simplifyDataFrame = TRUE)
partner_countries <- fromJSON('https://comtrade.un.org/Data/cache/partnerAreas.json', simplifyDataFrame = TRUE)

all_countries <- bind_rows(reporting_countries$results, partner_countries$results) %>%
  filter(!duplicated(.))
write_csv(all_countries, '/nfs/qread-data/cfs_io_analysis/comtrade_country_lookup.csv')

reporting_x_year <- expand_grid(year = years, country = reporting_countries$results$id) %>%
  filter(!country %in% c('all', '841', '842'))
partner_x_year <- expand_grid(year = years, country = partner_countries$results$id) %>%
  filter(!country %in% c('all', '841', '842'))

get_one_year <- function(year, detail, reporting, partner) {
  Sys.sleep(5) # Slow things down to not overwhelm the API
  req <- try(GET(paste0(base_url, 'fmt=json&px=HS&freq=A&max=100000&r=', reporting, '&p=', partner, '&cc=AG', detail, '&ps=', year)), TRUE)
  if (inherits(req, 'try-error')) return('error')
  dat <- fromJSON(content(req, as = 'text'), simplifyDataFrame = TRUE)
  return(dat$dataset)
}

all_partner <- pmap(reporting_x_year, function(year, country) get_one_year(year = year, detail = 4, reporting = country, partner = 842))
all_reporting <- pmap(partner_x_year, function(year, country) get_one_year(year = year, detail = 4, reporting = 842, partner = country))

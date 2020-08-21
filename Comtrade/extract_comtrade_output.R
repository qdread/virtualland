# Extract and check Comtrade output

library(tidyverse)
library(httr)
library(jsonlite)

fp_comtrade <- '/nfs/qread-data/raw_data/commodity_flows/Comtrade'
base_url <- 'https://comtrade.un.org/api/get?'

load(file.path(fp_comtrade, 'comtrade_rawdata2020-06-04.RData'))

all_countries <- read_csv(file.path(fp_comtrade, 'comtrade_country_lookup.csv'))
reporting_countries <- fromJSON(file.path(fp_comtrade, 'reporterAreas.json'), simplifyDataFrame = TRUE)
partner_countries <- fromJSON(file.path(fp_comtrade, 'partnerAreas.json'), simplifyDataFrame = TRUE)



# Check output ------------------------------------------------------------



years <- 2010:2019

reporting_x_year <- expand_grid(year = years, country = reporting_countries$results$id) %>%
  filter(!country %in% c('all', '841', '842'))
partner_x_year <- expand_grid(year = years, country = partner_countries$results$id) %>%
  filter(!country %in% c('all', '841', '842'))

table(sapply(all_partner, class))
table(sapply(all_reporting, class))

table(unlist(all_partner[which(sapply(all_partner, class) == 'character')]))
table(unlist(all_reporting[which(sapply(all_reporting, class) == 'character')]))

reporting_error_log <- partner_x_year %>%
  mutate(class = map_chr(all_reporting, class)) %>%
  left_join(all_countries, by = c('country' = 'id'))

reporting_error_log <- reporting_error_log %>%
  mutate(error_msg = sapply(1:length(all_reporting), function(i) ifelse(reporting_error_log$class[i] == 'character', all_reporting[[i]], 'none')))

partner_error_log <- reporting_x_year %>%
  mutate(class = map_chr(all_partner, class)) %>%
  left_join(all_countries, by = c('country' = 'id'))

partner_error_log <- partner_error_log %>%
  mutate(error_msg = sapply(1:length(all_partner), function(i) ifelse(partner_error_log$class[i] == 'character', all_partner[[i]], 'none')))
         
# Some are data.frame meaning there is data, some are empty list meaning there is no trade flow
# Some are character meaning that there is an error

# 44 errors getting, most errors were in JSON.
get_one_year <- function(year, detail, reporting, partner) {
  #Sys.sleep(5) # Slow things down to not overwhelm the API
  message('downloading data reported by ', reporting, ' from ', partner, ' in ', year)
  req <- try(GET(paste0(base_url, 'fmt=json&px=HS&freq=A&max=100000&r=', reporting, '&p=', partner, '&cc=AG', detail, '&ps=', year),
                 config(ssl_verifypeer = FALSE)), TRUE)
  if (inherits(req, 'try-error')) return('error in get')
  dat <- try(fromJSON(content(req, as = 'text'), simplifyDataFrame = TRUE), TRUE)
  if (inherits(dat, 'try-error')) return('error in json')
  return(dat$dataset)
}

# # The error in "get" still not doing anything
# get_errors_reporting <- reporting_error_log %>% filter(error_msg == 'error in get')
# 
# retry_reporting <- pmap(get_errors_reporting, function(year, country, ...) get_one_year(year = year, detail = 4, reporting = 842, partner = country))
# 
# get_errors_partner <- partner_error_log %>% filter(error_msg == 'error in get')
# 
# retry_partner <- pmap(get_errors_partner, function(year, country, ...) get_one_year(year = year, detail = 4, reporting = country, partner = 842))

### What about JSON errors

json_errors_partner <- partner_error_log %>% filter(error_msg == 'error in json')

retry2_partner <- pmap(json_errors_partner[1:10,], function(year, country, ...) get_one_year(year = year, detail = 4, reporting = country, partner = 842))

year <- 2010
reporting <- '499'
detail <- 4
partner <- '842'
req <- try(GET(paste0(base_url, 'fmt=json&px=HS&freq=A&max=100000&r=', reporting, '&p=', partner, '&cc=AG', detail, '&ps=', year),
               config(ssl_verifypeer = FALSE)), TRUE)


year <- 2010
reporting <- '4'
detail <- 4
partner <- '842'
req <- try(GET(paste0(base_url, 'fmt=json&px=HS&freq=A&max=100000&r=', reporting, '&p=', partner, '&cc=AG', detail, '&ps=', year)), TRUE)



# Put good output into DF -------------------------------------------------

reporting_good_output <- map(all_reporting, ~ if(class(.) == 'data.frame') . else NULL) %>%
  bind_rows
partner_good_output <- map(all_partner, ~ if(class(.) == 'data.frame') . else NULL) %>%
  bind_rows 

# Initial tests to see if it is OK
# Data reported by USA
reporting_world <- reporting_good_output %>% filter(ptTitle %in% 'World')
reporting_countries <- reporting_good_output %>% filter(!ptTitle %in% 'World')

reporting_world %>% group_by(yr, rgDesc) %>% summarize(value = sum(TradeValue)/1e9) %>% filter(yr == 2018)
reporting_countries %>% group_by(yr, rgDesc) %>% summarize(value = sum(TradeValue)/1e9) %>% filter(yr == 2018) 
# The two match.


# Write out the data to CSVs ----------------------------------------------

write_csv(reporting_good_output, file.path(fp_comtrade, 'comtrade_USAreported.csv'))
write_csv(partner_good_output, file.path(fp_comtrade, 'comtrade_partnerreported.csv'))

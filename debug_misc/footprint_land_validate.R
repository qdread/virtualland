# Validate county level land footprint

fips_codes <- mutate(fips_codes, county_code = paste0(state_code,county_code))

county_land_consumption <- left_join(county_land_consumption, fips_codes)

consumption_wide <- county_land_consumption %>% pivot_wider(names_from = landuse_type, values_from = consumption)

consumption_wide %>% arrange(-`Annual crops`)
# This isn't correct. The exchanges don't work that way.
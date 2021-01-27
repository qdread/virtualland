# Check inbound versus outbound

county_goods_flow_sums %>%
  filter(scenario_diet == 'baseline', scenario_waste == 'baseline', BEA_code == "1111A0") %>%
  arrange(-flow_inbound)
county_goods_flow_sums %>%
  filter(scenario_diet == 'baseline', scenario_waste == 'baseline', BEA_code == "1111A0") %>%
  arrange(-flow_outbound)
tnc_land_flow_sums %>%
  filter(scenario_diet == 'baseline', scenario_waste == 'baseline', land_type == "annual_cropland") %>%
  arrange(-flow_outbound)
tncmap <- st_read('data/raw_data/landuse/ecoregions/tnc_usa_aea.gpkg')
tncmap %>% filter(ECO_CODE %in% c('NA0804','NA0407'))

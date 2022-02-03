# Calculation of numbers shown in manuscript that changed for revision 2.
# QDR / Virtualland / 03 Feb 2022

library(data.table)

load('data/cfs_io_analysis/scenarios/all_app_data.RData')

# goods flow (monetary)
total_goods_flow <- county_goods_flow_sums[, .(flow = sum(flow_inbound_domestic)), by = .(scenario_diet, scenario_waste)]
base_flow <- total_goods_flow[scenario_diet == 'baseline' & scenario_waste == 'baseline']$flow
total_goods_flow[, pct_change := 100 * (flow - base_flow)/base_flow]

# land flow (ignoring landtype)
total_land_flow <- county_land_flow_sums[, .(flow = sum(flow_inbound_total)), by = .(scenario_diet, scenario_waste)]
base_flow <- total_land_flow[scenario_diet == 'baseline' & scenario_waste == 'baseline']$flow
total_land_flow[, pct_change := 100 * (flow - base_flow)/base_flow]

# biodiversity flow (ignoring landtype and taxon)
total_bio_flow <- county_extinction_flow_sums[, .(flow = sum(flow_inbound_total)), by = .(scenario_diet, scenario_waste)]
base_flow <- total_bio_flow[scenario_diet == 'baseline' & scenario_waste == 'baseline']$flow
total_bio_flow[, pct_change := 100 * (flow - base_flow)/base_flow]

# Sanity check: Do the different ways of dividing up flows all add up to the same numbers

faf_flows_tnc_joined <- read_csv(file.path(fp_out, 'FAF_all_flows_x_TNC.csv'))
faf_flows_tnc_pop <- read_csv(file.path(fp_out, 'FAF_all_flows_TNC_x_TNC.csv'))
tnc_flows <- read_csv(file.path(fp_out, 'TNC_x_TNC_all_flows.csv'))

# Sum up. All are the same.
sum(faf_flows_tnc_joined$cropland_flow, na.rm = TRUE)
sum(faf_flows_tnc_pop$cropland_flow, na.rm = TRUE)
sum(tnc_flows$cropland_flow, na.rm = TRUE)

# They also appear to be the same for the 
tnc_flows %>% group_by(TNC_orig, trade_type) %>% summarize(cf = sum(cropland_flow, na.rm = T))
faf_flows_tnc_joined %>% group_by(ECO_CODE, trade_type) %>% summarize(cf = sum(cropland_flow, na.rm = T))
faf_flows_tnc_pop %>% group_by(TNC_orig, trade_type) %>% summarize(cf = sum(cropland_flow, na.rm = T))



# We can harmonize the production codes with the trade codes because there should be some catch-all codes in production.
# Write codes to CSV

codes_trade_names <- unique(trade_tousa_qty[,c('Item Code', 'Item')])
codes_production_names <- map_dfr(list(production_crops, production_cropsprocessed, production_livestock, production_livestockprimary, production_livestockprocessed), ~ select(., `Item Code`, Item)) %>% unique

write_csv(codes_trade_names, '/nfs/qread-data/crossreference_tables/faostat_trade_codes.csv')
write_csv(codes_production_names, '/nfs/qread-data/crossreference_tables/faostat_production_codes.csv')

# Combine the two and write them for harmonization.
names(codes_trade_names) <- c('code', 'name_trade')
names(codes_production_names) <- c('code', 'name_production')

codes_all_names <- full_join(codes_trade_names, codes_production_names) %>% 
  arrange(code)

write_csv(codes_all_names, '/nfs/qread-data/crossreference_tables/faostat_all_codes.csv')
setdiff(codes_prodcrops_names$Item, codes_trade_names$name_trade)

# Try to check the codes that are "total" that are in production_crops but not trade.
codes_trade_names <- unique(trade_tousa_qty[,c('Item Code', 'Item')])
codes_prodcrops_names <- unique(production_crops[,c('Item Code', 'Item')])
codes_prodcropsproc_names <- unique(production_cropsprocessed[,c('Item Code', 'Item')])
codes_prodlivestock_names <- unique(production_livestock[,c('Item Code', 'Item')])
codes_prodlivestockprim_names <- unique(production_livestockprimary[,c('Item Code', 'Item')])
codes_prodlivestockproc_names <- unique(production_livestockprocessed[,c('Item Code', 'Item')])
setdiff(codes_prodcrops_names, codes_trade_names) %>% arrange(`Item Code`) %>% print(n=nrow(.))
setdiff(codes_prodcropsproc_names, codes_trade_names) %>% arrange(`Item Code`) %>% print(n=nrow(.)) # Good. Not much difference
setdiff(codes_prodlivestock_names, codes_trade_names) %>% arrange(`Item Code`) %>% print(n=nrow(.)) # Also 17 and 18 codes.
setdiff(codes_prodlivestockprim_names, codes_trade_names) %>% arrange(`Item Code`) %>% print(n=nrow(.)) # Same. 17 and 18.
setdiff(codes_prodlivestockproc_names, codes_trade_names) %>% arrange(`Item Code`) %>% print(n=nrow(.)) # Same.
# Codes beginning with 17 and 18 are double count codes (totals).

# Do the same for the other codes.

whichmaxfaforigin <- faf_flows %>% filter(trade_type==1) %>% group_by(BEA_Code, dms_orig) %>% summarize(value_2012=sum(value_2012)) %>% group_by(BEA_Code) %>% filter(value_2012==max(value_2012))
faflook <- read_csv(file.path(fp_out, 'faf_region_lookup.csv'))
faflook %>% filter(Code %in% whichmaxfaforigin$dms_orig)

whichmaxfafdest <- faf_flows %>% filter(trade_type==1) %>% group_by(BEA_Code, dms_dest) %>% summarize(value_2012=sum(value_2012)) %>% group_by(BEA_Code) %>% filter(value_2012==max(value_2012))
faflook %>% filter(Code %in% whichmaxfafdest$dms_dest)

howmanyincoming <- faf_flows %>% filter(trade_type == 1) %>% group_by(BEA_Code, dms_dest) %>% summarize(n_orig = length(unique(dms_orig)))

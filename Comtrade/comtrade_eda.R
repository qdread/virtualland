# Exploratory visualizations for Comtrade data
# QDR / Virtualland / 20 Aug 2020

library(tidyverse)

fp_comtrade <- '/nfs/qread-data/raw_data/commodity_flows/Comtrade'

# Load comtrade data. Both are several hundred MB.
ct_reporting <- read_csv(file.path(fp_comtrade, 'comtrade_USAreported.csv'))
ct_partner <- read_csv(file.path(fp_comtrade, 'comtrade_partnerreported.csv'))


# Country summaries across commodities ------------------------------------

# Value by country incoming by year. Sum across all types of commodities.
ct_reporting_sums <- ct_reporting %>% 
  group_by(yr, rgCode, rgDesc, ptCode, ptTitle, pt3ISO, ptCode2, ptTitle2, pt3ISO2) %>%
  summarize(NetWeight = sum(NetWeight, na.rm = TRUE),
            GrossWeight = sum(GrossWeight, na.rm = TRUE),
            TradeValue = sum(TradeValue, na.rm = TRUE))
ct_partner_sums <- ct_partner %>% 
  group_by(yr, rgCode, rgDesc, rtCode, rtTitle, rt3ISO) %>%
  summarize(NetWeight = sum(NetWeight, na.rm = TRUE),
            GrossWeight = sum(GrossWeight, na.rm = TRUE),
            TradeValue = sum(TradeValue, na.rm = TRUE))


# Get the top ten trade partners by year, removing the world summed value.
# Separately for import and export
ct_reporting_topten <- ct_reporting_sums %>%
  filter(!pt3ISO %in% "WLD") %>%
  group_by(yr, rgDesc) %>%
  arrange(-TradeValue) %>%
  slice(1:10)

ct_partner_topten <- ct_partner_sums %>%
  group_by(yr, rgDesc) %>%
  arrange(-TradeValue) %>%
  slice(1:10)

# US Reported top 10 import and export trading partners by year

theme_set(theme_minimal())


# Colors by country
country_list <- unique(c(ct_reporting_topten$ptTitle, ct_partner_topten$rtTitle))
fill_countries <- scale_fill_manual(values = sample(RColorBrewer::brewer.pal(7, "Dark2"), length(country_list), replace = TRUE),
                                    breaks = country_list)

# Split up by year and then create the plot.
reporting_import_topten_plots <- ct_reporting_topten %>%
  filter(rgDesc %in% "Import") %>%
  group_by(yr) %>%
  mutate(TradeValue = TradeValue/1e9) %>%
  nest() %>%
  mutate(plot = map2(data, yr, ~ ggplot(.x %>% arrange(TradeValue) %>%
                                          mutate(ptTitle = factor(ptTitle, levels = unique(ptTitle))), 
                                        aes(x = ptTitle, y = TradeValue, fill = ptTitle)) + 
                       geom_col() + 
                       labs(x = 'trading partner', y = 'value (billion$)') +
                       coord_flip() + 
                       fill_countries +
                       ggtitle(.y) +
                       theme(legend.position = "none"))) 
    
reporting_export_topten_plots <- ct_reporting_topten %>%
  filter(rgDesc %in% "Export") %>%
  group_by(yr) %>%
  mutate(TradeValue = TradeValue/1e9) %>%
  nest() %>%
  mutate(plot = map2(data, yr, ~ ggplot(.x %>% arrange(TradeValue) %>%
                                          mutate(ptTitle = factor(ptTitle, levels = unique(ptTitle))), 
                                        aes(x = ptTitle, y = TradeValue, fill = ptTitle)) + 
                       geom_col() + 
                       labs(x = 'trading partner', y = 'value (billion$)') +
                       coord_flip() + 
                       fill_countries +
                       ggtitle(.y) +
                       theme(legend.position = "none"))) 

# The partner dataset does not include China so we should probably use the reporting dataset.
partner_import_topten_plots <- ct_partner_topten %>%
  filter(rgDesc %in% "Import") %>%
  group_by(yr) %>%
  mutate(TradeValue = TradeValue/1e9) %>%
  nest() %>%
  mutate(plot = map2(data, yr, ~ ggplot(.x %>% arrange(TradeValue) %>%
                                          mutate(rtTitle = factor(rtTitle, levels = unique(rtTitle))), 
                                        aes(x = rtTitle, y = TradeValue, fill = rtTitle)) + 
                       geom_col() + 
                       labs(x = 'trading partner', y = 'value (billion$)') +
                       coord_flip() + 
                       fill_countries +
                       ggtitle(.y) +
                       theme(legend.position = "none"))) 

partner_export_topten_plots <- ct_partner_topten %>%
  filter(rgDesc %in% "Export") %>%
  group_by(yr) %>%
  mutate(TradeValue = TradeValue/1e9) %>%
  nest() %>%
  mutate(plot = map2(data, yr, ~ ggplot(.x %>% arrange(TradeValue) %>%
                                          mutate(rtTitle = factor(rtTitle, levels = unique(rtTitle))), 
                                        aes(x = rtTitle, y = TradeValue, fill = rtTitle)) + 
                       geom_col() + 
                       labs(x = 'trading partner', y = 'value (billion$)') +
                       coord_flip() + 
                       fill_countries +
                       ggtitle(.y) +
                       theme(legend.position = "none"))) 


# Commodity summaries -----------------------------------------------------

# Find the most-imported and most-exported commodities by year and country.
# Use ct reporting data.

# There are approximately 1000 commodity codes.
# Load the commodity lookup table.
commod_lookup <- readxl::read_xlsx(file.path(fp_comtrade, 'UNComtradeCommodityClassifications.xlsx'))

# Let's look at food only.
food_lookup <- commod_lookup %>% filter(grepl("^1", Code))

# Two digit codes.
food_2digit <- food_lookup %>% filter(nchar(Code) == 2)
food_3digit <- food_lookup %>% filter(nchar(Code) == 3) # Not really a major category
food_4digit <- food_lookup %>% filter(nchar(Code) == 4) 

# Most exported food commodities from the USA, and where they are sent to, by year.
food_trade <- ct_reporting %>%
  filter(!ptTitle %in% c('World'), nchar(cmdCode) == 4, grepl("^1", cmdCode))
  
# So far the top ten summarized does not work.
# Per year group the data by commodity, find the top ten countries, all else become other, add those up.
summarize_topten_by_year <- function(dat) {
  dat %>%
    select(ptTitle, pt3ISO, cmdCode, cmdDescE, TradeValue) %>%
    group_by(cmdCode, cmdDescE) %>%
    mutate(is_topten = TradeValue %in% sort(unique(TradeValue), decreasing = TRUE)[1:10]) %>%
    mutate(ptTitle_other = if_else(is_topten, ptTitle, 'other'),
           pt3ISO_other = if_else(is_topten, pt3ISO, 'other')) %>%
    group_by(ptTitle_other, pt3ISO_other, cmdCode, cmdDescE) %>%
    summarize(TradeValue = sum(TradeValue))
}

food_topten_exports <- food_trade %>%
  filter(rgDesc %in% 'Export') %>%
  group_by(yr, cmdCode) %>%
  mutate(totalValue = sum(TradeValue)) %>%
  group_by(yr) %>%
  nest() %>%
  mutate(topten = map(data, ~ filter(., totalValue %in% sort(unique(totalValue), decreasing = TRUE)[1:10]))) %>%
  mutate(topten_summarized = map(topten, summarize_topten_by_year))

# For each of the top ten exported foods in 2019, get the top 10 and then add up the rest as "other".
ggplot(food_topten_exports$topten_summarized[[10]], aes(x = substr(cmdDescE, 1, 20), y = TradeValue, fill = pt3ISO_other)) +
  geom_col(position = 'stack') +
  coord_flip()

food_topten_imports <- food_trade %>%
  filter(rgDesc %in% 'Import') %>%
  group_by(yr, cmdCode) %>%
  mutate(totalValue = sum(TradeValue)) %>%
  group_by(yr) %>%
  nest() %>%
  mutate(topten = map(data, ~ filter(., totalValue %in% sort(unique(totalValue), decreasing = TRUE)[1:10]))) %>%
  mutate(topten_summarized = map(topten, summarize_topten_by_year))

ggplot(food_topten_imports$topten_summarized[[10]], aes(x = substr(cmdDescE, 1, 20), y = TradeValue, fill = pt3ISO_other)) +
  geom_col(position = 'stack') +
  coord_flip()

# Exploratory visualizations for Comtrade data
# QDR / Virtualland / 20 Aug 2020

library(tidyverse)

# Load comtrade data. Both are several hundred MB.
ct_reporting <- read_csv('/nfs/qread-data/cfs_io_analysis/comtrade_USAreported.csv')
ct_partner <- read_csv('/nfs/qread-data/cfs_io_analysis/comtrade_partnerreported.csv')

# Value by country incoming by year. Sum across all types of commodities.
ct_reporting_sums <- ct_reporting %>% 
  group_by(yr, rgCode, rgDesc, ptCode, ptTitle, pt3ISO, ptCode2, ptTitle2, pt3ISO2) %>%
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

# US Reported top 10 import and export trading partners by year

theme_set(theme_minimal())

# Split up by year and then create the plot.
reporting_import_topten_plots <- ct_reporting_topten %>%
  filter(rgDesc %in% "Import") %>%
  group_by(yr) %>%
  mutate(TradeValue = TradeValue/1e9) %>%
  arrange(TradeValue) %>%
  mutate(ptTitle = factor(ptTitle, levels = unique(ptTitle))) %>%
  nest() %>%
  mutate(plot = map2(data, yr, ~ ggplot(.x, aes(x = ptTitle, y = TradeValue)) + 
                       geom_col() + 
                       labs(x = 'trading partner', y = 'value (billion$)') +
                       coord_flip() + 
                       ggtitle(.y))) %>%
  arrange(yr)
    

reporting_export_topten_plots <- ct_reporting_topten %>%
  filter(rgDesc %in% "Export") %>%
  group_by(yr) %>%
  mutate(TradeValue = TradeValue/1e9) %>%
  arrange(TradeValue) %>%
  mutate(ptTitle = factor(ptTitle, levels = unique(ptTitle))) %>%
  nest() %>%
  mutate(plot = map2(data, yr, ~ ggplot(.x, aes(x = ptTitle, y = TradeValue)) + 
                       geom_col() + 
                       labs(x = 'trading partner', y = 'value (billion$)') +
                       coord_flip() + 
                       ggtitle(.y))) %>%
  arrange(yr)


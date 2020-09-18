# Maps to visualize which foreign ecoregions are contributing the most cropland and pastureland 
# to virtual land transfers that end up in the USA (and foreign countries)
# QDR / virtualland / 18 Sep 2020

# Needed: global TNC Mollweide projection map
# Foreign VLT by TNC region

# Load packages and data

library(tidyverse)
library(sf)

is_local <- dir.exists('Q:/')

fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_crosswalk <- file.path(fp, 'crossreference_tables')
fp_out <- file.path(fp, 'cfs_io_analysis')
fp_github <- ifelse(is_local, '~/Documents/GitHub/foodwaste/virtualland', '~/virtualland')
fp_eco <- file.path(fp, 'raw_data/landuse/ecoregions')

# Load maps
countrymap <- st_read(file.path(fp_eco, 'countries_global_equalarea.gpkg'))
tncglobalmap <- st_read(file.path(fp_eco, 'tnc_global_equalarea.gpkg'))

# Load data
vlt_country <- read_csv(file.path(fp_out, 'foreign_VLT_by_country.csv'))
vlt_country_tnc <- read_csv(file.path(fp_out, 'foreign_VLT_by_country_x_TNC.csv'))

# Sum countryxTNC to get TNC
vlt_tnc <- vlt_country_tnc %>%
  group_by(ECO_CODE, ECO_NAME) %>%
  summarize(VLT_crop = sum(VLT_crop_region, na.rm = TRUE),
            VLT_pasture = sum(VLT_pasture_region, na.rm = TRUE))

# Change the names of VLT dataframes to match the country names in countrymap.
names_to_match <- c("Bolivia (Plurinational State of)", "China, mainland", "China, Hong Kong SAR", 
                    "Iran (Islamic Republic of)", "United Republic of Tanzania")
names_corrected <- c("Bolivia", "China", "Hong Kong", "Iran", "Tanzania")

for (i in 1:length(names_to_match)) {
  vlt_country$country_name[vlt_country$country_name == names_to_match[i]] <- names_corrected[i]
  vlt_country_tnc$country_name[vlt_country_tnc$country_name == names_to_match[i]] <- names_corrected[i]
}

# Join data with sf objects
countrymap <- left_join(countrymap, vlt_country, by = c('NAME_LONG' = 'country_name'))
tncglobalmap <- left_join(tncglobalmap, vlt_tnc)

# Set theme
theme_set(theme_minimal() + theme(legend.position = 'bottom',
                                  legend.text = element_text(size = 6)))

# Plot maps
p_crop_country <- ggplot(countrymap, aes(fill = VLT_crop)) +
  geom_sf(lwd = 0.1) +
  scale_fill_viridis_c(trans = 'log10', name = parse(text = 'Cropland~transfer~(km^2)')) +
  ggtitle('Virtual cropland transfer to USA by country')

p_past_country <- ggplot(countrymap, aes(fill = VLT_pasture)) +
  geom_sf(lwd = 0.1) +
  scale_fill_viridis_c(trans = 'log10', name = parse(text = 'Pastureland~transfer~(km^2)')) +
  ggtitle('Virtual pastureland transfer to USA by country')

p_crop_tnc <- ggplot(tncglobalmap, aes(fill = VLT_crop)) +
  geom_sf(lwd = 0.1) +
  scale_fill_viridis_c(trans = 'log10', name = parse(text = 'Cropland~transfer~(km^2)')) +
  ggtitle('Virtual cropland transfer to USA by TNC ecoregion')

p_past_tnc <- ggplot(tncglobalmap, aes(fill = VLT_pasture)) +
  geom_sf(lwd = 0.1) +
  scale_fill_viridis_c(trans = 'log10', name = parse(text = 'Pastureland~transfer~(km^2)')) +
  ggtitle('Virtual pastureland transfer to USA by TNC ecoregion')

png(file.path(fp_out, 'maps/global_vlt_to_usa.png'), width = 9, height = 6, res = 300, units = 'in')
gridExtra::grid.arrange(p_crop_country, p_past_country, p_crop_tnc, p_past_tnc, nrow = 2)
dev.off()
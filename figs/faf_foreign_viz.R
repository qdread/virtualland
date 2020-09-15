# Summary Stats to visualize the foreign imports

# Which FAF regions receive the most imported virtual land transfers, as ports of entry
port_of_entry <- faf_by_bea_foreign %>%
  group_by(FAF_foreign_region, fr_orig, dms_orig) %>%
  summarize(cropland_flow = sum(cropland_flow, na.rm = TRUE),
            pastureland_flow = sum(pastureland_flow, na.rm = TRUE)) %>%
  left_join(faf_lookup, by = c('dms_orig' = 'Code'))

# Get top ten ports of entry for each foreign origin
### cropland
topten_cropland_entry <- port_of_entry %>%
  group_by(FAF_foreign_region) %>%
  arrange(-cropland_flow) %>%
  slice(1:10)

ggplot(topten_cropland_entry, aes(x = FAF_Region, y = cropland_flow)) +
  geom_point() +
  facet_wrap(~ FAF_foreign_region, scales = 'free') +
  theme_bw() +
  coord_flip()

### pastureland
topten_pastureland_entry <- port_of_entry %>%
  group_by(FAF_foreign_region) %>%
  arrange(-pastureland_flow) %>%
  slice(1:10)

ggplot(topten_pastureland_entry, aes(x = FAF_Region, y = pastureland_flow)) +
  geom_point() +
  facet_wrap(~ FAF_foreign_region, scales = 'free') +
  theme_bw() +
  coord_flip()

# Which FAF regions receive the most imported virtual land transfers, as final destination
final_destination <- faf_by_bea_foreign %>%
  group_by(FAF_foreign_region, fr_orig, dms_dest) %>%
  summarize(cropland_flow = sum(cropland_flow, na.rm = TRUE),
            pastureland_flow = sum(pastureland_flow, na.rm = TRUE)) %>%
  left_join(faf_lookup, by = c('dms_dest' = 'Code'))

# Get top ten destinations for each foreign origin
### cropland
topten_cropland_dest <- final_destination %>%
  group_by(FAF_foreign_region) %>%
  arrange(-cropland_flow) %>%
  slice(1:10)

ggplot(topten_cropland_dest, aes(x = FAF_Region, y = cropland_flow)) +
  geom_point() +
  facet_wrap(~ FAF_foreign_region, scales = 'free') +
  theme_bw() +
  coord_flip()

### pastureland
topten_pastureland_dest <- final_destination %>%
  group_by(FAF_foreign_region) %>%
  arrange(-pastureland_flow) %>%
  slice(1:10)

ggplot(topten_pastureland_dest, aes(x = FAF_Region, y = pastureland_flow)) +
  geom_point() +
  facet_wrap(~ FAF_foreign_region, scales = 'free') +
  theme_bw() +
  coord_flip()

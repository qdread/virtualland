# Lookup table is needed because 2019 CFs present ecoregion's full name, not code :-(
# Requires map
library(sf)
tnc_map <- st_read(file.path(fp, 'raw_data/landuse/ecoregions/tnc_terr_ecoregions.shp'))
tnc_lookup <- tnc_map %>% select(ECO_CODE, ECO_NAME) %>% st_drop_geometry()

# Join 2019 CF with lookup table

# First assign all exact matches.
unique_names <- unique(chaudsi2019$Ecoregion_name)
chaud_eco_names <- data.frame(chaud_name = unique_names, chaud_lower_name = tolower(unique_names))
tnc_lookup <- mutate(tnc_lookup, tnc_lower_name = tolower(ECO_NAME))

chaud_eco_names <- left_join(chaud_eco_names, tnc_lookup, by = c('chaud_lower_name' = 'tnc_lower_name'))

unmatched_names <- chaud_eco_names$chaud_lower_name[is.na(chaud_eco_names$ECO_CODE)] # 187

# do agrep to find the matches if possible.
fuzzy_match <- sapply(unmatched_names, agrep, x = tolower(tnc_lookup$ECO_NAME), ignore.case = TRUE, max.distance = 0.1)
table(sapply(fuzzy_match, length))

# Match only single matches.
matched_names <- unmatched_names[sapply(fuzzy_match, length) == 1]
matched_idx <- unlist(fuzzy_match[sapply(fuzzy_match, length) == 1])
matched_names_lookup <- tolower(tnc_lookup$ECO_NAME)[matched_idx]
eco_code_lookup <- tnc_lookup$ECO_CODE[matched_idx]

chaud_eco_names$ECO_CODE[match(matched_names, chaud_eco_names$chaud_lower_name)] <- eco_code_lookup

# Iterate the procedure.
unmatched_names <- chaud_eco_names$chaud_lower_name[is.na(chaud_eco_names$ECO_CODE)] # 145

fuzzy_match <- sapply(unmatched_names, agrep, x = tolower(tnc_lookup$ECO_NAME), ignore.case = TRUE, max.distance = 0.2)

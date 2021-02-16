# State and county extinction maps
# QDR / Virtualland / 15 Feb 2021

source('figs/figs_v2_loaddata.R')

library(stplanr)
library(tmap)
library(Rutilitybelt)

# State network maps ------------------------------------------------------

#https://cran.r-project.org/web/packages/stplanr/vignettes/stplanr-od.html

# Convert landflows to line geometry
# Exclude AK and HI for now
ak_hi_fips <- c('02', '15')
state_map48 <- state_map %>% filter(!STATEFP %in% ak_hi_fips)

# State ext. flows -- add animals together, keep separate from plants.
# Also widen by land use.

state_extinction_flows_wide <- copy(state_extinction_flows)
setDT(state_extinction_flows_wide)

state_extinction_flows_wide[!taxon %in% 'plants', taxon := 'animals']
state_extinction_flows_wide <- state_extinction_flows_wide[, .(species_lost = sum(species_lost)), by = .(scenario, state_from, state_to, land_use, taxon)]
state_extinction_flows_wide <- dcast(state_extinction_flows_wide, scenario + state_from + state_to + taxon ~ land_use, value.var = 'species_lost')
state_extinction_flows_wide[, total := annual + pasture + permanent]

# Remove AK and HI for now
state_extinction_flows_wide <- state_extinction_flows_wide[!state_from %in% ak_hi_fips & !state_to %in% ak_hi_fips]

# Nest by scenario and taxon and create the line features.
state_extinction_flows_lines <- group_nest_dt(state_extinction_flows_wide, scenario, taxon)
state_extinction_flows_lines[, lines := map(data, ~ od2line(flow = ., zones = state_map48))]

# Make these into great circles as well.
# https://www.jessesadler.com/post/great-circles-sp-sf/
line2greatcircle <- function(l) {
  old_crs <- st_crs(l)
  l %>%
    st_transform(4326) %>%
    st_segmentize(units::set_units(10, 'km')) %>% 
    st_transform(old_crs)
}

# Plot with tmap.
# Total species lost for all 3 land types, animals, baseline x baseline
tm_shape(state_map48) + 
  tm_borders() +
  tm_shape(state_extinction_flows_lines$lines[[3]] %>% arrange(-total) %>% head(100)) + 
  tm_lines(palette = 'plasma', 
           lwd = 'total', 
           col = 'total',
           scale = 10,
           alpha = 0.5,
           legend.lwd.show = FALSE) +
  tm_layout(main.title = 'virtual transfers of animal species committed to extinction',
            title = 'scenario: baseline diet x baseline waste')

# Plot with ggplot to show arrows
# See https://rpubs.com/cyclemumner/327465

# Convert the lines to a data frame, extracting start and endpoint
test_lines <- state_extinction_flows_lines$lines[[3]] %>% 
  arrange(-total) %>% 
  head(100) %>%
  line2greatcircle %>%
  group_by(state_from, state_to, annual, pasture, permanent, total) %>%
  group_modify(~ as.data.frame(st_coordinates(.)))

ggplot() +
  geom_sf(data = state_map48, fill = 'transparent') +
  geom_path(data = test_lines,
            aes(x = X, y = Y, group = interaction(state_from, state_to), color = total, size = total),
            arrow = arrow(length = unit(0.2, 'cm'), type = 'open', ends = 'last', angle = 40),
            alpha = 0.5) +
  scale_color_viridis_c(name = 'species') +
  scale_size_continuous(range = c(0.2, 2), guide = 'none') +
  theme(axis.title = element_blank(),
        legend.position = c(0.16, 0.12),
        legend.direction = 'horizontal',
        legend.background = element_blank()) 

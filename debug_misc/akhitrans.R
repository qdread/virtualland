# Script to transform Alaska and Hawaii into small areas to the SW of the L48

rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

crs_lambert <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"

county_map_trans <- county_map %>%
  st_transform(crs = crs_lambert)

alaska <- county_map_trans %>% filter(fips_state %in% '02')
alaska_g <- st_geometry(alaska)
alaska_centroid <- st_centroid(st_union(alaska_g))

alaska_trans <- (alaska_g - alaska_centroid) * rot(-39 * pi/180) / 2.3 + alaska_centroid + c(1000000, -5000000)
alaska <- st_set_geometry(alaska, alaska_trans) %>% st_set_crs(st_crs(county_map_trans))


hi_crs <- '+proj=aea +lat_1=8 +lat_2=18 +lat_0=3 +lon_0=-157 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs'
hi_box <- c(xmin = -400000, ymin = 1761000, xmax = 230000, ymax = 2130000)

hawaii <- county_map %>% 
  filter(fips_state %in% '15') %>%
  st_transform(crs = hi_crs) %>%
  st_crop(hi_box) %>%
  st_transform(crs = crs_lambert)

hawaii_g <- st_geometry(hawaii)
hawaii_centroid <- st_centroid(st_union(hawaii_g))

hawaii_trans <- (hawaii_g - hawaii_centroid) * rot(-35 * pi/180) + hawaii_centroid + c(5200000, -1400000)
hawaii <- st_set_geometry(hawaii, hawaii_trans) %>% st_set_crs(st_crs(county_map_trans))

final <- county_map_trans %>%
  filter(!fips_state %in% c('02', '15')) %>%
  rbind(alaska) %>%
  rbind(hawaii)

plot(st_geometry(final))

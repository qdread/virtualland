# Transform county map to AEA and write to GPKG

library(sf)
system2('gdalsrsinfo', 'data/raw_data/landuse/ecoregions/tnc_usa_aea.prj')
tnc_p4s <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs'
countymap <- st_read(dsn = '/nfs/public-data/census-tiger-2013/cb_2014_us_county_500k', layer = 'cb_2014_us_county_500k')

county_aea <- st_transform(countymap, tnc_p4s)

st_write(county_aea, 'data/raw_data/landuse/USA/USA_county_2014_aea.gpkg', driver = 'GPKG')

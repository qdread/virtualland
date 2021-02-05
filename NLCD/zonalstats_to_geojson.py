from rasterstats import zonal_stats
from sys import argv
import geojson

script, vector_file, raster_file, output_file = argv

stats = zonal_stats(vector_file, raster_file, categorical = True, geojson_out = True)

result = {"type": "FeatureCollection","features": stats}
with open(output_file, 'w') as outfile:
   geojson.dump(result, outfile) 

from rasterstats import zonal_stats
from sys import argv
import pandas as pd

script, vector_file, raster_file, output_file = argv

tabulated_geojson = zonal_stats(vector_file, raster_file, categorical = True, geojson_out = True)

output_properties = [x['properties'] for x in tabulated_geojson]
outputdf = pd.DataFrame(output_properties)

outputdf.to_csv(output_file)

from rasterstats import zonal_stats
import pandas as pd

output = zonal_stats("/nfs/qread-data/cfs_io_analysis/countries_tnc_intersect.gpkg", "/nfs/qread-data/raw_data/landuse/global_aglands/pasture_equalarea.vrt", stats = "count min mean max median sum", nodata = float('-inf'))

outputdf = pd.DataFrame(output)

outputdf.to_csv("/nfs/qread-data/raw_data/landuse/output_csvs/global_count_pasture.csv")

# Algorithm to get transfer of biodiversity threats due to domestic food production in the USA

- Find land used to produce $1 of each crop within each state, from NASS data compiled by USEEIO, using imputation where necessary.
- Find total amount of acreage dedicated to crops and pastures within each state - or even more finely with cropland data layer, from NASS
- Map all the crops and agricultural goods to the appropriate NAICS and BEA code, and crosswalk this with the SCTG code
- Determine the net transfer of agricultural goods, by using FAF4 dataset. Map the proportions directly to the production data to avoid double-counting.
- Disaggregate some of the coarse information on global imports and exports using COMTRADE
- Create maps of land transfers
- Convert CFS regions to Nature Conservancy ecoregions, by area, possibly weighted by population density.
- Find among-ecoregion land transfers from the FAF4 transfers
- Use Chaudhary model for each taxon to get biodiversity threat transfers from the land transfers
- Use transportation mode information from FAF4 to get emissions and other impacts directly associated with the transportation of goods among regions.
- To address the question of how much of this biodiversity threat is associated with wasted food in particular, combine the total threat data with LAFA data or other sources of food waste data.

# Updated algorithm, 22 May 2020 (draft)

- Extract land use by crop data from NASS and impute all missing values
- Map all the crops and agricultural goods to the appropriate NAICS and BEA code, and crosswalk this with the SCTG code
- Determine the net transfer of agricultural goods, by using FAF4 dataset. Map the proportions directly to the production data to avoid double-counting
- Find total amount of acreage dedicated to crops and pastures within each FAF region and each TNC ecoregion with cropland data layer, from NASS
- Convert the FAF4 agricultural goods transfers to land transfers using the NASS land use by crop data
- Use the FAF x TNC acreage data to calculate the among-ecoregion land transfers from the FAF4 land transfers 
  - this may require some kind of network optimization method
  - alternative: use the county food flows from Lin et al. 2019 if that would be better
- Disaggregate some of the coarse information on global imports and exports using COMTRADE
- Convert COMTRADE data to land transfers as well (need to get a method here)
- Use Chaudhary model for each taxon to get biodiversity threat transfers from the land transfers, both domestic and foreign.
  - explore alternatives here as well.
- Use transportation mode information from FAF4 to get emissions and other impacts directly associated with the transportation of goods among regions.
- To address the question of how much of this biodiversity threat is associated with wasted food in particular, combine the total threat data with LAFA data or other sources of food waste data.
# Revised approach to estimating production and consumption footprints
# QDR / 16 Dec 2020

#       Method
# ==================
# For each region (county or CFS region), estimate both production and consumption of goods in all categories.
# Use BEA codes. 
# For consumption, use the county BEA code table. Convert LAFA categories to BEA categories, then 
# Then, use the I-O model to get the indirect consumption required to satisfy that direct consumption.
# This can be converted to land area required to satisfy consumption in each county.
# We also have the land area of production in each county (NLCD sums).
# Scale the production and consumption land areas so they add up to the same.
# Next, use the FAF dataset (harmonization between BEA and SCTG codes) to work backwards and get 
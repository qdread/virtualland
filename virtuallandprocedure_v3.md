# Methods outline: virtual land 

Version 3.0
QDR, 22 Dec 2020

This supersedes virtuallandprocedure.md and virtualland_alternative_methods.md

## I. Calculate consumption in baseline scenario

- Get USA personal consumption expenditure from BEA data
- Multiply personal consumption expenditure by the share of personal income that each county makes up (data from Lin et al. supplement)

This results in consumption of final goods in each county in 2012, in BEA categories, in the baseline case.

## II. Construct diet shift and waste reduction consumption scenarios

- Harmonize BEA categories with LAFA categories with food pattern categories for the different diet scenarios.
- Calculate the change in LAFA category demand in the diet shift scenario and the waste reduction scenario, and both
- Transform back into BEA units

This results in consumption of final goods in each county in 2012, in BEA categories, for the baseline case and three alternative scenarios (D, W, DW)

## III. Estimate consumption of primary goods for each scenario

- Use the direct requirements coefficients matrix (USEEIO2012) and multiply the matrix by the consumption vector for each county and each scenario 

This results in consumption of primary goods required to satisfy final county consumption in 2012, in BEA categories, for the four scenarios.

## IV. Allocate consumption to producing region for each scenario

For now, use the simple assumption that all the USA regions send to all other regions proportional to demand and ignoring distance between the regions. Also ignore foreign imports (or just use rest of the world)

- Multiply each county's consumption of primary goods by the share of production of primary goods that each other county makes up.

This may or may not need to be aggregated to FAF region. If we are not using FAF data, we can use county level data.

This results in a matrix for each BEA primary good that shows the proportion of consumption from each producing region, for each of the four scenarios for 2012.

## V. Allocate consumption to producing region assuming local agriculture for each scenario

- Use the optimal transport model. This may require aggregation to FAF level.

## VI. Convert agricultural good consumption to land consumption

- Aggregate the from county to county matrix to a from state to county matrix for each good.
- Use the land exchange tables from USEEIO and multiply the land exchange matrix for the three land use types by state, by the goods consumption for each county (by state origin of the goods)

## VII. Estimate biodiversity threats embodied in land consumption

- Use Chaudhary's characterization factors. Multiply CF for each land type times the land consumed for each county.

## VIII. Visualization of results

- TBD

 
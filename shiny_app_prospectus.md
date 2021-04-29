# Structure of Shiny app to accompany virtual land and biodiversity ms

The user will be able to select various options and then display the data in three different forms

- Table
- Bar plot
- Map

There will be two tabs. 

- In tab 1, the user can select options including which output variable and which specific spatial region(s) to aggregate over, and display the corresponding data in table and barplot form next to each other. All 10 scenarios (5 diet x 2 waste) will be shown. **Note: The underlying data has 4 waste scenarios, for a total of 20 combinations, but to simplify things I got rid of 2 of them**
- In tab 2, the user can select options of which output variable and which scenario to display, then see a choropleth map showing imports or exports for that variable for that scenario. There will be two map views they can choose from: USA and world. If USA, it will be a choropleth map of the USA (by county, state, or ecoregion) showing domestic inbound or outbound transfers. If world, it will be a choropleth map of the world (by country or ecoregion) showing exports from other countries/ecoregions into the USA.

## Options for tab 1: table and figure of data

The options will be as follows:

1. Select whether to display raw values or percentage values relative to the baseline.
1. Select variable: foods (consumption only), agricultural goods, land use, or biodiversity threat (extinctions
2. Within the variable selected in #1, select one or more categories. The list will differ depending on the variables. Ag. goods have 10 categories, land use has 3 categories, and extinctions has 15 categories (5 taxa x 3 land uses).
3. Select imports (consumption) or exports (production) 
4. If imports, select domestic, foreign, or both
5. Select type of spatial aggregation: county, state, ecoregion (for domestic data) or country, ecoregion (for foreign data)
6. Select individual county, state, or ecoregion to display data, or choose to show all.

The result will be a table and barplot showing the desired combinations for each scenario. (Bar plot with 10 bars, one for each scenario, grouped by diet scenario. If there are multiple options chosen, it will be a faceted plot with one facet for each output variable, and free y axis)

## Options for tab 2: maps

1. Select diet and waste scenario to display. 
1. Select whether to display raw values or percentage values relative to the baseline.
1. Select variable: agricultural goods, land use, or extinctions
2. Within the variable selected in #1, select one or more categories. The list will differ depending on the variables. Ag. goods have 10 categories, land use has 3 categories, and extinctions has 15 categories (5 taxa x 3 land uses).
4. Select imports (consumption) or exports (production) 
5. If imports, select domestic, foreign, or both
2. Select view: world view or USA view
3. Select type of spatial aggregation: ecoregion or political region (countries if world view, county or state if USA view)

The result will be a choropleth showing the value of the given variable for each scenario. If it's world view, it will be a world map in a nice equal area projection such as Robinson, if USA view, Albers with Alaska and Hawaii floating off somewhere to the side.

## Overall summary of options

How to subset data:

- diet scenario
- waste scenario
- by ecoregion or by county
- production side or consumption side
- raw values or relative to baseline
- including or excluding foreign imports

Which data to plot:

- consumption of final foods
- production or consumption of primary agricultural goods
- land production or consumption
- biodiversity threat imports or exports
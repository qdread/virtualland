---
title: "Biodiversity threats embedded in USA food production"
author: "Quentin D. Read"
date: "January 22, 2020"
header-includes:
  - \usepackage[multiple]{footmisc}
output: pdf_document
---

## Goal

Land transformation and degradation caused by food production massively threaten biodiversity worldwide, with 70% being a commonly cited number for the percent of biodiversity threats caused by habitat loss or land use change. Previous work[^1][^2] has shown how international food trade virtually "exports" biodiversity threats from one nation to another, with the United States being a lead importer of food products that threaten biodiversity of other nations. 

Biodiversity threats resulting from global trade are well known, but it is less well understood that the network of agricultural shipments within the United States also transmit biodiversity threats from one part of the country to another. When food is produced in one region and transported and consumed in another, the producing region is "virtually exporting" a piece of its land to the consuming region. Whatever threats to biodiversity result from the production of that agricultural good are virtually exported along with it. Most US consumers have a poor idea of how complex and wide-ranging the supply chains are that bring food to their table -- this information would help them become more aware of how their food purchases impact the environment and biodiversity in far-off parts of the country (and world).

This is a verbal description of a research workflow that I am currently developing to assess biodiversity threats due to food production in the United States. Ultimately we will be able to use this framework to compare biodiversity threat reduction in the United States caused by (1) *food waste reduction*, (2) *diet changes*, and (3) *shifts from globalized to local food production systems*.

## Proposed approach

- Determine the land, water, and fertilizer requirements to produce $1 of each major crop and livestock type in each US state, using USDA NASS data, imputing values where necessary.
- Find the total amount of acreage dedicated to individual crops and pasture types within each defined region (Commodity Flow Survey, aka CFS, area) of the US using USDA's Cropland Data Layer.
- Map all crops and other agricultural goods considered for analysis to the appropriate NAICS code and BEA code, then crosswalk these codes with SCTG codes.
- Determine the transfer, in units of dollars, of agricultural goods among USA regions using the Freight Analysis Framework 4 (FAF4) dataset developed from the Commodity Flow Survey data and other sources. 
- Correct for double-counting of production in the FAF4 dataset in cases where raw agricultural goods are transported, processed into secondary products, and transported again.
- Disaggregate the coarse information on global imports and exports from FAF4 using the COMTRADE database or similar.
- Use an environmentally-extended input-output approach to convert the dollar units into land (and if desired water and fertilizer) inputs required to produce each good and multiply this by the values transported from one region to another.
- Create maps and other visualizations of the "virtual land transfers" among regions.
- Convert the CFS areas to Nature Conservancy ecoregions and weight the areas by population density.
- Convert FAF4 among-region virtual land transfers to among-ecoregion land transfers.
- Use the model developed by Chaudhary & Kastner, or our own data, to get biodiversity threat transfers resulting from the virtual land transfers.
- Use transportation mode information from FAF4 to get emissions and other impacts directly associated with the transportation of goods among regions.
- To address the question of how much of this biodiversity threat is associated with wasted food in particular, combine the total threat data with USDA LAFA data or other sources of food waste data.

## Expected research outcome

This research will produce visualizations and data that will be interesting to consumers and stakeholders, increasing awareness of threats to environment and biodiversity that result from the complex tangle of supply chains that make up our food system. As mentioned above, we will be able to incorporate these results in a broader framework to compare the "big three" ways to increase food system sustainability: waste reduction, diet shifts, and localization of food systems.

[^1]: Lenzen et al. 2012. International trade drives biodiversity threats in developing nations. *Nature*.

[^2]: Chaudhary & Kastner. 2016. Land use biodiversity impacts embodied in international food trade. *Global Environmental Change.*
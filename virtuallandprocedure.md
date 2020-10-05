# Methods Outline: Virtual Land

QDR, 02 Oct 2020

This is an update to `biodiversitythreatprocedure.md` but does not necessarily replace everything in it.

## Agricultural Goods Flows

- Tabulate cropland and pastureland areas for the CFS regions
- Split up the cropland and pastureland areas by NAICS/BEA code by state to assign land areas to the different crop codes then proportionally assign the cropland area by state to CFS area by state
- Convert FAF shipments from SCTG code to BEA crop codes using proportional production from each region (harmonizes SCTG and BEA)
  + Note: we assume that the outgoing shipments from each region are composed proportionally of the products produced in that region (fairly weak assumption)
  + We also divide up the crop codes from each region so that each region it exports to receives the same mix of goods (stronger assumption)
  
## Scenario Construction

- **Optimal transport**: For the 10 BEA crop categories, use the optimal transport algorithm to minimize the number of ton-miles each good must move.
- **Diet shift**: Assume that 50% of animal-derived calories (evenly divided among food types by proportion) are replaced by an even mix of plant-derived calories (all plant-based foods are increased proportionally as well). Rescale agricultural shipments accordingly.
- **Waste reduction**: Assume 50% waste reduction at primary, retail, and consumer levels, based on LAFA augmented by FAO waste rates (using weighted average to convert from LAFA/FAO to BEA categories). Rescale agricultural shipments accordingly.

## Virtual Land Flows

- For the baseline case and each of the three alternative scenarios, convert the FAF shipments to land transfers, using the crop and pasture areas in each CFS region and proportionally converting weights of shipments to land area.
- Convert FAF land flows to TNC land flows.
  + Outgoing flows are divided based on the proportion of cropland and pastureland by TNC region within each CFS region
  + Incoming flows are divided based on the proportion of population by TNC region within each CFS region

## Biodiversity Footprints

- The characterization factors are all due to Chaudhary and Kastner. They are for each major terrestrial vertebrate taxon in each ecoregion.
- They used the Countryside SAR, which has a parameter for the affinity of taxa to each modified land use type relative to natural unmodified land in each ecoregion: $h_{g,i,j}$: taxon g, modified land use i, region j. Each region has its own SAR slope $z_{j}$. This is used to get the local CF.
- The derivative of the SAR is used to get the marginal species loss of modifying one square meter of land, from which we get the regional CF.
- They also get a global CF by getting the fraction of endemic species for each taxon in each ecoregion, converting that into a vulnerability score, then multiplying the regional CF by the vulnerability score. (This seems like an underestimate because if a species could be lost globally even if it's not endemic to a single ecoregion, if it's extirpated from multiple regions.)
- We multiply our land area flows by the species lost per unit land area for each ecoregion in each scenario.
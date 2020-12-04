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

# Things that still need to be done to improve the methods

Summarized on 04 Dec 2020 from the [github issues page](https://github.com/qdread/virtualland/issues).

## Big things

- It might be good to change the order of how biodiversity threats are calculated. Now, we calculate exported flows and convert them to biodiversity threat. Instead, we should calculate biodiversity threat from production within each ecoregion, allocate the exports to the "trading partners" of the ecoregion, and assign them proportionally. That might help with the raw vs. non-raw materials.
- Land flows from non-raw materials. This may be addressed by the above point. Right now we can fairly easily track where raw goods are produced then shipped. Once they are transformed into other types of materials, the problem begins. How to keep track of the original production/resources embodied in those shipments?
- Alternative biodiversity models: we currently only use one (Chaudhary) but studies like Leclere use an ensemble approach which is likely to be more robust.
- Better treatment of foreign imports and exports

## Small technical things

- The original plan was to use an input-output approach with BEA tables so all categories were harmonized to BEA. If we end up using a LCA approach, this is probably not necessary and only adds to uncertainty. This could also improve the optimal transport scenario.
- Food waste rates are a mix of LAFA and FAO. We need to make sure appropriate data is used.
- Land transformation characterization factors could be used in addition to occupation.
- Use a better model (optimization based on constraints of being as close as possible to the nutrients of meat) to reallocate meat calories to plants
- Ensure that there is no double-dipping in the way the land flows are adjusted for each scenario. This may be the reason why the diet x waste has such a big effect.

# Generate tables and other visualizations for foreign goods imports
# QDR / virtualland / 26 May 2021

library(data.table)
library(gt)
library(Rutilitybelt)

import_crop <- fread('/nfs/qread-data/cfs_io_analysis/fao_VLT_provisional_crops_disaggregated.csv')
import_animal <- fread('/nfs/qread-data/cfs_io_analysis/fao_VLT_provisional_animalonly.csv')
import_biodiv <- fread('data/cfs_io_analysis/scenarios/foreign_species_lost_by_export_country_x_tnc.csv')

# Filter imports by crop; baseline only
import_crop <- import_crop[virtual_land_transfer > 0]
import_crop_base <- import_crop[scenario %in% 'D_baseline_WR_baseline']

# Biodiv: aggregate back across ecoregions
import_biodiv <- import_biodiv[species_lost > 0]
import_biodiv_base <- import_biodiv[scenario_diet %in% 'baseline' & scenario_waste %in% 'baseline', .(species_lost = sum(species_lost)),
                                    by = .(country_name, ISO_A3, land_use, taxon)]

import_animal_base <- import_animal[scenario %in% 'D_baseline_WR_baseline'] 

crop_bea_crosstab <- with(import_crop_base, table(item, BEA_389_code))

import_crop_base_sums <- import_crop_base[, .(export_qty = sum(export_qty), VLT = sum(virtual_land_transfer)), by = .(country_name, country_code)]

# Sum biodiv by taxon and land use for both
import_biodiv_base_sums <- import_biodiv_base[, .(species_lost = sum(species_lost)), by = .(country_name, ISO_A3)]

# Create an "other" category for those countries accounting for a small proportion of the VLT and/or the biodiversity transfers
import_crop_base_sums <- import_crop_base_sums[order(-VLT)]
import_crop_base_sums[, prop_VLT := cumsum(VLT)/sum(VLT)]

import_biodiv_base_sums <- import_biodiv_base_sums[order(-species_lost)]
import_biodiv_base_sums[, prop_VBT := cumsum(species_lost)/sum(species_lost)]

# The top 20 account for 92% of the virtual land transfers so we will put everything else in "other."
# The top 20 countries account for 98.6% of the virtual biodiversity transfers so we can just look at those.

top20biodiv <- import_biodiv_base_sums$country_name[1:20]

# Paste ISO3 code to the end of the country names
top20biodiv_iso <- import_biodiv$ISO_A3[match(top20biodiv, import_biodiv$country_name)]
top20biodiv_withcode <- paste0(top20biodiv, ' (', top20biodiv_iso, ')')

# Create land/goods import df ---------------------------------------------

# virtual land transfers by product imported x goods
import_crop_base <- import_crop_base[, .(export_qty = sum(export_qty), VLT = sum(virtual_land_transfer)), by = .(country_name, country_code, item, crop_type)]

import_crop_base[, crop_type := paste0('VLT_', crop_type)]
import_crop_base_wide <- dcast(import_crop_base, country_name + country_code + item + export_qty ~ crop_type, value.var = c('VLT'), fill = 0)

import_animal_base[, item := paste0(livestock_product_type_broad, ', ', livestock_name)]
import_animal_base_wide <- import_animal_base[, .(country_name, country_code, item, export_qty, VLT_annual, VLT_permanent, VLT_pasture)]

import_goods_base_wide <- rbindlist(list(import_crop_base_wide, import_animal_base_wide), fill = TRUE)
replace_na_dt(import_goods_base_wide)

# Aggregate to "other"
# Fix cote d'ivoire
import_goods_base_wide[, country_name := gsub('\xf4', 'ô', country_name)]
import_goods_base_wide[!country_name %in% top20biodiv, country_name := 'Other']
cols_to_sum <- c("export_qty", "VLT_annual", "VLT_mixed", "VLT_permanent", "VLT_pasture")
import_goods_base_wide <- import_goods_base_wide[, lapply(.SD, sum), by = .(country_name, item), .SDcols = cols_to_sum]

# Sum the virtual land transfer for each country x land type
import_goods_base_wide[, VLT_total := VLT_annual + VLT_pasture + VLT_permanent + VLT_mixed]

# Aggregate biodiversity dataframe to "other"
import_biodiv_base[!country_name %in% top20biodiv, country_name := 'Other']
import_biodiv_base <- import_biodiv_base[, .(species_lost = sum(species_lost)), by = .(country_name, land_use, taxon)]
import_biodiv_base[, land_use := paste0('VBT_', land_use)]
import_biodiv_base_wide <- dcast(import_biodiv_base, country_name + taxon ~ land_use)
import_biodiv_base_wide[, VBT_total := VBT_annual + VBT_pasture + VBT_permanent]

# Sort descending order of total virtual biodiversity transfer
import_goods_base_wide[, country_name := factor(country_name, levels = c(top20biodiv, 'Other'), labels = c(top20biodiv_withcode, 'Other'))]
import_biodiv_base_wide[, country_name := factor(country_name, levels = c(top20biodiv, 'Other'), labels = c(top20biodiv_withcode, 'Other'))]
import_biodiv_base_wide[, taxon := factor(taxon, levels = c('plants', 'amphibians', 'birds', 'mammals', 'reptiles'))]

import_goods_base_wide <- import_goods_base_wide[order(country_name, -VLT_total), .(country_name, item, export_qty, VLT_total)]
import_biodiv_base_wide <- import_biodiv_base_wide[order(country_name, -VBT_total), .(country_name, taxon, VBT_total)]


# Generate gt tables ------------------------------------------------------

# for virtual land transfer, convert to km^2 (was ha so divide by 100)

library(dplyr)

gt_import_goods_land <- import_goods_base_wide %>%
  as_tibble %>%
  mutate(item = tolower(gsub('\xe9', 'é', item)), # for yerba mate
         VLT_total = signif(VLT_total/100, 3)) %>%
  gt(groupname_col = 'country_name') %>%
  fmt_number(c(export_qty, VLT_total), n_sigfig = 3) %>%
  cols_label(
    item = 'Item',
    export_qty = html('Export quantity<br><small><i>tonnes</i></small>'),
    VLT_total = html('Virtual land export<br><small><i>km<sup>2</sup></i></small>')
  )

# Make biodiversity table even wider (by taxon)
import_biodiv_base_wider <- dcast(import_biodiv_base_wide, country_name ~ taxon)
import_biodiv_base_wider[, animals := amphibians + birds + mammals + reptiles]
import_biodiv_base_wider[, total := plants + animals]

gt_import_biodiv <- import_biodiv_base_wider %>%
  gt(rowname_col = 'country_name') %>%
  fmt_number(2:8, n_sigfig = 3) %>%
  tab_spanner(
    label = html("Virtual biodiversity threat export by taxonomic group<br><small><i>unit: extinctions</i></small>"),
    columns = 2:8
  ) %>%
  data_color(columns = 2:8, "Reds", alpha = 0.75)

saveRDS(gt_import_goods_land, 'data/cfs_io_analysis/scenario_v2_figs/gt_tables/gt_import_goods_land.RDS')
saveRDS(gt_import_biodiv, 'data/cfs_io_analysis/scenario_v2_figs/gt_tables/gt_import_biodiv.RDS')

# Reds palette function
red_pal <- RColorBrewer::brewer.pal(9, 'Reds')

palette_mapper <- function(x, pal) {
  limits <- range(x)
  pal[findInterval(x,seq(limits[1],limits[2],length.out=length(pal)+1), all.inside=TRUE)]
}

# Custom color function for the individual columns, not including the total and grand total rows
flow_bkg_color <- function(x) {
  xcol <- palette_mapper(x[c(1:3, 5:7)], red_pal)
  c(xcol[1:3], 'white', xcol[4:6], 'white', 'white')
}

# Rearrange columns
all_flows_withtotals <- all_flows_withtotals[, .(land_use, VLT, VBT_animals, VBT_plants, VBT_total, VBT_animals_intensity, VBT_plants_intensity, VBT_total_intensity)]

# Manually color the dark cell text white
txtcol1 <- rep(c('black','white','black'), c(2, 1, 6))
txtcol2 <- rep(c('black','white','black'), c(5, 1, 3))

# To 2 sig figs
nc <- names(all_flows_withtotals)[sapply(all_flows_withtotals, is.numeric)]
all_flows_withtotals[, (nc) := lapply(.SD, signif, digits = 2), .SDcols = nc]

# Column names
# FIXME put in something to have the column names with little subtitles for the units

kbl(all_flows_withtotals,
    col.names = linebreak(c('', 'land\n1000 km^2', 'animals', 'plants', 'total', 'animals', 'plants', 'total')),
    escape = FALSE) %>%
  column_spec(2, background = flow_bkg_color(all_flows_withtotals$VLT), color = txtcol1) %>%
  column_spec(3, background = flow_bkg_color(all_flows_withtotals$VBT_animals), color = txtcol1) %>%
  column_spec(4, background = flow_bkg_color(all_flows_withtotals$VBT_plants), color = txtcol1) %>%
  column_spec(5, background = flow_bkg_color(all_flows_withtotals$VBT_total), color = txtcol1) %>%
  column_spec(6, background = flow_bkg_color(all_flows_withtotals$VBT_animals_intensity), color = txtcol2) %>%
  column_spec(7, background = flow_bkg_color(all_flows_withtotals$VBT_plants_intensity), color = txtcol2) %>%
  column_spec(8, background = flow_bkg_color(all_flows_withtotals$VBT_total_intensity), color = txtcol2) %>%
  kable_styling() %>%
  add_header_above(c(" " = 1, "Total footprint" = 4, "Footprint intensity" = 3)) %>%
  pack_rows("domestic origin", 1, 4, latex_gap_space = '2em', background = "#D4EBF2") %>%
  pack_rows("foreign origin", 5, 8, latex_gap_space = '2em', background = "#D4EBF2")

gt_intensity <- as_tibble(all_flows) %>%
  mutate(across(where(is.numeric), ~ signif(., 2))) %>%
  group_by(origin) %>%
  gt(rowname_col = 'land_use') %>%
  tab_spanner(
    label = "Total footprint",
    columns = c(VLT, VBT_animals, VBT_plants, VBT_total)
  ) %>%
  tab_spanner(
    label = "Footprint intensity",
    columns = contains('intensity')
  ) %>%
  cols_label(
    VBT_animals = html('animals<br><small><i>extinctions</i></small>'),
    VBT_plants = html('plants<br><small><i>extinctions</i></small>'),
    VBT_total = html('total<br><small><i>extinctions</i></small>'),
    VLT = html('land<br><small><i>1000 km<sup>2</sup></i></small>'),
    VBT_animals_intensity = html('animals<br><small><i>extinctions/1000 km<sup>2</sup></i></small>'),
    VBT_plants_intensity = html('plants<br><small><i>extinctions/1000 km<sup>2</sup></i></small>'),
    VBT_total_intensity = html('total<br><small><i>extinctions/1000 km<sup>2</sup></i></small>')
  ) %>%
  data_color(
    columns = starts_with('V'),
    colors = scales::col_numeric('Reds', domain = NULL),
    apply_to = c("fill"),
    alpha = 0.75,
    autocolor_text = TRUE
  )
gt_intensity %>% 
  summary_rows(
    groups = TRUE,
    columns = c(VBT_animals, VBT_plants, VBT_total, VLT),
    fns = list(TOTAL = 'sum'),
    formatter = fmt_number,
    decimals = 0,
    use_seps = TRUE
  ) %>%
  grand_summary_rows(
    columns = c(VBT_animals, VBT_plants, VBT_total, VLT),
    fns = list(`GRAND TOTAL` = 'sum'),
    formatter = fmt_number,
    decimals = 0,
    use_seps = TRUE
  ) %>%
  tab_options(
    row_group.background.color = "#D4EBF2",
    row_group.font.weight = 'bold',
    column_labels.font.weight = 'bold'
  )

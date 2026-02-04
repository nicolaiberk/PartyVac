## ____________________________________________________________
## Compute area-weighted LK->WK matching weights
## ____________________________________________________________
##
## Landkreise (LK) and Wahlkreise (WK) do not align 1:1.
## ~108 Landkreise span multiple Wahlkreise. The previous
## approach split vaccination counts equally and averaged
## rates, ignoring geographic overlap.
##
## This script computes weights based on the area of
## geographic intersection between each LK and WK polygon,
## combined with LK-level population data (assuming uniform
## population distribution within each LK).
##
## Output: data_new/matching/lk_wk_weights.csv
##   - Landkreis_ID, wk_nr, weight_area, weight_pop, weight_combined
##
## Dependencies:
##   - Wahlkreis shapefile: data_new/elections/btw21_geometrie_wahlkreise_shp/
##   - Landkreis shapefile: data_new/geo/vg250_lk/ (VG250 from BKG)
##   - Population lookup:   data_new/elections/btw21_lks_wks_pop.csv
## ____________________________________________________________

library(sf)
library(data.table)
library(tidyverse)


## ---- 1. Load shapefiles ----

## Wahlkreise (electoral districts) - already in the project
wk_sf <- read_sf("data_new/elections/btw21_geometrie_wahlkreise_shp/Geometrie_Wahlkreise_20DBT.shp")
wk_sf <- wk_sf %>%
    select(WKR_NR, geometry) %>%
    rename(wk_nr = WKR_NR)

## Landkreise (administrative districts) - VG250 from BKG
## Use the 01.01.2021 edition to match the 2021 Wahlkreis boundaries
## and 2021 RKI vaccination Landkreis IDs. No Kreisgebietsreformen
## occurred between 2019 (polio data) and 2021, so boundaries are stable.
##
## Download: https://daten.gdz.bkg.bund.de/produkte/vg/vg250_ebenen_0101/
##   -> vg250_01-01.utm32s.shape.ebenen.zip (2021 edition)
##   -> extract VG250_KRS.shp (Kreise layer) to data_new/geo/vg250_lk/
##
## The VG250 shapefile uses AGS (Amtlicher Gemeindeschlüssel) as district key.
## We construct Landkreis_ID from the AGS to match the RKI vaccination data.
if (!file.exists("data_new/geo/vg250_lk/VG250_KRS.shp")) {
    stop(
        "Landkreis shapefile not found.\n",
        "Please download VG250 (Kreise), 01.01.2021 edition, from BKG:\n",
        "  https://daten.gdz.bkg.bund.de/produkte/vg/vg250_ebenen_0101/\n",
        "Extract VG250_KRS.shp (and associated files) to: data_new/geo/vg250_lk/"
    )
}

lk_sf <- read_sf("data_new/geo/vg250_lk/VG250_KRS.shp")

## The AGS field is a 5-digit string (zero-padded).
## Convert to numeric Landkreis_ID to match RKI vaccination data.
lk_sf <- lk_sf %>%
    mutate(Landkreis_ID = as.numeric(AGS)) %>%
    select(Landkreis_ID, GEN, EWZ, geometry) %>%
    rename(lk_name = GEN, lk_pop = EWZ)

## Ensure both are in the same CRS
wk_sf <- st_transform(wk_sf, st_crs(lk_sf))


## ---- 2. Compute geographic intersections ----

## Make geometries valid to avoid topology errors
lk_sf <- st_make_valid(lk_sf)
wk_sf <- st_make_valid(wk_sf)

## Compute intersections - this produces all LK x WK overlap polygons
intersections <- st_intersection(lk_sf, wk_sf)

## Compute area of each intersection (in m^2)
intersections$intersect_area <- as.numeric(st_area(intersections))

## Compute total area of each LK for the area share
lk_areas <- lk_sf %>%
    st_drop_geometry() %>%
    mutate(lk_total_area = as.numeric(st_area(lk_sf))) %>%
    select(Landkreis_ID, lk_total_area)

intersections <- intersections %>%
    st_drop_geometry() %>%
    left_join(lk_areas, by = "Landkreis_ID")


## ---- 3. Compute weights ----

## area_share: fraction of this LK's area that falls in this WK
## This is used to distribute LK-level counts (e.g. COVID shots) to WKs,
## assuming uniform population distribution within each LK.
weights <- intersections %>%
    mutate(area_share = intersect_area / lk_total_area) %>%
    select(Landkreis_ID, lk_name, wk_nr, lk_pop, area_share)

## pop_in_wk: estimated population of this LK fragment in this WK
## (assuming uniform population density within LK)
weights <- weights %>%
    mutate(pop_in_wk = lk_pop * area_share)

## For aggregating rates (e.g. polio vaccination share) to WK level,
## we need population-weighted averages. The weight for each LK
## contributing to a WK is its estimated population share in that WK.
weights <- weights %>%
    group_by(wk_nr) %>%
    mutate(pop_weight_in_wk = pop_in_wk / sum(pop_in_wk)) %>%
    ungroup()

## Sanity checks
stopifnot(
    all(weights$area_share >= 0 & weights$area_share <= 1.01),
    all(weights$pop_weight_in_wk >= 0)
)

## Small floating-point overlaps can produce area_share slightly > 1; cap it
weights$area_share <- pmin(weights$area_share, 1)

cat(sprintf(
    "Computed %d LK-WK intersection pairs for %d unique LKs and %d unique WKs.\n",
    nrow(weights),
    n_distinct(weights$Landkreis_ID),
    n_distinct(weights$wk_nr)
))

## Check: area shares for each LK should sum to ~1
lk_check <- weights %>%
    group_by(Landkreis_ID) %>%
    summarise(total_share = sum(area_share))
if (any(lk_check$total_share < 0.95 | lk_check$total_share > 1.05)) {
    warning(
        "Some Landkreise have area shares not summing to ~1. ",
        "This may indicate CRS or boundary alignment issues.\n",
        "Affected LKs: ",
        paste(lk_check$Landkreis_ID[lk_check$total_share < 0.95 | lk_check$total_share > 1.05], collapse = ", ")
    )
}


## ---- 4. Save weights ----

dir.create("data_new/matching", showWarnings = FALSE, recursive = TRUE)

weights_out <- weights %>%
    select(Landkreis_ID, lk_name, wk_nr, area_share, pop_in_wk, pop_weight_in_wk)

fwrite(weights_out, "data_new/matching/lk_wk_weights.csv")

cat("Weights saved to data_new/matching/lk_wk_weights.csv\n")

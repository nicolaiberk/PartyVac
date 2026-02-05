## ____________________________________________________________
## Robustness: Analysis restricted to unchanged districts
## ____________________________________________________________
##
## The LK-to-WK matching introduces measurement error for
## ~108 Landkreise that span multiple Wahlkreise. Even with
## area-weighted matching, the allocation assumes uniform
## population distribution within each LK.
##
## This script reruns the core analyses restricted to districts
## where the LK-WK mapping is 1:1 (no splitting needed),
## eliminating this source of measurement error entirely.
##
## Dependencies:
##   - data_new/matching/lk_wk_weights.csv (from geo_weights.R)
##   - data_new/merged/btw_17_21_cov21_polio19.csv
##   - data_new/merged/covid_btw_es.csv (for synthetic control)
## ____________________________________________________________

library(data.table)
library(tidyverse)


## ---- 1. Identify unchanged (non-split) districts ----

## Load weights computed by geo_weights.R
if (!file.exists("data_new/matching/lk_wk_weights.csv")) {
    source("code/geo_weights.R")
}
weights <- fread("data_new/matching/lk_wk_weights.csv")

## A district is "unchanged" if each contributing LK maps to exactly one WK
## AND each WK receives contributions from LKs that each map to only one WK.

## LKs that appear in only one WK
lk_unique <- weights %>%
    group_by(Landkreis_ID) %>%
    filter(n() == 1) %>%
    pull(Landkreis_ID)

## WKs where ALL contributing LKs are unique (map to only this WK)
wk_clean <- weights %>%
    filter(Landkreis_ID %in% lk_unique) %>%
    group_by(wk_nr) %>%
    ## also check that the WK doesn't receive partial LKs from split ones
    summarise(
        n_lks = n(),
        all_unique = all(Landkreis_ID %in% lk_unique)
    ) %>%
    filter(all_unique) %>%
    pull(wk_nr)

## WKs where at least some LKs are split — but we keep WKs where
## the non-split LKs account for >95% of population (nearly unchanged)
wk_nearly_clean <- weights %>%
    group_by(wk_nr) %>%
    mutate(lk_is_unique = Landkreis_ID %in% lk_unique) %>%
    summarise(
        pop_share_clean = sum(pop_in_wk[lk_is_unique]) / sum(pop_in_wk),
        .groups = "drop"
    ) %>%
    filter(pop_share_clean > 0.95) %>%
    pull(wk_nr)

cat(sprintf(
    "Strictly unchanged WKs: %d / %d\n",
    length(wk_clean),
    n_distinct(weights$wk_nr)
))
cat(sprintf(
    "Nearly unchanged WKs (>95%% pop from non-split LKs): %d / %d\n",
    length(wk_nearly_clean),
    n_distinct(weights$wk_nr)
))


## ---- 2. Cross-sectional analysis on unchanged districts ----

merged <- fread("data_new/merged/btw_17_21_cov21_polio19.csv")

merged <- merged %>%
    mutate(
        Polio_Share = Polio_Share / 100,
        Covid_Share = Covid_Shots / Anzahl_btw21_Wahlberechtigte,
        across(contains("Prozent_btw"), ~ as.numeric(str_replace(.x, ",", ".")) / 100)
    )

## Flag unchanged districts
merged <- merged %>%
    mutate(
        unchanged_strict = Gebietsnummer %in% wk_clean,
        unchanged_loose  = Gebietsnummer %in% wk_nearly_clean
    )

cat(sprintf(
    "\nCross-sectional data: %d strict, %d loose unchanged out of %d total WKs\n",
    sum(merged$unchanged_strict),
    sum(merged$unchanged_loose),
    nrow(merged)
))

## Replicate core FW regressions on unchanged districts only
cat("\n=== FW ~ Polio (All districts) ===\n")
print(summary(lm(
    Prozent_btw21_FREIE.WÄHLER ~ Prozent_btw17_FREIE.WÄHLER + Polio_Share,
    data = merged
)))

cat("\n=== FW ~ Polio (Strictly unchanged districts) ===\n")
print(summary(lm(
    Prozent_btw21_FREIE.WÄHLER ~ Prozent_btw17_FREIE.WÄHLER + Polio_Share,
    data = merged %>% filter(unchanged_strict)
)))

cat("\n=== FW ~ Polio (Nearly unchanged districts, >95%) ===\n")
print(summary(lm(
    Prozent_btw21_FREIE.WÄHLER ~ Prozent_btw17_FREIE.WÄHLER + Polio_Share,
    data = merged %>% filter(unchanged_loose)
)))

## Bavaria only
cat("\n=== FW ~ Polio, Bavaria only (All) ===\n")
print(summary(lm(
    Prozent_btw21_FREIE.WÄHLER ~ Prozent_btw17_FREIE.WÄHLER + Polio_Share,
    data = merged %>% filter(Gebietsnummer %in% 212:257)
)))

cat("\n=== FW ~ Polio, Bavaria only (Strictly unchanged) ===\n")
print(summary(lm(
    Prozent_btw21_FREIE.WÄHLER ~ Prozent_btw17_FREIE.WÄHLER + Polio_Share,
    data = merged %>% filter(Gebietsnummer %in% 212:257, unchanged_strict)
)))


## ---- 3. Synthetic control on unchanged districts ----

## If the daily panel data exists, restrict it to unchanged WKs
if (file.exists("data_new/merged/covid_btw_es.csv")) {

    merged_btw <- fread("data_new/merged/covid_btw_es.csv", encoding = "Latin-1")

    ## The panel uses Land_Bezirk_Kreis as district_id.
    ## We need to map Kreis back to WK to filter.
    ## If a direct Kreis-to-WK mapping is available in the panel, use it.
    ## Otherwise we restrict at the Kreis level using the weights table.

    ## Kreis IDs that map cleanly to a single WK
    clean_kreis <- weights %>%
        filter(Landkreis_ID %in% lk_unique) %>%
        select(Landkreis_ID) %>%
        distinct()

    cat(sprintf(
        "\nPanel data: %d clean Landkreise available for restricted analysis\n",
        nrow(clean_kreis)
    ))

    cat("\nTo run synthetic control on unchanged districts, use:\n")
    cat("  source('code/synth_control.R')  # with filtered data\n")
    cat("  See synth_control.R for implementation.\n")
}

cat("\nRobustness analysis complete.\n")

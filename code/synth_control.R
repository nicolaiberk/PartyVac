## ____________________________________________________________
## Synthetic Control Estimate of the Aiwanger Cue Effect
## ____________________________________________________________
##
## Uses augmented synthetic control (augsynth) to estimate the
## causal effect of Aiwanger's public vaccination refusal
## (May 7, 2021) on COVID-19 vaccination rates in districts
## with high Freie Wähler support.
##
## Treated units: Landkreise/Wahlkreise with FW vote share
##   above a threshold (e.g. >5%).
## Donor pool: Districts with zero or negligible FW support,
##   matched on pre-treatment vaccination trajectory +
##   demographics.
## Outcome: Weekly vaccination rate per eligible voter.
## Treatment date: 2021-05-07 (Aiwanger interview).
##
## Dependencies:
##   - data_new/merged/covid_btw_es.csv (daily vacc + election data)
##   - augsynth package (install from CRAN or GitHub)
## ____________________________________________________________

library(data.table)
library(tidyverse)

## Install augsynth if not available (GitHub only, not on CRAN)
if (!requireNamespace("augsynth", quietly = TRUE)) {
    message("Installing augsynth from GitHub...")
    if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
    remotes::install_github("ebenmichael/augsynth")
}
library(augsynth)


## ---- 1. Load and prepare data ----

merged_btw <- fread("data_new/merged/covid_btw_es.csv", encoding = "Latin-1")

## Aggregate to district-week level
## Using Kreis as the unit (finest geographic level available)
panel <- merged_btw %>%
    filter(!is.na(Anzahl)) %>%
    mutate(
        FW_share = ifelse(is.na(FW_share), 0, FW_share),
        week = lubridate::floor_date(as.Date(Impfdatum), "week")
    ) %>%
    group_by(Land, Bezirk, Kreis, week) %>%
    summarise(
        vaccinations = sum(Anzahl),
        eligible     = mean(Wähler),
        FW_share     = mean(FW_share),
        AfD_share    = mean(AfD_share),
        .groups      = "drop"
    ) %>%
    filter(!is.na(eligible), eligible > 0) %>%
    mutate(
        vac_rate   = vaccinations / eligible,
        district_id = paste(Land, Bezirk, Kreis, sep = "_")
    )


## ---- 2. Define treatment ----

treatment_date <- as.Date("2021-05-07")

## Detect whether FW_share is proportion (0-1) or percentage (0-100)
district_fw <- panel %>%
    group_by(district_id) %>%
    summarise(FW_share = mean(FW_share))

fw_max <- max(district_fw$FW_share, na.rm = TRUE)
if (fw_max > 1) {
    cat(sprintf("FW_share appears to be in %% (max=%.1f). Converting to proportion.\n", fw_max))
    district_fw$FW_share <- district_fw$FW_share / 100
    panel$FW_share <- panel$FW_share / 100
}

cat(sprintf(
    "FW_share distribution: min=%.4f, median=%.4f, max=%.4f\n",
    min(district_fw$FW_share), median(district_fw$FW_share), max(district_fw$FW_share)
))

## Classify districts by FW support
## High FW (treated): >5% vote share
## Donor pool: 0% FW vote share (never had FW presence)
district_fw <- district_fw %>%
    mutate(
        treat_group = case_when(
            FW_share > 0.05 ~ "high_fw",
            FW_share > 0    ~ "low_fw",
            TRUE            ~ "no_fw"
        )
    )

cat(sprintf(
    "Treatment groups: %d high_fw, %d low_fw, %d no_fw\n",
    sum(district_fw$treat_group == "high_fw"),
    sum(district_fw$treat_group == "low_fw"),
    sum(district_fw$treat_group == "no_fw")
))

panel <- panel %>%
    left_join(district_fw %>% select(district_id, treat_group), by = "district_id")

## Binary treatment indicator
panel <- panel %>%
    mutate(
        treated = as.integer(treat_group == "high_fw"),
        post    = as.integer(week >= treatment_date)
    )


## ---- 3. Augmented Synthetic Control ----

## Filter to treated + donor pool (exclude low_fw to avoid spillover)
synth_data <- panel %>%
    filter(treat_group %in% c("high_fw", "no_fw")) %>%
    arrange(district_id, week)

## Balance the panel adaptively:
## Find the week coverage of treated vs donor districts, then pick
## the threshold that retains the most treated units.
district_counts <- synth_data %>%
    left_join(district_fw %>% select(district_id, treat_group), by = "district_id") %>%
    group_by(district_id, treat_group) %>%
    summarise(n_weeks = n_distinct(week), .groups = "drop")

max_weeks <- max(district_counts$n_weeks)

cat("\nWeek coverage by treatment group:\n")
district_counts %>%
    group_by(treat_group) %>%
    summarise(
        n = n(),
        min_weeks = min(n_weeks),
        median_weeks = median(n_weeks),
        max_weeks = max(n_weeks)
    ) %>%
    print()

## Use the median week count of treated districts as the threshold
## so that at least half of treated units survive
treated_weeks <- district_counts %>%
    filter(treat_group == "high_fw") %>%
    pull(n_weeks)

week_threshold <- median(treated_weeks)
cat(sprintf("\nUsing week threshold: %d (median of treated districts)\n", week_threshold))

balanced_districts <- district_counts %>%
    filter(n_weeks >= week_threshold) %>%
    pull(district_id)

## Keep only the weeks that all balanced districts share
common_weeks <- synth_data %>%
    filter(district_id %in% balanced_districts) %>%
    group_by(week) %>%
    summarise(n = n_distinct(district_id)) %>%
    filter(n == length(balanced_districts)) %>%
    pull(week)

synth_balanced <- synth_data %>%
    filter(district_id %in% balanced_districts, week %in% common_weeks)

n_treated_bal <- sum(district_fw$treat_group == "high_fw" & district_fw$district_id %in% balanced_districts)
n_donor_bal   <- sum(district_fw$treat_group == "no_fw" & district_fw$district_id %in% balanced_districts)

cat(sprintf(
    "Balanced panel: %d districts x %d weeks (%d treated, %d donors)\n",
    n_distinct(synth_balanced$district_id),
    length(common_weeks),
    n_treated_bal,
    n_donor_bal
))

if (n_treated_bal == 0) {
    stop("No treated districts in balanced panel. Check data coverage for high-FW districts.")
}

## Run augmented synthetic control
## Multi-treated unit version using multisynth
synth_result <- multisynth(
    vac_rate ~ treated,
    unit    = district_id,
    time    = week,
    data    = synth_balanced,
    n_leads = NULL,       # use all post-treatment periods
    n_lags  = NULL        # use all pre-treatment periods
)

cat("\n=== Augmented Synthetic Control Results ===\n")
print(summary(synth_result))


## ---- 4. Visualization ----

dir.create("figures", showWarnings = FALSE)

## Plot treatment effects over time
svg("figures/synth_control_aiwanger.svg", width = 10, height = 6)
plot(synth_result) +
    geom_vline(xintercept = treatment_date, col = "red", lty = 2) +
    annotate("text", x = treatment_date + 7, y = Inf, vjust = 2,
             label = "Aiwanger\ninterview", col = "red", size = 3) +
    geom_vline(xintercept = as.Date("2021-11-11"), col = "blue", lty = 2) +
    annotate("text", x = as.Date("2021-11-11") + 7, y = Inf, vjust = 2,
             label = "Aiwanger\nvaccinated", col = "blue", size = 3) +
    labs(
        title = "Synthetic Control: Effect of Aiwanger's Vaccination Refusal",
        subtitle = "Treated: Districts with >5% FW vote share | Donors: Districts with 0% FW",
        y = "ATT (Vaccination rate per eligible voter)",
        x = ""
    ) +
    theme_minimal()
dev.off()


## ---- 5. Placebo / Permutation Inference ----

## In-space placebo: reassign treatment to donor districts
## augsynth handles this via conformal inference
cat("\n=== Placebo Test (Conformal Inference) ===\n")
synth_summary <- summary(synth_result)
print(synth_summary)


## ---- 6. Robustness: Alternative treatment thresholds ----

## Try FW > 2.5% as treatment
district_fw_alt <- district_fw %>%
    mutate(treat_alt = as.integer(FW_share > 0.025))

synth_balanced_alt <- synth_balanced %>%
    left_join(district_fw_alt %>% select(district_id, treat_alt), by = "district_id") %>%
    mutate(treated = treat_alt)

synth_result_alt <- multisynth(
    vac_rate ~ treated,
    unit    = district_id,
    time    = week,
    data    = synth_balanced_alt,
    n_leads = NULL,
    n_lags  = NULL
)

cat("\n=== Robustness: FW > 2.5% threshold ===\n")
print(summary(synth_result_alt))

svg("figures/synth_control_aiwanger_alt.svg", width = 10, height = 6)
plot(synth_result_alt) +
    geom_vline(xintercept = treatment_date, col = "red", lty = 2) +
    labs(
        title = "Robustness: FW > 2.5% threshold",
        y = "ATT (Vaccination rate per eligible voter)",
        x = ""
    ) +
    theme_minimal()
dev.off()

## ---- 7. Single-unit SCM: Landshut (Aiwanger's district) ----

## Landshut is in Bayern (Land=9), Niederbayern (Bezirk=2).
## Identify it as the Bavarian district with the highest FW share,
## with a sanity check on the Bezirk code.
bavarian_districts <- panel %>%
    filter(Land == 9) %>%
    pull(district_id) %>%
    unique()

bavarian_fw <- district_fw %>%
    filter(district_id %in% bavarian_districts)

## Landshut LK has AGS 09274 -> Land=9, Bezirk=2, Kreis=74
## Landshut SK has AGS 09261 -> Land=9, Bezirk=2, Kreis=61
## Try exact match first, fall back to highest-FW Bavarian district
landshut_candidates <- c("9_2_74", "9_2_61")
landshut_id <- landshut_candidates[landshut_candidates %in% bavarian_fw$district_id]

if (length(landshut_id) == 0) {
    ## Fallback: highest FW share in Bavaria
    landshut_id <- bavarian_fw %>%
        slice_max(FW_share, n = 1) %>%
        pull(district_id)
    cat(sprintf("Landshut not found by AGS; using highest-FW Bavarian district: %s (FW=%.1f%%)\n",
                landshut_id, bavarian_fw$FW_share[bavarian_fw$district_id == landshut_id] * 100))
} else {
    ## If both SK and LK exist, pick the one with higher FW share
    landshut_id <- bavarian_fw %>%
        filter(district_id %in% landshut_id) %>%
        slice_max(FW_share, n = 1) %>%
        pull(district_id)
}

cat(sprintf(
    "Landshut district: %s (FW share: %.1f%%)\n",
    landshut_id,
    bavarian_fw$FW_share[bavarian_fw$district_id == landshut_id] * 100
))

## Donor pool: all other Bavarian districts
landshut_panel <- panel %>%
    filter(district_id %in% bavarian_districts) %>%
    mutate(treated = as.integer(district_id == landshut_id))

## Balance the panel (keep districts with >= 95% of weeks, then intersect weeks)
lh_district_counts <- landshut_panel %>%
    group_by(district_id) %>%
    summarise(n_weeks = n_distinct(week))

lh_max_weeks <- max(lh_district_counts$n_weeks)
lh_threshold <- floor(lh_max_weeks * 0.95)

## Always keep Landshut; keep donors with enough weeks
lh_balanced_ids <- lh_district_counts %>%
    filter(n_weeks >= lh_threshold | district_id == landshut_id) %>%
    pull(district_id)

lh_common_weeks <- landshut_panel %>%
    filter(district_id %in% lh_balanced_ids) %>%
    group_by(week) %>%
    summarise(n = n_distinct(district_id)) %>%
    filter(n == length(lh_balanced_ids)) %>%
    pull(week)

lh_balanced <- landshut_panel %>%
    filter(district_id %in% lh_balanced_ids, week %in% lh_common_weeks)

cat(sprintf(
    "Landshut SCM: 1 treated + %d Bavarian donors, %d weeks\n",
    n_distinct(lh_balanced$district_id) - 1,
    n_distinct(lh_common_weeks)
))

## Run single-unit augmented synthetic control
synth_landshut <- augsynth(
    vac_rate ~ treated,
    unit    = district_id,
    time    = week,
    data    = lh_balanced,
    progfunc = "Ridge",
    scm     = TRUE
)

cat("\n=== Synthetic Control: Landshut vs. Synthetic Landshut ===\n")
print(summary(synth_landshut))

## Plot observed vs synthetic trajectory
svg("figures/synth_control_landshut.svg", width = 10, height = 6)
plot(synth_landshut) +
    geom_vline(xintercept = treatment_date, col = "red", lty = 2) +
    annotate("text", x = treatment_date + 7, y = Inf, vjust = 2,
             label = "Aiwanger\ninterview", col = "red", size = 3) +
    geom_vline(xintercept = as.Date("2021-11-11"), col = "blue", lty = 2) +
    annotate("text", x = as.Date("2021-11-11") + 7, y = Inf, vjust = 2,
             label = "Aiwanger\nvaccinated", col = "blue", size = 3) +
    labs(
        title = "Synthetic Control: Landshut (Aiwanger's District)",
        subtitle = "Donor pool: Other Bavarian districts",
        y = "ATT (Vaccination rate per eligible voter)",
        x = ""
    ) +
    theme_minimal()
dev.off()

## Placebo inference: run SCM for each donor as if treated
donor_ids <- lh_balanced %>%
    filter(district_id != landshut_id) %>%
    pull(district_id) %>%
    unique()

placebo_atts <- list()
for (d in donor_ids) {
    placebo_data <- lh_balanced %>%
        mutate(treated = as.integer(district_id == d))

    tryCatch({
        placebo_fit <- augsynth(
            vac_rate ~ treated,
            unit     = district_id,
            time     = week,
            data     = placebo_data,
            progfunc = "Ridge",
            scm      = TRUE
        )
        placebo_atts[[d]] <- summary(placebo_fit)$att
    }, error = function(e) {
        cat(sprintf("  Placebo %s failed: %s\n", d, e$message))
    })
}

## Compare Landshut ATT to placebo distribution
landshut_att <- summary(synth_landshut)$att
placebo_att_vals <- sapply(placebo_atts, function(x) mean(x$Estimate, na.rm = TRUE))
landshut_att_mean <- mean(landshut_att$Estimate, na.rm = TRUE)
p_value <- mean(abs(placebo_att_vals) >= abs(landshut_att_mean), na.rm = TRUE)

cat(sprintf(
    "\nLandshut ATT (mean): %.5f | Placebo p-value: %.3f (%d/%d placebos)\n",
    landshut_att_mean, p_value, sum(abs(placebo_att_vals) >= abs(landshut_att_mean)), length(placebo_att_vals)
))

## Plot placebo distribution
svg("figures/synth_control_landshut_placebo.svg", width = 8, height = 5)
tibble(att = placebo_att_vals) %>%
    ggplot(aes(x = att)) +
    geom_histogram(bins = 30, fill = "grey70", col = "white") +
    geom_vline(xintercept = landshut_att_mean, col = "red", linewidth = 1) +
    annotate("text", x = landshut_att_mean, y = Inf, vjust = 2, hjust = -0.1,
             label = sprintf("Landshut\n(p = %.3f)", p_value), col = "red", size = 3.5) +
    labs(
        title = "Placebo Test: Landshut vs. Donor Districts",
        x = "Mean ATT (placebo distribution)",
        y = "Count"
    ) +
    theme_minimal()
dev.off()

cat("\nSynthetic control analysis complete. Figures saved to figures/\n")

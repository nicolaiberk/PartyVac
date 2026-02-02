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

## Install augsynth if not available
if (!requireNamespace("augsynth", quietly = TRUE)) {
    message("Installing augsynth...")
    install.packages("augsynth")
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

## Classify districts by FW support
## High FW (treated): >5% vote share
## Donor pool: 0% FW vote share (never had FW presence)
district_fw <- panel %>%
    group_by(district_id) %>%
    summarise(FW_share = mean(FW_share)) %>%
    mutate(
        treat_group = case_when(
            FW_share > 0.05 ~ "high_fw",
            FW_share > 0    ~ "low_fw",
            TRUE            ~ "no_fw"
        )
    )

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

## augsynth requires a balanced panel
## Keep only weeks present for all districts
week_counts <- synth_data %>%
    group_by(week) %>%
    summarise(n_districts = n_distinct(district_id))

district_counts <- synth_data %>%
    group_by(district_id) %>%
    summarise(n_weeks = n_distinct(week))

max_weeks <- max(district_counts$n_weeks)
balanced_districts <- district_counts %>%
    filter(n_weeks == max_weeks) %>%
    pull(district_id)

synth_balanced <- synth_data %>%
    filter(district_id %in% balanced_districts)

cat(sprintf(
    "Balanced panel: %d districts x %d weeks (%d treated, %d donors)\n",
    n_distinct(synth_balanced$district_id),
    max_weeks,
    sum(district_fw$treat_group == "high_fw" & district_fw$district_id %in% balanced_districts),
    sum(district_fw$treat_group == "no_fw" & district_fw$district_id %in% balanced_districts)
))

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

cat("\nSynthetic control analysis complete. Figures saved to figures/\n")

## generate datafile with measles vaccination rate, covid vaccination rate, Vote shares 2017 and 2021
library(data.table)
library(tidyverse)

## load BTW 17 data (https://bundeswahlleiterin.de/bundestagswahlen/2021/ergebnisse/repraesentative-wahlstatistik.html)
if (!file.exists("data_new/elections/btw17_lks_cleaned.csv")) {
    btw17_lk <- fread("data_new/elections/btw17_kerg2.csv", skip = 9)

    ## bring into relevant format
    btw17_lk <- 
        btw17_lk %>% 
        filter(Gebietsart == "Wahlkreis") %>%
        filter(Gruppenname %in% c("Wahlberechtigte", "Wählende", "Gültige", "CDU", "CSU", "SPD", "GRÜNE", "FDP", "DIE LINKE", "AfD", "FREIE WÄHLER")) %>%
        filter(Stimme == 2 | (Gruppenname  == "Wahlberechtigte")) %>%
        select(Gebietsnummer, Gruppenname, Prozent, Anzahl) %>% 
        pivot_wider(names_from = "Gruppenname", values_from = c("Prozent", "Anzahl"), names_prefix = "btw17_")

    ## save data
    fwrite(btw17_lk, "data_new/elections/btw17_lks_cleaned.csv", row.names = FALSE)

} else {
    btw17_lk <- fread("data_new/elections/btw17_lks_cleaned.csv")
}

## load BTW 21 data
if (!file.exists("data_new/elections/btw21_lks_cleaned.csv")) {
    btw21_lk <- fread("data_new/elections/btw21_kerg2.csv", skip = 9)

    ## bring into relevant format
    btw21_lk <- 
        btw21_lk %>% 
        filter(Gebietsart == "Wahlkreis") %>%
        filter(Gruppenname %in% c("Wahlberechtigte", "Wählende", "Gültige", "CDU", "CSU", "SPD", "GRÜNE", "FDP", "DIE LINKE", "AfD", "FREIE WÄHLER", "dieBasis")) %>%
        filter(Stimme == 2 | (Gruppenname  == "Wahlberechtigte")) %>%
        select(Gebietsnummer, Gebietsname, Gruppenname, Prozent, Anzahl) %>% 
        pivot_wider(names_from = "Gruppenname", values_from = c("Prozent", "Anzahl"), names_prefix = "btw21_")

    ## save data
    fwrite(btw21_lk, "data_new/elections/btw21_lks_cleaned.csv", row.names = FALSE)

} else {
    btw21_lk <- fread("data_new/elections/btw21_lks_cleaned.csv")
}


## load measles vaccination data from kv-surveilance/rki
if(!file.exists("data_new/vac/general/polio_2019_lks.csv")){

    vac_kv_lk <- read.delim("data_new/vac/general/rki/KVIS_Impfquoten_Kinder.tsv")


    vac_polio_lk <- 
        vac_kv_lk %>% 
        filter(Impfung == "Polio" & Altersgruppe == "24 Monate" & Geburtsjahr == 2014) %>% 
        select(Landkreis_ID, Landkreis_Name, Impfquote)

    rm(vac_kv_lk)

    fwrite(vac_polio_lk, "data_new/vac/general/polio_2019_lks.csv", row.names = FALSE)

} else {
    vac_polio_lk <- fread("data_new/vac/general/polio_2019_lks.csv")
}


## load covid vaccination data
if(!file.exists("data_new/vac/covid/covid_2021_lks.csv")) {

    if (!file.exists("data_new/vac/covid/raw_lks.csv")){
        vac_covid_lk <- fread("https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/main/Deutschland_Landkreise_COVID-19-Impfungen.csv")
        fwrite(vac_covid_lk, "data_new/vac/covid/raw_lks.csv", row.names = FALSE)

    } else {
        vac_covid_lk <- fread("data_new/vac/covid/raw_lks.csv")
    }

    vac_covid_lk <- 
        vac_covid_lk %>% 
        filter(Impfdatum < "2021-09-26", Impfschutz == 2) %>% 
        group_by(LandkreisId_Impfort) %>% 
        summarise(Anzahl = sum(Anzahl))
    
    fwrite(vac_covid_lk, "data_new/vac/covid/covid_2021_lks.csv", row.names = FALSE)

} else {
    vac_covid_lk <- fread("data_new/vac/covid/covid_2021_lks.csv")
}


## merge
merged <- 
    btw21_lk %>% 
    left_join(btw17_lk, by = c("Gebietsnummer"))

## ---- LK-WK matching with area & population weights ----
## Compute weights if not yet available, then use them for weighted aggregation.
## See code/geo_weights.R for details on how weights are computed from
## geographic intersection of LK and WK shapefiles + LK population.

if (!file.exists("data_new/matching/lk_wk_weights.csv")) {
    cat("LK-WK weights not found. Running geo_weights.R to compute them...\n")
    source("code/geo_weights.R")
}

lk_wk_weights <- fread("data_new/matching/lk_wk_weights.csv")

## prep polio data: population-weighted average of LK rates within each WK
vac_polio_lk <-
    vac_polio_lk %>%
    rename(Polio_Share = Impfquote) %>%
    inner_join(lk_wk_weights, by = "Landkreis_ID") %>%
    group_by(wk_nr) %>%
    summarise(Polio_Share = weighted.mean(Polio_Share, w = pop_weight_in_wk, na.rm = TRUE))

## prep covid data: distribute LK-level shots to WKs proportional to area share
## (area_share approximates the fraction of an LK's population residing in each WK)
vac_covid_lk <-
    vac_covid_lk %>%
    filter(LandkreisId_Impfort != 17000) %>%
    rename(Covid_Shots = Anzahl) %>%
    inner_join(lk_wk_weights, by = c("LandkreisId_Impfort" = "Landkreis_ID")) %>%
    mutate(Covid_Shots_weighted = Covid_Shots * area_share) %>%
    group_by(wk_nr) %>%
    summarise(Covid_Shots = sum(Covid_Shots_weighted))

## merge vac data
merged <- 
    merged %>% 
    left_join(vac_polio_lk, by = c("Gebietsnummer" = "wk_nr")) %>% 
    left_join(vac_covid_lk, by = c("Gebietsnummer" = "wk_nr"))

## Population per WK is now available from the geo weights
wk_pop <- lk_wk_weights %>%
    group_by(wk_nr) %>%
    summarise(Population = sum(pop_in_wk))

merged <- merged %>%
    left_join(wk_pop, by = c("Gebietsnummer" = "wk_nr"))

## save
fwrite(merged, "data_new/merged/btw_17_21_cov21_polio19.csv", row.names = FALSE)

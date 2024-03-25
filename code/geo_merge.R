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

## load lookup table for landkrei-wahlkreis matching
btw21_lks_wks <- 
    fread("data_new/elections/btw21_lks_wks.csv", skip = 7) %>% 
    select(`Wahlkreis-Nr`, `Wahlkreis-Bez`, RGS_Land, RGS_RegBez, RGS_Kreis, Kreisname) %>% 
    rename(
        wk_nr = `Wahlkreis-Nr`,
        wk_name = `Wahlkreis-Bez`
    ) %>%
    mutate(
        Landkreis_ID = 
            as.numeric(
                paste0(
                    RGS_Land,
                    RGS_RegBez,
                    ifelse(
                        (str_length(as.character(RGS_Kreis)) == 1) & (str_length(as.character(RGS_RegBez)) == 1),
                        paste0("0", RGS_Kreis),
                        RGS_Kreis))
            )
    ) %>% 
    select(Landkreis_ID, wk_nr) %>% 
    unique()

## prep polio data with landkreis-wahlkreis matching
## note that we're assuming identical rates for lKs spanning multiple WKs
vac_polio_lk <- 
    vac_polio_lk %>% 
    left_join(y = btw21_lks_wks, by = c("Landkreis_ID")) %>%
    rename(Polio_Share = Impfquote) %>% 
    select(wk_nr, Landkreis_Name, Polio_Share) %>% 
    group_by(wk_nr) %>%
    summarise(Polio_Share = mean(Polio_Share))

## prep covid data with landkreis-wahlkreis matching
vac_covid_lk <- 
    vac_covid_lk %>% 
    filter(LandkreisId_Impfort != 17000) %>% # Impfungen des Bundesressorts werden separat ausgewiesen, da die Impfstellen des Bundes ohne exakte Angabe des Impfortes melden
    left_join(btw21_lks_wks, by = c("LandkreisId_Impfort" = "Landkreis_ID")) %>% 
    rename(Covid_Shots = Anzahl) %>%

    ## if lk matches mulitple wks, distribute shots equally
    group_by(LandkreisId_Impfort) %>%
    mutate(Covid_Shots = Covid_Shots/n()) %>%
    ungroup() %>%

    group_by(wk_nr) %>%
    summarise(Covid_Shots = sum(Covid_Shots))

## NOTE: ideally do this with weighting of population and geographical overlap - in total 108 LKs with multiple WKs!

## merge vac data
merged <- 
    merged %>% 
    left_join(vac_polio_lk, by = c("Gebietsnummer" = "wk_nr")) %>% 
    left_join(vac_covid_lk, by = c("Gebietsnummer" = "wk_nr"))

## merge population-level data - these stats are bs, weight by eligible voters for now
# btw21_wks_pop <- 
#     fread("data_new/elections/btw21_lks_wks_pop.csv") %>% 
#     rename(wk_nr = `Bundestagswahlkreis 2021`) %>%
#     mutate(population = as.numeric(str_remove_all(`insgesamt`, " "))) %>%
#     group_by(wk_nr) %>%
#     summarise(Population = sum(population)) %>% 
#     select(wk_nr, Population)

# merged <- 
#     merged %>% 
#     left_join(btw21_wks_pop, by = c("Gebietsnummer" = "wk_nr"))

## save
fwrite(merged, "data_new/merged/btw_17_21_cov21_polio19.csv", row.names = FALSE)

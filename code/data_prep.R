# ______________________________________________
# Aiwanger
# Goal: Data Prep
# Procedure: load data, merge, save
# ______________________________________________
# Date:  Thu Jun 09 10:57:45 2022
# Author: Nicolai Berk
#  R version 4.1.1 (2021-08-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(dplyr)
library(here)
library(data.table)


## covid-vaccinations ####
# (see https://github.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/blob/master/Readme.md)
vacc_lk <- fread('https://github.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/raw/master/Aktuell_Deutschland_Landkreise_COVID-19-Impfungen.csv')



vacc_lk_tot <- 
  vacc_lk %>% 
  mutate(LK = LandkreisId_Impfort) %>% 
  filter(LK != 'u') %>% 
  # group_by(LK, Impfdatum) %>% 
  # summarise(Anzahl = sum(Anzahl)) %>% 
  mutate(
    Land   = as.numeric(substr(LK, 1, 2)),
    Bezirk = as.numeric(substr(LK, 3, 3)),
    Kreis  = as.numeric(substr(LK, 4, 5))
  )

rm(vacc_lk)

fwrite(vacc_lk_tot, here('data/rki_vaccinations.csv'))


# BTW 2017 ####
BTW_17_LK <- fread("https://bundeswahlleiter.de/dam/jcr/2e018ffc-0368-4c87-b85f-23dae3a5c8f5/btw2017kreis.csv", 
                   skip = 5, header = T, nrows = 402)
colnames(BTW_17_LK) <- 
  fread("https://bundeswahlleiter.de/dam/jcr/2e018ffc-0368-4c87-b85f-23dae3a5c8f5/btw2017kreis.csv", 
        skip = 4, header = T, nrows = 0) %>% 
  colnames()


BTW_17_LK_ES <- BTW_17_LK[ , Land:Übrige] %>% 
  mutate(
    Land   = floor(`Statistische Kennziffer`/1000),
    Bezirk = floor((`Statistische Kennziffer`-Land*1000)/100),
    Kreis  = floor(`Statistische Kennziffer`-Land*1000-Bezirk*100)
  )

BTW_17_LK_ZS <- BTW_17_LK[ , Land:Wähler]
BTW_17_LK_ZS <- 
  cbind(
    BTW_17_LK_ZS,
    BTW_17_LK[ , 46:81])

## simple corr
merged_btw <-
  BTW_17_LK_ES %>%
  full_join(vacc_lk_tot %>% filter(Impfschutz == 1), by = c('Land', 'Bezirk', 'Kreis')) %>% 
  mutate(treatment = `FREIE WÄHLER` > 0)  %>% 
  mutate(post_vac  = Impfdatum >= as.Date("2021-11-11")) %>% 
  mutate(post_iv   = Impfdatum >= as.Date("2021-05-07")) %>% 
  mutate(FW_share  = `FREIE WÄHLER`/Wähler) %>% 
  mutate(AfD_share  = AfD/Wähler) %>% 
  group_by(Land, Bezirk, Kreis) %>% 
  mutate(Impfweek = lubridate::floor_date(Impfdatum, 'week')) %>% 
  mutate(cum_vacc = cumsum(Anzahl))

fwrite(merged_btw, "data/covid_btw_es.csv")

merged_btw_cum <- 
  merged_btw %>% 
  group_by(Land, Bezirk, Kreis) %>% 
  summarise(
    FW_share = mean(FW_share),
    AfD_share = mean(AfD_share),
    vac_share = sum(Anzahl)/mean(Wahlberechtigte),
    Wähler = mean(Wahlberechtigte)
  )
fwrite(merged_btw_cum, "data/covid_btw_es_cum.csv")

merged_btw_reg <- # aggregate over age groups
  merged_btw %>%
  group_by(LandkreisId_Impfort, Impfdatum) %>% 
  summarise(
    Anzahl = sum(Anzahl),
    FW_share = mean(FW_share),
    AfD_share = mean(AfD_share)
  )  %>% 
  mutate(treatment = FW_share > 0)  %>% 
  mutate(treatment = AfD_share > 0)  %>% 
  mutate(post_vac  = Impfdatum >= as.Date("2021-11-11")) %>% 
  mutate(post_iv   = Impfdatum >= as.Date("2021-05-07")) %>% 
  mutate(treat_group = cut(FW_share, breaks = c(-Inf, 0, 0.01, 0.025, 0.05, 0.1)))

fwrite(merged_btw_reg, "data/covid_btw_es_reg.csv")


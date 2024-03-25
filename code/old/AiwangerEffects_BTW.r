## Aiwanger Effects
library(tidyverse)
library(dplyr)
library(here)
library(data.table)


## this data might be better suited: https://github.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/blob/main/Deutschland_Impfquoten_COVID-19.csv
merged_btw <- fread("data_new/merged/covid_btw_es.csv", encoding = "Latin-1")

## data prep
merged_btw <- 
  merged_btw %>% 
  filter(!is.na(Anzahl)) %>% 
  mutate(
    FW_share = ifelse(is.na(FW_share), 0, FW_share)
  ) %>% 
  group_by(Land, Bezirk, Kreis, Impfdatum) %>%
  summarise(
    Anzahl = sum(Anzahl),
    FW_share = mean(FW_share),
    AfD_share = mean(AfD_share),
    voters = sum(Wähler),
    vac_share = sum(Anzahl)/mean(Wähler)
  ) %>%
  mutate(post_iv   = Impfdatum >= as.Date("2021-05-07")) %>% 
  mutate(post_vac  = Impfdatum >= as.Date("2021-11-11")) %>% 
  mutate(
    date_num = as.numeric(as.Date(Impfdatum)),
    dose = cut(
      FW_share,
      breaks = c(-Inf, 0, 0.05, 0.1, Inf),
      labels = c("0", "<5%", "<10%", ">10%")
    )
  )


## calculate post-FE residuals
merged_btw$res_fe <-
  fixest::feols(Anzahl ~ 1 | Land + Bezirk + Kreis + Impfdatum, data = merged_btw)$residuals
merged_btw[!is.na(merged_btw$voters), "res_fe_share"] <-
  fixest::feols(vac_share ~ 1 | Land + Bezirk + Kreis + Impfdatum, data = merged_btw[!is.na(merged_btw$voters),])$residuals


## TWFE models for aiwanger interview and vaccination effect
summary(fixest::feols(Anzahl ~ FW_share * post_iv | Land + Bezirk + Kreis + Impfdatum, data = merged_btw %>% filter(!post_vac))) # looks like control accommodated to skeptical FW districts after intervew
summary(fixest::feols(Anzahl ~ FW_share * post_vac | Land + Bezirk + Kreis + Impfdatum, data = merged_btw %>% filter(post_iv)))

summary(fixest::feols(vac_share ~ FW_share * post_iv | Land + Bezirk + Kreis + Impfdatum, data = merged_btw %>% filter(!post_vac))) # looks like control accommodated to skeptical FW districts after intervew
summary(fixest::feols(vac_share ~ FW_share * post_vac | Land + Bezirk + Kreis + Impfdatum, data = merged_btw %>% filter(post_iv)))


## RDD
hubsi_dates <- as.numeric(c(as.Date("2021-05-07"), as.Date("2021-11-11")))
high_fw_dists <- merged_btw %>% filter(FW_share > 0.05)
low_fw_dists <- merged_btw %>% filter(FW_share < 0.05 & FW_share > 0)
no_fw_dists <- merged_btw %>% filter(FW_share == 0)


## RDD raw number among high FW share districts (opposite direction, plots weird)
summary(rdrobust::rdrobust(high_fw_dists$res_fe, high_fw_dists$date_num, hubsi_dates[1]))
rdrobust::rdplot(high_fw_dists$res_fe, high_fw_dists$date_num, hubsi_dates[1])
summary(rdrobust::rdrobust(high_fw_dists$res_fe, high_fw_dists$date_num, hubsi_dates[2]))
rdrobust::rdplot(high_fw_dists$res_fe, high_fw_dists$date_num, hubsi_dates[2])

## RDD raw number among low FW share districts (none, weird plots)
summary(rdrobust::rdrobust(low_fw_dists$res_fe, low_fw_dists$date_num, hubsi_dates[1]))
rdrobust::rdplot(low_fw_dists$res_fe, low_fw_dists$date_num, hubsi_dates[1])
summary(rdrobust::rdrobust(low_fw_dists$res_fe, low_fw_dists$date_num, hubsi_dates[2]))
rdrobust::rdplot(low_fw_dists$res_fe, low_fw_dists$date_num, hubsi_dates[2])

## RDD raw number among no FW share districts (none)
summary(rdrobust::rdrobust(no_fw_dists$res_fe, no_fw_dists$date_num, hubsi_dates[1]))
rdrobust::rdplot(no_fw_dists$res_fe, no_fw_dists$date_num, hubsi_dates[1])
summary(rdrobust::rdrobust(no_fw_dists$res_fe, no_fw_dists$date_num, hubsi_dates[2]))
rdrobust::rdplot(no_fw_dists$res_fe, no_fw_dists$date_num, hubsi_dates[2])

# RDD shares

## RDD share among high FW share districts (still weird)
summary(rdrobust::rdrobust(high_fw_dists$res_fe_share, high_fw_dists$date_num, hubsi_dates[1]))
rdrobust::rdplot(high_fw_dists$res_fe_share, high_fw_dists$date_num, hubsi_dates[1])
summary(rdrobust::rdrobust(high_fw_dists$res_fe_share, high_fw_dists$date_num, hubsi_dates[2]))
rdrobust::rdplot(high_fw_dists$res_fe_share, high_fw_dists$date_num, hubsi_dates[2])


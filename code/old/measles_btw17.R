# ______________________________________________
# Aiwanger cue taking or sorting
# Goal: Estimate effect of Aiwanger on Bavarian Impfdecision
# Procedure: load data, merge, estimate
# ______________________________________________
# Date:  Tue Dec 21 10:25:12 2021
# Author: Nicolai Berk
#  R version 4.1.1 (2021-08-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(dplyr)
library(here)
library(data.table)
library(glue)

# load data ####

## measles vacc ####

### Länder ####
if (!file.exists("data_new/vac/measles_BL_2008-2014.csv")){
  if (exists('vacc_measles_BL')){rm(vacc_measles_BL)}
  for (year in 2008:2014){
    vacc_temp <- 
      fread(glue('data/MasernImpfung/Masern_Impfquoten_Bundeslaender_{year}_24Monate_2Impfung.csv'), 
            encoding = 'UTF-8') %>% 
      mutate(year = year)
    
    if (exists('vacc_measles_BL')){
      vacc_measles_BL <- 
        vacc_measles_BL %>% 
        rbind(vacc_temp)
    }else{
      vacc_measles_BL <- vacc_temp
    }
  }

  fwrite(vacc_measles_BL, "data_new/vac/measles_BL_2008-2014.csv")
  rm(vacc_temp, vacc_measles_BL)
}


### LKs ####

if(!file.exists("data_new/vac/measles_LK_2008-2014.csv")){
  if (exists('vacc_measles_LK')){rm(vacc_measles_LK)}
  for (year in 2008:2014){
    vacc_temp <- 
      fread(glue('data/MasernImpfung/Masern_Impfquoten_Landkreise_{year}_24Monate_2Impfung.csv'), 
            encoding = 'UTF-8') %>% 
      mutate(year = year)
    
    if (exists('vacc_measles_LK')){
      vacc_measles_LK <- 
        vacc_measles_LK %>% 
        rbind(vacc_temp)
    }else{
      vacc_measles_LK <- vacc_temp
    }
  }
  fwrite(vacc_measles_LK, "data/measles_LK_2008-2014.csv")
  rm(vacc_temp)
} else {
  vacc_measles_LK <- fread("data/measles_LK_2008-2014.csv")
}


## BTW data 

if (!file.exists("data_new/elections/BTW_17_LK.csv")){

  BTW_17_LK <- fread("https://bundeswahlleiterin.de/dam/jcr/2e018ffc-0368-4c87-b85f-23dae3a5c8f5/btw2017kreis.csv", 
                    skip = 5, header = T, nrows = 402, encoding = "Latin-1")
  colnames(BTW_17_LK) <- 
    fread("https://bundeswahlleiterin.de/dam/jcr/2e018ffc-0368-4c87-b85f-23dae3a5c8f5/btw2017kreis.csv", 
          skip = 4, header = T, nrows = 0, encoding = "Latin-1") %>% 
    colnames()



  BTW_17_LK_ES <- BTW_17_LK[ , Land:Übrige] %>% 
    mutate(
      Land   = floor(`Statistische Kennziffer`/1000),
      Bezirk = floor((`Statistische Kennziffer`-Land*1000)/100),
      Kreis  = floor(`Statistische Kennziffer`-Land*1000-Bezirk*100)
    )

  rm(BTW_17_LK)

  ## clean
  BTW_17_LK_ES <- 
    BTW_17_LK_ES %>% 
    mutate(LK = str_replace_all(`Kreisfreie Stadt bzw. Stadtkreis, Landkreis bzw. Kreis`, 
                                ", Stadt", 
                                "")) %>% 
    mutate(LK = str_replace_all(LK, 
                                ", Freie und Hansestadt", 
                                "")) %>% 
    mutate(LK = str_replace_all(LK, 
                                "Städteregion", 
                                "SR")) %>% 
    mutate(LK = str_replace_all(LK, 
                                ", Hansestadt", 
                                "")) %>% 
    mutate(LK = str_replace_all(LK, 
                                ", Landeshauptstadt", 
                                ""))  %>% 
    mutate(LK = str_replace_all(LK, " der FernUniversität", ""))  %>% 
    mutate(LK = str_replace_all(LK, ", .*tadt", ""))   %>% 
    mutate(LK = str_replace_all(LK, 
                                "i\\.", 
                                "")) %>% 
    mutate(LK = str_replace_all(LK, 
                                "d\\.", 
                                ""))  %>% 
    mutate(LK = str_replace_all(LK, 
                                "a\\.", 
                                "")) %>% 
    mutate(LK = str_replace_all(LK, 
                                "–", 
                                "-")) %>% 
    mutate(LK = str_replace_all(LK, 
                                "<U+0096>", 
                                "-"))

  BTW_17_LK_ES %>% 
    select(`Kreisfreie Stadt bzw. Stadtkreis, Landkreis bzw. Kreis`, LK) %>% 
    head(20)

  vacc_measles_LK <- 
    vacc_measles_LK %>% 
    filter(Landkreis != "undefined") %>%
    mutate(LK = str_replace_all(Landkreis, 
                                "SK Offenbach", 
                                "Offenbach am Main")) %>%
    mutate(LK = str_replace_all(LK, 
                                "Halle", 
                                "Halle (Saale)")) %>%
    mutate(LK = str_replace_all(LK, 
                                "Altenkirchen", 
                                "Altenkirchen (Westerwald)")) %>%
    mutate(LK = str_replace_all(LK, 
                                "Bitburg-Prüm", 
                                "Eifelkreis Bitburg-Prüm")) %>%
    mutate(LK = str_replace_all(LK, 
                                "Ludwigshafen", 
                                "Ludwigshafen am Rhein")) %>%
    mutate(LK = str_replace_all(LK, 
                                "Lindau", 
                                "Lindau (Bodensee)")) %>%
    mutate(LK = str_replace_all(LK, 
                                "Kempten", 
                                "Kempten (Allgäu)")) %>%
    mutate(LK = str_replace_all(LK, 
                                "Frankenthal", 
                                "Frankenthal (Pfalz)")) %>%
    mutate(LK = str_replace_all(LK, 
                                "SK Oldenburg", 
                                "Oldenburg (Oldenburg)")) %>%
    mutate(LK = str_replace_all(LK, 
                                "LK Rostock", 
                                "Landkreis Rostock")) %>% 
    mutate(LK = str_replace_all(LK, "LK ", "")) %>% 
    mutate(LK = str_replace_all(LK, "SK ", ""))  %>% 
    mutate(LK = str_replace_all(LK, " im", ""))  %>% 
    mutate(LK = str_replace_all(LK, " in der", ""))  %>% 
    mutate(LK = str_replace_all(LK, " an der", ""))  %>% 
    mutate(LK = str_replace_all(LK, " am", "")) %>% 
    mutate(LK = str_replace_all(LK, 
                                "i\\.", 
                                "")) %>% 
    mutate(LK = str_replace_all(LK, 
                                "d\\.", 
                                ""))  %>% 
    mutate(LK = str_replace_all(LK, 
                                "a\\.", 
                                "")) %>% 
    mutate(LK = str_replace_all(LK, 
                                "<U+0096>", 
                                "-")) %>% 
    # assign Berlin districts (https://de.wikipedia.org/wiki/Verwaltungsgliederung_Berlins#Nach_der_Wiedervereinigung)
    mutate(LK = str_replace_all(LK, 
                                "Berlin Friedrichshain", 
                                "Berlin Ost")) %>%
    mutate(LK = str_replace_all(LK, 
                                "Berlin Hellersdorf", 
                                "Berlin Ost")) %>%
    mutate(LK = str_replace_all(LK, 
                                "Berlin Hohenschönhausen", 
                                "Berlin Ost")) %>%
    mutate(LK = str_replace_all(LK, 
                                "Berlin Köpenick", 
                                "Berlin Ost")) %>%
    mutate(LK = str_replace_all(LK, 
                                "Berlin Lichtenberg", 
                                "Berlin Ost")) %>%
    mutate(LK = str_replace_all(LK, 
                                "Berlin Marzahn", 
                                "Berlin Ost")) %>%
    mutate(LK = str_replace_all(LK, 
                                "Berlin Mitte", 
                                "Berlin Ost")) %>%
    mutate(LK = str_replace_all(LK, 
                                "Berlin Pankow", 
                                "Berlin Ost")) %>%
    mutate(LK = str_replace_all(LK, 
                                "Berlin Prenzlauer Berg", 
                                "Berlin Ost")) %>%
    mutate(LK = str_replace_all(LK, 
                                "Berlin Treptow", 
                                "Berlin Ost")) %>%
    mutate(LK = str_replace_all(LK, 
                                "Berlin Weißensee", 
                                "Berlin Ost")) %>%
    mutate(LK = str_replace_all(LK, 
                                "Berlin Charlottenburg", 
                                "Berlin West")) %>%
    mutate(LK = str_replace_all(LK, 
                                "Berlin Kreuzberg", 
                                "Berlin West")) %>%
    mutate(LK = str_replace_all(LK, 
                                "Berlin Neukölln", 
                                "Berlin West")) %>%
    mutate(LK = str_replace_all(LK, 
                                "Berlin Reinickendorf", 
                                "Berlin West")) %>%
    mutate(LK = str_replace_all(LK, 
                                "Berlin Schöneberg", 
                                "Berlin West")) %>%
    mutate(LK = str_replace_all(LK, 
                                "Berlin Spandau", 
                                "Berlin West")) %>%
    mutate(LK = str_replace_all(LK, 
                                "Berlin Steglitz", 
                                "Berlin West")) %>%
    mutate(LK = str_replace_all(LK, 
                                "Berlin Tempelhof", 
                                "Berlin West")) %>%
    mutate(LK = str_replace_all(LK, 
                                "Berlin Tiergarten", 
                                "Berlin West")) %>%
    mutate(LK = str_replace_all(LK, 
                                "Berlin Tiergarten", 
                                "Berlin West")) %>%
    mutate(LK = str_replace_all(LK, 
                                "Berlin Wedding", 
                                "Berlin West")) %>%
    mutate(LK = str_replace_all(LK, 
                                "Berlin Wedding", 
                                "Berlin West")) %>%
    mutate(LK = str_replace_all(LK, 
                                "Berlin Wilmersdorf", 
                                "Berlin West")) %>%
    mutate(LK = str_replace_all(LK, 
                                "Berlin Zehlendorf", 
                                "Berlin West")) %>%
    mutate(LK = str_replace_all(LK, 
                                "Osterode Harz", 
                                "Göttingen"))

  ## 363/414 identical after cleaning
  sum(unique(vacc_measles_LK$LK) %in% BTW_17_LK_ES$LK)
  sort(BTW_17_LK_ES$LK[!BTW_17_LK_ES$LK %in% unique(vacc_measles_LK$LK)])
  sort(unique(vacc_measles_LK$LK)[!unique(vacc_measles_LK$LK) %in% BTW_17_LK_ES$LK])


  ## use levenshtein distance
  library(stringdist)
  levenshtein_matrix <-
    stringdistmatrix(
      vacc_measles_LK$LK, 
      BTW_17_LK_ES$LK)

  vacc_measles_LK$LK_ID <- 
    BTW_17_LK_ES$LK[apply(levenshtein_matrix, 1, which.min)]

  BTW_17_LK_ES$LK_ID <- 
    vacc_measles_LK$LK[apply(levenshtein_matrix, 2, which.min)]


  ## look at changed cases
  vacc_measles_LK %>% 
    filter(apply(levenshtein_matrix, 1, min) > 0) %>% 
    select(LK, LK_ID) %>% 
    unique()

  BTW_17_LK_ES %>% 
    filter(apply(levenshtein_matrix, 2, min) > 0) %>% 
    select(LK, LK_ID) %>% 
    unique()
    
  BTW_17_LK_ES <- 
    BTW_17_LK_ES %>% 
    select(Wahlberechtigte:LK, -Kreis, Land) %>% 
    group_by(Bezirk, LK, Land) %>% 
    summarise_all(sum)

  ## assign
  vacc_measles_LK <- 
    vacc_measles_LK %>% 
    mutate(LK = LK_ID)


# merge ####
vacc_btw <- 
  vacc_measles_LK %>% 
  left_join(BTW_17_LK_ES, by = c("LK"))

}


## correlation
vacc_btw <- 
  vacc_btw %>% 
  mutate(
    FW = `FREIE WÄHLER`,
    FW_share = FW / Wahlberechtigte,
    AfD = AfD,
    AfD_share = AfD / Wahlberechtigte
  )

vacc_btw %>% 
  group_by(Land) %>% 
  summarise(
    Impfquote = mean(Impfquote)
  )

## FE models
vacc_btw$res_iq <- 
  fixest::feols(Impfquote ~ 1 | Land, data = vacc_btw)$residuals

## weak neg. correlation measles vacc rate and fw share BTW 17 - stronger if Saxony excluded
vacc_btw %>% 
  filter(year == 2008) %>%
  ggplot(aes(Impfquote, FW_share, col = factor(Land), group = 1)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(
    x = "Impfquote",
    y = "FW share"
  )

## very neg. correlation measles vacc rate and afd share BTW 17 - mostly driven by Land differences
vacc_btw %>% 
  filter(year == 2008) %>%
  ggplot(aes(Impfquote, AfD_share, col = factor(Land), group = 1)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(
    x = "Impfquote",
    y = "AfD share"
  )

## look at changes in Saxonian districts over time
vacc_btw %>% 
  filter(Land == 14) %>% 
  ggplot(aes(year, Impfquote, col = factor(Landkreis))) +
  geom_line() +
  theme_minimal() +
  labs(
    x = "year",
    y = "Impfquote"
  )

vacc_btw %>%
  group_by(Land, year) %>% 
  summarise(
    Impfquote = mean(Impfquote)
  ) %>% 
  pivot_wider(
    names_from = year,
    values_from = Impfquote
  )

# ______________________________________________
# Aiwanger
# Goal: Analysis voter changes
# ______________________________________________
# Date:  Wed Dec 22 13:36:47 2021
# Author: Nicolai Berk
#  R version 4.1.1 (2021-08-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(dplyr)
library(here)
library(data.table)



## load btw 21 ####
BTW_21 <- 
  fread("data/btw21.csv", fill = T, encoding = "UTF-8")

tempnames <- 
  colnames(BTW_21)[4:length(colnames(BTW_21))]

for (i in 1:length(tempnames)){
  if (i %% 4 == 1){
    varname <- tempnames[i]
  }
  if (i %% 4 %in% c(1,2)){
    newname <- paste0(varname, "_ES")
  }else{
    newname <- paste0(varname, "_ZS")
  }
  if (i %% 2 == 0){
    tempnames[i] <- paste0(newname, "_last")
  }else{
    tempnames[i] <- paste0(newname, "_final")
  }
  
}


colnames(BTW_21)[4:ncol(BTW_21)] <- tempnames
rm(tempnames, newname, varname)

BTW_21 <- 
  BTW_21 %>% 
  select(-ends_with("_last")) %>% 
  filter(!Gebiet %in% c("", "Bundesgebiet"))

BTW_21 <- BTW_21[3:nrow(BTW_21),]


## BTW17 data 
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

rm(BTW_17_LK)


sort(BTW_21$Gebiet[!BTW_21$Gebiet %in% BTW_17_LK_ES$`Kreisfreie Stadt bzw. Stadtkreis, Landkreis bzw. Kreis`])
sort(BTW_17_LK_ES$`Kreisfreie Stadt bzw. Stadtkreis, Landkreis bzw. Kreis`[!BTW_17_LK_ES$`Kreisfreie Stadt bzw. Stadtkreis, Landkreis bzw. Kreis` %in% BTW_21$Gebiet])



## load covid vacc data
## covid-vaccinations ####
# (see https://github.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/blob/master/Readme.md)
vacc_lk <- fread('https://github.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/raw/master/Aktuell_Deutschland_Landkreise_COVID-19-Impfungen.csv')
# fwrite(vacc_lk, paste0('rki_vaccinations_', Sys.Date(), '.csv'))

# vacc_lk %>% 
#   ggplot(aes(x = Impfdatum, y = Anzahl, col = Altersgruppe)) +
#   geom_area()

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

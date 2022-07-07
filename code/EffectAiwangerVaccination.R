# ______________________________________________
# Cue-taking or issue sorting- the case of covid vaccinations
# Goal: Estimate effect of Aiwanger non-vaccination on 
#        vaccination rates among FW voters
# ______________________________________________
# Date:  Fri Dec 17 14:16:51 2021
# Author: Nicolai Berk
#  R version 4.1.1 (2021-08-10)
# ______________________________________________


# Setup ####
library(tidyverse)
library(dplyr)
library(here)
library(data.table)

# load and prepare data ####

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


## local election results 2018 ####
LTW_BY_18 <- 
  fread('data/14_10_2018_Landtagswahl_2018_Stimmkreise_Bayern.csv') %>% 
  select(-Parteiname) %>% 
  mutate(Wahlkreis = as.character(Schlüsselnummer)) %>% 
  mutate(Wahlkreis_new =
           case_when(
             Wahlkreis %in% as.character(101:109) ~ '101-109', # München
             Wahlkreis %in% as.character(c(501, 504)) ~ '501-504', # Nürnberg
             Wahlkreis %in% as.character(701) ~ '701-702', # Augsburg
             (!Wahlkreis %in% as.character(c(101:109, 501, 504, 701))) ~ Wahlkreis
           )) %>% 
  mutate(Wahlkreis = Wahlkreis_new) %>% 
  mutate(
    Turnout = as.numeric(str_replace(`Wahlbeteiligung in %`, ',', '.')),
  ) %>%
  select(contains(c('FREIE WÄHLER', 'CSU', 'SPD', 'GRÜNE')),
         Wahlkreis, Turnout, Stimmberechtigte, Wähler) %>% 
  # mutate(across(contains('Erststimme', 'Zweitstimme', 'Gesamtstimme'),
  #               ~ as.numeric(ifelse(.x == 'X', NA, .x)))) %>% 
  group_by(Wahlkreis) %>% 
  summarise(across(Stimmberechtigte:Wähler, sum),
            across(Turnout, mean),
            across(contains('Zweitstimme'), sum))

## wahlkreiszuordnung election 2018 ####
LTW_WK_LK <-
  fread('data/ltw18_btw17_gde_stkr_wkr.csv') %>% 
  mutate(BezirkKreis = str_sub(key, 1, 3),
         Wahlkreis = sk_ltw_nr) %>% 
  select(-key, -gemeinde, -vgem_nr, -vgem_name, -sk_ltw_nr, -wk_btw_nr, -wk_btw_name) %>% 
  unique()

## lookup for landkreise ####
AGS_LK <-
  read.fwf('data/GV100AD_301121.txt',
           fileEncoding = 'UTF-8',
           widths    = c(        2,       8,      2,        1,       2,          3,                 4,            50,      50,                 6,       2,       2,        11,          11,          11,      4,     5,         5,       2,                 4,                 4,                      5,                          3,                         3,       4,    20),
           col.names = c('Satzart', 'Stand', 'Land', 'Bezirk', 'Kreis', 'Gemeinde', 'Gemeindeverband', 'Bezeichnung', 'LeerB', 'Schlüsselfelder', 'Leer3', 'Leer4', 'Fläche', 'Bev.Gesamt', 'Bev.Männl', 'Leer', 'PLZ', 'zus.PLZ', 'Leer5', 'Finanzamtbezirk', 'Gerichtsbarkeit', 'Arbeitsagenturbezirk', 'BundestagswahlkreisStart', 'BundestagswahlkreisEnde', 'Leer6', 'Frei')) %>%
  filter(Satzart == 60) %>% 
  select(Land, Bezirk, Kreis, Bezeichnung, Bev.Gesamt, Fläche) %>% 
  group_by(Land, Bezirk, Kreis) %>% 
  summarise(Population = sum(Bev.Gesamt)/10000)


# merge ####

## overview and vacc covid ####
# merged_data_vacc <- 
  # AGS_LK %>%
  # full_join(vacc_lk_tot, by = c('Land', 'Bezirk', 'Kreis'))

## LTW to Kreise ####
merged_data_ltw <- 
  LTW_WK_LK %>% 
  right_join(., LTW_BY_18, by = c('Wahlkreis')) %>% 
  mutate(
    Land = 9,
    Bezirk = as.numeric(substr(BezirkKreis, 1, 1)),
    Kreis  = as.numeric(substr(BezirkKreis, 2, 3)),
  )
  
## vacc + ltw ####
merged_final <-
  merged_data_ltw %>%
  select(-landkreis) %>% 
  left_join(., AGS_LK %>% filter(Land == 9), by = c('Bezirk', 'Kreis')) %>% 
  full_join(vacc_lk_tot %>% filter(Land == 9), by = c('Bezirk', 'Kreis')) %>% 
  group_by(Wahlkreis, Impfdatum, Altersgruppe) %>% 
  summarise(across(c(contains('Zweitstimme'), Stimmberechtigte, Wähler), mean),
            Turnout = mean(Turnout),
            Anzahl = sum(Anzahl),
            Population = sum(Population)) %>%
  mutate(FW_share = `Zweitstimmen FREIE WÄHLER 2018`/Wähler)

## save
fwrite(merged_final, "data/covid_ltw.csv")

# analyse ####

## simple corr covid vacc & FW share
merged_cum <- 
  merged_final %>% 
  filter(Impfdatum < as.Date('2021-11-11')) %>% # subset vaccinations pre Aiwanger-vaccination
  group_by(Wahlkreis) %>% 
  summarise(
    FW_share = mean(FW_share),
    Wähler = mean(Wähler),
    Impfungen = sum(Anzahl),
    Population = mean(Population)
  ) %>% 
  mutate(vacc_share = Impfungen/Population)

fwrite(merged_cum, "data/covid_ltw_cum.csv")

merged_cum %>% 
  ggplot(aes(x = FW_share, y = vacc_share, size = Wähler)) +
  geom_point() +
  geom_smooth(method = 'lm')


## pseudo-DiD ####
# with random cutoff of +/- 10%
merged_final_reg <- 
  merged_final %>% 
  group_by(Wahlkreis, Impfdatum) %>% 
  summarise(
    Anzahl = sum(Anzahl),
    FW_share = mean(FW_share)
  ) %>% 
  mutate(treatment  = FW_share >= 0.1) %>% # this random cutoff does not make sense, might wanna group
  mutate(post_vac    = Impfdatum >= as.Date("2021-11-11")) %>% 
  mutate(post_iv   = Impfdatum >= as.Date("2021-05-07"))

### Aiwanger iv model ####

## binary
summary(lm(Anzahl ~ treatment * post_iv, data = merged_final_reg %>% filter(!post_vac)))
# indeed effect, but pre-treatment differences

## continuous
summary(lm(Anzahl ~ FW_share * post_iv, data = merged_final_reg %>% filter(!post_vac)))
# here as well



### Aiwanger interview
## binary
summary(lm(Anzahl ~ treatment * post_vac, data = merged_final_reg %>% filter(post_iv)))
# even negative effect

## continuous
summary(lm(Anzahl ~ FW_share * post_vac, data = merged_final_reg %>% filter(post_iv)))
# here as well


## plot trends
merged_final %>% 
  mutate(treatment = cut(FW_share, c(-Inf, 0.025, 0.05, 0.1, 0.2, Inf))) %>% 
  mutate(Impfdatum = lubridate::floor_date(Impfdatum, "month")) %>% 
  group_by(treatment, Impfdatum, Altersgruppe) %>% 
  summarise(Anzahl = sum(Anzahl)) %>% 
  ggplot(aes( x = Impfdatum, y = Anzahl, col = treatment, lty = treatment)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2021-05-07"), col = "red", lty = 2) +
  geom_vline(xintercept = as.Date("2021-11-11"), col = "red", lty = 2) +
  facet_wrap(~Altersgruppe, scales = "free_y")



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
  group_by(Land, Bezirk, Kreis) %>% 
  mutate(Impfweek = lubridate::floor_date(Impfdatum, 'week')) %>% 
  mutate(cum_vacc = cumsum(Anzahl))
    
fwrite(merged_btw, "data/covid_btw_es.csv")

merged_btw_cum <- 
  merged_btw %>% 
  group_by(Land, Bezirk, Kreis) %>% 
  summarise(
    FW_share = mean(FW_share),
    vac_share = sum(Anzahl)/mean(Wahlberechtigte),
    Wähler = mean(Wahlberechtigte)
  )
fwrite(merged_btw_cum, "data/covid_btw_es_cum.csv")





## DiD
merged_btw_reg <- # aggregate over age groups
  merged_btw %>%
  group_by(LandkreisId_Impfort, Impfdatum) %>% 
  summarise(
    Anzahl = sum(Anzahl),
    FW_share = mean(FW_share),
  )  %>% 
  mutate(treatment = FW_share > 0)  %>% 
  mutate(post_vac  = Impfdatum >= as.Date("2021-11-11")) %>% 
  mutate(post_iv   = Impfdatum >= as.Date("2021-05-07")) %>% 
  mutate(treat_group = cut(FW_share, breaks = c(-Inf, 0, 0.01, 0.025, 0.05, 0.1)))



### iv
summary(lm(Anzahl ~ treatment*post_iv, data = merged_btw_reg %>% filter(!post_vac)))
fixest::feglm(Anzahl ~ treatment*post_iv | LandkreisId_Impfort, 
              data = merged_btw_reg %>% 
                filter(!post_vac)
              )

# negative
summary(lm(Anzahl ~ FW_share*post_iv, 
           data = merged_btw_reg %>% 
             filter(!post_vac & treatment)))

# negative
fixest::feglm(Anzahl ~ FW_share*post_iv | LandkreisId_Impfort, 
           data = merged_btw_reg %>% 
             filter(!post_vac & treatment))
# negative

## plot
merged_btw_reg %>%
  filter(!is.na(FW_share)) %>% 
  ggplot(aes(x = Impfdatum, y = Anzahl, col = treatment)) +
  geom_smooth() +
  geom_vline(xintercept = as.Date("2021-05-07"), col = "red", lty = 2) +
  geom_vline(xintercept = as.Date("2021-11-11"), col = "red", lty = 2) +
  theme_minimal()


merged_btw_reg %>%
  mutate(Impfdatum = lubridate::floor_date(Impfdatum, "week")) %>% 
  group_by(treatment, Impfdatum) %>% 
  summarise(FW_share = mean(FW_share, na.rm = T),
            Anzahl = mean(Anzahl, na.rm = T)) %>% 
  filter(!is.na(FW_share)) %>% 
  ggplot(aes(x = Impfdatum, y = Anzahl, col = treatment)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2021-05-07"), col = "red", lty = 2) +
  geom_vline(xintercept = as.Date("2021-11-11"), col = "red", lty = 2) +
  xlab("") + ylab ("# vaccinations") +
  theme_minimal()


merged_btw %>% 
  filter(!is.na(FW_share), Altersgruppe %in% c("18-59", "60+")) %>%
  group_by(treatment, Impfweek) %>% 
  summarise(
    Anzahl = sum(Anzahl),
    Wähler = sum(Wähler)
  ) %>% 
  mutate(Impfquote = Anzahl/Wähler)%>% 
  ggplot(aes(x = Impfweek, y = Impfquote, col = treatment)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2021-05-07"), col = "red", lty = 2) +
  geom_vline(xintercept = as.Date("2021-11-11"), col = "red", lty = 2) +
  xlab("") + ylab ("Weekly Vacc./Eligible Voter") +
  theme_minimal()




merged_btw_agg <- 
  merged_btw %>% 
  filter(!is.na(FW_share)) %>%
  filter(!is.na(Impfweek)) %>%
  group_by(treatment, Impfweek) %>% 
  summarise(
    cum_vacc = sum(cum_vacc),
    Wahlberechtigte = sum(Wahlberechtigte)
  ) %>% 
  mutate(Impfquote = cum_vacc/Wahlberechtigte)

merged_btw_groups <- 
  merged_btw %>% 
  filter(!is.na(FW_share)) %>%
  filter(!is.na(Impfweek)) %>%
  mutate(treat_group = cut(FW_share, breaks = c(-Inf, 0, 0.01, 0.025, 0.05, Inf))) %>% 
  group_by(treat_group, Impfweek) %>% 
  summarise(
    cum_vacc = sum(cum_vacc),
    Wahlberechtigte = sum(Wahlberechtigte)
  ) %>% 
  mutate(Impfquote = cum_vacc/Wahlberechtigte)


cum_plot <- 
  merged_btw_agg %>% 
  ggplot(aes(x = Impfweek, y = Impfquote, col = treatment)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = as.Date("2021-05-07"), col = "red", lty = 2) +
  geom_vline(xintercept = as.Date("2021-11-11"), col = "red", lty = 2)

trends <- fread(here("data/googleTrends.csv"))
cum_plot +
  geom_area(inherit.aes = F, data = trends, aes(x = as.Date(trends$Woche), y = trends$`aiwanger impfung: (Deutschland)`/100), alpha = 0.4, fill = "lightblue") +
  theme_minimal()

cum_plot_groups <- 
  merged_btw_groups %>% 
  ggplot(aes(x = Impfweek, y = Impfquote, col = treat_group)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = as.Date("2021-05-07"), col = "red", lty = 2) +
  geom_vline(xintercept = as.Date("2021-11-11"), col = "red", lty = 2)

cum_plot_groups +
  geom_area(inherit.aes = F, data = trends, aes(x = as.Date(trends$Woche), y = trends$`aiwanger impfung: (Deutschland)`/100), alpha = 0.4, fill = "lightblue") +
  theme_minimal()

# de-trended
merged_btw_agg$daily_residuals <- lm(Impfquote ~ as.factor(Impfweek), data = merged_btw_agg)$residuals
merged_btw_groups$daily_residuals <- lm(Impfquote ~ as.factor(Impfweek), data = merged_btw_groups)$residuals

detrended_plot <- 
  merged_btw_agg %>% 
  ggplot(aes(x = Impfweek, y = daily_residuals, col = treatment)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = as.Date("2021-05-07"), col = "red", lty = 2) +
  geom_vline(xintercept = as.Date("2021-11-11"), col = "red", lty = 2)

## add google trends data
detrended_plot +
  geom_area(inherit.aes = F, data = trends, aes(x = as.Date(trends$Woche), y = trends$`aiwanger impfung: (Deutschland)`/100), alpha = 0.4, fill = "lightblue")

## groups
detrended_plot_groups <- 
  merged_btw_groups %>% 
  ggplot(aes(x = Impfweek, y = daily_residuals, col = treat_group)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = as.Date("2021-05-07"), col = "red", lty = 2) +
  geom_vline(xintercept = as.Date("2021-11-11"), col = "red", lty = 2)

## add google trends data
detrended_plot_groups +
  geom_area(inherit.aes = F, data = trends, aes(x = as.Date(trends$Woche), y = trends$`aiwanger impfung: (Deutschland)`/100), alpha = 0.4, fill = "lightblue")



### vac
summary(lm(FW_share ~ treatment*post_vac, data = merged_btw_reg %>% filter(post_iv)))


# BY vs rest
merged_btw %>% 
  mutate(Impfdatum = lubridate::floor_date(Impfdatum, "week")) %>% 
  mutate(Bavaria = Land == 9) %>% 
  filter(!is.na(Wahlberechtigte)) %>% 
  group_by(Bavaria, Impfdatum) %>% 
  summarise(
    Wahlberechtigte = sum(Wahlberechtigte),
    Anzahl = sum(Anzahl),
    FW_share = mean(FW_share),
  ) %>% 
  ggplot(aes(x = Impfdatum, y = Anzahl/Wahlberechtigte, col = Bavaria)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2021-05-07"), col = "red", lty = 2) +
  geom_vline(xintercept = as.Date("2021-11-11"), col = "red", lty = 2) +
  theme_minimal()
  
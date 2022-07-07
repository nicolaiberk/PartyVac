# ______________________________________________
# Aiwanger
# Goal: functions
# ______________________________________________
# Date:  Thu Jun 09 10:58:25 2022
# Author: Nicolai Berk
#  R version 4.1.1 (2021-08-10)
# ______________________________________________


VaccDesc <- function() {
  fread(here('data/rki_vaccinations.csv')) %>%
    mutate(date_month = lubridate::floor_date(Impfdatum, "week")) %>% 
    group_by(date_month) %>% 
    summarise(vaccinations = sum(Anzahl)) %>% 
    ggplot(aes(x = date_month, y = vaccinations)) +
    geom_line() +
    geom_vline(xintercept = as.Date("2021-05-01"), col = "red", lty = 2) +
    theme_minimal() +
    ylab("Vaccinations") + xlab("") %>% 
    return()
}

SimpleCor <- function() {
  
  fread(here("data/covid_btw_es_cum.csv")) %>% 
    filter(FW_share > 0) %>% 
    ggplot(aes(x = log(FW_share), y = log(vac_share), size = Wähler)) +
    geom_point() +
    geom_smooth(method = 'lm') %>% 
    return()
  
}


DiDTable <- function() {
  
  lm(Impfquote ~ treatment*post_iv, 
     data = 
       fread(here("data/covid_btw_es.csv")) %>% 
       filter(!is.na(FW_share), Altersgruppe %in% c("18-59"), Impfschutz == 1) %>%
       group_by(`Kreisfreie Stadt bzw. Stadtkreis, Landkreis bzw. Kreis`, Impfdatum) %>% 
       summarise(
         Anzahl = sum(Anzahl),
         Wähler = min(Wähler),
         FW_share = mean(FW_share)
       ) %>% 
       mutate(treatment = FW_share > 0)  %>% 
       mutate(post_vac  = Impfdatum >= as.Date("2021-11-11")) %>% 
       mutate(post_iv   = Impfdatum >= as.Date("2021-05-07")) %>% 
       filter(!post_vac) %>% 
       mutate(Impfquote = (Anzahl/Wähler)*100)) %>% 
    summary() %>% 
    modelsummary::modelsummary(output = "markdown", 
                               stars = T, 
                               statistic = NULL,
                               gof_omit = ".*") %>% 
    return()

}

DiDPlot <- function(size = 1) {
  
  trends <- fread(here("data/googleTrends.csv"))
  
  fread(here("data/covid_btw_es.csv")) %>% 
    filter(!is.na(FW_share), Altersgruppe %in% c("18-59"), Impfschutz == 1) %>%
    group_by(treatment, Impfdatum) %>% 
    summarise(
      Anzahl = sum(Anzahl),
      Wähler = sum(Wähler),
      FW_share = mean(FW_share),
      Impfweek = min(Impfweek)
    ) %>% 
    group_by(treatment, Impfweek) %>%  
    summarise(
      Anzahl = sum(Anzahl),
      Wähler = mean(Wähler),
      FW_share = mean(FW_share)
    ) %>% 
    mutate(treatment = FW_share > 0)  %>% 
    mutate(Impfquote = (Anzahl/Wähler)) %>% 
    ggplot(aes(x = Impfweek, y = Impfquote, col = treatment, lty = treatment)) +
    geom_line(size = size) +
    geom_vline(xintercept = as.Date("2021-05-07"), col = "red", lty = 2, size = size) +
    geom_vline(xintercept = as.Date("2021-11-11"), col = "red", lty = 2, size = size) +
    xlab("") + ylab ("Weekly Vacc./Eligible Voter") +
    scale_x_date(date_labels = "%b") +
    theme_minimal() +
    geom_area(inherit.aes = F, data = trends, aes(x = as.Date(trends$Woche), y = trends$`aiwanger impfung: (Deutschland)`/1000), alpha = 0.4) +
    theme_minimal() %>% 
    return()
  
}

GroupedDiDPlot <- function(size = 1) {
  
  
  trends <- fread(here("data/googleTrends.csv"))
  
  fread(here("data/covid_btw_es.csv")) %>% 
    filter(!is.na(FW_share), Altersgruppe %in% c("18-59"), Impfschutz == 1) %>%
    mutate(treat_group = cut(FW_share, breaks = c(-Inf, 0, 0.01, 0.025, 0.05, Inf))) %>% 
    group_by(treat_group, Impfdatum) %>% 
    summarise(
      Anzahl = sum(Anzahl),
      Wähler = sum(Wähler),
      Impfweek = min(Impfweek)
    ) %>% 
    group_by(treat_group, Impfweek) %>% 
    summarise(
      Anzahl = sum(Anzahl),
      Wähler = mean(Wähler)
    ) %>% 
    mutate(Impfquote = Anzahl/Wähler) %>% 
    ggplot(aes(x = Impfweek, y = Impfquote, col = treat_group)) +
    geom_line(size = size) +
    geom_vline(xintercept = as.Date("2021-05-07"), col = "red", lty = 2, size = size) +
    geom_vline(xintercept = as.Date("2021-11-11"), col = "red", lty = 2, size = size) +
    xlab("") + ylab ("Weekly Vacc./Eligible Voter") +
    geom_area(inherit.aes = F, data = trends, aes(x = as.Date(trends$Woche), y = trends$`aiwanger impfung: (Deutschland)`/1000), alpha = 0.4) +
    theme_minimal() %>% 
    return()
  
}
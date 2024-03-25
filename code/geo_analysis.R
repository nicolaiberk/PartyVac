## Analyse relationship of polio vaccine, election results and covid vaccination
library(tidyverse)
merged <- read.csv("data_new/merged/btw_17_21_cov21_polio19.csv")

## calculate vaccination rates
merged <- 
    merged %>% 
    mutate(
        Polio_Share = Polio_Share / 100,
        Covid_Share = Covid_Shots / Anzahl_btw21_Wahlberechtigte,
        across(contains("Prozent_btw"), ~ as.numeric(str_replace(.x, ",", ".")) / 100)
    )

## Polio Share not Predictive of Covid Share
cor(merged$Covid_Share, merged$Polio_Share, use = "pairwise.complete.obs") # no correlation!!!

svg("figures/covid_hist.svg")
hist(
    merged$Covid_Share, 
    main = "Geographical Distribution of Covid Vaccination",
    xlab = "Covid Vaccinations/Population",
    ) ## most places don't vacccinate, few vaccinate a lot
dev.off()

svg("figures/polio_hist.svg")
hist(merged$Polio_Share, ## most places have high vaccination share
    main = "Geographical Distribution of Polio Vaccination",
    xlab = "Polio Vaccination Coverage",
    )
dev.off()

## maps
library(sf)
btw21_geo <- read_sf("data_new/elections/btw21_geometrie_wahlkreise_shp/Geometrie_Wahlkreise_20DBT.shp")

btw21_geo$Polio_Share <- merged$Polio_Share[match(btw21_geo$WKR_NR, merged$Gebietsnummer)]
btw21_geo$Covid_Share <- merged$Covid_Share[match(btw21_geo$WKR_NR, merged$Gebietsnummer)]
btw21_geo$Covid_Shots <- merged$Covid_Shots[match(btw21_geo$WKR_NR, merged$Gebietsnummer)]

## polio
library(ggplot2)
polio_map <- 
  ggplot(data = btw21_geo, aes(fill = Polio_Share)) +
  geom_sf() +
  theme_void() +
  scale_fill_gradient(high = "lightgreen") +
  ggtitle("Polio Vaccination Share by Wahlkreis")
polio_map
ggsave("figures/polio_map.svg", height = 6, width = 6)

## covid
covid_map <- 
  ggplot(data = btw21_geo, aes(fill = Covid_Share)) +
  geom_sf() +
  theme_void() +
  scale_fill_gradient(high = "lightgreen") +
  ggtitle("Covid Vaccination Share by Wahlkreis")
covid_map
ggsave("figures/covid_map.svg", height = 6, width = 6)

## covid absolute
ggplot(data = btw21_geo, aes(fill = Covid_Shots)) +
  geom_sf() +
  theme_void() +
  scale_fill_gradient(high = "lightgreen") +
  ggtitle("Covid Vaccinations by Wahlkreis (absolute)")
ggsave("figures/covid_map_abs.svg", height = 6, width = 6)

## table of top ten vaccination WKs
merged %>% 
    mutate(Gebietsname = str_trunc(Gebietsname, 30)) %>%
    rename(Wahlberechtigte = Anzahl_btw21_Wahlberechtigte) %>%
    arrange(desc(Covid_Share)) %>% 
    select(Gebietsname, Covid_Shots, Covid_Share) %>% 
    head(10) %>% 
    knitr::kable()

## Maps of Party Support
btw21_geo$FW21_Share <- merged$Prozent_btw21_FREIE.WÄHLER[match(btw21_geo$WKR_NR, merged$Gebietsnummer)]
btw21_geo$FW17_Share <- merged$Prozent_btw17_FREIE.WÄHLER[match(btw21_geo$WKR_NR, merged$Gebietsnummer)]

## FW change
btw21_geo$FW_Change <- btw21_geo$FW21_Share - btw21_geo$FW17_Share
fw_delta_map <- 
  ggplot(data = btw21_geo, aes(fill = FW21_Share - FW17_Share)) +
  geom_sf() +
  theme_void() +
  scale_fill_gradient(high = "orange") +
  ggtitle("Change in Freie Wähler Vote Share 2017-2021")

library(patchwork)
polio_map + fw_delta_map
ggsave("figures/fw_polio_map.svg", height = 6, width = 12)


fw_delta_map_by <- 
    btw21_geo %>% 
    filter(WKR_NR %in% 212:257) %>%
    ggplot(aes(fill = FW_Change)) +
    geom_sf() +
    theme_void() +
    scale_fill_gradient(high = "orange") +
    ggtitle("Change in Freie Wähler Vote Share 2017-2021 (Bavaria)")

polio_map_by <- 
    btw21_geo %>% 
    filter(WKR_NR %in% 212:257) %>%
    ggplot(aes(fill = Polio_Share)) +
    geom_sf() +
    theme_void() +
    scale_fill_gradient(high = "lightgreen") +
    ggtitle("Polio Vaccination Share (Bavaria)")

polio_map_by + fw_delta_map_by
ggsave("figures/fw_polio_map_by.svg", height = 6, width = 12)

polio_fw_delta <- 
    btw21_geo %>% 
    filter(WKR_NR %in% 212:257) %>%
    ggplot(aes(x = Polio_Share, y = FW_Change)) +
    geom_point() +
    geom_smooth(method = "lm") +
    theme_minimal() +
    labs(title = "Change in Freie Wähler Vote Share in Bavaria 2017-2021",
         x = "Polio Vaccination Share",
         y = "Change in Vote Share")

polio_fw_17 <- 
    btw21_geo %>% 
    filter(WKR_NR %in% 212:257) %>%
    ggplot(aes(x = Polio_Share, y = FW17_Share)) +
    geom_point() +
    geom_smooth(method = "lm") +
    theme_minimal() +
    labs(title = "Freie Wähler Vote Share in Bavaria 2017",
         x = "Polio Vaccination Share",
         y = "Vote Share FW 2017")

polio_fw_17 + polio_fw_delta
ggsave("figures/fw_polio_cor.svg", height = 6, width = 12)


## SPD Change
btw21_geo$SPD21_Share <- merged$Prozent_btw21_SPD[match(btw21_geo$WKR_NR, merged$Gebietsnummer)]
btw21_geo$SPD17_Share <- merged$Prozent_btw17_SPD[match(btw21_geo$WKR_NR, merged$Gebietsnummer)]
btw21_geo$SPD_Change <- btw21_geo$SPD21_Share - btw21_geo$SPD17_Share

sp_delta_map <- 
  ggplot(data = btw21_geo, aes(fill = SPD_Change)) +
  geom_sf() +
  theme_void() +
  scale_fill_gradient(high = "red") +
  ggtitle("Change in SPD Vote Share 2017-2021")

polio_map + sp_delta_map
ggsave("figures/spd_polio_map.svg", height = 6, width = 12)

polio_spd_delta <- 
    btw21_geo %>% 
    ggplot(aes(x = Polio_Share, y = SPD_Change)) +
    geom_point() +
    geom_smooth(method = "lm") +
    theme_minimal() +
    labs(title = "Change in SPD Vote Share 2017-2021",
         x = "Polio Vaccination Share",
         y = "Change in Vote Share")

polio_spd_17 <- 
    btw21_geo %>% 
    ggplot(aes(x = Polio_Share, y = SPD17_Share)) +
    geom_point() +
    geom_smooth(method = "lm") +
    theme_minimal() +
    labs(title = "SPD Vote Share 2017",
         x = "Polio Vaccination Share",
         y = "Vote Share SPD 2017")

polio_spd_17 + polio_spd_delta
ggsave("figures/spd_polio_cor.svg", height = 6, width = 12)



# ## voters following AfD
# lm_polio <- lm(Covid_Share ~ Polio_Share + Prozent_btw17_AfD, data = merged)
# summary(lm_polio)

# merged %>% 
#     ggplot(aes(x = Polio_Share, y = Covid_Share, size = Population)) +
#     geom_point() +
#     geom_smooth(method = "lm")

## Voters voting due to vaccines (still meaningful)

### AfD
lm_afd <- lm(Prozent_btw21_AfD ~ Prozent_btw17_AfD + Polio_Share, data = merged)
summary(lm_afd) # low positive correlation

### Freie Wähler
lm_fw <- lm(Prozent_btw21_FREIE.WÄHLER ~ Prozent_btw17_FREIE.WÄHLER + Polio_Share, data = merged)
summary(lm_fw) # strong negative correlatio


## especially strong in BY
lm_fw_by <- 
    lm(
        Prozent_btw21_FREIE.WÄHLER ~ Prozent_btw17_FREIE.WÄHLER + Polio_Share, 
        data = merged %>% filter(Gebietsnummer %in% 212:257)
        )
summary(lm_fw_by) # very strong negative correlation, 7% increase across observed range, highly significant


### CDU
merged$Prozent_btw17_Union <- ifelse(is.na(merged$Prozent_btw17_CDU), merged$Prozent_btw17_CSU, merged$Prozent_btw17_CDU)
merged$Prozent_btw21_Union <- ifelse(is.na(merged$Prozent_btw21_CDU), merged$Prozent_btw21_CSU, merged$Prozent_btw21_CDU)
lm_Union <- lm(Prozent_btw21_Union ~ Prozent_btw17_Union + Polio_Share, data = merged)
summary(lm_Union) # pos correlation

### CSU
lm_CSU <- lm(Prozent_btw21_CSU ~ Prozent_btw17_CSU + Polio_Share, data = merged)
summary(lm_CSU) # low pos correlation

### SPD
lm_SPD <- lm(Prozent_btw21_SPD ~ Prozent_btw17_SPD + Polio_Share, data = merged)
summary(lm_SPD) # high pre-covid vac rates, high SPD voting (strong effect)

### Die Basis
lm_Basis <- lm(Prozent_btw21_dieBasis ~ Polio_Share, data = merged)
summary(lm_Basis) # clear negative correlation (difficult to control here)
plot(merged$Polio_Share, merged$Prozent_btw21_dieBasis)

## load data from https://github.com/cornelius-erfort/germany-53-21-districts (note this is only WEST Germany)
btws <- 
    haven::read_dta("data_new/elections/election-results-53-21.dta") %>% 
    pivot_wider(names_from = "party", values_from = "voteshare") %>% 
    group_by(countyname, AGS, state, year) %>%

    ## adding LK & SK together - ideally figure this out later with AGS
    summarise(
        across(SPD:`DIE-LINKE`, mean),
        across(validvotes:regvoters, sum)
    ) %>% 
    mutate(year = ifelse(year == 1921, 2021, year))



vax <- 
    fread("data/measles_LK_2008-2014.csv")
    
vax_wide <- 
    vax %>% 
    group_by(year) %>% 
    mutate(row = row_number()) %>%
    pivot_wider(
        values_from = "Impfquote",
        names_from = "year",
        names_prefix = "measles_vac_"
        ) %>% 
    select(-row) %>% 
    mutate(Landkreis = str_replace_all(Landkreis, "SK Oldenburg", "Oldenburg (Oldb)")) %>% 
    mutate(Landkreis = str_replace_all(Landkreis, "SK Offenbach", "Offenbach am Main")) %>%
    mutate(Landkreis = str_replace_all(Landkreis, "LK Bitburg-Prüm", "Eifelkreis Bitburg-Prüm")) %>%
    
    # mutate(Landkreis = str_replace_all(Landkreis, "SK Ansbach", "Ansbach (SK)")) %>%
    # mutate(Landkreis = str_replace_all(Landkreis, "SK Aschaffenburg", "Aschaffenburg (SK)")) %>%
    # mutate(Landkreis = str_replace_all(Landkreis, "SK Augsburg", "Augsburg (SK)")) %>%
    # mutate(Landkreis = str_replace_all(Landkreis, "SK Bamberg", "Bamberg (SK)")) %>%
    # mutate(Landkreis = str_replace_all(Landkreis, "SK Bayreuth", "Bayreuth (SK)")) %>%
    # mutate(Landkreis = str_replace_all(Landkreis, "SK Coburg", "Coburg (SK)")) %>%
    # mutate(Landkreis = str_replace_all(Landkreis, "SK Fürth", "Fürth (SK)")) %>%
    # mutate(Landkreis = str_replace_all(Landkreis, "SK Heilbronn", "Heilbronn (SK)")) %>%
    # mutate(Landkreis = str_replace_all(Landkreis, "SK Hof", "Hof (SK)")) %>%
    # mutate(Landkreis = str_replace_all(Landkreis, "SK Kaiserslautern", "Kaiserslautern (SK)")) %>%
    # mutate(Landkreis = str_replace_all(Landkreis, "SK Karlsruhe", "Karlsruhe (SK)")) %>%
    # mutate(Landkreis = str_replace_all(Landkreis, "SK Kassel", "Kassel (SK)")) %>%
    # mutate(Landkreis = str_replace_all(Landkreis, "SK Landshut", "Landshut (SK)")) %>%
    # mutate(Landkreis = str_replace_all(Landkreis, "SK München", "München (SK)")) %>%
    # mutate(Landkreis = str_replace_all(Landkreis, "SK Osnabrück", "Osnabrück (SK)")) %>%
    # mutate(Landkreis = str_replace_all(Landkreis, "SK Passau", "Passau (SK)")) %>%
    # mutate(Landkreis = str_replace_all(Landkreis, "SK Würzburg", "Würzburg (SK)")) %>%

    # mutate(Landkreis = str_replace_all(Landkreis, "LK Ansbach", "Ansbach (LK)")) %>%
    # mutate(Landkreis = str_replace_all(Landkreis, "LK Aschaffenburg", "Aschaffenburg (LK)")) %>%
    # mutate(Landkreis = str_replace_all(Landkreis, "LK Augsburg", "Augsburg (LK)")) %>%
    # mutate(Landkreis = str_replace_all(Landkreis, "LK Bamberg", "Bamberg (LK)")) %>%
    # mutate(Landkreis = str_replace_all(Landkreis, "LK Bayreuth", "Bayreuth (LK)")) %>%
    # mutate(Landkreis = str_replace_all(Landkreis, "LK Coburg", "Coburg (LK)")) %>%
    # mutate(Landkreis = str_replace_all(Landkreis, "LK Fürth", "Fürth (LK)")) %>%
    # mutate(Landkreis = str_replace_all(Landkreis, "LK Heilbronn", "Heilbronn (LK)")) %>%
    # mutate(Landkreis = str_replace_all(Landkreis, "LK Hof", "Hof (LK)")) %>%
    # mutate(Landkreis = str_replace_all(Landkreis, "LK Kaiserslautern", "Kaiserslautern (LK)")) %>%
    # mutate(Landkreis = str_replace_all(Landkreis, "LK Karlsruhe", "Karlsruhe (LK)")) %>%
    # mutate(Landkreis = str_replace_all(Landkreis, "LK Kassel", "Kassel (LK)")) %>%
    # mutate(Landkreis = str_replace_all(Landkreis, "LK Landshut", "Landshut (LK)")) %>%
    # mutate(Landkreis = str_replace_all(Landkreis, "LK München", "München (LK)")) %>%
    # mutate(Landkreis = str_replace_all(Landkreis, "LK Osnabrück", "Osnabrück (LK)")) %>%
    # mutate(Landkreis = str_replace_all(Landkreis, "LK Passau", "Passau (LK)")) %>%
    # mutate(Landkreis = str_replace_all(Landkreis, "LK Würzburg", "Würzburg (LK)")) %>%
    
    mutate(Landkreis = str_replace_all(Landkreis, "LK |SK ", "")) %>% 
    mutate(Landkreis = str_replace_all(Landkreis, "SR ", "Städteregion ")) %>%
    mutate(Landkreis = str_replace_all(Landkreis, "Sankt", "St.")) %>%
    mutate(Landkreis = str_replace_all(Landkreis, "Stadtverband", "Regionalverband")) %>%
    mutate(Landkreis = str_replace_all(Landkreis, "Saar-Pfalz-Kreis", "Saarpfalz-Kreis")) %>% 
    mutate(Landkreis = str_replace_all(Landkreis, "Mülheim a.d.Ruhr", "Mülheim an der Ruhr")) %>% 
    mutate(Landkreis = str_replace_all(Landkreis, "Landau i.d.Pfalz", "Landau in der Pfalz")) %>%
    mutate(Landkreis = str_replace_all(Landkreis, "Altenkirchen", "Altenkirchen (Westerwald)")) %>% 
    mutate(Landkreis = str_replace_all(Landkreis, "Frankenthal", "Frankenthal (Pfalz)")) %>%
    mutate(Landkreis = str_replace_all(Landkreis, "Ludwigshafen", "Ludwigshafen am Rhein")) %>%
    mutate(Landkreis = str_replace_all(Landkreis, "Neustadt a.d.Waldnaab", "Neustadt a.d. Waldnaab")) %>%
    mutate(Landkreis = str_replace_all(Landkreis, "Neustadt/Aisch-Bad Windsheim", "Neustadt a.d. Aisch-Bad Windsheim")) %>%
    mutate(Landkreis = str_replace_all(Landkreis, "Neustadt a.d.Weinstraße", "Neustadt an der Weinstraße")) %>% 
    mutate(Landkreis = str_replace_all(Landkreis, "Freiburg i.Breisgau", "Freiburg im Breisgau")) %>%
    mutate(Landkreis = str_replace_all(Landkreis, "Landsberg a.Lech", "Landsberg am Lech")) %>% 
    mutate(Landkreis = str_replace_all(Landkreis, "Mühldorf a.Inn", "Mühldorf a. Inn")) %>%
    mutate(Landkreis = str_replace_all(Landkreis, "Pfaffenhofen a.d.Ilm", "Pfaffenhofen a.d. Ilm")) %>% 
    mutate(Landkreis = str_replace_all(Landkreis, "i.d.OPf.", "i.d. OPf.")) %>% 
    mutate(Landkreis = str_replace_all(Landkreis, "i.Fichtel", "i. Fichtel")) %>%
    mutate(Landkreis = str_replace_all(Landkreis, "Kempten", "Kempten (Allgäu)")) %>%
    mutate(Landkreis = str_replace_all(Landkreis, "Dillingen a.d.Donau", "Dillingen a.d. Donau")) %>%
    mutate(Landkreis = str_replace_all(Landkreis, "Lindau", "Lindau (Bodensee)")) %>% 

    group_by(Landkreis) %>%
    summarise_all(mean)


## merge
merged <- 
    vax_wide %>%
    right_join(btws, by = c("Landkreis" = "countyname"))

merged %>% 
    write.csv("data_new/merged/measles_btw_53_21.csv")

## calculate correlation each election with AfD
cors <- 
    merged %>%
    group_by(year) %>% 
    summarise(
        cor_vac14_afd = cor(AfD, measles_vac_2014)
        )

cors %>% 
    filter(!is.na(cor_vac14_afd)) %>% 
    ggplot(aes(year, cor_vac14_afd)) +
    geom_point() +
    geom_line() +
    theme_minimal() +
    labs(
        x = "Year",
        y = "Correlation AfD and measles vaccination rate"
    )

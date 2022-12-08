source("Rcode-for-database/00-preamble.R")

# this script prepares all data inputs for the german language version
# of the shiny app

# prepare data for shiny app ----------------------------------------------

# load plant lists for each federal state ---------------------------------
d <- read_csv("Data-outputs/naturadb/naturadb_redlist_fed_states_only_cg_wide.csv")
head(d)

d %>% select(fed_state) %>% distinct
nrow(d)

# bring variables in right order ------------------------------------------
d <- d %>% rename(
  Bundesland = fed_state,
  `Wissenschaftlicher Name` = species_cleaned,
  `Deutscher Name` = naturadb_common_name,
  Gefährdung =  rl_cat)

# change species name, and right order
d <- d %>% 
  filter(!str_detect(`Wissenschaftlicher Name`, "[.]") ) %>% 
  select(-species_naturadb)

d <- d %>% select(Bundesland,
                  `Wissenschaftlicher Name`,
                  `Deutscher Name`,
                  Familie,
                  Gefährdung,
                  Licht,
                  Wasser,
                  Nährstoffe,
                  `PH-Wert`,
                  Boden,
                  Frostverträglich,
                  Höhe,
                  Blütenfarbe,
                  Bienen,
                  Schmetterlinge,
                  Vögel,
                  Säugetiere,
                  `Dachbegrünung geeignet`,
                  `Kübel/Balkon geeignet`)


# höhe, simplify ----------------------------------------------------------
d <- d %>% 
  separate(Höhe, sep = " - ", c("min", "max")) %>% 
  separate(max, sep = " ", c("max", "unit")) %>% 
  mutate(min = as.numeric(gsub(",", ".", gsub("\\.", "", min)))) %>% 
  mutate(max = as.numeric(gsub(",", ".", gsub("\\.", "", max)))) %>% 
  mutate(min = ifelse(unit == "m", min*100, min)) %>% 
  mutate(max = ifelse(unit == "m", max*100, max)) %>% 
  mutate(max = ifelse(max <= min, min, max)) %>% 
  mutate(unit = ifelse(unit == "m", "cm", "cm")) %>% 
  select(-min, -unit) %>% 
  rename(`Maximale Höhe`=max) %>% 
  mutate(h_binned = cut_number(`Maximale Höhe`, n = 4)) %>%
  mutate(h_binned = recode_factor(h_binned, 
                                  "[3,40]" = "0-40 cm",
                                  "(40,60]" = "40-60 cm",
                                  "(60,100]" = "60-100 cm",
                                  "(100,5e+03]" = "100+ cm"))



# a bit more cleaning, as there are warnings in the water column ---------
d <- d %>% mutate(Wasser = ifelse(str_detect(Wasser, "Warning:") == TRUE, NA, Wasser))
d <- d %>% rename(Dachbegrünung = `Dachbegrünung geeignet`, Balkon = `Kübel/Balkon geeignet`)

d <- d %>% group_by(Bundesland) %>% arrange(`Wissenschaftlicher Name`)
write_csv(d, "Cg-app-de/data-shiny/shiny_data.csv")



# prepare data for species not amenable to CG -----------------------------
d_non_cg <- read_csv("Data-outputs/naturadb/naturadb_redlist_fed_states_not_cg.csv")
d_non_cg <- d_non_cg %>% 
  select(Bundesland = fed_state, 
         `Wissenschaftlicher Name` = species_cleaned, 
         Gefährdung = rl_cat, 
         NaturaDB = naturadb_common_name)

write_csv(d_non_cg, "CG-app-de/data-shiny/shiny_data_noncg.csv")



# prepare master red list -------------------------------------------------
d_rl <- read_csv("Data-inputs/RLSynthesis_masterlist_OCT2022.csv")

d_rl <- d_rl %>% 
  select(Bundesland = fed_state, 
         `Wissenschaftlicher Name` = species_cleaned, 
         Gefährdung = rl_cat)

write_csv(d_rl, "CG-app-de/data-shiny/shiny_data_rl.csv")



# prepare seller list --------------------------------------------------
# load cg species
cg <- read_csv("Cg-app-de/data-shiny/shiny_data.csv")

# these are all cg species spread across germany
cg <- cg %>% select(`Wissenschaftlicher Name`, `Deutscher Name`) %>% distinct



# strickler ---------------------------------------------------------------
# load strickler
strickler_species <- read_csv("Data-outputs/strickler/strickler_species.csv")

# remove everything in paranthesis
strickler_species$name <- str_replace(strickler_species$name , " \\s*\\([^\\)]+\\)", "")
# only part before --
strickler_species$name  <- str_split(strickler_species$name , ' -- ', simplify = TRUE)[,1]
# only first two words
strickler_species <- strickler_species %>% mutate(name = 
                                                   ifelse(
                                                     str_detect(name, " x ") == TRUE,
                                                     word(name, 1,3), word(name, 1,2)
                                                   ))
# remove * and comma
strickler_species <- strickler_species %>% 
  mutate(name = str_remove(name, "\\*" )) %>%  # remove *
  mutate(name = str_remove(name, "\\," )) %>%  # remove ,
  mutate(name = trimws(name))  # remove white space

# left join with cg species
strickler <- left_join(cg, strickler_species, by = c("Wissenschaftlicher Name" = "name"))  %>% 
  mutate(values = ifelse(is.na(values) == T, NA, paste0("https://www.gaertnerei-strickler.de/", values))) %>% 
  rename(URL = values) %>% 
  mutate(Produzent = "Kräuter- und Wildpflanzen-Gärtnerei Strickler") %>% 
  select(`Wissenschaftlicher Name`, `Deutscher Name`, Produzent, URL)

write_excel_csv(strickler, "Cg-app-de/data-shiny/strickler_cg.csv")


# hof berg garten Lists ----------------------------
berg_garten_species <- read_csv("Data-outputs/hof-berg-garten/berg_garten_species.csv")

# remove duplicates
berg_garten_species <- distinct(berg_garten_species)

# only first two words
berg_garten_species <- berg_garten_species %>% mutate(name = 
                                                        ifelse(
                                                          str_detect(name, " x ") == TRUE,
                                                          word(name, 1,3), word(name, 1,2)
                                                        ))

# left join with cg species
berg_garten <- left_join(cg, berg_garten_species, by = c("Wissenschaftlicher Name" = "name"))  %>% 
  rename(URL = values) %>% 
  mutate(Produzent = "Hof Berg-Garten") %>% 
  select(`Wissenschaftlicher Name`, `Deutscher Name`, Produzent, URL)

write_excel_csv(berg_garten, "Cg-app-de/data-shiny/berggarten_cg.csv" )


# prepare spatz und frank list ----------------------------
spatz_frank_species <- read_csv("Data-outputs/spatzfrank/spatzfrank_species.csv")

# change wrong entry
spatz_frank_species$name[spatz_frank_species$name=="Atropa belladonna (Tollkirsche)"]<- "Tollkirsche, (Atropia belladonna)"

# only keep scientific species names --> those are in brackets
spatz_frank_species$name <- stringr::str_extract(string = spatz_frank_species$name,
                                                        pattern = "(?<=\\().*(?=\\))")
# only first two words
spatz_frank_species <- spatz_frank_species %>% mutate(name = 
                                                        ifelse(
                                                          str_detect(name, " x ") == TRUE,
                                                          word(name, 1,3), word(name, 1,2)
                                                        ))

# left join with cg species
spatz_frank <- left_join(cg, spatz_frank_species, by = c("Wissenschaftlicher Name" = "name"))  %>%
  rename(URL = values) %>% 
  mutate(Produzent = "Staudengärtnerei StaudenSpatz") %>% 
  select(`Wissenschaftlicher Name`, `Deutscher Name`, Produzent, URL)

write_excel_csv(spatz_frank, "Cg-app-de/data-shiny/spatzfrank_cg.csv" )


# prepare blauetikett list ----------------------------
# load list
blauetikett_species <- read_csv("Data-outputs/blauetikett/blauetikett_species.csv")

# change wrong entries
blauetikett_species$name[blauetikett_species$name=="Knoblauchrauke, Alliaria petiolata  Samen"] <- "Alliaria petiolata, Knoblauchrauke  Samen"
blauetikett_species2$name[blauetikett_species2$name=="Chamaemelum nobile  Samen"] <- " Chamaemelum nobile  Samen"

blauetikett_species2 <- blauetikett_species[c(1:34),]
blauetikett_species3 <- blauetikett_species[-c(1:34),]

# only keep characters after comma
blauetikett_species2$name <-  gsub(".*\\,", "", blauetikett_species2$name)

# only keep characters before comma
blauetikett_species3$name <-  gsub("\\,.*", "", blauetikett_species3$name)

# merge those dfs
blauetikett_species4 <- rbind(blauetikett_species2, blauetikett_species3)
blauetikett_species4$name <- iconv(blauetikett_species4$name, "UTF-8", "UTF-8",sub='')
blauetikett_species4$name <- trimws(blauetikett_species4$name)

# only keep first two words
blauetikett_species4 <- blauetikett_species4 %>% 
  mutate(name = ifelse(is.na(word(name, 1, 2)), name, word(name, 1, 2)))

# left join with cg species
blauetikett <- left_join(cg, blauetikett_species4, by = c("Wissenschaftlicher Name" = "name"))  %>%
  rename(URL = values) %>% 
  mutate(Produzent = "Blauetikett Bornträger") %>% 
  select(`Wissenschaftlicher Name`, `Deutscher Name`, Produzent, URL)

write_excel_csv(blauetikett, "Cg-app-de/data-shiny/blauetikett_cg.csv" )



# prepare rieger-hofmann masterlist ----------------------------
# load list
rieger_hof_species <-  read_csv("Data-outputs/rieger-hof/rieger_hof_species.csv")

# only keep characters in front of /
rieger_hof_species$name <-  gsub("/.*", "", rieger_hof_species$name)

# only keep first two words
rieger_hof_species <- rieger_hof_species %>% mutate(name = 
                                                    ifelse(
                                                      str_detect(name, " x ") == TRUE,
                                                      word(name, 1,3), word(name, 1,2)
                                                    ))
# left join with cg species
rieger_hof <- left_join(cg, rieger_hof_species, by = c("Wissenschaftlicher Name" = "name"))  %>%
  rename(URL = values) %>% 
  mutate(Produzent = "Rieger-Hofmann") %>% 
  select(`Wissenschaftlicher Name`, `Deutscher Name`, Produzent, URL)

write_excel_csv(rieger_hof, "Cg-app-de/data-shiny/riegerhof_cg.csv" )



# prepare master list of sellers ------------------------------------------
riegerhof <- read_csv("Cg-app-de/data-shiny/riegerhof_cg.csv" )
blauetikett <- read_csv("Cg-app-de/data-shiny/blauetikett_cg.csv" )
seller <- spatzfrank <- read_csv("Cg-app-de/data-shiny/spatzfrank_cg.csv" )
strickler <- read_csv("Cg-app-de/data-shiny/strickler_cg.csv" )
berggarten <- read_csv("Cg-app-de/data-shiny/berggarten_cg.csv" )
giessmayer <-  read_csv("Data-outputs/giessmayer/giessmayer_species.csv") %>% 
  right_join(cg, by = c("species" = "Wissenschaftlicher Name")) %>% 
  mutate(Produzent = "Staudengärtnerei Gaißmayer") %>% 
    rename(URL = url) %>% 
    select(`Wissenschaftlicher Name` = species, `Deutscher Name`, Produzent, URL)

seller <- bind_rows(list(riegerhof, 
                         blauetikett, 
                         spatzfrank, 
                         strickler, 
                         berggarten,
                         giessmayer))

# remove all nas in URLS
seller <- seller %>% filter(!is.na(URL))

# join back to cg
seller <- left_join(cg, seller %>% select(-`Deutscher Name` ))

write_excel_csv(seller, "Cg-app-de/data-shiny/seller_cg.csv" )


# prepare data frame of plants amenable but not produced ------------------
seller <- fread("Cg-app-de/data-shiny/seller_cg.csv")

not_produced <- seller %>% mutate(avail = ifelse(is.na(Produzent), "not available", "available")) %>% 
  select(species = `Wissenschaftlicher Name`, avail) %>% 
  distinct %>% 
  filter(avail == "not available")


write_excel_csv(not_produced, "Cg-app-de/data-shiny/not-produced.csv" )

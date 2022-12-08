source("Rcode-for-database/00-preamble.R")

# this script prepares all data inputs for the german language version
# of the shiny app

# prepare data for shiny app ----------------------------------------------

# load plant lists for each federal state ---------------------------------
d <- read_csv("Data-outputs/naturadb/naturadb_redlist_fed_states_only_cg_wide.csv")
head(d)

d %>% select(fed_state) %>% distinct
nrow(d)

# rename fed states in english
d$fed_state[d$fed_state == "Baden-Württemberg"] <- "Baden-Württemberg"
d$fed_state[d$fed_state == "Bayern"] <- "Bavaria"
d$fed_state[d$fed_state == "Berlin"] <- "Berlin"
d$fed_state[d$fed_state == "Brandenburg"] <- "Brandenburg"
d$fed_state[d$fed_state == "Bremen/Niedersachsen"] <- "Bremen/Lower Saxony"
d$fed_state[d$fed_state == "Hamburg"] <- "Hamburg"
d$fed_state[d$fed_state == "Hessen"] <- "Hesse"
d$fed_state[d$fed_state == "Mecklenburg-Vorpommern"] <- "Mecklenburg-Western Pomerania"
d$fed_state[d$fed_state == "Nordrhein-Westfalen"] <- "North Rhine-Westphalia"
d$fed_state[d$fed_state == "Rheinland-Pfalz"] <- "Rhineland-Palatinate"
d$fed_state[d$fed_state == "Saarland"] <- "Saarland"
d$fed_state[d$fed_state == "Sachsen"] <- "Saxony"
d$fed_state[d$fed_state == "Sachsen-Anhalt"] <- "Saxony-Anhalt"
d$fed_state[d$fed_state == "Schleswig-Holstein"] <- "Schleswig-Holstein"
d$fed_state[d$fed_state == "Thüringen"] <- "Thuringia"



# bring variables in right order ------------------------------------------
d <- d %>% rename(
  State = fed_state,
  Species = species_cleaned,
  Endangerment =  rl_cat)

# change species name, and right order
d <- d %>% 
  filter(!str_detect(Species, "[.]") ) %>% 
  select(-species_naturadb)

d <- d %>% select(State,
                  Species,
                  Endangerment,
                  Light = Licht,
                  Water = Wasser,
                  Nutrients = Nährstoffe,
                  `PH-value` = `PH-Wert`,
                  Soil = Boden,
                  `Freezing tolerance` = Frostverträglich,
                  Height = Höhe,
                  `Flower color` = Blütenfarbe,
                  Bees = Bienen,
                  Butterflies = Schmetterlinge,
                  Birds = Vögel,
                  Mammals = Säugetiere,
                  Roof = `Dachbegrünung geeignet`,
                  Balcony = `Kübel/Balkon geeignet`)


# höhe, simplify ----------------------------------------------------------
d <- d %>% 
  separate(Height, sep = " - ", c("min", "max")) %>% 
  separate(max, sep = " ", c("max", "unit")) %>% 
  mutate(min = as.numeric(gsub(",", ".", gsub("\\.", "", min)))) %>% 
  mutate(max = as.numeric(gsub(",", ".", gsub("\\.", "", max)))) %>% 
  mutate(min = ifelse(unit == "m", min*100, min)) %>% 
  mutate(max = ifelse(unit == "m", max*100, max)) %>% 
  mutate(max = ifelse(max <= min, min, max)) %>% 
  mutate(unit = ifelse(unit == "m", "cm", "cm")) %>% 
  select(-min, -unit) %>% 
  rename(`Maximale height`=max) %>% 
  mutate(h_binned = cut_number(`Maximale height`, n = 4)) %>%
  mutate(h_binned = recode_factor(h_binned, 
                                  "[3,40]" = "0-40 cm",
                                  "(40,60]" = "40-60 cm",
                                  "(60,100]" = "60-100 cm",
                                  "(100,5e+03]" = "100+ cm"))



# a bit more cleaning, as there are warnings in the water column ---------
d <- d %>% mutate(Water = ifelse(str_detect(Water, "Warning:") == TRUE, NA, Water))
d <- d %>% group_by(State) %>% arrange(Species)




# translate levels to english ---------------------------------------------


# light
d %>% ungroup %>% select(Light) %>% distinct
d$Light[d$Light == "Sonne bis Schatten"] <- "Sun to Shade"
d$Light[d$Light == "Sonne bis Halbschatten"] <- "Sun to Half-shade"
d$Light[d$Light == "Sonne"] <- "Sun"
d$Light[d$Light == "Halbschatten bis Schatten"] <- "Half-shade to Shade"
d$Light[d$Light == "Halbschatten"] <- "Half-shade"
d$Light[d$Light == "Schatten"] <- "Shade"

# water
d %>% ungroup %>% select(Water) %>% distinct

d$Water[d$Water == "frisch"] <- "moist"
d$Water[d$Water == "frisch bis trocken"] <- "moist to dry"
d$Water[d$Water == "feucht bis frisch"] <- "wet to moist"
d$Water[d$Water == "trocken"] <- "dry"
d$Water[d$Water == "feucht"] <- "wet"
d$Water[d$Water == "Wasserpflanze"] <- "aquatic"
d$Water[d$Water == "feucht bis trocken"] <- "wet to dry"

# nutrients
d %>% ungroup %>% select(Nutrients) %>% distinct

d$Nutrients[d$Nutrients == "nährstoffreicher Boden"] <- "N-rich"
d$Nutrients[d$Nutrients == "nährstoffarmer bis normaler Boden"] <- "N-poor to intermediate"
d$Nutrients[d$Nutrients == "normaler Boden"] <- "intermediate"
d$Nutrients[d$Nutrients == "normaler bis nährstoffreicher Boden"] <- "intermediate to N-rich"
d$Nutrients[d$Nutrients == "nährstoffarmer Boden"] <- "N-poor"
d$Nutrients[d$Nutrients == "nährstoffarmer bis nährstoffreicher Boden"] <- "N-poor to N-rich"


# ph
d %>% ungroup %>% select(`PH-value`) %>% distinct

d$`PH-value`[d$`PH-value` == "basisch / kalk"] <- "alkaline"
d$`PH-value`[d$`PH-value` == "sauer"] <- "acidic"
d$`PH-value`[d$`PH-value` == "sauer bis kalkhaltig"] <- "acidic to alkaline"


# soil
d %>% ungroup %>% select(Soil) %>% distinct

d$Soil[d$Soil == "normal bis humus"] <- "normal to humus"
d$Soil[d$Soil == "durchlässig bis normal"] <- "permeable to normal"
d$Soil[d$Soil == "normal"] <- "normal"
d$Soil[d$Soil == "lehmig"] <- "loamy"
d$Soil[d$Soil == "durchlässig"] <- "permeable"
d$Soil[d$Soil == "normal bis lehmig"] <- "normal to loamy"
d$Soil[d$Soil == "durchlässig bis humus"] <- "permeable to humus"
d$Soil[d$Soil == "durchlässig bis lehmig"] <- "permeable to loamy"
d$Soil[d$Soil == "humus"] <- "humus"


# freezing tolerance
d %>% ungroup %>% select(`Freezing tolerance`) %>% distinct

d$`Freezing tolerance`[d$`Freezing tolerance` == "bis -23 °C (bis Klimazone 6)"] <- "to -23 °C (to Climatic zone 6)"
d$`Freezing tolerance`[d$`Freezing tolerance` == "bis -28 °C (bis Klimazone 5)"] <- "to -28 °C (to Climatic zone 5)"
d$`Freezing tolerance`[d$`Freezing tolerance` == "bis -12 °C (bis Klimazone 8)"] <- "to -12 °C (to Climatic zone 8)"
d$`Freezing tolerance`[d$`Freezing tolerance` == "bis -17 °C (bis Klimazone 7)"] <- "to -17 °C (to Climatic zone 7)"
d$`Freezing tolerance`[d$`Freezing tolerance` == "bis -6 °C (bis Klimazone 9)"] <-  "to -6 °C (to Climatic zone 9)"
d$`Freezing tolerance`[d$`Freezing tolerance` == "bis -40 °C (bis Klimazone 3)"] <- "to -40 °C (to Climatic zone 3)"
d$`Freezing tolerance`[d$`Freezing tolerance` == "bis -34 °C (bis Klimazone 4)"] <- "to -34 °C (to Climatic zone 4)"
d$`Freezing tolerance`[d$`Freezing tolerance` == "bis -34 °C (bis Klimazone 4)"] <- "to -34 °C (to Climatic zone 4)"
d$`Freezing tolerance`[d$`Freezing tolerance` == "unter -45,5 °C (bis Klimazone 1)"] <- "under -45,5 °C (to Climatic zone 1)"

# flower color
d %>% ungroup %>% select(`Flower color`) %>% distinct

d$`Flower color`[d$`Flower color` == "gelb"] <- "yellow"
d$`Flower color`[d$`Flower color` == "grün"] <- "green"
d$`Flower color`[d$`Flower color` == "weiß"] <- "white"
d$`Flower color`[d$`Flower color` == "blau"] <- "blue"
d$`Flower color`[d$`Flower color` == "violett"] <- "violet"
d$`Flower color`[d$`Flower color` == "rot"] <- "red"
d$`Flower color`[d$`Flower color` == "rosa"] <- "rose"
d$`Flower color`[d$`Flower color` == "pink"] <- "pink"
d$`Flower color`[d$`Flower color` == "braun"] <- "brown"
d$`Flower color`[d$`Flower color` == "silbergrau"] <- "silvergrey"
d$`Flower color`[d$`Flower color` == "lila"] <- "purple"
d$`Flower color`[d$`Flower color` == "schwarz"] <- "black"
d$`Flower color`[d$`Flower color` == "orange"] <- "orange"


# biodiv
d %>% ungroup %>% select(Bees) %>% distinct
d$Bees <- gsub("Bienenweide", "Bees", d$Bees)
d$Bees <- gsub("Arten", "spp.", d$Bees)

d %>% ungroup %>% select(Butterflies) %>% distinct
d$Butterflies <- gsub("Schmetterlingsweide", "Butterflies", d$Butterflies)
d$Butterflies <- gsub("Arten", "spp.", d$Butterflies)


d %>% ungroup %>% select(Birds) %>% distinct
d$Birds <- gsub("Vogelnährgehölz", "Birds", d$Birds)
d$Birds <- gsub("Arten", "spp.", d$Birds)


d %>% ungroup %>% select(Mammals) %>% distinct
d$Mammals <- gsub("fressende Arten", "spp. of mammals", d$Mammals)


# roof 
d %>% ungroup %>% select(Roof) %>% distinct
d$Roof[d$Roof == "ja"] <- "yes"

# roof 
d %>% ungroup %>% select(Balcony) %>% distinct
d$Balcony[d$Balcony == "ja"] <- "yes"
d$Balcony[d$Balcony == "ja, Kübelgröße klein"] <- "yes, small pots"
d$Balcony[d$Balcony == "ja, Kübelgröße mittel"] <- "yes, medium pots"
d$Balcony[d$Balcony == "ja, Kübelgröße, groß"] <- "yes, large pots"

write_csv(d, "Cg-app-en/data-shiny/shiny_data.csv")



# threat summary ----------------------------------------------------------

red_lists_summary <- read_excel("CG-app-en/data-shiny/red_lists_summary.xlsx")
head(red_lists_summary)
View(red_lists_summary)
# rename fed states in english
red_lists_summary$Bundesland2[red_lists_summary$Bundesland == "Baden-Württemberg"] <- "Baden-Württemberg"
red_lists_summary$Bundesland2[red_lists_summary$Bundesland == "Bayern"] <- "Bavaria"
red_lists_summary$Bundesland2[red_lists_summary$Bundesland == "Berlin"] <- "Berlin"
red_lists_summary$Bundesland2[red_lists_summary$Bundesland == "Brandenburg"] <- "Brandenburg"
red_lists_summary$Bundesland2[red_lists_summary$Bundesland == "Bremen/Niedersachsen"] <- "Bremen/Lower Saxony"
red_lists_summary$Bundesland2[red_lists_summary$Bundesland == "Hamburg"] <- "Hamburg"
red_lists_summary$Bundesland2[red_lists_summary$Bundesland == "Hessen"] <- "Hesse"
red_lists_summary$Bundesland2[red_lists_summary$Bundesland == "Mecklenburg-Vorpommern"] <- "Mecklenburg-Western Pomerania"
red_lists_summary$Bundesland2[red_lists_summary$Bundesland == "Nordrhein-Westfalen"] <- "North Rhine-Westphalia"
red_lists_summary$Bundesland2[red_lists_summary$Bundesland == "Rheinland-Pfalz"] <- "Rhineland-Palatinate"
red_lists_summary$Bundesland2[red_lists_summary$Bundesland == "Saarland"] <- "Saarland"
red_lists_summary$Bundesland2[red_lists_summary$Bundesland == "Sachsen"] <- "Saxony"
red_lists_summary$Bundesland2[red_lists_summary$Bundesland == "Sachsen-Anhalt"] <- "Saxony-Anhalt"
red_lists_summary$Bundesland2[red_lists_summary$Bundesland == "Schleswig-Holstein"] <- "Schleswig-Holstein"
red_lists_summary$Bundesland2[red_lists_summary$Bundesland == "Thüringen"] <- "Thuringia"

write_csv(red_lists_summary, "Cg-app-en/data-shiny/red_lists_summary.csv")


# prepare data for species not amenable to CG -----------------------------
d_non_cg <- read_csv("Data-outputs/naturadb/naturadb_redlist_fed_states_not_cg.csv")

d_non_cg <- d_non_cg %>% 
  select(State = fed_state, 
         Species = species_cleaned, 
         Endangerment = rl_cat, 
         NaturaDB = naturadb_common_name)


d_non_cg$State[d_non_cg$State == "Baden-Württemberg"] <- "Baden-Württemberg"
d_non_cg$State[d_non_cg$State == "Bayern"] <- "Bavaria"
d_non_cg$State[d_non_cg$State == "Berlin"] <- "Berlin"
d_non_cg$State[d_non_cg$State == "Brandenburg"] <- "Brandenburg"
d_non_cg$State[d_non_cg$State == "Bremen/Niedersachsen"] <- "Bremen/Lower Saxony"
d_non_cg$State[d_non_cg$State == "Hamburg"] <- "Hamburg"
d_non_cg$State[d_non_cg$State == "Hessen"] <- "Hesse"
d_non_cg$State[d_non_cg$State == "Mecklenburg-Vorpommern"] <- "Mecklenburg-Western Pomerania"
d_non_cg$State[d_non_cg$State == "Nordrhein-Westfalen"] <- "North Rhine-Westphalia"
d_non_cg$State[d_non_cg$State == "Rheinland-Pfalz"] <- "Rhineland-Palatinate"
d_non_cg$State[d_non_cg$State == "Saarland"] <- "Saarland"
d_non_cg$State[d_non_cg$State == "Sachsen"] <- "Saxony"
d_non_cg$State[d_non_cg$State == "Sachsen-Anhalt"] <- "Saxony-Anhalt"
d_non_cg$State[d_non_cg$State == "Schleswig-Holstein"] <- "Schleswig-Holstein"
d_non_cg$State[d_non_cg$State == "Thüringen"] <- "Thuringia"

d_non_cg %>% select(State) %>% distinct

write_csv(d_non_cg, "CG-app-en/data-shiny/shiny_data_noncg.csv")



# prepare master red list -------------------------------------------------
d_rl <- read_csv("Data-inputs/RLSynthesis_masterlist_OCT2022.csv")

d_rl <- d_rl %>% 
  select(State = fed_state, 
         Species = species_cleaned, 
         Endangerment = rl_cat)

write_csv(d_rl, "Cg-app-en/data-shiny/shiny_data_rl.csv")



# prepare seller list --------------------------------------------------
# load cg species
cg <- read_csv("Cg-app-en/data-shiny/shiny_data.csv")

# these are all cg species spread across germany
cg <- cg %>% select(Species) %>% distinct



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
strickler <- left_join(cg, strickler_species, by = c("Species" = "name"))  %>% 
  mutate(values = ifelse(is.na(values) == T, NA, paste0("https://www.gaertnerei-strickler.de/", values))) %>% 
  rename(URL = values) %>% 
  mutate(Produzent = "Kräuter- und Wildpflanzen-Gärtnerei Strickler") %>% 
  select(Species, Producer = Produzent, URL)

write_excel_csv(strickler, "Cg-app-en/data-shiny/strickler_cg.csv")


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
berg_garten <- left_join(cg, berg_garten_species, by = c("Species" = "name"))  %>% 
  rename(URL = values) %>% 
  mutate(Produzent = "Hof Berg-Garten") %>% 
  select(Species, Producer = Produzent, URL)

write_excel_csv(berg_garten, "Cg-app-en/data-shiny/berggarten_cg.csv" )


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
spatz_frank <- left_join(cg, spatz_frank_species, by = c("Species" = "name"))  %>%
  rename(URL = values) %>% 
  mutate(Produzent = "Staudengärtnerei StaudenSpatz") %>% 
  select(Species, Producer = Produzent, URL)

write_excel_csv(spatz_frank, "Cg-app-en/data-shiny/spatzfrank_cg.csv" )


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
blauetikett <- left_join(cg, blauetikett_species4, by = c("Species" = "name"))  %>%
  rename(URL = values) %>% 
  mutate(Produzent = "Blauetikett Bornträger") %>% 
  select(Species, Producer = Produzent, URL)

write_excel_csv(blauetikett, "Cg-app-en/data-shiny/blauetikett_cg.csv" )



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
rieger_hof <- left_join(cg, rieger_hof_species, by = c("Species" = "name"))  %>%
  rename(URL = values) %>% 
  mutate(Produzent = "Rieger-Hofmann") %>% 
  select(Species, Producer = Produzent, URL)

write_excel_csv(rieger_hof, "Cg-app-en/data-shiny/riegerhof_cg.csv" )



# prepare master list of sellers ------------------------------------------
riegerhof <- read_csv("Cg-app-en/data-shiny/riegerhof_cg.csv" )
blauetikett <- read_csv("Cg-app-en/data-shiny/blauetikett_cg.csv" )
seller <- spatzfrank <- read_csv("Cg-app-en/data-shiny/spatzfrank_cg.csv" )
strickler <- read_csv("Cg-app-en/data-shiny/strickler_cg.csv" )
berggarten <- read_csv("Cg-app-en/data-shiny/berggarten_cg.csv" )

giessmayer <-  read_csv("Data-outputs/giessmayer/giessmayer_species.csv") %>% 
  right_join(cg, by = c("species" = "Species")) %>% 
  mutate(Producer = "Staudengärtnerei Gaißmayer") %>% 
  rename(URL = url) %>% 
  select(Species = species, Producer, URL)

seller <- bind_rows(list(riegerhof, 
                         blauetikett, 
                         spatzfrank, 
                         strickler, 
                         berggarten,
                         giessmayer))

# remove all nas in URLS
seller <- seller %>% filter(!is.na(URL))

# join back to cg
seller <- left_join(cg, seller)

write_excel_csv(seller, "Cg-app-en/data-shiny/seller_cg.csv" )


# prepare data frame of plants amenable but not produced ------------------
seller <- fread("Cg-app-en/data-shiny/seller_cg.csv")

not_produced <- seller %>% mutate(avail = ifelse(is.na(Producer), "not available", "available")) %>% 
  select(Species, avail) %>% 
  distinct %>% 
  filter(avail == "not available")


write_excel_csv(not_produced, "Cg-app-en/data-shiny/not-produced.csv" )

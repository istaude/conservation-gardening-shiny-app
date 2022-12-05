source("Rcode-for-database/00-preamble.R")

# for one example plant
naturadb_example <- "https://www.naturadb.de/pflanzen/achillea-macrophylla/"
naturadb_example <- read_html(naturadb_example)
naturadb_example
str(naturadb_example)

# scraping planting info
name <- naturadb_example %>% 
  rvest::html_nodes('body') %>% 
  xml2::xml_find_all("//h1") %>% 
  rvest::html_text() %>% 
  str_replace_all(., "[\t\n]" , "")


category <- naturadb_example %>% 
  rvest::html_nodes('body') %>% 
  xml2::xml_find_all("//td[1]") %>% 
  rvest::html_text() %>% 
  str_replace_all(., "[\t\n]" , "")

values <- naturadb_example %>% 
  rvest::html_nodes('body') %>% 
  xml2::xml_find_all("//td[2]") %>% 
  rvest::html_text() %>% 
  str_replace_all(., "[\t\n]" , "")

data.frame(cat = category, val = values)



# create urls
d <- read_csv("Data-inputs/RLSynthesis_masterlist_OCT2022.csv")
d_name <- unique(d$species_naturadb)
length(d_name)

input <- paste0("https://www.naturadb.de/pflanzen/", d_name, "/") 

species_list <-  NULL
for(i in 1:length(input)){
  
page <- try({read_html(input[i])})
species <- d_name[i]
if(class(page)[1] == "try-error") {next} else{
  
  name <- page %>%
    rvest::html_nodes('body') %>% 
    xml2::xml_find_all("//h1") %>% 
    rvest::html_text() %>% 
    str_replace_all(., "[\t\n]" , "")
  
  category <- page %>% 
    rvest::html_nodes('body') %>% 
    xml2::xml_find_all("//td[1]") %>% 
    rvest::html_text() %>% 
    str_replace_all(., "[\t\n]" , "")
  
  values <- page %>% 
    rvest::html_nodes('body') %>% 
    xml2::xml_find_all("//td[2]") %>% 
    rvest::html_text() %>% 
    str_replace_all(., "[\t\n]" , "")

dt <- data.frame(cat = category, val = values)
dt <- dt %>% filter(grepl(':', cat))
dt <- dt %>% mutate(species = species, common_name = name)

# now add to list
species_list[[i]] <-  dt
print(i)
}
}

length(species_list)

dx <- bind_rows(species_list)


# for how many species retrieved data from naturadb?
length(d_name)
dx %>% select(species) %>% distinct %>% nrow
# that's pretty good!


# merge back the red list category
dx <- left_join(d %>% select(-common_name), dx, by = c("species_naturadb" = "species"))

dx <- dx %>% 
  select(fed_state, species_cleaned, species_naturadb, rl_cat, common_name, cat, val) %>% 
  mutate(val = trimws(val)) %>% 
  mutate(common_name = ifelse(is.na(common_name), "not in naturadb", common_name)) %>% 
  mutate(val = ifelse(is.na(val), "not in naturadb", val)) %>% 
  mutate(cat = ifelse(is.na(cat), "not in naturadb", cat)) %>% 
  rename(naturadb_common_name = common_name, naturadb_cat = cat, naturadb_val = val)


# save, this file shows also species that are not amenable to cg
write.csv(dx, "Data-outputs/naturadb/naturadb_redlist_fed_states_all_species.csv", row.names = F)



# filter those species with no match in naturadb
dx_nn <- dx %>% filter(naturadb_cat == "not in naturadb")
dx_rl <- dx %>% filter(naturadb_cat != "not in naturadb")

# save, species apparently not amenable to cg
write.csv(dx_nn, "Data-outputs/naturadb/naturadb_redlist_fed_states_not_cg.csv", row.names = F)


# save, only natura db species
write.csv(dx_rl, "Data-outputs/naturadb/naturadb_redlist_fed_states_only_cg.csv", row.names = F)



# which rl cats and how many
(dx_howmany <- dx_rl %>% 
  group_by(fed_state) %>% 
  select(species_naturadb, rl_cat) %>% 
  distinct %>% 
  count(rl_cat))

# bar chart to visualize
ggplot(dx_howmany, aes(x="", y=n, fill=rl_cat)) +
  facet_wrap(~fed_state) +
  geom_bar(stat="identity", width=1, color="white")



# wide format and only a few columns for shiny app
dx_rl <- dx_rl %>% 
  mutate(naturadb_cat = gsub(':', '', naturadb_cat)) %>%
  filter(
    naturadb_cat == "Familie"|
      naturadb_cat == "Gattung"|
      naturadb_cat == "Licht" | 
      naturadb_cat == "Boden"|
      naturadb_cat == "Wasser"|
      naturadb_cat == "Nährstoffe"|
      naturadb_cat == "Höhe"|
      naturadb_cat == "PH-Wert"|
      naturadb_cat == "Frostverträglich"|
      naturadb_cat == "Kübel/Balkon geeignet"|
      naturadb_cat == "Dachbegrünung geeignet"|
      naturadb_cat == "Bienen"|
      naturadb_cat == "Schmetterlinge"|
      naturadb_cat == "Säugetiere"|
      naturadb_cat == "Vögel"|
      naturadb_cat == "Blütenfarbe") %>% 
  distinct %>% 
spread("naturadb_cat", "naturadb_val", fill = NA)

write.csv(dx_rl, "Data-outputs/naturadb/naturadb_redlist_fed_states_only_cg_wide.csv", row.names = F)








source("Rcode-for-database/00-preamble.R")

# updated website, similar to original textmining approach however
# plants produced
spatzfrank_list <- list()

for (i in 1:64){
  spatzfrank <- paste0("https://www.staudenspatz.de/pflanzensuche-459/?page=", i)
  spatzfrank <- read_html(spatzfrank)
  
  name <- spatzfrank %>% 
    rvest::html_nodes('.title') %>% 
    rvest::html_text() %>% 
    str_replace_all(., "[\t\n]" , "")
  
  values <- spatzfrank %>% 
    rvest::html_nodes('.product-url') %>% 
    rvest::html_attr('href')
  values <- data.frame(values)
  
  data <-  values %>% mutate(name = name)
  spatzfrank_list[[i]] <- data
  print(i)
}


# combine these list elements into one dataframe
spatzfrank_species <- bind_rows(spatzfrank_list)


# harmonize with cg species already here
# load cg species
cg <- read_csv("Cg-app-en/data-shiny/shiny_data.csv")

# these are all cg species spread across germany
cg <- cg %>% select(Species) %>% distinct

# only keep scientific species names --> those are in brackets
spatzfrank_species$name2 <- stringr::str_extract(string = spatzfrank_species$name,
                                                 pattern = "(?<=\\().*(?=\\))")

# keep everything before semicolon
spatzfrank_species$name3 <- str_split_fixed(string = spatzfrank_species$name2, ";", 2)[, 1]

# only keep if it is two words (three indicate often a cultivar)
# filter rows that match the pattern: 
# two words, first word starts with a capital letter, second word starts with a lowercase letter
spatzfrank_species <- spatzfrank_species %>% 
  filter(grepl("^[A-Z][a-z]+\\s[a-z][a-z]+$", name3))

# left join with cg species
spatz_frank <- left_join(cg, spatzfrank_species, by = c("Species" = "name3"))  %>%
  rename(URL = values) %>% 
  mutate(Produzent = "Staudengärtnerei StaudenSpatz") %>% 
  select(Species, Producer = Produzent, URL)

# save
write_excel_csv(spatz_frank, "Data-outputs/spatzfrank/spatzfrank_species_AUG2024.csv")

# save for english version
write_excel_csv(spatz_frank, "Cg-app-en/data-shiny/spatzfrank_cg.csv")


# save for german shiny app
spatz_frank <- spatz_frank %>% 
  select(`Wissenschaftlicher Name` = Species, Produzent = Producer, URL)

write_excel_csv(spatz_frank, "Cg-app-de/data-shiny/spatzfrank_cg.csv")


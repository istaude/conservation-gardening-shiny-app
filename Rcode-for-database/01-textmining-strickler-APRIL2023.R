source("Rcode-for-database/00-preamble.R")

# load cg species
cg <- read_csv("Cg-app-de/data-shiny/shiny_data.csv")
cg <- cg %>% select(`Wissenschaftlicher Name`, `Deutscher Name`) %>% distinct
species1 <- unique(cg$`Wissenschaftlicher Name`)
species2 <- gsub(" ", "+", species)

# plants produced
strickler_list <- list()

for (i in 1:length(species)){
  
  # search for species
  strickler <- paste0("https://www.gaertnerei-strickler.de/suche?q=", species2[i])
  download.file(strickler, destfile = "strikler.html", quiet=TRUE)
  strickler <- read_html( "strikler.html")
  
  name <- strickler %>% 
    html_elements("span.h4.mt-10.mb-0.text-center") %>% 
    rvest::html_text()
  
  url <- strickler %>% 
    html_elements("a#link-to-article.addToBasketProfile.btn.btn-custom.btn-primary.m-0.stanze-button") %>% 
    html_attr('href')
    
  d <- data.frame(species = name, url = url) %>% mutate(species_sub = species1[i])
  
  strickler_list[[i]] <- d
  print(i)
}

# combine these list elements into onw dataframe
strickler_species <- bind_rows(strickler_list)


# pick the most relevant link and complete url, bring into right format for app
strickler_species <- strickler_species %>% group_by(species_sub) %>% slice(1) 
strickler_species <- strickler_species %>% 
  mutate(URL = paste0("https://www.gaertnerei-strickler.de", url)) %>% 
  mutate(Produzent = "Kräuter- und Wildpflanzen-Gärtnerei Strickler") %>% 
  left_join(cg, by = c("species_sub" = "Wissenschaftlicher Name")) %>% 
  select(species_sub, `Deutscher Name`, Produzent, URL) %>% 
  rename(`Wissenschaftlicher Name` = species_sub)


# save
write_excel_csv(strickler_species, "Data-outputs/strickler/strickler_species_APRIL2023.csv")

# save for shiny app
write_excel_csv(strickler_species, "Cg-app-de/data-shiny/strickler_cg.csv")


# save for english version
strickler_species <- strickler_species %>% 
  select(Species = `Wissenschaftlicher Name`, Producer = Produzent, URL)

write_excel_csv(strickler_species, "Cg-app-en/data-shiny/strickler_cg.csv")

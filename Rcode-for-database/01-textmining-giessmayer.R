source("Rcode-for-database/00-preamble.R")

# all cg species
cg <- read_csv("Data-shiny/shiny_data.csv")

# vector
cg_sp <- unique(cg$`Wissenschaftlicher Name`)

# replace space with %20
cg_sp_url <- paste0(word(cg_sp, 1), "%20", word(cg_sp, 2))


input <- paste0("https://www.gaissmayer.de/web/shop/suche/produkte/?suche=", cg_sp_url) 
species_list <-  NULL
for(i in 1:length(input)){
  
  page <- try({
    (read_html(input[i]) %>% 
      rvest::html_nodes('p') %>% 
      html_text2)[2]
    })
  species <- cg_sp[i]
  
  if(page == "Wir haben 0 Produkte zu ihren Auswahlkriterien gefunden.") {next} else{
    
    species_list[[i]] <-  data.frame(species = species, url = input[i])

    print(i)
  }
}

giessmayer_species <- bind_rows(species_list)

write.csv(giessmayer_species, "Data-outputs/giessmayer/giessmayer_species.csv", row.names = F)


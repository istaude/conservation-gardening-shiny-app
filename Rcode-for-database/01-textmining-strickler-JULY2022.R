source("Rcode-for-database/00-preamble.R")

# on Gärtnerei Strickler there is a total of 2215 entries seperate over 45 pages/tabs
# we need to generate urls that include page number and then extract all the available plants on that page

# plants produced
strickler_list <- list()

for (i in 1:56){
  
  strickler <- paste0("https://www.gaertnerei-strickler.de/suche.php?suche=&kuerzel=&standort=&hoehemin=&hoehemax=&monat=&farbe=&heimisch=&seite=", i)
  
  strickler <- read_html(strickler)
  
  name <- strickler %>% 
    rvest::html_nodes('tbody') %>% 
    xml2::xml_find_all("//td") %>% 
    rvest::html_text() %>% 
    str_replace_all(., "[\t\n]" , "") %>% 
    .[grepl("--", .)]
  
  
  values <- strickler %>% 
    rvest::html_nodes('body') %>% 
    xml2::xml_find_all("//td[2]")
  
  values <- sub('.*parent.location.href=', '', values)
  values <- sub('class.*', '', values)
  values <- gsub('"', '', values)
  values <- gsub("'", '', values)
  values <- trimws(values)
  values <- unique(values)
  values <- data.frame(values) %>% filter(!str_detect(values, "onclick"))
  
  data <-  values %>% mutate(name = name)
  
  strickler_list[[i]] <- data
  print(i)
}

# combine these list elements into onw dataframe
strickler_species <- bind_rows(strickler_list)

write.csv(strickler_species, "Data-outputs/strickler/strickler_species.csv", row.names = F)


source("Rcode-for-database/00-preamble.R")

# on blauetikett.de there is a total of 279 seed entries separate over 31 pages/tabs
# we need to generate urls that include page number and then extract all the available plants on that page

# plants produced
blauetikett_list <- list()

for (i in 1:31){
  
  blauetikett <- paste0("https://www.blauetikett.de/samen-1/wildblumen.html?p=", i )
  
  blauetikett <- read_html(blauetikett)
  
  name <- blauetikett %>% 
    rvest::html_nodes('.product-name') %>% 
   # rvest::html_attr('title') %>% 
    rvest::html_text() %>% 
    str_replace_all(., "[\t\n]" , "")
  
  
  
  values <- blauetikett %>% 
    rvest::html_nodes('.product-name a') %>% 
    rvest::html_attr('href')
  values <- data.frame(values)
  
  data <-  values %>% mutate(name = name)
  blauetikett_list[[i]] <- data
  print(i)
}


# combine these list elements into one dataframe
blauetikett_species <- bind_rows(blauetikett_list)

write.csv(blauetikett_species, "Data-outputs/blauetikett/blauetikett_species.csv", row.names = F)


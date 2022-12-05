source("Rcode-for-database/00-preamble.R")

# on Hof Berg- Garten there is a total of 244 seed entries separate over 31 pages/tabs
# there is a total of 354 potted plant entries separate over 45 pages
# we need to generate urls that include page number and then extract all the available plants on that page

# seeds produced
berg_garten_list <- list()

for (i in 1:31){
  
  berg_garten <- paste0("https://shop.hof-berggarten.de/samen-kaufen/einzelsaatgut/?page=", i)
  
  berg_garten <- read_html(berg_garten)
  
  name <- berg_garten %>% 
    rvest::html_nodes('.title') %>% 
    rvest::html_text() %>% 
    str_replace_all(., "[\t\n]" , "")
  
  

  values <- berg_garten %>% 
    rvest::html_nodes('.product-url') %>% 
    rvest::html_attr('href')
   values <- data.frame(values)
  
   data <-  values %>% mutate(name = name)
  berg_garten_list[[i]] <- data
  print(i)
}


# combine these list elements into one dataframe
berg_garten_species <- bind_rows(berg_garten_list)


# potted plants produced
berg_garten_list_pot <- list()

for (i in 1:45){
  
  berg_garten <- paste0("https://shop.hof-berggarten.de/pflanzen-bio-versand/pflanze-im-topf/?page=", i)
  
  berg_garten <- read_html(berg_garten)
  
  name <- berg_garten %>% 
    rvest::html_nodes('.title') %>% 
    rvest::html_text() %>% 
    str_replace_all(., "[\t\n]" , "")
  
  
  
  values <- berg_garten %>% 
    rvest::html_nodes('.product-url') %>% 
    rvest::html_attr('href')
  values <- data.frame(values)
  
  data <-  values %>% mutate(name = name)
  berg_garten_list_pot[[i]] <- data
  print(i)
}

# combine these list elements into one dataframe
berg_garten_species_pot <- bind_rows(berg_garten_list_pot)


# combine both dataframes
berg_garten <- bind_rows(berg_garten_species, berg_garten_species_pot)

write.csv(berg_garten, "Data-outputs/hof-berg-garten/berg_garten_species.csv", row.names = F)


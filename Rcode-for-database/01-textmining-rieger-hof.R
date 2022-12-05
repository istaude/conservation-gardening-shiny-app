source("Rcode-for-database/00-preamble.R")

# on rieger hofmann there is a total of 210 flower seed entries separate over 7 pages/tabs
# there are 36 herb/pharmaceutical seed entries over 2 pages
# there are 41 grass entries over 2 pages
# there are 16 shrub/grove entries over 1 page
# we need to generate urls that include page number and then extract all the available plants on that page

#### flowers #### 
# plants produced
rieger_hof_list_fl <- list()

for (i in 1:7)  {
  rieger_hof1 <- paste0("https://www.rieger-hofmann.de/rieger-hofmann-shop/einzelarten/blumen.html?tt_products%5Bpp%5D=", i-1)
  
  rieger_hof <- read_html(rieger_hof1)
  name <- rieger_hof %>% 
    rvest::html_nodes('h2') %>% 
    rvest::html_text() %>% 
    str_replace_all(., "[\t\n]" , "")
  name <-  name[!name %in% 'Blumen' ]
  
  values <- rep(rieger_hof1, length(name))

  data <-  data.frame(values) %>% mutate(name = name)
  rieger_hof_list_fl[[i]] <- data
  print(i)
}


rieger_hof_list_fl <- bind_rows(rieger_hof_list_fl)

#### herbs ####
# plants produced
rieger_hof_list_he <- list()

for (i in 1:2)  {
  rieger_hof1 <- paste0("https://www.rieger-hofmann.de/rieger-hofmann-shop/einzelarten/arznei-gewuerz-und-kulturpflanzen.html?tt_products%5Bpp%5D=", i-1)
  
  rieger_hof <- read_html(rieger_hof1)
  name <- rieger_hof %>% 
    rvest::html_nodes('h2') %>% 
    rvest::html_text() %>% 
    str_replace_all(., "[\t\n]" , "")
  name <-  name[!name %in% 'Arznei-, Gewürz- und Kulturpflanzen' ] 
  
  values <- rep(rieger_hof1, length(name))
  
  data <-  data.frame(values) %>% mutate(name = name)
  rieger_hof_list_he[[i]] <- data
  print(i)
}

rieger_hof_species_he <- bind_rows(rieger_hof_list_he)

#### grasses #### 
# plants produced

rieger_hof_list_gr <- list()

for (i in 1:2)  {
  rieger_hof1 <- paste0("https://www.rieger-hofmann.de/rieger-hofmann-shop/einzelarten/graeser.html?tt_products%5Bpp%5D=", i-1)
  
  rieger_hof <- read_html(rieger_hof1)
  name <- rieger_hof %>% 
    rvest::html_nodes('h2') %>% 
    rvest::html_text() %>% 
    str_replace_all(., "[\t\n]" , "")
  name <-  name[!name %in% 'Gräser' ] 
  
  values <- rep(rieger_hof1, length(name))
  
  data <-  data.frame(values) %>% mutate(name = name)
  rieger_hof_list_gr[[i]] <- data
  print(i)
}

rieger_hof_list_gr <- bind_rows(rieger_hof_list_gr)
View(rieger_hof_list_gr)


#### shrubs #### 
# plants produced

rieger_hof <- paste0("https://www.rieger-hofmann.de/rieger-hofmann-shop/einzelarten/gehoelze.html")
rieger_hof <- read_html(rieger_hof)
name <- rieger_hof %>% 
  rvest::html_nodes('h2') %>% 
  rvest::html_text() %>% 
  str_replace_all(., "[\t\n]" , "")
name <-  name[!name %in% c('Anbau im Württembergisch-Fränkischen Hügelland','Gehölze') ] 
values <- rep("https://www.rieger-hofmann.de/rieger-hofmann-shop/einzelarten/gehoelze.html", times =16)
values <- data.frame(values)
data <-  values %>% mutate(name = name)
rieger_hof_list_sh <- data


# combine everything
rieger_hof <- bind_rows(list(rieger_hof_list_fl,  rieger_hof_list_he, rieger_hof_list_gr, rieger_hof_list_sh))

write.csv(rieger_hof, "Data-outputs/rieger-hof/rieger_hof_species.csv", row.names = F)



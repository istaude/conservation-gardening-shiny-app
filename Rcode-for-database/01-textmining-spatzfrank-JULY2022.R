source("Rcode-for-database/00-preamble.R")

# on Gärtnerei Spatz und Frank there is a total of 531 seed entries separate over 23 pages/tabs
# we need to generate urls that include page number and then extract all the available plants on that page

# plants produced
spatzfrank_list <- list()

for (i in 1:23){
  
  spatzfrank <- paste0("https://www.stauden-spatzundfrank.de/stauden/?feature_categories_id=0&filter_categories_id=0&filter_fv_id%5B3%5D%5B0%5D=&filter_fv_id%5B3%5D%5B1%5D=&filter_fv_id%5B4%5D%5B0%5D=&filter_fv_id%5B4%5D%5B1%5D=&filter_fv_id%5B1%5D%5B0%5D=&filter_fv_id%5B1%5D%5B1%5D=&filter_fv_id%5B6%5D%5B0%5D=&filter_fv_id%5B6%5D%5B1%5D=&filter_fv_id%5B7%5D%5B0%5D=&filter_fv_id%5B7%5D%5B1%5D=&filter_fv_id%5B2%5D%5B0%5D=&filter_fv_id%5B2%5D%5B1%5D=&filter_fv_id%5B5%5D%5B0%5D=32&filter_fv_id%5B5%5D%5B1%5D=32&value_conjunction%5B3%5D=1&value_conjunction%5B4%5D=1&value_conjunction%5B1%5D=1&value_conjunction%5B6%5D=1&value_conjunction%5B7%5D=1&value_conjunction%5B2%5D=1&value_conjunction%5B5%5D=1&filter_url=stauden%252F&page=", i)
  
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

write.csv(spatzfrank_species, "Data-outputs/spatzfrank/spatzfrank_species.csv", row.names = F)

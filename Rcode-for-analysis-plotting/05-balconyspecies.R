source("Rcode-for-database/00-preamble.R")
showtext_auto()

d <- read_csv("Data-shiny/shiny_data.csv")

# only threatened species suitable for balconies
d <- d %>% 
  filter(Balkon == "ja") %>% 
  filter(Gefährdung != "V")


# join with info on seller
ds <- read_csv("Data-shiny/seller_cg.csv") %>% select(- `Deutscher Name`) %>% 
  filter(Produzent == "Kräuter- und Wildpflanzen-Gärtnerei Strickler")


# now the challenge is to get their price,
# first filter plants not produced
ds <- left_join(d, ds) %>% filter(!is.na(Produzent))

# now only the species and url
urls <- ds %>% select(`Wissenschaftlicher Name`, URL) %>% distinct %>% pull(URL)

# for these species extract the price
datalist <- NULL
for (i in 1:length(urls)){
page <- read_html(urls[i])
data <- page %>%
  rvest::html_nodes('table') %>% 
  xml2::xml_find_all("//tr//td") %>% 
  rvest::html_text() %>% 
  str_replace_all(., "[\t\n]" , "")

datalist[[i]] <- data.frame(price  = data[str_which(data, pattern = "Preis") +1], 
           size = data[str_which(data, pattern = "Verkaufsgröße:") +1])
print(i)
}

dt <- cbind(ds %>% select(`Wissenschaftlicher Name`, URL) %>% distinct, 
      bind_rows(datalist)) 

price1 <- regmatches(dt$price, regexpr("[:^]*:[^**]*", dt$price))
price1 <-  sub(": ", "", price1)
price1 <-  sub(",", ".", price1)
price1 <-  sub("€", "", price1)
price1 <- trimws(price1)
price1 <- as.numeric(price1)

dt$price1 <- price1

# join back to federal state data
d <- left_join(dt, d) %>% arrange(Bundesland)

View(d)
# selection, semi random
d_select_from <- d %>% 
  rename(species = `Wissenschaftlicher Name`, rl_cat = Gefährdung) %>% 
  filter(price != 0) %>% 
  filter(!grepl("Salix",species)) %>% 
  filter(!grepl("Rosa", species)) %>% 
  filter(Wasser != "Wasserpflanze") %>% 
  filter(Wasser != "feucht") %>% 
  filter(size != "Tb 60/80") %>% 
  filter(Wasser != "feucht bis frisch") %>% 
  select(Bundesland, species, rl_cat, price = price1, size) %>% 
  distinct %>% 
  group_by(Bundesland, species) %>% 
  sample_n(1) %>% 
  ungroup()

write_excel_csv(d_select_from, "Data-outputs/balcony/balcony_cg_possibilities.csv")


d_select <- d_select_from %>% group_by(Bundesland) %>% sample_n(5)

ds <- d_select %>%
  group_by(Bundesland) %>% 
  summarise(spec_vec = 
              paste(species, " ",
                    "(", rl_cat, ")", "; ", price, "€", "\n",
                    sep ="", collapse = "") 
  )
ds$spec_vec <-  gsub('.{1}$', '', ds$spec_vec)

# plot of germany ---------------------------------------------------------
germany <- ne_states(country = "Germany", returnclass = "sf")

germany <- germany %>% 
  filter(name != "Bremen") %>% 
  mutate(name = ifelse(name == "Niedersachsen", "Bremen/Niedersachsen", name)) %>% 
  left_join(ds, by = c("name" = "Bundesland"))
  
germany$name
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
cols = gg_color_hue(15)
ggplot(germany) +
  geom_sf(size =0.1, alpha = 0.3, fill = cols) +
  geom_sf_label_repel(aes(label = spec_vec), size = 3,
                      seed = 7, 
                      fill = alpha(cols, 0.2), max.overlaps = 20, 
                      force = 1000, hjust = 0, box.padding = 0,
                      segment.curvature = -1e-20,
                      segment.linetype = 1,
                      segment.size = 0.4, 
                      segment.alpha = 0.6,
                       family = "Roboto Condensed", fontface = "italic") + 
  scale_x_continuous(expand = expansion(mult = 0.8)) +
  scale_y_continuous(expand = expansion(mult = 0.4)) +
  theme_void() +
  labs(title = "5 conservation gardening species for the balcony per federal state") +
  theme(title = element_text(family = "Roboto Condensed"))

showtext_opts(dpi=600)
ggsave("Figures/figure5.png", 
       height = 8, 
       width = 9.2, 
       dpi = 600,
       bg = "white")

showtext_opts(dpi=96)




# notes on italic etc, does not work -----------------------------------
ds <- d_select %>%
  mutate(sp = gsub(" ", "~", species)) %>% 
  group_by(Bundesland) %>% 
  summarise(spec_vec = paste0('"',
                              
                              paste(
                                    'italic(',sp,')', 
                                    '~',
                                    'bold(', "'" , rl_cat, "'", ')', 
                                    ';~',
                                    price,
                                    '€',
                                    sep ="", collapse = ""), 
                              
                              '"') 
  )
ds$spec_vec <-  gsub('.{2}$', '', ds$spec_vec)

ds <- d_select %>%
  mutate(sp = gsub(" ", "~", species)) %>% 
  group_by(Bundesland) %>% 
  summarise(spec_vec = paste0('"',
                              paste('*italic(',sp,')', " ",
                                    '*bold(',rl_cat,')', "; ", price, "€", "\n",
                                    '', sep ="", collapse = ""), '"') 
  )


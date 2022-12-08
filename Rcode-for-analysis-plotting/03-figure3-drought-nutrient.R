source("Rcode-for-database/00-preamble.R")
showtext_auto()

# figure comparing proportion of drought tolerant conventional vs  --------

# load cultivated plants
kpf <- read_excel("Data-inputs/kulturpflanzen.xlsx")
unique(kpf$Use)

kpf <- kpf %>% filter(
  Use == "Rasen- und Wiesensaaten" |
    Use == "Ein- und zweijährige Blumen" |
    Use == "Gemüse"  |
    Use == "Garten- und Parkgehölze"|
    Use == "Zwiebel- und Knollengewächse" |
    Use == "Zierstauden"
)

kpf$sp <- unlist(lapply(str_split(kpf$Species, " - "),  `[`, 1))
kpf$triv <- unlist(lapply(str_split(kpf$Species, " - "),  `[`, 2))


# correct manually
write_excel_csv(kpf, "Data-inputs/kulturpflanzen_refined.csv")
kpf <- read_excel("Data-inputs/cultivated_plants.xlsx")

# prepare for textmining naturadb
kpf <- kpf %>% select(sp) %>% distinct %>% pull
kpf <- sub(" ", "-", kpf)

input <- paste0("https://www.naturadb.de/pflanzen/", kpf, "/") 

species_list <-  NULL
for(i in 1:length(input)){
  
  page <- try({read_html(input[i])})
  species <- kpf[i]
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

dx <- bind_rows(species_list)

# write data frame
write_excel_csv(dx, "Data-inputs/cultivated_plants_naturadb.csv")

# comparison --------------------------------------------------------------
dx <- read_csv("Data-inputs/cultivated_plants_naturadb.csv")
d <- read_csv("Cg-app-de/data-shiny/shiny_data.csv")

# Water
d_water <- bind_rows(
  d %>% select(species = `Wissenschaftlicher Name`, Wasser) %>% 
    distinct %>% 
    count(Wasser) %>% filter(!is.na(Wasser)) %>% mutate(type = "conservation"),
  dx %>% filter(cat == "Wasser:") %>% count(val) %>% filter(val != "") %>% 
    mutate(type = "conventional") %>% rename(Wasser = val)
) %>% pivot_wider(names_from = "type", values_from = "n") %>% 
  filter(Wasser == "trocken" | 
           Wasser == "frisch" | 
           Wasser == "feucht") %>% 
  mutate(prop_cg = conservation / sum(conservation), 
         prop_con = conventional / sum(conventional))

sum(d_water$conventional)
sum(d_water$conservation)

# Nutrients
d_nutrients <- bind_rows(
  d %>% select(species = `Wissenschaftlicher Name`, Nährstoffe) %>% 
    distinct %>% 
    count(Nährstoffe) %>% filter(!is.na(Nährstoffe)) %>% mutate(type = "conservation"),
  dx %>% filter(cat == "Nährstoffe:") %>% count(val) %>% filter(val != "") %>% 
    mutate(type = "conventional") %>% rename(Nährstoffe = val)
) %>% pivot_wider(names_from = "type", values_from = "n") %>% 
  filter(Nährstoffe == "nährstoffarmer Boden" | 
           Nährstoffe == "nährstoffreicher Boden" | 
           Nährstoffe == "normaler Boden") %>% 
  mutate(prop_cg = conservation / sum(conservation, na.rm = T), 
         prop_con = conventional / sum(conventional, na.rm = T))

sum(d_nutrients$conventional)
sum(d_nutrients$conservation)


# plot comparison ---------------------------------------------------------
d <- bind_rows(
d_water %>% 
  pivot_longer(cols = c("prop_cg", "prop_con")) %>% 
  arrange(name) %>% 
  mutate(resource = "Water") %>% rename(resource_value = Wasser) %>% 
  mutate(resource_value = ifelse(resource_value == "feucht", "wet",
                                 ifelse(resource_value == "trocken", "dry", "moist")))
,
d_nutrients %>% 
  pivot_longer(cols = c("prop_cg", "prop_con")) %>% 
  arrange(name) %>% 
  mutate(resource = "Nutrients") %>% rename(resource_value=Nährstoffe) %>% 
  mutate(resource_value = ifelse(resource_value == "nährstoffarmer Boden", "N poor",
                                 ifelse(resource_value == "nährstoffreicher Boden", "N rich", "intermediate")))
) %>% mutate(name = ifelse(name == "prop_cg", "Conservation gardening spp.", "Conventional gardening spp."))


d %>% filter(resource == "Water") %>% 
  mutate(resource_value = factor(resource_value, levels=c("dry", "moist", "wet"))) %>%  
  ggplot(aes(name, value, group = resource_value, fill = resource_value)) +
  geom_chicklet(width = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "dry" = "#e5cf5f",
      "moist" = "#fbc675",
      "wet" = "#7fdded"
    )
  ) +
  coord_flip() +
  labs(x="", y="Percent species", subtitle = "Water")  +
  theme_ipsum_rc(grid="X") +
  theme(axis.text.x = element_text(color = "gray60", size = 10)) +
  theme(legend.position = "top", legend.justification = "left")+
  theme(
        legend.title=element_blank(),
        plot.margin = margin(0, 0, 0, 0, "pt")) -> plot_water

d %>% filter(resource == "Nutrients") %>% 
  mutate(resource_value = factor(resource_value, levels=c("N poor", "intermediate", "N rich"))) %>%  
  ggplot(aes(name, value, group = resource_value, fill = resource_value)) +
  geom_chicklet(width = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "N poor" = "#acd95f",
      "intermediate" = "#bad36d",
      "N rich" = "#6be1b9"
    )
  ) +
  coord_flip() +
  labs(x="", y="Percent species", subtitle = "Nutrients")  +
  theme_ipsum_rc(grid="X") +
  theme(axis.text.x = element_text(color = "gray60", size = 10),
        axis.text.y = element_blank()) +
  theme(legend.position = "top", legend.justification = "left")+
  theme(
        legend.title=element_blank(),
        plot.margin = margin(0, 0, 0, 0, "pt")) -> plot_nutrients


plot_water + plot_spacer()+ plot_nutrients +
  plot_layout(widths = c(2, 1, 2)) +
  plot_annotation(title = "The resource needs of conservation vs. conventional gardening species",
                  theme = theme(text = element_text(family="Roboto Condensed")),
                  tag_levels = list(c('a', 'b')))

showtext_opts(dpi=600)
ggsave(
  "Figures/figure3.png",
  height= 3.58,
  width = 8.9,
  dpi = 600,
  bg = "white"
)
showtext_opts(dpi=96)


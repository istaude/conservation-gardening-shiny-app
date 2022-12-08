source("Rcode-for-database/00-preamble.R")
showtext_auto()


# info on threat in germany for text in ms --------------------------------
threat_summary <- read_excel("Cg-app-de/data-shiny/red_lists_summary.xlsx")
new_threat_summary <- threat_summary %>%
  rowwise() %>%
  mutate(threatened = sum(c_across(c(4:10)), na.rm = T)) %>%
  mutate(perc_threatened = threatened / `Bewertete etablierte Taxa`)


# figure of germany threat status -----------------------------------------
d <-
  read_csv("Data-outputs/naturadb/naturadb_redlist_fed_states_all_species.csv")

# reduce data frame to relevant columns
d <- d %>% 
  select(fed_state, species_cleaned, rl_cat, naturadb_common_name) %>% 
  distinct

# to calculate the number of species available, we are excluding ssp.
# this is because naturadb doesnt really list those and we cant query them
d <- d %>% filter(!grepl("ssp.", species_cleaned))

# create column that indicates whether cg or not
d <- d %>% mutate(cg_amenable = ifelse(naturadb_common_name == "not in naturadb", "not_cg", "cg")) %>%
  select(-naturadb_common_name,-rl_cat) %>% distinct

# now we can calculate the proportion of species amenable to cg per state, and overall
# per state
dstate <- d %>%
  group_by(fed_state) %>%
  count(cg_amenable) %>%
  pivot_wider(names_from = "cg_amenable", values_from = "n")

# for plotting
dstate <- dstate %>% mutate(fed_state = strsplit(as.character(fed_state), "/")) %>%
  unnest(fed_state) %>% full_join(
    new_threat_summary %>%
      select(fed_state = Bundesland,
             threatened,
             assessed = `Bewertete etablierte Taxa`)
  ) %>%
  mutate(
    current_threat = threatened / assessed,
    threat_with_cg = (threatened - cg) / assessed
  ) %>%
  mutate(perc_reduction_in_threat = 1 - threat_with_cg / current_threat)

# summary stats
median(dstate$threat_with_cg, na.rm = T)
median(dstate$current_threat, na.rm = T)
dstate %>% arrange(perc_reduction_in_threat)
dstate %>% arrange(threat_with_cg)

dstate <- dstate %>% select(fed_state, current_threat, threat_with_cg) %>%
  pivot_longer(!fed_state,
               values_to = "perc_threat",
               names_to = c("cg_amenable")) %>%
  mutate(
    cg_amenable = ifelse(
      cg_amenable == "threat_with_cg",
      "With conservation gardening",
      "Current threat"
    )
  )


# join to spatial polygon data frame of german federal states
germany <- ne_states(country = "Germany", returnclass = "sf")
germany <- full_join(germany, dstate, by = c("name" = "fed_state"))

(ggplot() +
    geom_sf(data = germany, aes(fill = perc_threat), size =0.1) +
    facet_wrap(~cg_amenable) +
    theme_void() +
    scale_fill_viridis(option = "H", 
                       begin = .5,
                       end = 1,
                       alpha = 0.9,
                       labels = scales::percent,
                       name = "Percent endangered spp.") +
    theme_ipsum_rc(grid="") +
    theme(
      plot.margin = margin(0, 0, 0, 0, "pt")) +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major = element_blank(),
          legend.key.size = unit(.5, 'cm'))  -> p1)


# overall reduction in Germany --------------------------------------------

# load red list from Germany
d <- read_excel("Data-inputs/02_Datentabelle_RL_Farn-_und_Bluetenpflanzen_2018_Deutschland_20210317-1607.xlsx")

# select relevant cols
d <- d %>% filter(Arten == "Arten") %>% 
  select(species = Name, rl_cat = `RL Kat.`)

# filter subspecies and agg.
d <- d %>% filter(!grepl("subsp.",species))
d <- d %>% filter(!grepl("agg.",species))

for (i in 1:nrow(d)){
 d$species_cleaned[i] <-  remove.authors(d$species[i])
}

nspp_germanrl <- d %>% select(rl_cat, species_cleaned)

# only filter endangered spp.
unique(d$rl_cat)
d <- d %>%
  filter(rl_cat == 0 |
           rl_cat == 1 |
           rl_cat == 2 |
           rl_cat == 3 |
           rl_cat == "R" |
           rl_cat == "G" |
           rl_cat == "V")

# prepare for natura db
d$species_naturadb <- sub(" ", "-", d$species_cleaned)
# create urls
input <- paste0("https://www.naturadb.de/pflanzen/", d$species_naturadb, "/") 

species_list <-  NULL
for(i in 1:length(input)){
  
  page <- try({read_html(input[i])})
  species <- d$species_cleaned[i]
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

# merge back the red list category
dx <- left_join(d, dx, by = c("species_cleaned" = "species"))

dx <- dx %>% 
  select(species_cleaned, rl_cat, common_name, cat, val) %>% 
  mutate(val = trimws(val)) %>% 
  mutate(common_name = ifelse(is.na(common_name), "not in naturadb", "naturadb")) %>% 
  mutate(val = ifelse(is.na(val), "not in naturadb", val)) %>% 
  mutate(cat = ifelse(is.na(cat), "not in naturadb", cat)) %>% 
  rename(naturadb_common_name = common_name, naturadb_cat = cat, naturadb_val = val)


dx <- dx %>% select(species_cleaned, rl_cat, naturadb_common_name) %>% distinct
write_excel_csv(dx, "Data-inputs/german_rl_naturadb.csv")


# circular bar charts
d <- read_csv("Data-inputs/german_rl_naturadb.csv")

dc <- d %>% count(rl_cat) %>% mutate(cat = "Current threat status") %>% 
  bind_rows(d %>% 
              group_by(naturadb_common_name) %>%  
              count(rl_cat) %>% 
              filter(naturadb_common_name == "not in naturadb") %>% 
              mutate(cat = "Potential of conservation gardening") %>%
              ungroup() %>% 
              select(-naturadb_common_name))

dc$ alph <- c(rep(1, 7),
dc %>% pivot_wider(names_from=cat, values_from = n) %>% 
  mutate(alph = `Potential of conservation gardening`/ `Current threat status`) %>% 
  pull(alph))

dc %>% group_by(cat) %>% summarise(sum(n))

(ggplot(dc) +
  facet_wrap(~cat) +
  geom_col(
    aes(
      x = rl_cat,
      y = n,
      alpha = alph
    ),
    col = "grey20",
    fill = "#FB8222",
    position = "dodge2",
    show.legend = TRUE
  ) +
  theme_ipsum_rc(grid = "Y") +
  theme(
    axis.title = element_blank(),
    panel.grid.major.y = element_line(linetype = 3, colour = "grey60"),
    strip.text = element_blank(),
    axis.text.x = element_text(color = "gray50", size = 14),
    legend.position = "none",
    plot.margin = margin(0, 0, 0, 0, "pt"),
  ) +
  labs(x = "", y = "Number of species") +
  coord_polar() -> p2)

p1 / p2 +
  plot_annotation(title = "Bending the curve of biodiversity loss with conservation gardening",
                  theme = theme(text = element_text(family="Roboto Condensed")),
                  tag_levels = list(c('a', 'b', 'c')))

showtext_opts(dpi=600)
ggsave(
  "Figures/figure4.png",
  height= 6.55,
  width = 7.16,
  dpi = 600,
  bg = "white"
)
showtext_opts(dpi=96)

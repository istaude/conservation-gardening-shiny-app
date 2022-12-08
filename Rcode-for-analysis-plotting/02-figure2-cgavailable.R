source("Rcode-for-database/00-preamble.R")
showtext_auto()

# figure of cg species available online -----------------------------------

# load cg species and seller information
d_producer <-  read_csv("Cg-app-de/data-shiny/seller_cg.csv")

# donut chart for overall percentage available
data <- d_producer %>% 
  mutate(avail = ifelse(is.na(Produzent), "not available", "available")) %>% 
  select(species = `Wissenschaftlicher Name`, avail) %>% 
  distinct %>% 
  count(avail) %>% 
  mutate(avail = c("produced", "not\nproduced"))

labs <- paste0(data$avail,"\n n = " , data$n )

(donut <- ggdonutchart(data, "n", label = labs, lab.pos = "in",
                       fill = "avail", color = "white", lab.font= c(3, "plain","grey20"),
                       font.family = "Roboto Condensed",
                       palette = c("grey90", "#47ea5f") ) +
    theme_ipsum_rc(grid="") +
    theme(axis.text.y = element_blank(), 
          axis.text.x = element_blank(),
          axis.title  = element_blank()) +
    labs(y = "", x = "") +
    theme(legend.position = "none", 
          plot.margin = margin(0, 0, 0, 0, "pt")))


# bar plot for overall number available by each seller
(seller_share <- d_producer %>% 
  mutate(avail = ifelse(is.na(Produzent), "not available", "available")) %>% 
  select(species = `Wissenschaftlicher Name`, avail, Produzent) %>% 
  distinct %>% 
  group_by(Produzent) %>% 
  count(avail) %>% filter(!is.na(Produzent)) %>% 
  arrange(n) %>% 
  mutate(Produzent = ifelse(Produzent == "Kräuter- und Wildpflanzen-Gärtnerei Strickler", "Strickler",
         ifelse(Produzent == "Staudengärtnerei Gaißmayer", "Gaißmayer",
                ifelse(Produzent == "Staudengärtnerei StaudenSpatz", "StaudenSpatz", Produzent )))) %>% 
  ungroup %>% 
  mutate(Produzent = factor(Produzent, levels = Produzent)) %>% 
  ggplot(aes(Produzent, n, fill = Produzent)) +
  geom_chicklet(width = 0.75) +
  guides(
    fill = guide_legend(nrow = 1)
  ) +
  coord_flip() +
  theme_ipsum_rc(grid="X") +
  labs(x = "Producer", y = "Number of species") +
  theme(axis.text.x = element_text(color = "gray60", size = 10)) +
  theme(legend.position = "none")+
  #theme(legend.key.size = unit(.3, 'cm')) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "Strickler" = "#17cd32",
      "Gaißmayer" = "#1ae438",
      "StaudenSpatz" = "#47ea5f",
      "Hof Berg-Garten" = "#5eed73",
      "Rieger-Hofmann" = "#8cf29b",
      "Blauetikett Bornträger" = "#c2f8ca"
    )
  ) )


# availability by red list category
d_producer <-  read_csv("Cg-app-de/data-shiny/seller_cg.csv")
d_amenable <- read_csv("Data-outputs/naturadb/naturadb_redlist_fed_states_all_species.csv")

(rlcat_seller <- left_join(
  d_producer %>% 
    mutate(avail = ifelse(is.na(Produzent), "not available", "available")) %>% 
    select(species = `Wissenschaftlicher Name`, avail) %>% distinct
  ,
  d_amenable %>% 
    select(fed_state, species = species_cleaned,  rl_cat) %>% 
    distinct
) %>% arrange(fed_state) %>% 
  filter(avail == "available") %>% 
  group_by(fed_state) %>% 
  count(rl_cat) %>% 
  mutate(total = sum(n)) %>% 
  arrange(total) %>% 
  ungroup() %>% 
  group_by(rl_cat) %>% 
  mutate(fed_state = factor(fed_state, levels=fed_state)) %>% 
  ggplot(aes(fed_state, n, fill = rl_cat)) +
  geom_chicklet(width = 0.75) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "0" = "#37fa54",
      "1" = "#47ea5f",
      "2" = "#57da6a",
      "3" = "#67ca75",
      "R" = "#76bb80",
      "G" = "#86ab8c",
      "V" = "#969b97"
    )
  ) +
  guides(
    fill = guide_legend(nrow = 1)
  ) +
  coord_flip() +
  scale_x_discrete(position = "top") +
  labs(x="Federal state", y="Number of species") +
  theme_ipsum_rc(grid="X") +
  theme(axis.text.x = element_text(color = "gray60", size = 10)) +
  theme(legend.position = "top", legend.justification = "left")+
  theme(legend.key.size = unit(.3, 'cm'),
        plot.margin = margin(0, 0, 0, 0, "pt")))








# putting everything together ---------------------------------------------

(donut + seller_share ) /
 ( rlcat_seller + plot_spacer()) + 
  plot_layout(heights = c(3, 4.5), widths = c(4, 2)) +
  plot_annotation(title = "Conservation gardening species produced",
                  theme = theme(text = element_text(family="Roboto Condensed")),
                  tag_levels = list(c('a', 'b', 'c')))

showtext_opts(dpi=600)
ggsave(
  "Figures/figure2.png",
  height= 6.82,
  width = 6.3,
  dpi = 600,
  bg = "white"
)
showtext_opts(dpi=96)

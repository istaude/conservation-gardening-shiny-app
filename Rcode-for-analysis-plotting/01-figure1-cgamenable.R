source("Rcode-for-database/00-preamble.R")
showtext_auto()


# figure of species amenable to cg per federal state ----------------------
d <- read_csv("Data-outputs/naturadb/naturadb_redlist_fed_states_all_species.csv")

# reduce data frame to relevant columns
d <- d %>% select(fed_state, species_cleaned, species_naturadb, rl_cat, naturadb_common_name) %>% distinct

# to calculate the number of species available, we are excluding ssp. nad var.
# this is because naturadb doesnt really list those and we cant query them
d <- d %>% filter(!grepl("ssp.",species_cleaned))
d <- d %>% filter(!grepl("var.",species_cleaned))

# how many species in total threatened in at least one federal state?
d %>% select(species_cleaned) %>% distinct %>% nrow


# create column that indicates whether cg or not
d <- d %>% mutate(cg_amenable = ifelse(naturadb_common_name == "not in naturadb", "not_cg", "cg")) %>% 
  select(-naturadb_common_name, -rl_cat) %>% distinct

# how many of the 3142 species are amenable to gardening in total
d %>% select(species_cleaned, cg_amenable) %>% distinct %>% 
  count(cg_amenable)


# now we can calculate the proportion of species amenable to cg per state, and overall
# per state
dstate <- d %>% 
  group_by(fed_state) %>% 
  count(cg_amenable)

# create data frame for plotting
dstate <- bind_rows(dstate, 
                    dstate %>% 
                      group_by(fed_state) %>% 
                      summarize(n = sum(n)) %>% 
                      mutate(cg_amenable = "total")
) %>% 
  mutate(xend = 0) %>% 
  filter(cg_amenable != "not_cg") %>% 
  group_by(cg_amenable) %>% 
  group_by(n) %>% 
  mutate(fed_state = factor(fed_state, levels=fed_state)) 




# bar plots
(bars <- ggplot(dstate)+
    geom_segment(aes(x = n, xend = xend, y = fed_state, yend = fed_state, color = "line"),
                 show.legend = FALSE, size = 2)+
    geom_tile(aes(x = n, y = fed_state, width = 25, height = 0.5, fill = cg_amenable), size = 1)+
    scale_fill_manual("", 
                      values = c("cg" = "#ea5f47", "total" = "#0a5268"), 
                      labels = c("total" = "Endangered", "cg" = "Amenable"))+
    scale_color_manual(values = c("line" = "#a8adb3"))+
    labs(y="", x="Number of species") +
    theme_ipsum_rc(grid="X") + 
    theme(axis.text.x = element_text(color = "gray60", size = 10)) +
    theme(legend.position = "top", legend.justification = "left") +
    theme(legend.key.size = unit(.5, 'cm')))

# text plot
percentage <- dstate %>% filter(cg_amenable == "cg") %>% 
    full_join(dstate %>% filter(cg_amenable == "total") %>% select(fed_state, n_total = n) ) %>% 
    mutate(perc = n/n_total) %>% 
    mutate(fed_state = fct_rev(fed_state))

(text <- ggplot(NULL, aes(x = 1, y = 1)) + 
  ylim(0.8, 1.2) +
  geom_text(data = percentage, aes(x = 1, y = 1, label = percent(round(perc, 2))), 
            fontface = "italic" ) +
  facet_wrap(~ fed_state, dir = "v", ncol = 1) +
  theme_void() +
  theme(strip.text = element_blank(), panel.spacing.y = unit(0, "pt"),
        axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank(),
        panel.grid.major = element_blank()))

# pie plot
dstate <- d %>% 
  group_by(fed_state) %>% 
  count(cg_amenable) %>% 
  group_by(cg_amenable) %>% 
  group_by(n) %>% 
  mutate(fed_state = factor(fed_state, levels=fed_state))

(pies <- dstate %>% 
  mutate(fed_state = fct_rev(fed_state)) %>%  
  ggplot(aes(x = "", y = n, fill = cg_amenable, width = 0.15)) + 
  geom_bar(stat = "identity", position = position_fill(), show.legend = FALSE, size = 0.1) +
  coord_polar(theta = "y", direction = -1) +
  facet_wrap(~ fed_state, dir = "v", ncol = 1)  +
  scale_fill_manual("", values = c("cg" = "#ea5f47", "not_cg" = "grey90")) +
  labs(x = NULL, y = NULL) +
  theme_void() +
  theme(strip.text = element_blank(), panel.spacing.y = unit(0, "pt"),
        axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank(),
        panel.grid.major = element_blank())) 



# cg amenable species per red list category -------------------------------
d <- read_csv("Data-outputs/naturadb/naturadb_redlist_fed_states_all_species.csv")
d <- d %>% filter(!grepl("ssp.",species_cleaned))
# reduce data frame to relevant columns
rl_catsn <- d %>% select(fed_state, species_cleaned, rl_cat, naturadb_common_name) %>% 
  distinct %>% 
  filter(naturadb_common_name != "not in naturadb") %>% 
  select(-naturadb_common_name) %>% distinct %>% 
  group_by(fed_state) %>%
  count(rl_cat)

rl_catsn <- rl_catsn %>% group_by(fed_state) %>% mutate(total = sum(n)) %>% arrange(total) %>% 
  group_by(rl_cat) %>% 
  mutate(fed_state = factor(fed_state, levels=fed_state))

(rls <- ggplot(rl_catsn, aes(fed_state, n, fill = rl_cat)) +
  geom_chicklet(width = 0.75) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "0" = "#fa5437",
      "1" = "#e2654f",
      "2" = "#ca7567",
      "3" = "#b3867e",
      "R" = "#ab8c86",
      "G" = "#a3918e",
      "V" = "#9b9796"
    )
  ) +
  guides(
    fill = guide_legend(nrow = 1)
  ) +
  coord_flip() +
  labs(x="", y="Number of species") +
  theme_ipsum_rc(grid="X") +
  theme(axis.text.x = element_text(color = "gray60", size = 10)) +
  theme(legend.position = "top", legend.justification = "left", axis.text.y = element_blank())+
    theme(legend.key.size = unit(.3, 'cm')))



# put everything together -------------------------------------------------
bars + pies + text + rls +
  plot_layout(widths= c(3, .5, .5, 3.5)) +
  plot_annotation(title = "Species amenable to conservation gardening",
                  theme = theme(text = element_text(family="Roboto Condensed")),
                  tag_levels = list(c('a', 'b','', 'c')))


showtext_opts(dpi=600)
ggsave(
  "Figures/figure1.png",
  height= 5.20,
  width = 8.7,
  dpi = 600,
  bg = "white"
)
showtext_opts(dpi=96)

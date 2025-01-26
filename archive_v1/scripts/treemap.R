pacman::p_load( treemapify, readr, ggplot2, showtext, tidyverse, svglite)
cvs <- read_csv("data/cvsm.csv", trim_ws = FALSE)
font_add_google("Sanchez", "sanchez")
showtext_auto()

paleta <- c( "#808080", "#CAB09A", "#BAA6A5", "#5297AC", "#A6C6C1", "#DEBFB5" )

g_cvs <- cvs %>% select(subsector, institucion, experiencia) %>% 
  filter(cvs$nombre == "gonzalo") %>% 
  group_by(institucion) %>% 
  summarise( exp = sum(experiencia),
             sub = first(subsector))

plot_cv <- ggplot(g_cvs, aes(area = exp, fill = sub, label = institucion, 
                subgroup = sub)) +
  geom_treemap()+
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(place = "bottom", grow = T, alpha = 0.5, colour =
                               "black", family = "sanchez", min.size = 0) +
  geom_treemap_text(colour = "white",family = "sanchez", place = "topleft", reflow = T) + 
  theme(legend.position = "bottom") + 
  scale_fill_manual(values = paleta) + 
  guides(fill = "none") +
  theme(panel.background = element_rect( color = "#E6E0CF"))
  #ggsave("experiencia2.svg", device = "svg", width = 30, height = 20, units = "cm")

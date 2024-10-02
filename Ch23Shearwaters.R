## packages used in many chapters
suppressPackageStartupMessages({
  library(knitr)
  library(tidyverse)
  library(ggthemes)
  library(patchwork)
  library(lubridate)
  library(tufte)
  library(ggmosaic)
  library(readxl)
  library(sf)
  library(tmap)
  library(tmaptools)
  library(GmooG)
})


## packages for the chapter
suppressPackageStartupMessages({
library(ggpcp)
library(gt)
})


theme_set(theme(plot.title = element_text(hjust = 0.5)))


## Fig 23.1 'Barcharts of five variables comparing two species,
## with Audubon\'s shearwater in brown and Galápagos shearwater in dark blue'

data(SeaBirds, package="GmooG")
seaB <- SeaBirds %>% filter(!species == "Tropical")

pu <- ggplot(seaB) +
  ylab(NULL) +
  ylim(0, 40) +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("darkorange4", "steelblue4"))
  
puG <- pu + geom_bar(aes(sex, fill = factor(species)),
                     position = position_dodge2(preserve = "single"))
puE <- pu + geom_bar(aes(eyebrows, fill = factor(species)),
                     position = position_dodge2(preserve = "single")) +
            theme(axis.text.x = element_text(angle = -45, hjust = 0.5, vjust = 0.5))
puC <- pu + geom_bar(aes(collar, fill = factor(species)),
                     position = position_dodge2(preserve = "single")) +
            theme(axis.text.x = element_text(angle = -45, hjust = 0.5, vjust = 0.5))
puS <- pu + geom_bar(aes(undertail, fill = factor(species)),
                     position = position_dodge2(preserve = "single")) +
            theme(axis.text.x = element_text(angle = -45, hjust = 0.5, vjust = 0.5))
puB <- pu + geom_bar(aes(border, fill = factor(species)),
                     position = position_dodge2(preserve = "single"))
                     
(plot_spacer() + puG + plot_spacer() + puE + plot_spacer() +
                 plot_layout(widths = c(1, 4, 1, 4.5, 1.5))) / (puC + puS + puB)


## Fig 23.2 'Parallel coordinate plot for Audubon and Galápagos shearwaters
## with three descriptive variables'

seaB <- seaB %>% mutate(undertailR = fct_rev(undertail))

seaB %>%
  pcp_select(species, collar, eyebrows, undertailR) %>%
  pcp_scale() %>%
  pcp_arrange() %>%
  ggplot(aes_pcp()) +
  geom_pcp_axes(colour = "white") +
  geom_pcp(aes(colour = species), alpha = 0.8) +
  geom_pcp_boxes(fill = "white", colour = "black", linewidth = 0.4) +
  geom_pcp_labels(fill = "white") +
  ylab(NULL) +
  xlab(NULL) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_colour_manual(values = c("darkorange4", "steelblue4"))


## Fig 23.3 'A comparison plot of Audubon and Galápagos shearwaters,
## using barcharts of `collar` faceted by `eyebrows`'

ggplot(seaB, aes(collar, fill = species)) +
  geom_bar() +
  facet_grid(rows = vars(species), cols = vars(eyebrows)) +
  scale_fill_manual(values = c("darkorange4", "steelblue4")) +
  ylab(NULL) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = -45, hjust = 0.5, vjust = 0.5))


## Fig 23.4 'Barcharts of collar faceted by eyebrows, border, and undertail
## for the Audubon and Galápagos shearwaters'

puffmosY <- ggplot(seaB, aes(collar)) +
  geom_bar(fill = "red") +
  facet_grid(rows = vars(border, undertail), cols = vars(eyebrows)) +
  ylab(NULL) +
  theme(axis.text.x = element_text(angle = -45, hjust = 0.5, vjust = 0.5)) +
  xlab(NULL)
puffmosY


## Table
seaB <- seaB %>% mutate(cluster = factor(ifelse(eyebrows == "Very pronounced" |
                                    (eyebrows == "Pronounced" & !collar == "Longdashed"),
                                    "clus1", "clus2")))
seaB %>%
  count(species, cluster, .drop = FALSE) %>%
  pivot_wider(values_from = "n", names_from = "cluster") %>%
  gt()


## Fig 23.5 'Barcharts of collar faceted by eyebrows, border, and undertail,
## coloured by species for the full dataset of three species'

ggplot(SeaBirds, aes(collar)) +
  geom_bar(aes(fill = species)) +
  facet_grid(rows = vars(border, undertail), cols = vars(eyebrows)) +
  ylab(NULL) +
  theme(legend.position = "bottom",
        legend.title = element_blank(), 
        axis.text.x = element_text(angle = -45, hjust = 0)) +
  xlab(NULL) +
  scale_fill_manual(values = c("darkorange4", "steelblue4", "springgreen4")) +
  scale_y_continuous(breaks = seq(0, 30, 15))

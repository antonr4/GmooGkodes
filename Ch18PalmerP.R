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
suppressPackageStartupMessages({library(ggpcp)
library(scales)
library(palmerpenguins)
})

theme_set(theme(plot.title = element_text(hjust = 0.5)))


## Fig 18.1'Four plots of the Palmer penguin dataset'
p2 <- penguins
p2 <- p2 %>% mutate(species = fct_recode(species, Adélie = "Adelie"))
g1 <- ggplot(p2, aes(species)) +
  geom_bar(fill = "grey70", width = 0.75) +
  ylab(NULL) +
  ylim(0, 200)
g2 <- ggplot(p2, aes(island)) +
  geom_bar(fill = "grey70", width = 0.75) +
  ylab(NULL) +
  ylim(0, 200)
g3 <- ggplot(p2, aes(body_mass_g)) +
  geom_histogram(boundary = 0, binwidth = 250, fill = "grey70") +
  xlim(2500, 6500) +
  ylab(NULL)
g4 <- ggplot(p2, aes(body_mass_g, flipper_length_mm)) +
  geom_point(colour = "grey70") +
  xlim(2500, 6500)
(g1 / g2 | g3 / g4) + plot_layout(widths = c(2, 3))


## Fig 18.2 'Gentoo penguins highlighted in all plots'
p2g <- p2 %>% filter(species == "Gentoo")
blueGentoo <- colorblind_pal()(3)[3]
g1x <- g1 + geom_bar(data = p2g, fill = blueGentoo, width = 0.75)
g2x <- g2 + geom_bar(data = p2g, fill = blueGentoo, width = 0.75)
g3x <- g3 + geom_histogram(data = p2g, boundary = 0, binwidth = 250, fill = blueGentoo)
g4x <- g4 + geom_point(data = p2g, colour = blueGentoo)
gu1 <- (g1x / g2x | g3x / g4x)
palens1X <- gu1 + plot_layout(widths = c(2, 3))
palens1X


## Fig 18.3 'Dream Island penguins highlighted in all plots'
p2d <- p2 %>% filter(island == "Dream")
g1y <- g1 + geom_bar(data = p2d, fill = "red", width = 0.75)
g2y <- g2 + geom_bar(data = p2d, fill = "red", width = 0.75)
g3y <- g3 + geom_histogram(data = p2d, boundary = 0, binwidth = 250, fill = "red")
g4y <- g4 + geom_point(data = p2d, colour = "red")
gu2 <- (g1y / g2y | g3y / g4y)
palens2X <- gu2 + plot_layout(widths = c(2, 3))
palens2X


## Fig 18.4 'Penguins weighing no more than 3 kg highlighted in all plots'
p2w <- p2 %>% filter(body_mass_g < 3010)
g1z <- g1 + geom_bar(data = p2w, fill = "chocolate3", width = 0.75)
g2z <- g2 + geom_bar(data = p2w, fill = "chocolate3", width = 0.75)
g3z <- g3 + geom_histogram(data = p2w, boundary = 0, binwidth = 250, fill = "chocolate3")
g4z <- g4 + geom_point(data = p2w, colour = "chocolate3")
gu3 <- (g1z / g2z | g3z / g4z)
palens3X <- gu3 + plot_layout(widths = c(2, 3))
palens3X


## Fig 18.5 'Penguins weighing no more than 3 kg plotted beside the rest,
## comparing bill lengths and bill depths'
p2 <- p2 %>% mutate(wt = ifelse(body_mass_g < 3010, "light", "others"))
cc <- ggplot(p2 %>% filter(!(is.na(wt)))) +
  xlab(NULL) +
  coord_flip() +
  theme(legend.position = "none") +
  scale_colour_manual(values = c("chocolate3", "grey70"))
c1y <- cc + geom_boxplot(aes(wt, bill_length_mm, colour = wt), varwidth = TRUE)
c2y <- cc + geom_boxplot(aes(wt, bill_depth_mm, colour = wt), varwidth = TRUE)
c1y / c2y


## Fig 18.6 'Dream Island penguins plotted beside the rest,
## comparing bill lengths and bill depths'
p2 <- p2 %>% mutate(DreamI = ifelse(island == "Dream", "Dream", "Other"))
dd <- ggplot(p2 %>% filter(!(is.na(wt)))) +
  xlab(NULL) +
  coord_flip() +
  theme(legend.position = "none") +
  scale_colour_manual(values = c("red", "grey70"))
d1y <- dd + geom_boxplot(aes(DreamI, bill_length_mm, colour = DreamI), varwidth = TRUE)
d2y <- dd + geom_boxplot(aes(DreamI, bill_depth_mm, colour = DreamI), varwidth = TRUE)
d1y / d2y


## Fig 18.7 'Parallel coordinate plot of the Palmer Penguin data'
PalX <- p2 %>%
  pcp_select(species:body_mass_g) %>%
  pcp_scale() %>%
  pcp_arrange() %>%
  ggplot(aes_pcp()) +
  geom_pcp_axes(colour = "white") +
  geom_pcp(aes(colour = species)) +
  geom_pcp_boxes(fill = "white") +
  xlab(NULL) +
  ylab(NULL) +
  theme(legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  geom_pcp_labels(fill = "white", alpha = 1) +
  scale_colour_colorblind()
PalX


## Fig 18.8 'Reordered parallel coordinate plot of the Palmer Penguin data'
p2 <- p2 %>%
  mutate(Species = factor(species, levels = c("Chinstrap", "Adélie", "Gentoo")),
         Island = factor(island, levels = c("Dream", "Torgersen", "Biscoe")),
         r_bill_depth = 25 - bill_depth_mm)
colblindP <- c(colorblind_pal()(3)[c(2, 1, 3)])
PalY <- p2 %>%
  pcp_select(Island, Species, r_bill_depth,
             flipper_length_mm, body_mass_g, bill_length_mm) %>%
  pcp_scale() %>%
  pcp_arrange() %>%
  ggplot(aes_pcp()) +
  geom_pcp_axes(colour = "white") +
  geom_pcp(aes(colour = Species)) +
  geom_pcp_boxes(fill = "white") +
  xlab(NULL) +
  ylab(NULL) +
  theme(legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  geom_pcp_labels(fill = "white", alpha = 1) +
  scale_colour_manual(values = colblindP)
PalY


## Fig 18.9 'Scatterplots of bill depth with bill length and flipper length'
p2 <- p2 %>%
  mutate(SpeciesGCA = factor(species, levels = c("Gentoo", "Chinstrap", "Adélie")))
colblindPgca <- c(colorblind_pal()(3)[c(3, 2, 1)])
pscat1 <- ggplot(p2, aes(bill_depth_mm, bill_length_mm, colour = SpeciesGCA)) +
  geom_point() +
  scale_colour_manual(values = colblindPgca) +
  theme(legend.position = "bottom",
        legend.title = element_blank())
pscat2 <- ggplot(p2, aes(bill_depth_mm, flipper_length_mm, colour = SpeciesGCA)) +
  geom_point() +
  scale_colour_manual(values = colblindPgca) +
  theme(legend.position = "bottom",
        legend.title = element_blank())
pscat1 + pscat2


## Fig 18.10 'Scatterplots of bill lengths and depths of penguins
## by the three species and three islands'
ggplot(p2, aes(bill_depth_mm, bill_length_mm)) +
  geom_point(data = penguins %>% select(-island, -species),
             colour = alpha("black", 0.05)) +
  geom_point(aes(colour = Species)) +
  facet_grid(rows = vars(Species), cols = vars(fct_rev(Island))) +
  theme(legend.position = "none") +
  scale_colour_manual(values = colblindP)


## Calculating correlations used in text
cM <- penguins %>%
  group_by(species, island) %>%
  summarise(cx = cor(bill_length_mm, bill_depth_mm, use = "complete.obs"))
cl <- round(min(cM$cx), 2)
cm <- round(max(cM$cx), 2)


## Fig 18.11 'Measurements of four variables for the male (vertical axes) and
## female (horizontal axes) in each pair (green lines mark where values would be equal)'
p1 <- penguins_raw
p1 <- p1 %>% separate(`Individual ID`, c("PairID", "Gender"), sep = "A", remove = FALSE)
names(p1)[12:15] <- names(p2)[3:6]
p1$species <- p2$species
p1w <- p1 %>%
  select(-`Sample Number`, -`Individual ID`, -Gender, -Comments) %>%
  filter(!is.na(Sex)) %>%
  pivot_wider(names_from = "Sex", values_from = c(bill_length_mm:body_mass_g,
                                                `Delta 15 N (o/oo)`, `Delta 13 C (o/oo)`))
sg0 <- ggplot(p1w, aes(colour = species)) +
  scale_colour_colorblind() +
  geom_abline(intercept = 0, slope = 1, colour = "green") +
  ylab(NULL) +
  xlab(NULL) +
  theme(plot.title = element_text(vjust = 3),
        legend.position = "none")
sg1 <- sg0 + geom_point(aes(bill_length_mm_FEMALE, bill_length_mm_MALE)) +
             xlim(30, 60) +
             ylim(30, 60) +
             ggtitle("Bill length (mm)")
sg2 <- sg0 + geom_point(aes(bill_depth_mm_FEMALE, bill_depth_mm_MALE)) +
             xlim(13, 23) +
             ylim(13, 23) +
             ggtitle("Bill depth (mm)")
sg3 <- sg0 + geom_point(aes(flipper_length_mm_FEMALE, flipper_length_mm_MALE)) +
             xlim(170, 240) +
             ylim(170, 240) +
             ggtitle("Flipper length (mm)")
sg4 <- sg0 + geom_point(aes(body_mass_g_FEMALE, body_mass_g_MALE)) +
             xlim(2500, 6500) +
             ylim(2500, 6500) +
             ggtitle("Body mass (g)")
scatRy <- (sg1 + sg2) / (sg3 + sg4) + plot_layout(guides = "collect") &
            theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.direction = "horizontal")
scatRy

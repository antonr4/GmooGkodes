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


theme_set(theme(plot.title = element_text(hjust = 0.5)))


## Fig 25.1 "Belfast to Southampton to Cherbourg to Queenstown
## Map:© OpenStreetMap contributors"
##  The graphic can be found Here: "pngImages/Tports.png"


## Fig 25.2 'Numbers boarding the Titanic by port'

data(TitanicPassCrew, package="GmooG")

BSCQ <- ggplot(TitanicPassCrew, aes(Joined)) +
  geom_bar(fill = "lightcoral") +
  ylab(NULL) +
  xlab(NULL)
BSCQ


## Fig 25.3 'Numbers boarding the Titanic at each port by sex'

TitanMVy <- TitanicPassCrew %>% mutate(sex = factor(Gender, levels = c("Male", "Female")))

ggplot(TitanMVy, aes(Joined)) +
  geom_bar(aes(fill = sex)) +
  ylab(NULL) +
  xlab("Boarded") +
  facet_wrap(vars(sex)) +
  scale_fill_manual(values = c("springgreen3", "purple2")) +
  guides(fill = "none")


## Fig 25.4 'Numbers boarding the Titanic by sex and by passenger or crew'

TitanMVy <- TitanMVy %>% mutate(class = fct_collapse(Area, Crew = c("Deck",
                                                                    "Engineering",
                                                                    "Victualling",
                                                                    "Restaurant")))
TitanMVy <- TitanMVy %>% mutate(passCrew = ifelse(Area %in% c("Deck",
                                                              "Engineering",
                                                              "Victualling",
                                                              "Restaurant"),
                                                              "Crew", "Passenger"))
                                                              
ggplot(TitanMVy, aes(Joined)) +
  geom_bar(aes(fill = sex)) +
  ylab(NULL) +
  xlab("Boarded") +
  facet_grid(cols = vars(sex),
             rows = vars(passCrew)) +
  scale_fill_manual(values = c("springgreen3", "purple2")) +
  guides(fill = "none")


## Fig 25.5 'Numbers of passengers boarding the Titanic by sex and class'

ggplot(TitanMVy %>% filter(passCrew == "Passenger"), aes(Joined)) +
  geom_bar(aes(fill = sex)) +
  ylab(NULL) +
  xlab("Boarded") +
  facet_grid(cols = vars(sex),
             rows = vars(Group)) +
  scale_fill_manual(values = c("springgreen3", "purple2")) +
  guides(fill = "none")


## Fig 25.6 'Nationality of passengers on the Titanic by port of boarding'

TitanMVp <- TitanMVy %>%
  filter(passCrew == "Passenger") %>%
  mutate(country = fct_lump_min(Nationality, 26))
ggplot(TitanMVp, aes(country)) +
  geom_bar(aes(fill = country)) +
  facet_wrap(vars(Joined), nrow = 1) +
  ylab(NULL) +
  xlab(NULL) +
  theme(legend.position = "none") +
  coord_flip()


## Fig 25.7 'Nationality and class of passengers on the Titanic'

ggplot(TitanMVp, aes(Area)) +
  geom_bar(aes(fill = country)) +
  facet_wrap(vars(country)) +
  ylab(NULL) +
  xlab(NULL) +
  theme(legend.position = "none")


## Fig 25.8 'Titanic survival rates by passenger class and crew department'

titanCX <- ggplot(data = TitanMVy) +
  geom_mosaic(aes(x = product(Area), fill = survived),
              divider = ddecker(), offset = 0.0025) +
  theme(panel.background = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank()) +
  labs(x = "", y = "") +
  scale_fill_manual(values = c("red", "grey70")) +
  scale_x_productlist(expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = -60))
titanCX


## Fig 25.9 'Survival rates by sex and class for the passengers'

TitanMVz <- TitanMVy %>%
  filter(passCrew == "Passenger") %>%
  mutate(area = fct_drop(Area))
  
titanGPC <- ggplot(data = TitanMVz) +
  geom_mosaic(aes(x = product(area, Gender), fill = Gender, alpha = survived),
              divider = ddecker(), offset = 0.0025) +
  theme(panel.background = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = -60)) +
  labs(x = "", y = "") +
  scale_fill_manual(values = c("purple2", "springgreen3")) +
  scale_alpha_manual(values = c(.9, .2)) +
  scale_x_productlist(expand = c(0, 0))
titanGPC


## Fig 25.10 'Density estimates of age by class on the Titanic'

TitanMVy <- TitanMVy %>% mutate(GroupX = ifelse(Group %in% c("Deck Crew",
                                                             "Engineering Crew",
                                                             "Victualling Crew",
                                                             "Restaurant Staff"),
                                                             "Crew", Group))
colblindT <- colorblind_pal()(8)[c(8, 3, 7, 6)]

ggplot(TitanMVy, aes(Age)) +
  stat_density(aes(colour = GroupX), geom = "line", position = "identity") +
  xlab(NULL) +
  ylab(NULL) +
  scale_colour_manual(values = colblindT) +
  theme(legend.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  geom_vline(xintercept = 14, linetype = "dashed", colour = "brown")


## Fig 25.11 'Survival rates by age'

TitanMVy <- TitanMVy %>%
  group_by(Age) %>%
  mutate(aRate = sum(survived == "yes") / n())
  
ggplot(TitanMVy %>% filter(!is.na(Age)), aes(Age, aRate)) +
  geom_count() +
  geom_smooth() +
  xlab(NULL) +
  ylab(NULL) +
  theme(legend.position = "none")


## Fig 25.12 'Survival rates by age, sex, and class or crew'

TitanMVy <- TitanMVy %>%
  group_by(Age, GroupX, sex) %>%
  mutate(acsRate = sum(survived == "yes") / n())
  
ggplot(TitanMVy %>% filter(!is.na(Age)), aes(Age, acsRate, group = sex, colour = sex)) +
  geom_count() +
  geom_smooth(method = "gam") +
  scale_colour_manual(values = c("springgreen3", "purple2")) +
  facet_grid(rows = vars(GroupX)) +
  xlab(NULL) +
  ylab(NULL) +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0, 1, 0.5))

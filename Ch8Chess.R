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
library(ggridges)
library(ChessGmooG)
})

theme_set(theme(plot.title = element_text(hjust = 0.5)))


## Fig 8.1 'Histogram of ratings of all chess players rated by FIDE in December 2020'

data(Chess2020, package="ChessGmooG")

ggplot(Chess2020, aes(DEC20)) +
  geom_histogram(binwidth = 25, boundary = 0, fill = "rosybrown2") +
  ylim(0, 10000) +
  xlab("Ratings") +
  ylab(NULL)


## Fig 8.2 'Boxplot of ratings of all FIDE rated chess players in December 2020'

ggplot(Chess2020, aes("", DEC20)) +
  geom_boxplot(outlier.colour = "red") +
  coord_flip() +
  xlab(NULL) +
  ylab("Ratings") +
  theme(axis.ticks.y = element_blank())


## Fig 8.3 'Histograms of ratings for active and inactive players'

# Fix birthyear and age data
Chess2020 <- Chess2020 %>% mutate(Byear = as.numeric(Bday))
Chess2020 <- Chess2020 %>% mutate(Byear = ifelse(Byear > 0, Byear, NA))
Chess2020 <- Chess2020 %>% mutate(NoAge = ifelse(is.na(Byear), "No age", "Age"))
Chess2020 <- Chess2020 %>% mutate(Age = 2020 - Byear)
Chess2020 <- Chess2020 %>% mutate(AgeX = ifelse(Age < 110, Age, NA))

# Construct a variable for inactive from Flag
Chess2020 <- Chess2020 %>% mutate(inactive = ifelse(Flag %in% c("i", "wi"),
                                                    "inactive", "active"))
Chess2020A <- Chess2020 %>% filter(!(Flag %in% c("i", "wi")))

actInact <- ggplot(Chess2020, aes(DEC20)) +
  geom_histogram(binwidth = 10, boundary = 0, aes(fill = inactive)) +
  facet_wrap(vars(inactive)) +
  xlab("Ratings") +
  ylab(NULL) +
  ylim(0, 2000) +
  scale_fill_manual(values = c("royalblue1", "darkgrey")) +
  guides(fill = "none")
actInact


## Fig 8.4 'Density estimates of distributions of ratings
## for active and inactive players'

eloD <- ggplot(Chess2020, aes(DEC20, group = fct_rev(inactive))) +
  geom_density(aes(colour = inactive), key_glyph = draw_key_smooth, linewidth = 2) +
  theme(legend.position = "bottom",
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  labs(colour = "", y = NULL, x = "Ratings") +
  coord_cartesian(ylim = c(0, 0.0012)) +
  scale_colour_manual(values = c("royalblue1", "darkgrey"))
eloD


## Fig 8.5 'Density estimates of age distributions for active and inactive players'

ggplot(Chess2020, aes(Age, group = fct_rev(inactive))) +
  geom_density(aes(colour = inactive), key_glyph = draw_key_smooth, linewidth = 2) +
  theme(legend.position = "bottom",
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  labs(colour = "", y = NULL, x = "Age") +
  scale_colour_manual(values = c("royalblue1", "darkgrey")) +
  scale_x_continuous(breaks = seq(5, 95, 10))


## Fig 8.6 = 'Boxplots of ratings for active players by sex
## (boxplot widths are proportional to the square roots of the sizes of the groups
## and outliers are drawn in red)'

Chess2020A <- Chess2020A %>% mutate(sex = factor(Sex, levels = c("M", "F"),
                                                      labels = c("Male", "Female")))
                                                      
ggplot(Chess2020A, aes(sex, DEC20)) +
  geom_boxplot(aes(colour = sex, fill = sex), outlier.colour = "red", outlier.shape = 20,
                                              alpha = 0.5, varwidth = TRUE) +
  coord_flip() +
  ylab("Rating") +
  xlab(NULL) +
  scale_x_discrete(labels = c("M" = "Male", "F" = "Female")) +
  theme(legend.position = "none") +
  scale_colour_manual(values = c("springgreen3", "purple2")) +
  scale_fill_manual(values = c("springgreen3", "purple2"))


## Fig 8.7 'Ratings of the top 1000 male and top 1000 female players in December 2020
## with line widths proportional to group sizes'

np <- 1000
zChess1000 <- Chess2020A %>%
  group_by(sex) %>%
  slice_max(DEC20, n = np) %>%
  slice_head(n = np) %>%
  mutate(ordID = 1:np)
zChess1000w <- zChess1000 %>%
  select(ordID, sex, DEC20) %>%
  pivot_wider(names_from = sex, values_from = DEC20)
  
ggplot(zChess1000, aes(ordID, DEC20, group = sex)) +
  geom_line(aes(colour = sex, linewidth = Sex)) +
  xlab(NULL) +
  ylab(NULL) +
  theme(legend.position = "none") +
  ylim(0, 3000) +
  scale_linewidth_manual(values = c(0.5, 4.5)) +
  scale_colour_manual(values = c("springgreen3", "purple2"))


## Fig 8.8 'Differences between the ratings of the top 1000 male
## and top 1000 female players in December 2020'

ggplot(zChess1000w, aes(ordID, (Male - Female))) +
  geom_line() +
  xlab(NULL) +
  ylab(NULL) +
  ylim(0, 500)


## Fig 8.9 'Density estimates of age distributions of active players
## (females in purple, line widths proportional to group sizes)'

ggplot(Chess2020A %>% filter(!is.na(AgeX)), aes(Age, group = sex)) +
  geom_density(aes(colour = sex, linewidth = Sex)) +
  xlab("Age") +
  ylab(NULL) +
  scale_linewidth_manual(values = c(0.5, 4.5)) +
  theme(legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  scale_x_continuous(breaks = seq(5, 95, 10)) +
  scale_colour_manual(values = c("springgreen3", "purple2"))


## Fig 8.10 'Age distributions of active players (females in purple)'

sexAgePP <- ggplot(Chess2020A %>% filter(!is.na(AgeX)), aes(Age)) +
  geom_histogram(aes(fill = sex), breaks = seq(5, 95)) +
  xlab("Age") +
  ylab(NULL) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(5, 95, 10)) +
  facet_grid(cols = vars(sex)) +
  scale_fill_manual(values = c("springgreen3", "purple2"))
sexAgePP


## Fig 8.11 nationB, fig.height=4, fig.cap = 'Numbers of active rated players by country'

data(ChessCountries, package="ChessGmooG")

cols <- c(rgb(224, 148, 92, maxColorValue = 255),
          rgb(209, 65, 106, maxColorValue = 255),
          rgb(92, 160, 93, maxColorValue = 255),
          rgb(149, 99, 152, maxColorValue = 255),
          rgb(116, 116, 117, maxColorValue = 255),
          rgb(75, 142, 187, maxColorValue = 255),
          rgb(249, 207, 84, maxColorValue = 255))
          
ChCtry <- ggplot(ChessCountries, aes(C1a, weight = fT)) +
  geom_bar(aes(fill = Region)) +
  xlab(NULL) +
  ylab(NULL) +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = cols)
ChCtry1 <- ChCtry + coord_flip()
ChCtry1
ChCtry2 <- ChCtry + theme(legend.position = "none",
                          axis.text.x = element_text(angle = -30, hjust = 0))


## Fig 8.12 'Grandmasters and active rated players per million population,
## where circle areas are proportional to country population sizes'

ChessCountries <- ChessCountries %>% filter(!(Population == "NaN"))

ggplot(ChessCountries %>% filter(Population > 1000000), aes(fT, GM)) +
  geom_point(aes(size = Population, colour = Region)) +
  theme(legend.position = "bottom",
        legend.title = element_blank(), 
        axis.title.y = element_text(angle = 0)) +
  scale_colour_manual(values = cols) +
  xlab("Active rated players") +
  ylab("GM's   ") +
  guides(size = "none", colour = guide_legend(nrow = 2)) +
  scale_size_area(max_size = 15)


## Fig 8.13 'Chess ratings of active players for countries with over 3000 active'

chC <- left_join(Chess2020A, ChessCountries, by = c("Fed" = "Fed1"))

zChessAF3 <- chC %>%
  group_by(Fed) %>%
  mutate(fT = n()) %>%
  ungroup() %>%
  filter(fT > 3000) %>%
  mutate(Fed1 = fct_reorder(Fed, fT)) %>%
  mutate(Fed2 = fct_reorder(Fed, DEC20, median))
  
ggplot(zChessAF3, aes(Fed2, DEC20)) +
  geom_boxplot(varwidth = TRUE, outlier.colour = "red", aes(fill = Region)) +
  ylab(NULL) +
  xlab(NULL) +
  theme(legend.position = "none") +
  scale_fill_manual(values = cols[c(2, 4, 5, 6)])


## Loading and wrangling ratings from December 2015

data(Chess2015, package="ChessGmooG")

# Fix birthyear and age data
Chess2015 <- Chess2015 %>% mutate(Byear = as.numeric(Bday))
Chess2015 <- Chess2015 %>% mutate(Byear = ifelse(Byear > 0, Byear, NA))
Chess2015 <- Chess2015 %>% mutate(NoAge = ifelse(is.na(Byear), "No age", "Age"))
Chess2015 <- Chess2015 %>% mutate(Age = 2015 - Byear)
Chess2015 <- Chess2015 %>% mutate(AgeX = ifelse(Age < 110, Age, NA))

# Construct a variable for inactive from Flag
Chess2015 <- Chess2015 %>% mutate(inactive = ifelse(Flag %in% c("i", "wi"),
                                                                "inactive", "active"))

# Amend variable for Federation
Chess2015 <- Chess2015 %>% mutate(Fed = ifelse(Fed == "LIB", "LBN", Fed))
Chess2015 <- Chess2015 %>% mutate(Fed = ifelse(Fed == "SIN", "SGP", Fed))
Chess2015A <- Chess2015 %>% filter(!(Flag %in% c("i", "wi")))


## Fig 8.14 'Density estimates of distributions of ratings
## in December 2015 and December 2020'

ggplot(Chess2020A, aes(DEC20)) +
  geom_density(colour = "royalblue1") +
  geom_density(data = Chess2015A, aes(DEC15), colour = "darkorange") +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  labs(y = NULL, x = "Ratings of active players in 2015 (orange) and 2020 (blue)")


## Fig 8.15 'Density estimates of active players\' ages in 2015 and 2020'

ggplot(Chess2020A, aes(Age)) +
  geom_density(colour = "royalblue1") +
  geom_density(data = Chess2015A, aes(AgeX + 1), colour = "darkorange") +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  labs(y = NULL, x = "Ages of active players in 2015 (orange) and 2020 (blue)") +
  scale_x_continuous(breaks = seq(5, 95, 10))


## Fig 8.16 'Rating by age for active players in 2015
## (an alpha value of 0.05 has been used)'

ggplot(Chess2015A, aes(AgeX, DEC15)) +
  geom_point(alpha = 0.05) +
  labs(y = NULL, x = NULL) +
  scale_x_continuous(breaks = seq(5, 95, 10))


## Fig 8.17 'Rating by age for active players in 2015
## (players with ratings over 2700 are coloured red and
## an alpha value of 0.01 has been used for the others)'

ratAY <- ggplot(Chess2015A, aes(AgeX, DEC15)) +
  geom_point(data = Chess2015A %>% filter(DEC15 < 2701), alpha = 0.01) +
  geom_point(data = Chess2015A %>% filter(DEC15 > 2700), colour = "red") +
  labs(y = NULL, x = NULL)
ratAY


## Fig 8.18 'Rating by age for a 10\\% sample of active players in 2015
## with a nonlinear smooth and confidence band'

set.seed(122015)
Chess2015As <- Chess2015A %>% sample_frac(0.1)

ggplot(Chess2015As, aes(AgeX, DEC15)) +
  geom_point(alpha = 0.1) +
  ylab(NULL) +
  xlab("Age") +
  geom_smooth(colour = "darkorange")


## Fig 8.19 'Boxplots of ratings by age for active players in 2015'

ratAX <- ggplot(Chess2015A %>% filter(AgeX < 91, !is.na(AgeX)),
                aes(AgeX, DEC15, group = AgeX)) +
  geom_boxplot(outlier.colour = "red", colour = "darkorange") +
  ylab(NULL) +
  xlab("Age") +
  scale_x_continuous(breaks = seq(5, 95, 10))
ratAX


## Fig 8.20 'Ridgeplots of ratings for active players in 2015
## for ages from 15 to 65 in 5 year jumps'

ggplot(Chess2015A %>% filter(AgeX %in% seq(15, 65, 5)),
       aes(x = DEC15, y = as.factor(AgeX), height = after_stat(density))) +
  geom_density_ridges(stat = "density", rel_min_height = 0.01, fill = "darkorange") +
  ylab(NULL) +
  xlab("Rating")


## Fig 8.21 'Rating by age for active players in 2015 and 2020'

ar1 <- ggplot(Chess2015A, aes(AgeX, DEC15)) +
  geom_point(alpha = 0.01) +
  geom_smooth(colour = "darkorange") +
  labs(y = NULL, x = NULL) +
  scale_x_continuous(breaks = seq(5, 95, 10))
ar2 <- ggplot(Chess2020A, aes(Age, DEC20)) +
  geom_point(alpha = 0.01) +
  geom_smooth(colour = "royalblue1") +
  labs(y = NULL, x = NULL) +
  scale_x_continuous(breaks = seq(5, 95, 10))
ar1 + ar2


## Fig 8.22 'Smooths for ratings by age for active players in 2015 and 2020'

ags1520 <- ggplot(Chess2015A, aes(AgeX, DEC15)) +
  geom_smooth(colour = "darkorange") +
  geom_smooth(data = Chess2020A, aes(Age, DEC20), colour = "royalblue1") +
  labs(y = NULL, x = "Ratings by age for 2015 (orange) and 2020 (blue)") +
  coord_cartesian(ylim = c(1000, 2800)) +
  scale_x_continuous(breaks = seq(5, 95, 10))
ags1520


## Construct datasets of players rated only in 2015, only in 2020, rated in both years
 
zCh1520f <- full_join(Chess2015, Chess2020, by = "ID_Number")
zCh15left <- zCh1520f %>% filter(is.na(Fed.y))
zCh20new <- zCh1520f %>% filter(is.na(Fed.x))
zCh1520both <- zCh1520f %>% filter(!is.na(Fed.y) & !is.na(Fed.x))


## Fig 8.23 'Numbers of active titled players and their ratings by age'

Ch0 <- Chess2020A %>% filter(Byear > 0)
Ch0 <- Ch0 %>% mutate(Title = fct_relevel(factor(Tit), "CM", "FM", "IM", "GM",
                                                       "WCM", "WFM", "WIM", "WGM"))
                                                       
b1 <- ggplot(Ch0 %>% filter(!is.na(Title)), aes(Title)) +
  geom_bar(aes(fill = Title)) +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab(NULL)
b2 <- ggplot(Ch0 %>% filter(!is.na(Title)), aes(Age, DEC20)) +
  geom_point(aes(colour = Title)) +
  facet_wrap(vars(Title), nrow = 2) +
  xlab("Age") +
  ylab(NULL) +
  theme(legend.position = "none")
b1 / b2 + plot_layout(heights = c(1, 2))

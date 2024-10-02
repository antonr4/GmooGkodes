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
  library(gt)
  library(FilmsGmooG)
})

theme_set(theme(plot.title = element_text(hjust = 0.5)))
options(scipen = 999)


## Prepare data

data(films22, package="FilmsGmooG")

films22 <- films22 %>% separate(genres, c("gen1", "gen2", "gen3"),
                                sep = ",", remove = FALSE)
films22 <- films22 %>% mutate(ng = ifelse(is.na(gen1), 0, 1) +
                                   ifelse(is.na(gen2), 0, 1) + 
                                   ifelse(is.na(gen3), 0, 1))
films22 <- films22 %>%
  group_by(genres) %>%
  mutate(mm = n()) %>%
  ungroup()


## Fig 3.1 'Movie runtimes in weeks'

ggplot(films22, aes("", runtime / 10080)) +
  geom_boxplot() +
  coord_flip() +
  ylab(NULL) +
  xlab(NULL) +
  theme(axis.ticks.y = element_blank())


## Fig 3.2 'Distribution of movie runtimes in minutes for films of 3 hours or under'

films22s <- films22 %>% filter(runtime < 181)

f22s <- ggplot(films22s, aes(runtime)) +
  geom_histogram(boundary = 0, binwidth = 1, closed = "left", fill = "gold3") +
  ylab(NULL) +
  xlab(NULL) +
  scale_x_continuous(breaks = seq(0, 180, 30))
f22s


## Fig 3.3 'Movie runtimes in minutes by year of production
## for films of 3 hours or under using alpha=0.05'

ggplot(films22s, aes(year, runtime)) +
  geom_point(colour = "gold3", alpha = 0.05) +
  ylab(NULL) +
  xlab(NULL) +
  scale_x_continuous(breaks = seq(1870, 2020, 10))


## Fig 3.4 'Movie runtimes in minutes (above),
## number of movies by year (below) for films of 3 hours or under'

yb1 <- ggplot(films22s, aes(as.factor(year), runtime)) +
  geom_boxplot(fill = "gold3", colour = "gold3", outlier.size = 1,
               outlier.colour = "grey70", outlier.alpha = 0.3) +
  ylab(NULL) +
  xlab(NULL) +
  scale_y_continuous(breaks = seq(0, 180, 30), limits = c(0, 180)) +
  scale_x_discrete(breaks = seq(1870, 2020, 10))
  
yb2 <- ggplot(films22s, aes(year)) +
  geom_histogram(fill = "gold3", binwidth = 1) +
  scale_x_continuous(breaks = seq(1870, 2020, 10)) +
  ylab(NULL) +
  xlab(NULL)
  
yb1 / yb2


## Fig 3.5 'IMDb code numbers by year for films and shorts with over 100 votes'

ggplot(films22, aes(year, nr)) +
  geom_point(colour = "gold3") +
  ylab(NULL) +
  xlab(NULL)


## Fig 3.6 'IMDb code numbers by year for all items with ratings
## and with code numbers less than 1,000,000'
## The IMDb files to prepare this image were very large, so they are not included.
## The graphic can be found Here: "pngImages/movieTT2.png"


## Fig 3.7 'Average user ratings for movies with over 100 ratings'

ggplot(films22, aes(averageRating)) +
  geom_histogram(breaks = seq(1, 10, 0.2), fill = "darkorange1") +
  ylab(NULL) +
  xlab(NULL)


## Fig 3.8 'Numbers of votes for each movie on a log base 10 scale'

ggplot(films22, aes(numVotes)) +
  geom_histogram(bins = 50, fill = "royalblue") +
  ylab(NULL) +
  xlab(NULL) +
  scale_x_log10()


## Fig 3.9 'Average user rating by number of votes'

vr <- ggplot(films22, aes(numVotes / 1000000, averageRating)) +
  geom_point(colour = "darkorange1") +
  ylab(NULL) +
  xlab("Number of votes in millions") +
  xlim(-0.25, 3)
vr


## Fig 3.10 'User ratings by number of votes with features marked by boxes'

vr1 <- vr + annotate("rect", xmin = 1, xmax = 2.7, ymin = 1, ymax = 7.5,
                     colour = "green", alpha = 0, linewidth = 1.5)
vr2 <- vr + annotate("rect", xmin = 1, xmax = 2.7, ymin = 9.8, ymax = 7.6,
                     colour = "darkblue", alpha = 0, linewidth = 1.5)
vr3 <- vr + annotate("rect", xmin = -0.1, xmax = 0.2, ymin = 9.4, ymax = 10.2,
                     colour = "darkmagenta", alpha = 0, linewidth = 1.5)
vr4 <- vr + annotate("rect", xmin = 0.22, xmax = 0.35, ymin = 3.4, ymax = 4.4,
                     colour = "dodgerblue", alpha = 0, linewidth = 1.5)
vr1 / vr2 / vr3 / vr4


## Table of movie genres

gX <- films22 %>%
  count(genres) %>%
  arrange(-n)
gX <- gX %>% mutate(id = seq(1, dim(gX)[1]),
                    cn = cumsum(n),
                    cp = 100 * cn / dim(films22)[1])
gX <- gX %>% mutate(cumPerc = round(cp, 1))
gX %>%
  slice_head(n = 20) %>%
  select(genres, cumPerc) %>%
  gt()


## Fig 3.11 'Percentage of films by number of combinations of genres,
## almost 80\\% of films are covered by the first 100 combinations (dotted line)'

ggplot(gX, aes(id, cp)) +
  geom_line() +
  ylab(NULL) +
  xlab(NULL) +
  scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 100)) +
  geom_vline(xintercept = 100, linetype = "dotted")


## Fig 3.12 'Percentages of films using the genre descriptors Drama, Comedy, Romance'

films22 <- films22 %>%
  group_by(year) %>%
  mutate(nt = n())
gfilms22 <- films22 %>% separate_rows(genres, sep = ",")
gfilms22 <- gfilms22 %>%
  group_by(genres) %>%
  mutate(ng = n())
gfilms22 <- gfilms22 %>%
  group_by(genres, year) %>%
  mutate(gn = n(), pg = 100 * gn / nt) %>%
  ungroup()
gfilms22 <- gfilms22 %>% mutate(Genres = fct_reorder(genres, gn, max, .desc = TRUE))

# Define colour palette for the genres
colFilms <- c("darkgreen", "blue", "pink", "lightblue", "darkolivegreen2",
"red", "firebrick1", "black", "blue", "brown", "purple", "darkorange2",
"chartreuse1", "hotpink", "deepskyblue", "coral", "indianred2", "cyan",
"lightgreen", "deeppink2", "burlywood3", "cyan1")
names(colFilms) <- levels(gfilms22$Genres)[c(1:21, 24)]

hfilms22 <- gfilms22 %>% filter(year > 1900, year < 2020)

ggplot(hfilms22 %>% filter(Genres %in% c("Drama", "Comedy", "Romance")),
                           aes(year, pg, group = Genres, colour = Genres)) +
  geom_line() +
  ylab(NULL) +
  xlab(NULL) +
  scale_colour_manual(values = colFilms) +
  theme(legend.title = element_blank())


## Fig 3.13 = 'Number of films by year from 1901 to 2019,
## faceted by genre in order of highest number per year,
## a dotted line marks the introduction of sound in 1927
## and vertical scales are different for each row'

g0 <- ggplot() +
  ylab(NULL) +
  xlab(NULL) +
  geom_vline(xintercept = 1927, linetype = "dotted") +
  scale_x_continuous(breaks = c(1900, 1960, 2020)) +
  theme(axis.text.x = element_blank()) +
  ylab(NULL) +
  xlab(NULL) +
  guides(colour = "none") +
  scale_colour_manual(values = colFilms)
  
gL1 <- g0 + geom_line(data = hfilms22 %>%
                      filter(Genres %in% c("Comedy", "Drama")),
                      aes(year, gn, colour = Genres), linewidth = 1.25) +
                      facet_wrap(vars(Genres))
gL2 <- g0 + geom_line(data = hfilms22 %>%
                      filter(Genres %in% c("Thriller", "Horror", "Documentary", "Action")),
                      aes(year, gn, colour = Genres), linewidth = 1.25) +
                      facet_wrap(vars(Genres), nrow = 1)
gL3 <- g0 + geom_line(data = hfilms22 %>%
                      filter(Genres %in% c("Romance", "Crime", "Adventure", "Mystery")),
                      aes(year, gn, colour = Genres), linewidth = 1.25) +
                      facet_wrap(vars(Genres), nrow = 1)
gL4 <- g0 + geom_line(data = hfilms22 %>%
                      filter(Genres %in% c("Short", "Biography", "Family",
                                           "Sci-Fi", "Fantasy", "Animation")),
                      aes(year, gn, colour = Genres), linewidth = 1.25) +
                      facet_wrap(vars(Genres), nrow = 1)
gL5 <- g0 + geom_line(data = hfilms22 %>%
                      filter(Genres %in% c("History", "Music", "Sport",
                                           "War", "Western", "Musical")),
                      aes(year, gn, colour = Genres), linewidth = 1.25) +
                      facet_wrap(vars(Genres), nrow = 1)
layout <- "
AA####
BBBB##
CCCC##
DDDDDD
EEEEEE
"
gL1 + gL2 + gL3 + gL4 + gL5 + plot_layout(design = layout)


## Fig 3.14 = 'User ratings of two films'

data(RBSS, package="FilmsGmooG")

RBa <- sum((RBSS %>% filter(Film %in% c("Raging Bull")))$Freq)
SSa <- sum((RBSS %>% filter(Film %in% c("Sense & Sensibility")))$Freq)

data(RBSSr, package="FilmsGmooG")

RBs <- sum((RBSSr %>% filter(Film %in% c("Raging Bull")))$Freq)
SSs <- sum((RBSSr %>% filter(Film %in% c("Sense & Sensibility")))$Freq)

ggplot(RBSS, aes(factor(Rating), weight = Freq)) +
  geom_bar(aes(fill = Film)) +
  facet_wrap(vars(Film), scale = "free_y") +
  guides(fill = "none") +
  ylab(NULL) +
  xlab(NULL) +
  scale_fill_manual(values = c("orangered3", "powderblue"))


## Fig 3.15 'Average ratings of two films by user age and sex
## with point size proportional to number of ratings'

ggplot(RBSSr %>% filter(!Age %in% c("<18")), aes(Age, AvRating, size = Freq)) +
  geom_point(aes(colour = Film)) +
  facet_grid(cols = vars(Film, Sex)) +
  ylab(NULL) +
  guides(size = "none", colour = "none") +
  scale_size_area(max_size = 20) +
  scale_colour_manual(values = c("orangered3", "powderblue")) +
  xlab(NULL) +
  ylim(7, 8.5)

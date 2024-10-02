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
library(grid)
library(GGally)
library(ggpcp)
library(viridis)
})


theme_set(theme(plot.title = element_text(hjust = 0.5)))


## Fig 31.1 'Performances of the Nurgalieva identical twins in the
## Comrades Marathon races (same data, different vertical scales)'
## See Fig 21.13

nurg12


## Fig 31.2 'Barcharts using the default alphabetic ordering and
## the order in which the Titanic visited the ports'
## See Figure 25.2

TitanicPassCrew <- TitanicPassCrew %>% mutate(joined = as.character(Joined))

BSCP <- ggplot(TitanicPassCrew, aes(joined)) +
  geom_bar(fill = "lightcoral") +
  ylab(NULL) +
  xlab(NULL)
(BSCP + theme(axis.text = element_text(size = 8))) +
(BSCQ + theme(axis.text = element_text(size = 8)))


## Fig 31.3 'Numbers of countries and total populations in the four Gapminder regions'
## See Figs 2.7 and 2.8

reg4X + reg4Y


## Fig 31.4 'Annual sales of station wagons and SUVs in default ordering (left)
## and revised ordering (right)'
## See Fig 17.7

Veh1Fx <- Veh1f %>%
  group_by(VClass, year) %>%
  summarise(nn = n())
  
ggplot(Veh1Fx, aes(year, weight = nn)) +
  geom_bar() +
  facet_grid(rows = vars(VClass)) +
  ylab(NULL) +
  xlab(NULL) +
  theme(strip.text.y = element_text(angle = 0),
        legend.position = "none") +
  scale_y_continuous(breaks = c(0, 150))
vgf


## Fig 31.5 'Boxplots for the points in the 10 decathlon events
## in the order they take place for the top 116 decathletes in April 2021'

data(Decath21, package="GmooG")

Decath21 <- Decath21 %>% mutate(Run100m = Run100m / 100,
                                Run400m = Run400m / 100,
                                Hurdle110m = Hurdle110m / 100,
                                Run1500m = 60 * floor(Run1500m) +
                                100 * (Run1500m - floor(Run1500m)))
                                
Decath21 <- Decath21 %>% separate(Venue, c("Location", "Year"), remove = FALSE, sep = -5)

Decath21 <- Decath21 %>% mutate(`100m` = floor(25.437 * (18 - Run100m)**1.81),
                                Longjump = floor(0.14354 * (100 * LongJump - 220)**1.4),
                                Shotput = floor(51.39 * (ShotPut - 1.5)**1.05),
                                Highjump = floor(0.8465 * (100 * HighJump - 75)**1.42),
                                `400m` = floor(1.53775 * (82 - Run400m)**1.81),
                                `110mHurdles` = floor(5.74352 * (28.5 - Hurdle110m)**1.92),
                                Polevault = floor(0.2797 * (100 * PoleVault - 100)**1.35),
                                Discus = floor(12.91 * (DiscusD - 4)**1.1),
                                Javelin = floor(10.14 * (JavelinD - 7)**1.08),
                                `1500m` = floor(0.03768 * (480 - Run1500m)**1.85))
                                
Decath21 <- Decath21 %>%
  rowwise(Rank) %>%
  mutate(tot = sum(c_across(c(`100m`:`1500m`)))) %>%
  ungroup()
Decath21b <- Decath21 %>% pivot_longer(cols = `100m`:`1500m`,
                                       names_to = "Event",
                                       values_to = "Points")
Decath21b <- Decath21b %>%
             mutate(eventx = factor(Event,
                    levels = c("100m", "Longjump", "Shotput", "Highjump", "400m",
                               "110mHurdles", "Discus", "Polevault", "Javelin", "1500m")))
                               
ggplot(Decath21b, aes(eventx, Points)) +
  geom_boxplot() +
  xlab(NULL) +
  ylab(NULL)


## Fig 31.6 'Katinka Hosszu\'s best times by event (in purple) compared with
## the 200 best in each event, with events sorted by stroke and distance'
## See Fig 20.7

KHa


## Fig 31.7 'Mosaicplots of Titanic survival rates'
## See Figs 25.8 and 25.9

titanCX | titanGPC


## Fig 31.8 'Parallel coordinate plot of the top 79 ranked scores
## for each decathlon event in April 2021'

Decath21bs <- Decath21b %>%
  group_by(Event) %>%
  mutate(rEvent = rank(-Points)) %>%
  arrange(Event, rEvent)
ne <- dim(Decath21)[1]
Decath21bs <- Decath21bs %>% mutate(rrEvent = seq(1:ne))

d21bsW <- Decath21bs %>% pivot_wider(id_cols = rrEvent,
                                     names_from = Event,
                                     values_from = Points)
                                     
# Restrict to best 79
nt <- 79
mid <- (nt+1)/2

d21bsW <- d21bsW %>% mutate(across(`100m`:Shotput, ~ {
  .x - sort(.x)[(ne + 1 - mid)]
}))

Range <- function(v1) {
  max(v1) - min(v1)
}

qn <- d21bsW %>%
  filter(rrEvent < (nt+1)) %>%
  summarise(across(c(`100m`:Shotput), Range))
qn <- qn %>%
  pivot_longer(cols = everything(),
               names_to = "name",
               values_to = "Stat") %>%
  arrange(Stat)
  
d21bsW %>%
  filter(rrEvent < (nt+1)) %>%
  pcp_select("110mHurdles", Longjump, Shotput, "100m", Highjump,
             "1500m", "400m", Polevault, Discus, Javelin) %>%
  pcp_scale(method = "raw") %>%
  ggplot(aes_pcp()) +
  geom_pcp_axes() +
  geom_pcp(colour = "grey70") +
  geom_hline(yintercept = 0, colour = "brown4", lwd = 1.5) +
  ylab(NULL) +
  xlab(NULL) +
  theme(axis.text.x = element_text(angle = -45))


## Fig 31.9 'Parallel coordinate plot of the 10 decathlon events'
## Highlight best 4 decathletes of the 116

Decath21p <- Decath21 %>% mutate(Rankh = ne + 1 - rank(tot),
                                 Hil = ifelse(Rankh < 5, Rankh, 5))
decNames <- c(Decath21p$Decathlete[1:4], "others (112)")
decNames[3:4] <- c("Roman Sebrle", "Tomas Dvorak")

qn <- Decath21p %>% summarise(across(c(`100m`:`1500m`), median))
qn <- qn %>%
  pivot_longer(cols = everything(),
               names_to = "name",
               values_to = "Stat") %>%
  arrange(Stat)
  
dc21p <- Decath21p %>%
  arrange(-Hil) %>%
  pcp_select("1500m", Discus, Shotput, Javelin, Highjump,
             "400m", "100m", Polevault, "110mHurdles", Longjump) %>%
  pcp_scale(method = "raw") %>%
  ggplot(aes_pcp()) +
  geom_pcp_axes(colour = "white") +
  geom_pcp(overplot = "none", aes(colour = as.factor(Hil))) +
  ylab(NULL) +
  xlab(NULL) +
  theme(axis.text.x = element_text(angle = -45),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.key.size = unit(0.5, "cm")) +
  scale_colour_manual(values = c("red", "darkorchid3", "blue", "brown", "grey85"),
                      labels = decNames)
dc21p


## Fig 31.10 'Parallel coordinate plots of the Palmer Penguin data
## (initial and reordered)'
## See Figs 18.7 and 18.8

PalX
PalY


## Fig 31.11 'Barcharts of collar faceted by eyebrows, border, and undertail
## for the Audubon and Galápagos shearwaters'
## See Fig 23.4

puffmosY

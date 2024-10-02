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
  library(gtable)
  library(grid)
  library(GGally)
  library(ggrepel)
})

theme_set(theme(plot.title = element_text(hjust = 0.5)))


## Fig 2.1 'Life expectancy at birth in years for 187 countries
## over the years 1800 to 2016'

data(GapLifeE, package = "GmooG")

LE <- GapLifeE %>%
  select(country:`2016`) %>%
  pivot_longer(cols = `1800`:`2016`) %>%
  mutate(year = as.numeric(name))
p <- ggplot(LE, aes(year, value, group = country))
pgmX <- p + geom_line() + ylab(NULL)
pgmX


## Fig 2.2 'Life expectancy graphics:
## some sharp falls and rises marked (above), straight lines marked (below)'

p1 <- p + geom_line(alpha = 0.25) + ylab(NULL) + xlab(NULL)
p1a <- p1 + annotate("rect", xmin = 1802, xmax = 2000, ymin = 0, ymax = 15,
         colour = "red", alpha = 0, linewidth = 2)
p1b <- p1 + annotate("rect", xmin = 1798, xmax = 1925, ymin = 22, ymax = 40,
         colour = "yellow", alpha = 0, linewidth = 2)
p1a / p1b


## Fig 2.3 'Further features in the life expectancy graphic'

xp1 <- p1 + annotate("rect", xmin = 1820, xmax = 1880, ymin = 40, ymax = 55,
         colour = "green", alpha = 0, linewidth = 2)
xp2 <- p1 + annotate("rect", xmin = 1988, xmax = 2018, ymin = 6, ymax = 40,
         colour = "orange", alpha = 0, linewidth = 2)
xp3 <- p1 + annotate("rect", xmin = 1900, xmax = 1960, ymin = 20, ymax = 30,
         colour = "purple", alpha = 0, linewidth = 2)
xp4 <- p1 + annotate("rect", xmin = 1960, xmax = 1980, ymin = 60, ymax = 80,
         colour = "pink", alpha = 0, linewidth = 2)
(xp1 + xp2) / (xp3 + xp4)


## Fig 2.4 'Life expectancy for Denmark, Norway and Sweden'

ggplot(LE %>% filter(country %in% c("Denmark", "Norway", "Sweden")),
       aes(year, value, group = country)) +
  geom_line(aes(colour = country)) +
  expand_limits(y = 0) +
  ylab(NULL) +
  xlab(NULL) +
  scale_colour_colorblind() +
  theme(legend.position = "bottom",
        legend.title = element_blank())


## Fig 2.5 'Life expectancy time series coloured by the four regions classification'

data(GapRegions, package = "GmooG")

GapRegions <- GapRegions %>% 
  mutate(name = sub("Macedonia, FYR", "North Macedonia", name))
rc <- full_join(GapRegions, GapLifeE, by = c("name" = "country"))
rcX <- rc %>%
  select(-12) %>%
  filter(!is.na(`2000`), !is.na(`1900`)) %>%
  rename(country = name)
rcLE <- rcX %>%
  select(country:`2016`) %>%
  pivot_longer(cols = `1800`:`2016`) %>%
  mutate(year = as.numeric(name))

# Define the colour palette for the regions
colsG <- c("#E0566B", "#85BF41", "#104E8B", "#4EBCCB")
names(colsG) <- c("asia", "americas", "europe", "africa")

pr4 <- ggplot(rcLE, aes(year, value, group = country)) +
  geom_line(alpha = 0.5, aes(colour = four_regions)) +
  scale_color_manual(values = colsG) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ylab(NULL) +
  xlab(NULL)
pr4


## Fig 2.6 'Life expectancy time series drawn separately
## for the four regions classification'

pr4X <- pr4 +
            facet_wrap(vars(four_regions)) +
            theme(legend.position = "none")
pr4X


## Fig 2.7 'Numbers of countries in each of the four regions, ordered by the numbers'

reg4X <- ggplot(rcX, aes(fct_infreq(four_regions))) +
  geom_bar(aes(fill = four_regions)) +
  xlab(NULL) +
  ylab(NULL) +
  ylim(0, 70) +
  guides(fill = "none") +
  scale_fill_manual(values = colsG)
reg4X


## Fig 2.8 'Total populations for each of the four regions'

data(GapPop, package = "GmooG")

pops <- GapPop %>% filter(country %in% rcX$country)
pLE <- full_join(pops %>% select(country, `1956`, `2016`),
                 rcX %>% select(country, four_regions))
reg4Y <- ggplot(pLE,
                aes(fct_reorder(four_regions, -`2016`, sum), weight = `2016` / 1000000)) +
  geom_bar(aes(fill = four_regions)) +
  xlab(NULL) +
  ylab("Population (millions)") +
  ylim(0, 5000) +
  guides(fill = "none") +
  scale_fill_manual(values = colsG)
reg4Y


## Fig 2.9 'Life expectancy time series for the six biggest countries in 2016'

pops <- pops %>%
  mutate(p2016 = `2016`) %>%
  arrange(desc(p2016)) %>%
  mutate(cx = cumsum(p2016))
p16 <- pops %>%
  select(country, p2016) %>%
  slice_max(p2016, n = 6)
rcW <- rcX %>% filter(country %in% p16$country)
rcLE16 <- full_join(p16, rcW) %>%
  select(country:`2016`) %>%
  pivot_longer(cols = `1800`:`2016`)
rcLE16 <- rcLE16 %>% mutate(year = as.numeric(name))

ggplot(rcLE16, aes(year, value, group = country)) +
  geom_line(aes(colour = country)) +
  geom_text(data = rcLE16 %>% filter(year == last(year)),
            aes(label = country, x = year + 5, y = value, colour = country), hjust = 0) +
  guides(colour = "none") +
  xlab(NULL) +
  ylab(NULL) +
  xlim(1800, 2040) +
  expand_limits(y = 0)


## Fig 2.10 'Life expectancies for the six biggest countries
## with line widths scaled by their 2016 populations'

rcLE16 <- rcLE16 %>% mutate(Country = fct_reorder(country, p2016, mean))

ggplot(rcLE16, aes(year, value, group = country)) +
  geom_line(aes(colour = country, linewidth = p2016)) +
  xlab(NULL) +
  ylab(NULL) +
  facet_wrap(vars(Country), nrow = 3) +
  guides(colour = "none", linewidth = "none") +
  expand_limits(y = 0) +
  scale_linewidth(limits = c(0, 1500000000), range = c(0, 3))


## Fig 2.11 'Proportion of the world population over time
## made up by the six biggest countries in 2016'

popX <- pops %>% mutate(across(`1800`:`2016`, ~ . / sum(.)))
popY <- popX %>%
  filter(popX$country %in% p16$country) %>%
  select(country:`2016`) %>%
  pivot_longer(cols = `1800`:`2016`)
popY <- popY %>% mutate(year = as.numeric(name))

ggplot(popY, aes(year, value, group = country)) +
  geom_line(aes(colour = country)) +
  geom_text_repel(data = popY %>% filter(year == last(year)),
                  aes(label = country, x = year + 5, y = value, colour = country),
                  nudge_x = 5, min.segment.length = 4) +
  guides(colour = "none") +
  xlab(NULL) +
  ylab(NULL) +
  xlim(1800, 2040)

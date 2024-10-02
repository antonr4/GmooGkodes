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
library(tsibble)
library(feasts)
library(ggrepel)
library(scales)
})


theme_set(theme(plot.title = element_text(hjust = 0.5)))


## Fig 24.1 'Smoothed numbers of deer-vehicle accidents per day in Bavaria
## over ten years from Jan 1st 2002 to Dec 31st 2011'

data(DVCdeer, package="GmooG")

d1 <- DVCdeer %>% mutate(date = ymd(day),
                         time = hm(mins))
d1s <- d1 %>%
  group_by(date) %>%
  summarise(N = sum(Freq))
  
ggplot(d1s, aes(date, N)) +
  geom_smooth(se = FALSE, colour = "brown4") +
  ylab(NULL) +
  xlab(NULL) +
  coord_cartesian(ylim = c(0, 120))


## Fig 24.2 'Monthly numbers of deer-vehicle accidents
## across the ten years 2002 to 2011.'

d1sT <- as_tsibble(d1s, index = date)
d1sTm <- d1sT %>%
  index_by(Year_Month = ~ yearmonth(.)) %>%
  summarise(nn = sum(N))
  
d1sTm %>% gg_subseries(y = nn, colour = "brown4") +
         xlab(NULL) +
         ylab(NULL) +
         scale_x_yearmonth(breaks = yearmonth(c("Jan 2002", "Dec 2011")),
                           labels = c("", "")) +
         theme(axis.ticks.x = element_blank()) +
         ylim(0, NA)


## Fig 24.3 'Numbers of deer-vehicle accidents across all ten years
## overlaid from Jan 1st to Dec 31st'

g6 <- ggplot(d1s, aes(date, N)) +
  geom_smooth(formula = y ~ s(x, bs = "ad", k = 60), se = FALSE, n = 3652) +
  ylab(NULL) +
  xlab(NULL)
  
lg6 <- layer_data(g6)
d1s <- d1s %>% mutate(sm60 = lg6$y)
d1ss <- d1s %>% filter(month(date) == 12 & day(date) == 31)
d1ss <- d1ss %>%
  mutate(id = factor(year(date))) %>%
  as_tibble()
end <- as.Date("1974-01-10")

as_tsibble(d1s %>% select(date, sm60)) %>%
                   gg_season(y = sm60, period = "year", labels = "none") + 
           ylab(NULL) +
           xlab(NULL) +
           scale_x_date(date_labels = "%b", date_breaks = "1 month") +
           geom_text_repel(data = d1ss, aes(label = id, y = sm60, x = end),
                           min.segment.length = 2, nudge_x = 5, direction = "y") +
           guides(colour = "none") +
           ylim(0, NA)


## Fig 24.4 'Day of the week patterns in numbers of deer-vehicle accidents'

d1s <- d1s %>% mutate(year = year(date),
                      month = month(date),
                      day = day(date),
                      wd = wday(date, label = TRUE))
colblind3 <- colorblind_pal()(8)[c(1:4, 6:8)]

ggplot(d1s, aes(date, N, group = wd)) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 30),
              aes(colour = wd), se = FALSE) +
  ylab(NULL) +
  xlab(NULL) +
  facet_wrap(vars(wd)) +
  theme(legend.position = "none") +
  scale_colour_manual(values = colblind3)


## Fig 24.5 'Average daily patterns in numbers of deer-vehicle accidents
## by month (the coloured lines) and year'

d1 <- d1 %>% mutate(year = year(date),
                    month = month(date, label = TRUE),
                    day = day(date),
                    wd = wday(date, label = TRUE))
d1dms <- d1 %>%
  group_by(year, month, mins) %>%
  summarise(mu = mean(Freq))
d1dms <- d1dms %>%
  mutate(tim = seq(0, 23.5, 0.5)) %>%
  ungroup()
  
ggplot(d1dms, aes(tim, mu)) +
  geom_line(aes(colour = month)) +
  facet_wrap(vars(factor(year)), nrow = 2) +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_continuous(breaks = seq(0, 24, 6))


## Fig 24.6 'Daily patterns in numbers of deer-vehicle accidents
## by month across the years (the coloured lines)'

ggplot(d1dms, aes(tim, mu)) +
  geom_line(aes(colour = factor(year))) +
  facet_wrap(vars(month), nrow = 2) +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_continuous(breaks = seq(0, 24, 6))


## Fig 24.7 'Smoothed number of non-deer-vehicle accidents (blue) and
## deer-vehicle accidents (brown) per day in Bavaria over ten years'

data(DVCnot, package="GmooG")

v1 <- DVCnot %>% mutate(date = ymd(day),
                        time = hm(mins))
v1s <- v1 %>%
  group_by(date) %>%
  summarise(N = sum(Freq))
  
ggplot(v1s, aes(date, N)) +
  geom_smooth(se = FALSE) +
  ylab(NULL) +
  xlab(NULL) +
  coord_cartesian(ylim = c(0, 300)) +
  geom_smooth(data = d1s, aes(date, N), se = FALSE, colour = "brown4")


## Fig 24.8 'Daily patterns in numbers of non-deer-vehicle accidents
## by month across the years'

v1 <- v1 %>% mutate(year = year(date),
                    month = month(date, label = TRUE),
                    day = day(date),
                    wd = wday(date, label = TRUE))
v1dms <- v1 %>%
  group_by(year, month, mins) %>%
  summarise(mu = mean(Freq))
v1dms <- v1dms %>%
  mutate(tim = seq(0, 23.5, 0.5)) %>%
  ungroup()
  
ggplot(v1dms, aes(tim, mu)) +
  geom_line(aes(colour = factor(year))) +
  facet_wrap(vars(month), nrow = 2) +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_continuous(breaks = seq(0, 24, 6))


## Fig 24.9 'Daily patterns for Tuesdays in numbers
## of non-deer-vehicle accidents across the years'
v1wws <- v1 %>%
  group_by(year, wd, mins) %>%
  summarise(mu = mean(Freq))
v1wws <- v1wws %>%
  mutate(tim = seq(0, 23.5, 0.5)) %>%
  ungroup()
  
ggplot(v1wws %>% filter(wd == "Tue"), aes(tim, mu)) +
  geom_line(aes(colour = factor(year))) +
  facet_wrap(vars(year), nrow = 2) +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_continuous(breaks = seq(0, 24, 6))


## Fig 24.10 'Daily patterns for Tuesdays in numbers of non-deer-vehicle accidents
## for the years 2008, 2009, 2010'

ggplot(v1wws %>%
         filter(wd == "Tue",
                year %in% c("2008", "2009", "2010")),
  aes(tim, mu)) +
  geom_line(aes(colour = factor(year))) +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_continuous(breaks = seq(0, 24, 4)) +
  scale_colour_manual(values = hue_pal()(10)[c(7:9)])

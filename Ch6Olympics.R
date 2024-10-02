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

## Fig 6.1 'Summer Olympic events by total numbers of participants in the
## Games of 1896 to 2016, ordered lexicographically (the default),
## based on the first dataset'

data(OlympicPerfs, package="GmooG")

OlyX <- OlympicPerfs
ggplot(OlyX, aes(event)) +
  geom_bar(fill = "royalblue") +
  ylab(NULL) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())


## Fig 6.2 'Summer Olympics sporting disciplines by total numbers of participants in the
## Games of 1896 to 2016, based on the first dataset'

OlyXD <- OlyX %>% count(discipline)

ggplot(OlyXD, aes(fct_reorder(discipline, n), weight = n)) +
  geom_bar(fill = "darkgoldenrod1") +
  ylab(NULL) +
  xlab(NULL) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 20000))


## Fig 6.3 'Numbers of athletics and swimming events for men and women
## at the Summer Olympics from 1896 to 2016'

# Fix location and date
OlyX <- OlyX %>% separate(col = games, into = c("city", "year"),
                          sep = -4, remove = FALSE)
OlyX <- OlyX %>% separate(col = city, into = c("city"), sep = -1)
OlyX <- OlyX %>% mutate(year = as.numeric(year))

# Remove participants who did not finish
OlyX <- OlyX %>% filter(!rank %in% c(NA, "DSQ", "DNF", "DNS"))

# Fix event
OlyX <- OlyX %>% separate(event, into = c("Event", "Sex"),
                          sep = "\\-(?!.*-)", remove = FALSE)
OlyX <- OlyX %>% filter(!Sex %in% c("mixed", "open"))
OlyX <- OlyX %>% mutate(result_type = tolower(result_type))

# Create dataset with only gold medal winners
xG <- OlyX %>% filter(medalType == "GOLD")
xGas <- xG %>% filter(discipline %in% c("athletics", "swimming"))
xGas <- xGas %>%
  group_by(Sex, year, discipline) %>%
  summarise(nn = n())
xGas <- xGas %>%
  ungroup() %>%
  complete(year = seq(1896, 2016, 4), nesting(Sex, discipline)) %>%
  filter(!is.na(discipline))
  
olyASnY <- ggplot(xGas, aes(year, nn, group = Sex, colour = Sex)) +
  geom_line() +
  facet_wrap(vars(discipline)) +
  ylab(NULL) +
  xlab(NULL) +
  scale_colour_manual(values = c("springgreen3", "purple2")) +
  theme(legend.position = "bottom",
        legend.title = element_blank())
olyASnY


## Fig 6.4 'Athletics and swimming events for men and women for which gold medals
## were awarded by Olympic year, ordered by numbers of gold medals awarded'

xGx <- xG %>%
  group_by(Event) %>%
  mutate(nn = n())
  
# Order events by numbers of golds won (men + women)
xGx <- xGx %>%
  ungroup() %>%
  mutate(eventG = fct_reorder(Event, nn))
xGAS <- xGx %>% filter(discipline %in% c("athletics", "swimming"))

ggplot(xGAS, aes(year, eventG)) +
  geom_point(aes(colour = Sex)) +
  ylab(NULL) +
  xlab(NULL) +
  facet_grid(cols = vars(Sex), rows = vars(discipline), scales = "free_y") +
  scale_x_continuous(breaks = seq(1896, 2016, 20)) +
  scale_colour_manual(values = c("springgreen3", "purple2")) +
  theme(legend.position = "none",
        panel.spacing.x = unit(1, "lines"))


## Fig 6.5 'Athletic events by Olympic year after amendments'

# Correct data typos
OlyX$event <- str_replace(OlyX$event, "-metres", "m")
OlyX$event <- str_replace(OlyX$event, "1-500m", "1500m")
OlyX$event <- str_replace(OlyX$event, "10-000m", "10000m")
OlyX$event <- str_replace(OlyX$event, "3-000m", "3000m")
OlyX$event <- str_replace(OlyX$event, "-kilometer", "km")
OlyX$event <- str_replace(OlyX$event, "5-000m", "5000m")
OlyX$event <- str_replace(OlyX$event, "-x-", "x")
OlyX$event <- str_replace(OlyX$event, "20km-race-walk", "20km-walk")
OlyX$event <- str_replace(OlyX$event, "4x100m-freestyle-men",
                                      "4x100m-freestyle-relay-men")
OlyX <- OlyX %>% separate(event, into = c("Event", "Sex"),
                          sep = "\\-(?!.*-)", remove = FALSE)

# Fix variables in new corrected data
# xG must be redefined because OlyX has been changed
xG <- OlyX %>% filter(medalType == "GOLD")
xG <- xG %>%
  group_by(Event) %>%
  mutate(en = n()) %>%
  ungroup()
xGZ <- xG %>%
  filter(en > 4) %>%
  mutate(eventH = fct_reorder(Event, en))
xGZA <- xGZ %>% filter(discipline %in% c("athletics"))

ggplot(xGZA, aes(year, eventH)) +
  geom_point(aes(colour = Sex)) +
  ylab(NULL) +
  xlab(NULL) +
  facet_grid(cols = vars(Sex), scales = "free_y") +
  scale_x_continuous(breaks = seq(1896, 2016, 20)) +
  scale_colour_manual(values = c("springgreen3", "purple2")) +
  theme(legend.position = "none",
        panel.spacing.x = unit(1, "lines"))


## Fig 6.6 'Swimming events by Olympic year after amendments'

xGZS <- xGZ %>% filter(discipline %in% c("swimming"))

ggplot(xGZS, aes(year, eventH)) +
  geom_point(aes(colour = Sex)) +
  ylab(NULL) +
  xlab(NULL) +
  facet_grid(cols = vars(Sex), rows = vars(discipline), scales = "free_y") +
  scale_x_continuous(breaks = seq(1896, 2016, 20)) +
  scale_colour_manual(values = c("springgreen3", "purple2")) +
  theme(legend.position = "none",
        panel.spacing.x = unit(1, "lines"))


## Fig 6.7 'Percentage differences in gold medal performances in athletics events
## compared with averages over the last six Games'

xG <- xG %>%
  group_by(event) %>%
  mutate(en = n()) %>%
  ungroup() # grouping by event, not Event, this time
xGy <- xG %>% filter(discipline %in% c("athletics", "swimming"), en > 5)

# Clean up data
xGy <- xGy %>% filter(!is.na(result_value))
zGy <- xGy %>% filter(!(result_value == "mark unknown"))
zGy <- zGy[!duplicated(zGy %>% select(games, discipline, event, result_value)), ]
zGy <- zGy %>% filter(!Event %in% c("heptathlon", "decathlon", "pentathlon"))
zGy <- zGy %>% filter(!Event %in% c("80m-hurdles"))
zGy <- zGy %>% mutate(result_value = ifelse(result_value == "54 1/5", 54.2, result_value))
zGy <- zGy %>% mutate(rv = gsub("([0-9,.])w", "\\1", result_value))
zGy <- zGy %>% filter(!(year %in% c("1900", "1904") & event == "3000m-steeplechase-men"))
zGy <- zGy %>% mutate(rv = as.numeric(rv))

# Standardise by results in recent Olympics
zGy9616 <- zGy %>%
  filter(year > 1992) %>%
  group_by(event) %>%
  summarise(mrv = mean(rv, na.rm = TRUE)) %>%
  select(event, mrv)
zGyj <- left_join(zGy, zGy9616)
zGyj <- zGyj %>% mutate(Rv = 100 * (rv / mrv - 1))
zGyj <- zGyj %>% mutate(rv = ifelse(Rv < -99, 1000 * rv, rv))
zGyj <- zGyj %>% mutate(Rv = 100 * (rv / mrv - 1))

zGyjA <- zGyj %>% filter(discipline %in% c("athletics"))
zGyjA <- zGyjA %>%
  ungroup() %>%
  complete(year = seq(1896, 2016, 4), nesting(Sex, result_type, Event))
to_lab <- as_labeller(c("distance" = "field", "time" = "track"))

olyAX <- ggplot(zGyjA, aes(year, Rv, group = Event)) +
  geom_line(colour = "grey70") +
  facet_grid(cols = vars(Sex), rows = vars(result_type),
             labeller = labeller(result_type = to_lab), scales = "free_y") +
  geom_hline(aes(yintercept = 0), linetype = "dashed", colour = "grey60") +
  theme(legend.position = "none",
        panel.spacing.x = unit(1, "lines")) +
  xlab(NULL) +
  ylab(NULL)
olyAX


## Fig 6.8 'Field events for men: percentage differences in gold medal performances
## compared with averages over the last six Games'

AmdX <- ggplot(zGyjA %>% filter(Sex == "men", result_type == "distance"),
               aes(year, Rv, colour = Event)) +
  geom_line() +
  geom_point() +
  theme(legend.position = "none",
        panel.spacing.x = unit(1, "lines")) +
  xlab(NULL) +
  ylab(NULL) +
  facet_wrap(vars(Event), ncol = 3)
AmdX


## Fig 6.9 'Track events for men: percentage differences in gold medal performances
## compared with averages over the last six Games, events are ordered by distance'

zGyjA <- zGyjA %>% mutate(EventX = factor(Event, levels = c("100m",
                                                            "110m-hurdles",
                                                            "200m",
                                                            "400m", 
                                                            "400m-hurdles",
                                                            "4x100m-relay",
                                                            "800m",
                                                            "1500m",
                                                            "4x400m-relay",
                                                            "3000m-steeplechase",
                                                            "5000m",
                                                            "10000m",
                                                            "20km-walk",
                                                            "50km-walk",
                                                            "marathon")))
                                                            
ggplot(zGyjA %>% filter(Sex == "men", result_type == "time"),
       aes(year, Rv, colour = EventX)) +
  geom_line() +
  geom_point() +
  theme(legend.position = "none",
        panel.spacing.x = unit(1, "lines")) +
  xlab(NULL) +
  ylab(NULL) +
  facet_wrap(vars(EventX), ncol = 3)


## Fig 6.10 'Percentage differences in gold medal performances in swimming events
## for men and women compared with averages over the last six Games since 1920
## with the 100 m freestyle highlighted'

Swim <- zGyj %>%
  filter(discipline == "swimming") %>%
  mutate(EventX = factor(case_when(
    event == "100m-freestyle-women" ~ "w100f",
    event == "100m-freestyle-men" ~ "m100f",
    .default = "Rest"
  )))
Swim <- Swim %>%
  ungroup() %>%
  complete(year = seq(1896, 2016, 4), nesting(Sex, EventX, Event))
  
ggplot(Swim %>% filter(year > 1919), aes(year, Rv, group = Event)) +
  geom_line(data = Swim %>%
            filter(year > 1919, EventX == "Rest"), colour = "grey80") +
  geom_line(data = Swim %>%
            filter(year > 1919, !EventX == "Rest"), aes(colour = EventX)) +
  facet_wrap(vars(Sex), nrow = 1) +
  theme(legend.position = "none",
        panel.spacing.x = unit(1, "lines")) +
  xlab(NULL) +
  ylab(NULL) +
  scale_colour_manual(values = c("springgreen3", "purple2"))


## Fig 6.11 'Numbers of countries competing at the Olympic Games over the years
## (male numbers in green above, female in purple below)'

data(OlympicPeople, package="GmooG")

uOly <- OlympicPeople %>%
  group_by(Year, Sex) %>%
  distinct(NOC) %>%
  summarise(nn = n())
uOly <- uOly %>%
  ungroup() %>%
  complete(Year = seq(1896, 2016, 4), nesting(Sex))
  
ggplot(uOly, aes(Year, nn, group = Sex, colour = Sex)) +
  geom_line() +
  geom_point() +
  ylab(NULL) +
  xlab(NULL) +
  scale_colour_manual(values = c("purple2", "springgreen3")) +
  theme(legend.position = "none")


## Fig 6.12 'Numbers competing at the Olympic Games over the years'

uOlyp <- OlympicPeople %>%
  group_by(Year, Sex) %>%
  summarise(nn = n())
uOlyp <- uOlyp %>%
  ungroup() %>%
  complete(Year = seq(1896, 2016, 4), nesting(Sex))
  
ggplot(uOlyp, aes(Year, nn, group = Sex, colour = Sex)) +
  geom_line() +
  geom_point() +
  ylab(NULL) +
  xlab(NULL) +
  scale_colour_manual(values = c("purple2", "springgreen3")) +
  theme(legend.position = "none")


## Fig 6.13 'Numbers of participants at the Olympics for countries
## who have hosted the Games (in order of first hosting)'

OlympicPeople <- OlympicPeople %>%
  ungroup() %>%
  mutate(NOC = ifelse(NOC %in% c("FRG", "GDR"), "GER", NOC))
OlympicPeople <- OlympicPeople %>%
  ungroup() %>%
  mutate(NOC = ifelse(NOC %in% c("EUN", "URS"), "RUS", NOC))
uOlyP <- OlympicPeople %>%
  group_by(Year, NOC) %>%
  summarise(pn = n())
uOlyPx <- uOlyP %>% mutate(NOC = factor(NOC))
uOlyPt <- uOlyPx %>%
  ungroup() %>%
  complete(Year = seq(1896, 2016, 4), nesting(NOC))
hosts <- c("GRE",
           "FRA",
           "USA",
           "GBR",
           "SWE",
           "BEL",
           "NED",
           "GER",
           "FIN",
           "AUS",
           "ITA",
           "JPN",
           "MEX",
           "CAN",
           "RUS",
           "KOR",
           "ESP",
           "CHN",
           "BRA")
uY <- uOlyPt %>%
  filter(NOC %in% hosts) %>%
  mutate(NOC = fct_drop(NOC), NOC = fct_relevel(NOC,
                                               "GRE",
                                               "FRA",
                                               "USA",
                                               "GBR",
                                               "SWE",
                                               "BEL",
                                               "NED",
                                               "GER",
                                               "FIN",
                                               "AUS",
                                               "ITA",
                                               "JPN",
                                               "MEX",
                                               "CAN",
                                               "RUS",
                                               "KOR",
                                               "ESP",
                                               "CHN",
                                               "BRA"),
  noc = fct_recode(NOC,
                   AUSTRALIA = "AUS",
                   BELGIUM = "BEL",
                   BRAZIL = "BRA",
                   CANADA = "CAN",
                   CHINA = "CHN",
                   SPAIN = "ESP",
                   FINLAND = "FIN",
                   FRANCE = "FRA",
                   `GREAT BRITAIN` = "GBR",
                   GERMANY = "GER",
                   GREECE = "GRE",
                   ITALY = "ITA",
                   JAPAN = "JPN",
                   KOREA = "KOR",
                   MEXICO = "MEX",
                   NETHERLANDS = "NED",
                   RUSSIA = "RUS",
                   SWEDEN = "SWE",
                   USA = "USA"))
                   
ggplot(uY, aes(Year, pn, group = NOC, colour = NOC)) +
  geom_point() +
  geom_line() +
  ylab(NULL) +
  xlab(NULL) +
  theme(legend.position = "none",
        panel.spacing.x = unit(1, "lines")) +
  facet_wrap(vars(noc), ncol = 4)


## Fig 6.14 'Percentages of host country participants at the Olympic Games over the years
## for women (purple) and men (green)'

uOlyQ <- OlympicPeople %>% distinct(Year, City)
uOlyQ <- uOlyQ %>% filter(!(Year == "1956" & City == "Stockholm"))
uOlyQ <- uOlyQ %>% arrange(Year)
uOlyQ$hCountry <- c("GRE",
                    "FRA",
                    "USA",
                    "GBR",
                    "SWE",
                    "BEL",
                    "FRA",
                    "NED",
                    "USA",
                    "GER",
                    "GBR",
                    "FIN",
                    "AUS",
                    "ITA",
                    "JPN",
                    "MEX",
                    "GER",
                    "CAN",
                    "RUS",
                    "USA",
                    "KOR",
                    "ESP",
                    "USA",
                    "AUS",
                    "GRE",
                    "CHN",
                    "GBR",
                    "BRA")
uOlyS <- left_join(OlympicPeople, uOlyQ)
uOlyS <- uOlyS %>%
  group_by(Sex, Year, hCountry, NOC) %>%
  summarise(pn = n()) %>%
  ungroup() %>%
  group_by(Sex, Year) %>%
  mutate(N = sum(pn))
uOlyH <- uOlyS %>%
  ungroup() %>%
  filter(NOC == hCountry)
uOlyH <- uOlyH %>% mutate(sh = pn / N)
uOlyH <- uOlyH %>% complete(Year = seq(1896, 2016, 4), nesting(Sex))

ggplot(uOlyH, aes(Year, sh, group = Sex, colour = Sex)) +
  geom_point() +
  geom_line() +
  ylab(NULL) +
  xlab(NULL) +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_manual(values = c("purple2", "springgreen3"))

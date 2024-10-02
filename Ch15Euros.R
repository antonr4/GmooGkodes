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


## Fig 15.1 'Numbers of teams (above) and squad sizes (below) over the
## 16 European Championships.  A point\'s area in the lower plot is proportional to
## the number of countries with that value (and the vertical scale starts at 15).'
data(eu20p)
data(eu20col)
eu20col <- eu20col %>%
  mutate(coln = case_when(
    team_alpha3 == "GB-ENG" ~ "#add8e6",
    team_alpha3 == "DEU" ~ "#000000",
    team_alpha3 == "RUS" ~ "#FF0000",
    .default = kit_shirt
))
ColsN <- deframe(eu20col %>% select(team_alpha3, coln))
colScaleN <- scale_fill_manual(name = "team_alpha3", values = ColsN)
eu20pQ <- eu20p %>%
  group_by(year, nat_team) %>%
  summarise(nn = n())
eu20pT <- eu20pQ %>% summarise(m = n())
q1 <- ggplot(eu20pT, aes(year, weight = m)) +
  geom_bar(fill = "darkslategray2") +
  xlab(NULL) +
  ylab(NULL) +
  ylim(0, 30) +
  ggtitle("Number of teams") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2))
q2 <- ggplot(eu20pQ, aes(year, nn)) +
  geom_count(colour = "tan3") +
  xlab(NULL) +
  ylab(NULL) +
  ylim(15, 30) +
  xlim(1958, 2022) +
  scale_size_area() +
  ggtitle("Squad sizes") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2),
        legend.position = "none")
q1 / q2


## Fig 15.2 'Percentage of a nation\'s players who played in their own country\'s league
## for each of the 16 European Championships.  Circle area is proportional to the number
## of countries with that percentage.  A loess smooth has been added.'
eu20p <- eu20p %>% mutate(nat_team_alpha3 = ifelse(nat_team_alpha3 %in% c("CIS", "SUN"),
                                                   "RUS", nat_team_alpha3),
                          club_alpha3 = ifelse(club_alpha3 %in% c("CIS", "SUN"),
                                               "RUS", club_alpha3))
eu20p <- eu20p %>% mutate(nat_alpha = factor(nat_team_alpha3),
                          club_alpha = factor(club_alpha3))
ls <- lvls_union(list(eu20p$nat_alpha, eu20p$club_alpha))
eu20p <- eu20p %>% mutate(nat_alpha = factor(nat_alpha, levels = ls),
                          club_alpha = factor(club_alpha, levels = ls))
eu20T <- eu20p %>%
  group_by(year, nat_alpha) %>%
  mutate(m = n())
eu20T2 <- eu20T %>%
  group_by(year, nat_alpha, m, club_alpha) %>%
  mutate(nn = n(),
         perc = 100 * nn / m)
eu20T2a <- eu20T2 %>% summarise(mperc = mean(perc))
eu20T2b <- eu20T2a %>% mutate(Mperc = ifelse(nat_alpha == club_alpha, mperc, 0))
eu20T2c <- eu20T2b %>%
  group_by(year, nat_alpha) %>%
  summarise(mx = max(Mperc, na.rm = TRUE))
ggplot(eu20T2c, aes(year, round(mx))) +
  geom_count(colour = "tan3") +
  geom_smooth(se = FALSE) +
  ylab(NULL) +
  xlab(NULL) +
  theme(legend.position = "none") +
  scale_size_area()


## Fig 15.3 'Numbers of players from four major leagues'
eu20pC <- eu20p %>%
  group_by(year, club_country) %>%
  summarise(nn = n())
ggplot(eu20pC %>% filter(year > 1990,
                         club_country %in% c("England", "Italy", "Spain", "Germany")),
                  aes(year, nn, group = club_country, colour = club_country)) +
  geom_line() +
  scale_colour_manual(values = c("#add8e6", "#000000", "#103CD6", "#E00000")) +
  xlab(NULL) +
  ylab(NULL) +
  theme(legend.title = element_blank())


## Fig 15.4 'Numbers of Euro 2020 players playing in the different European leagues'
eu20pX <- eu20p %>%
  filter(year == "2020") %>%
  mutate(clubCountry = fct_infreq(club_country_harm),
         nat_team4 = factor(nat_team, levels = c(levels(clubCountry), "Wales")),
         clubC = fct_lump_min(clubCountry, 9),
         clubC = fct_relevel(clubC, "Other", after = Inf))
eu20pX <- eu20pX %>%
  group_by(nat_team) %>%
  mutate(hg = sum(clubC == nat_team),
         np = n(),
         phg = hg / np) %>%
  ungroup()
eu20pX <- eu20pX %>% mutate(nat_team0 = factor(nat_team),
                            nat_team0 = fct_reorder(nat_team0, phg, mean, .desc = TRUE))
ggplot(eu20pX, aes(fct_rev(clubC))) +
  geom_bar(aes(fill = club_alpha3)) +
  ylab(NULL) +
  xlab(NULL) +
  theme(legend.position = "none") +
  colScaleN +
  coord_flip()


## Fig 15.5 'Numbers of Euro 2020 players playing in various European national leagues'
## The map can be found here "pngImages/uefaMap.png"


## Fig 15.6 'Numbers of Finland\'s Euro 2020 players
## playing in various countries\' leagues'
ggplot(eu20pX %>% filter(nat_team0 == "Finland"), aes(fct_rev(clubC))) +
  geom_bar(aes(fill = club_alpha3)) +
  ylab(NULL) +
  xlab(NULL) +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  colScaleN +
  coord_flip() +
  scale_x_discrete(expand = expansion(add = 1), drop = FALSE)


## Fig 15.7 'Numbers of Euro 2020 players of each team
## playing in various countries\' leagues'
uefa20w <- ggplot(eu20pX, aes(fct_rev(clubC))) +
  geom_bar(aes(fill = club_alpha3)) +
  ylab(NULL) +
  xlab(NULL) +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  facet_wrap(vars(nat_team0), nrow = 4) +
  colScaleN +
  coord_flip() +
  scale_x_discrete(expand = expansion(add = 1))
uefa20w


## Fig 15.8 'Percentages of home players for the countries
## who played most often in the 16 European Championship finals'
eu20T2c <- eu20T2c %>%
  group_by(nat_alpha) %>%
  mutate(nn = n())
colScaleC <- scale_colour_manual(name = "team_alpha3", values = ColsN)
eu20T2d <- droplevels(eu20T2c %>% filter(nn > 7))
eu20T2d <- eu20T2d %>% mutate(nat_team = nat_alpha)
levels(eu20T2d$nat_team) <- c("Germany", "Denmark", "Spain", "France", "England",
                              "Italy", "Netherlands", "Portugal", "Russia")
ggplot(eu20T2d, aes(year, round(mx), group = nat_alpha, colour = nat_alpha)) +
  geom_point() +
  xlab(NULL) +
  theme(legend.position = "none") +
  facet_wrap(vars(nat_team)) +
  ylim(0, 100) +
  ylab(NULL) +
  colScaleC

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
library(engsoccerdata)
})

theme_set(theme(plot.title = element_text(hjust = 0.5)))


## Restructuring the dataset from games per case to teams per case
data(england, package="engsoccerdata")
esd <- as_tibble(england)
esd <- esd %>% mutate(Date = ymd(Date),
                      tier = factor(tier))
allhome <- esd %>% mutate(
  team = home,
  opp = visitor,
  GF = hgoal,
  GA = vgoal,
  venue = "home"
)
allaway <- esd %>% mutate(
  team = visitor,
  opp = home,
  GF = vgoal,
  GA = hgoal,
  venue = "away"
)
allboth <- rbind(allhome, allaway) %>%
  mutate(GD = GF - GA,
         result = (GD > 0) * 1 + (GD == 0) * 0.5) %>%
  select(
    Date, Season, division, tier, team, opp,
    GF, GA, GD, result, venue
  )


## Fig 16.1 'Rate of draws over the years in the English league\'s top tier with a gam (general additive model) smooth'
Draws <- esd %>%
  group_by(Season, tier) %>%
  summarise(ngames = n(), draws = sum(result == "D"), drx = draws / ngames)
ggplot(Draws %>% filter(tier %in% c(1)), aes(Season, drx)) +
  geom_point(size = 0.75, colour = "grey60") +
  ylab(NULL) +
  geom_vline(xintercept = 1981, colour = "#add8e6", linetype = "dashed", linewidth = 1.25) +
  ylim(0, 0.4) +
  stat_smooth(method = "gam", formula = y ~ s(x), colour = "#add8e6", se = FALSE)


## Fig 16.2 'Smooths of the rates of draws for the top tiers in
## Italy (dark blue), England (light blue), and Spain (red)'
data(italy, package="engsoccerdata")
DrawsI <- italy %>%
  mutate(result = (hgoal > vgoal) * 1 + (hgoal == vgoal) * 0.5) %>%
  group_by(Season) %>%
  summarise(ngames = n(), draws = sum(result == 0.5), drx = draws / ngames)
data(spain, package="engsoccerdata")
DrawsS <- spain %>%
  mutate(result = (hgoal > vgoal) * 1 + (hgoal == vgoal) * 0.5) %>%
  group_by(Season) %>%
  summarise(ngames = n(), draws = sum(result == 0.5), drx = draws / ngames)
drEIS <- ggplot(Draws %>% filter(tier == 1), aes(Season, drx)) +
  labs(y = NULL) +
  ylim(0, 0.4) +
  stat_smooth(method = "gam", formula = y ~ s(x), colour = "#add8e6", se = FALSE) +
  geom_vline(xintercept = 1981, colour = "#add8e6",
             linetype = "dashed", linewidth = 1.25) +
  geom_smooth(
    data = DrawsI, aes(Season, drx), method = "gam", formula = y ~ s(x),
    colour = "blue", se = FALSE
  ) +
  geom_vline(xintercept = 1994, colour = "blue", linetype = "dashed") +
  geom_smooth(
    data = DrawsS, aes(Season, drx), method = "gam", formula = y ~ s(x),
    colour = "#E00000", se = FALSE
  ) +
  geom_vline(xintercept = 1995, colour = "#E00000", linetype = "dashed")
drEIS


## Fig 16.3 'Proportions of points won at home over time
## in the various tiers of the English league'
HA3a <- allboth %>%
  filter(venue == "home") %>%
  group_by(Season, tier) %>%
  summarise(ng = n(), nh = sum((result == 1) * 3 + (result == 0.5) * 1),
            na = sum((result == 0) * 3 + (result == 0.5) * 1),
            HomeProportion = nh / (nh + na))
HA3a1 <- complete(data.frame(HA3a), Season = full_seq(Season, 1), tier)
levels(HA3a1$tier) <- c("Tier 1", "Tier 2", "Tier 3", "Tier 4")
ggplot(HA3a1, aes(Season, HomeProportion, group = tier, colour = tier)) +
  geom_line() +
  ylab("Home proportion of points") +
  facet_wrap(vars(tier)) +
  scale_y_continuous(breaks = seq(0.5, 0.8, 0.1)) +
  theme(legend.position = "none")


## Fig 16.4'Proportions of points won at home for every team for every season'
HA3x <- allboth %>%
  group_by(Season, tier, team, venue) %>%
  summarise(ng = n(), np = sum((result == 1) * 3 + (result == 0.5) * 1))
HA3y <- pivot_wider(HA3x %>% select(-ng), names_from = "venue", values_from = "np")
HA3y <- HA3y %>% mutate(HomeProportion = round(home / (home + away), 3))
levels(HA3y$tier) <- c("Tier 1", "Tier 2", "Tier 3", "Tier 4")
HA3y1 <- data.frame(HA3y %>% filter(HomeProportion > 0.999))
HA3y2 <- HA3y %>% filter(HomeProportion < 0.35)
socHX <- ggplot(HA3y, aes(Season, HomeProportion)) +
  geom_point(colour = "grey60") +
  facet_wrap(~tier, nrow = 2) +
  ylim(0, 1) +
  geom_point(data = HA3y1, colour = "orange", size = 2) +
  geom_point(data = HA3y2, colour = "green", size = 2) +
  ylab("Home proportion of points by team")
socHX


## Table
data.frame(HA3y1)


## Table
data.frame(HA3y2)


## Fig 16.5 'The numbers of goals per game for the four tiers of the English league
## (the change in the offside law is marked with a red dashed line)'
SGoals <- allboth %>%
  group_by(Season, tier) %>%
  summarise(gg = sum(GF + GA), ngames = n(), GPG = gg / ngames)
SG <- complete(data.frame(SGoals), Season = full_seq(Season, 1), tier)
ggplot(SG, aes(Season, GPG)) +
  ylim(0, 5) +
  ylab("Goals per game") +
  geom_line(aes(group = factor(tier), colour = factor(tier))) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  geom_vline(xintercept = 1925, linetype = "dashed", colour = "red")


## Calculating a ranking of teams over the four tiers
newX <- allboth %>%
  group_by(Season, team) %>%
  mutate(gameno = dense_rank(Date)) %>%
  arrange(Season, team, gameno) %>%
  mutate(CumGF = cumsum(GF),
         CumGA = cumsum(GA),
         CumGD = cumsum(GD)) %>%
  select(Season, tier, division, team, gameno, result, CumGF, CumGA, CumGD)
newY <- newX %>%
  group_by(Season, team) %>%
  mutate(pts = (result == 1) * (Season < 1981) * 2 + (result == 1) * (Season > 1980) * 3 +
    (result == 0.5) * 1,
         Cumpts = cumsum(pts),
         GoalAve = CumGF / CumGA)
Z1 <- newY %>%
  ungroup() %>%
  filter(Season < 1976) %>%
  arrange(Season, division, gameno, desc(Cumpts), desc(GoalAve))
Z2 <- newY %>%
  ungroup() %>%
  filter(Season > 1975) %>%
  arrange(Season, division, gameno, desc(Cumpts), desc(CumGD))
Zall <- rbind(Z1, Z2)
Zall <- Zall %>%
  mutate(drank = ave(as.character(team), Season, division, gameno, FUN = seq_along),
         drank = as.numeric(drank))
aZ1 <- Zall %>%
  filter(!(Season == 2019 & tier %in% c("3", "4"))) %>%
  group_by(Season, division) %>%
  filter(gameno == max(gameno))
aZ1 <- aZ1 %>%
  ungroup() %>%
  arrange(Season, tier, desc(Cumpts), desc(CumGD)) %>%
  mutate(lrank = ave(as.character(team), Season, FUN = seq_along),
         lrank = as.numeric(lrank))
zy <- Zall %>% filter(Season == 2019 & tier %in% c("3", "4"))
zy1 <- zy %>%
  group_by(division, team) %>%
  filter(gameno == max(gameno)) %>%
  mutate(ptsPg = Cumpts / gameno)
zy2 <- zy1 %>%
  ungroup() %>%
  group_by(division) %>%
  arrange(division, desc(ptsPg), desc(CumGD))
zy3 <- zy2 %>% mutate(lrank = ave(as.character(team), FUN = seq_along),
                      lrank = as.numeric(lrank) + 44 + 23 * (division == 4))
aZg <- zy3 %>%
  select(-ptsPg) %>%
  ungroup()
aZz <- rbind(aZ1, aZg)
aZz <- aZz %>% mutate(srank = lrank^0.5)
aZM <- complete(aZz, Season, team)
nTeams <- Zall %>%
  filter(gameno == 1) %>%
  group_by(Season, tier) %>%
  summarise(nt = n()) %>%
  mutate(cnt = cumsum(nt),
         snt = cnt^0.5)
nD <- split(nTeams, nTeams$tier)


## Fig 16.6 'Comparing the three South Coast teams:
## Bournemouth, Portsmouth, and Southampton'
aS <- aZM %>% filter(team %in% c("Portsmouth", "Southampton", "AFC Bournemouth"))
ggplot(aZM, aes(Season, lrank)) +
  geom_line(aes(group = team), alpha = 0.05) +
  geom_line(data = aS, aes(Season, lrank, group = team, colour = team), linewidth = 1.5) +
  scale_colour_manual(values = c("violet", "royalblue", "red2")) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank()) +
  labs(x = NULL, y = NULL) +
  scale_y_reverse() +
  scale_x_continuous(expand = c(0.02, 0)) +
  geom_step(data = nD[[1]], aes(Season, cnt)) +
  geom_step(data = nD[[2]], aes(Season, cnt)) +
  geom_step(data = nD[[3]], aes(Season, cnt)) +
  geom_step(data = nD[[4]], aes(Season, cnt))


## Fig 16.7 'Comparing Bournemouth, Portsmouth, and Southampton on a square root scale'
ggplot(aZM, aes(Season, srank)) +
  geom_line(aes(group = team), alpha = 0.05) +
  geom_line(data = aS, aes(Season, srank, group = team, colour = team), linewidth = 1.5) +
  scale_colour_manual(values = c("violet", "royalblue", "red2")) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank()) +
  labs(x = NULL, y = NULL) +
  scale_y_reverse() +
  scale_x_continuous(expand = c(0.02, 0)) +
  geom_step(data = nD[[1]], aes(Season, snt)) +
  geom_step(data = nD[[2]], aes(Season, snt)) +
  geom_step(data = nD[[3]], aes(Season, snt)) +
  geom_step(data = nD[[4]], aes(Season, snt))


## Fig 16.8 'Team performances in points above/below the average in 2015-16'
wormchart <- function(data, Seax, divx, nsel) {
  SeasonX <- data %>% filter(Season == Seax,
                             division == divx)
  SeasonX <- SeasonX %>%
    ungroup() %>%
    group_by(gameno) %>%
    mutate(meanP = mean(Cumpts))
  nteams <- with(SeasonX, nlevels(factor(team)))
  ngames <- max(SeasonX$gameno)
  pal1 <- palette_pander(2 * nsel)
  ZY <- SeasonX %>% filter(gameno == ngames)
  SelTeam <- ZY %>%
    filter(drank < (nsel + 1) | drank > (nteams - nsel)) %>%
    select(team, drank)
  Sx <- SeasonX %>% filter(gameno < (ngames + 1))
  Sy <- Sx %>% filter(team %in% SelTeam$team)
  Sy <- within(Sy, Team <- reorder(team, -drank, last))
  lS <- ZY %>% summarise(ul = ceiling(max(Cumpts - meanP)),
                         ll = floor(min(Cumpts - meanP)))
  ZY <- ZY %>% mutate(xx = gameno + 2,
                      yy = lS$ul - (drank - 1) * (lS$ul - lS$ll) / (nteams - 1))
  ZYt <- ZY %>% filter(team %in% SelTeam$team)
  ggplot(Sx, aes(gameno, (Cumpts - meanP))) +
    geom_line(aes(group = team), alpha = 0.1) +
    labs(y = NULL, x = "Number of games") +
    xlim(0, ngames + 16) +
    geom_line(data = Sy, aes(group = Team, colour = Team), linewidth = 1.5) +
    scale_colour_manual(values = pal1) +
    geom_text(data = ZY, aes(xx + 3, yy, label = team), hjust = 0) +
    geom_segment(data = ZY, aes(x = gameno + 1, xend = gameno + 5,
                                y = Cumpts - meanP, yend = yy, group = team),
                                linetype = 3) +
    geom_text(data = ZYt, aes(xx + 3, yy, label = team, colour = team), hjust = 0) +
    ggtitle(paste0("English League ", SeasonX$Season, "-", SeasonX$Season + 1)) +
    theme(plot.title = element_text(vjust = 2),
          legend.position = "none")
}
w15x <- wormchart(Zall, 2015, 1, 4)
w15x


## Fig 16.9 'Team performances in points above/below the average in season 1982-83).
## There were 22 teams and each played 42 games.'
wormchart(Zall, 1982, 1, 4)


## Fig 16.10 'Wormcharts for the two seasons with extreme point ranges for the
## English 1st Division Championship with 22 teams and two points for a win
## between 1919-20 and 1980-81'
Seax <- 1937
divx <- 1
nsel <- 4
SeasonX <- Zall %>% filter(Season == Seax,
                           division == divx)
SeasonX <- SeasonX %>%
  ungroup() %>%
  group_by(gameno) %>%
  mutate(meanP = mean(Cumpts))
nteams <- with(SeasonX, nlevels(factor(team)))
ngames <- max(SeasonX$gameno)
pal1 <- palette_pander(2 * nsel)
ZY <- SeasonX %>% filter(gameno == ngames)
SelTeam <- ZY %>%
  filter(drank < (nsel + 1) | drank > (nteams - nsel)) %>%
  select(team, drank)
Sx <- SeasonX %>% filter(gameno < (ngames + 1))
Sy <- Sx %>% filter(team %in% SelTeam$team)
Sy <- within(Sy, Team <- reorder(team, -drank, last))
lS <- ZY %>% summarise(ul = ceiling(max(Cumpts - meanP)),
                       ll = floor(min(Cumpts - meanP)))
ZY <- ZY %>% mutate(xx = gameno + 2,
                    yy = lS$ul - (drank - 1) * (lS$ul - lS$ll) / (nteams - 1))
ZYt <- ZY %>% filter(team %in% SelTeam$team)
w37s <- ggplot(Sx, aes(gameno, (Cumpts - meanP))) +
  geom_line(aes(group = team), alpha = 0.1) +
  labs(y = NULL, x = "Number of games") +
  xlim(0, ngames) +
  ylim(-25, 25) +
  geom_line(data = Sy, aes(group = Team, colour = Team), linewidth = 1.5) +
  scale_colour_manual(values = pal1) +
  geom_text(data = ZY, aes(xx + 7, yy, label = team)) +
  geom_segment(data = ZY, aes(
    x = gameno + 1, xend = gameno + 5, y = Cumpts - meanP, yend = yy,
    group = team
  ), linetype = 3) +
  geom_text(data = ZYt, aes(xx + 7, yy, label = team, colour = team)) +
  ggtitle(paste0("English League ", SeasonX$Season, "-", SeasonX$Season + 1)) +
  theme(plot.title = element_text(vjust = 2),
        legend.position = "none")
Seax <- 1968
divx <- 1
nsel <- 4
SeasonX <- Zall %>% filter(Season == Seax,
                           division == divx)
SeasonX <- SeasonX %>%
  ungroup() %>%
  group_by(gameno) %>%
  mutate(meanP = mean(Cumpts))
nteams <- with(SeasonX, nlevels(factor(team)))
ngames <- max(SeasonX$gameno)
pal1 <- palette_pander(2 * nsel)
ZY <- SeasonX %>% filter(gameno == ngames)
SelTeam <- ZY %>%
  filter(drank < (nsel + 1) | drank > (nteams - nsel)) %>%
  select(team, drank)
Sx <- SeasonX %>% filter(gameno < (ngames + 1))
Sy <- Sx %>% filter(team %in% SelTeam$team)
Sy <- within(Sy, Team <- reorder(team, -drank, last))
lS <- ZY %>% summarise(ul = ceiling(max(Cumpts - meanP)),
                       ll = floor(min(Cumpts - meanP)))
ZY <- ZY %>% mutate(xx = gameno + 2,
                    yy = lS$ul - (drank - 1) * (lS$ul - lS$ll) / (nteams - 1))
ZYt <- ZY %>% filter(team %in% SelTeam$team)
w68s <- ggplot(Sx, aes(gameno, (Cumpts - meanP))) +
  geom_line(aes(group = team), alpha = 0.1) +
  labs(y = NULL, x = "Number of games") +
  xlim(0, ngames) +
  ylim(-25, 25) +
  geom_line(data = Sy, aes(group = Team, colour = Team), linewidth = 1.5) +
  scale_colour_manual(values = pal1) +
  geom_text(data = ZY, aes(xx + 7, yy, label = team)) +
  geom_segment(data = ZY, aes(
    x = gameno + 1, xend = gameno + 5, y = Cumpts - meanP, yend = yy,
    group = team
  ), linetype = 3) +
  geom_text(data = ZYt, aes(xx + 7, yy, label = team, colour = team)) +
  ggtitle(paste0("English League ", SeasonX$Season, "-", SeasonX$Season + 1)) +
  theme(plot.title = element_text(vjust = 2),
        legend.position = "none")
w37s + w68s

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


## Fig 33.1 'Four different kinds of scatterplot of data for gold and silver medal
## performances by men and women in the Olympic freestyle 100 m'
## The dataset OlyX is constructed in the code for Figs 6.1, 6.3, and 6.5

XGSs <- OlyX %>% filter(Event == "100m-freestyle", medalType %in% c("GOLD", "SILVER"))
XGSs <- XGSs %>% mutate(time = as.numeric(result_value) / 1000)
pg1 <- ggplot(XGSs, aes(year, time)) + geom_point()
  
XGsmf <- unique(XGSs %>% filter(medalType == "GOLD") %>%
                         select(year, Sex, time)) %>%
                         pivot_wider(names_from = Sex, values_from = time)
pg2 <- ggplot(XGsmf, aes(men, women)) +
  geom_point() +
  xlim(45, 65) +
  ylim(50, 85)
  
XGms <- XGSs %>%
  filter(Sex == "men") %>%
  select(year, medalType, time) %>%
  pivot_wider(names_from = medalType, values_from = time) %>%
  mutate(dif = SILVER - GOLD, mm = (GOLD + SILVER) / 2)
pg3 <- ggplot(XGms, aes(mm, dif)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  ylab("difference") +
  xlab("mean")
  
m1 <- lm(data = XGsmf, women ~ men)
XGr <- XGsmf %>% filter(year > 1908)
XGr[, "residuals"] <- m1$residuals
XGr[, "fitted"] <- m1$fitted
pg4 <- ggplot(XGr, aes(fitted, residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dotted")
  
(pg1 + pg2) / (pg3 + pg4)


## Fig 33.2 'Boxplots of beak measurements (above) and
## beak height by species (below) on Isabela Island'
## See Fig 14.1 for the dataset mSH1

fp <- mSH1 %>%
  select(BeakW:N.UBkL) %>%
  pivot_longer(cols = BeakW:N.UBkL,
               names_to = "Var",
               values_to = "Val")
fpx <- ggplot(fp, aes(Var, Val)) +
  geom_boxplot() +
  ylab(NULL) +
  xlab(NULL) +
  coord_flip()
fpy <- ggplot(mSH1, aes(species, BeakH)) +
  xlab(NULL) +
  ylab(NULL) +
  geom_boxplot() +
  coord_flip()
fpx / fpy


## Fig 33.3 'Median times for men and women for "up" (orange) and "down" (black) races
## of the Comrades Marathon since 1980 and the numbers finishing'
## See Fig 21.8

CM12 + plot_layout(heights = c(1.2, 0.4))


## Fig 33.4 'The spineplot top left shows the treatment group was much bigger
## than the placebo group and the dropout rate was small for both groups.
## The scatterplots of PASI and DLQI scores at the start of the study top right
## show the two groups had similar initial distributions.
## The bottom plot shows that average initial scores on the 10 questions were similar
## for the groups (solid lines) and mostly slightly lower for the placebo group
## after 16 weeks (blue dashed line).  For the treatment group average scores were
## substantially less on all questions after 16 weeks (dark orange dashed line).'
## See Fig 12.1

qlEnsY


## Fig 33.5 'Voting at the 1912 Democratic convention shown in the initial plot'
## See Fig 4.1

cand1


## Fig 33.6 'Voting at the 1912 Democratic convention shown in the final plot'
## See Fig 4.6

cand2


## Fig 33.7 'Gapminder life expectancy data drawn for all countries (left)
## and with countries grouped by region (right)'
## See Figs 2.1 and 2.6

pgmX
pr4X


## Fig 33.8 'When charging stations were used'
## See Figs 13.5 and 13.7

plan1 <- "
  ##BBB
  AABBB
  AABBB
  "
ElecSt1 + ElecSt2 + plot_layout(design = plan1)


## Fig 33.9 'Support for same-sex marriage by state with 95\\% confidence intervals'
## See Fig 9.5

gayCId <- ggplot(USpsl, aes(NAME, p1)) +
  geom_point() +
  geom_errorbar(aes(ymin = low, ymax = upp)) +
  coord_flip()
gayCId + gayCIx


## Fig 33.10 'Mission times by year (left) and log mission times by year
## coloured by space station programme (right)'
## See Figs 10.4 and 10.6

fttX + ftlX


## Fig 33.11 'Default plot of numbers of Euro 2020 players
## of each team playing in other countries\' leagues'
## See Fig 15.7

uefa20y <- ggplot(eu20p %>% filter(year == "2020"), aes(club_country)) +
  geom_bar(aes(fill = club_country)) +
  facet_wrap(vars(nat_team)) +
  coord_flip()
uefa20y


## Fig 33.12 'Amended plot of numbers of Euro 2020 players
## of each team playing in other countries\' leagues'
## See Fig 15.7

uefa20w

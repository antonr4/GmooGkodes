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
library(palmerpenguins)
})


theme_set(theme(plot.title = element_text(hjust = 0.5)))


## Fig 28.1 'Individual spaceflight mission times over the years,
## plotted on a linear scale (left) and a logged scale (right)'
## See Figs 10.4 and 10.6

fttX
ftlX


## Fig 28.2 'Percentage differences in gold medal performances in athletics events
## compared with averages over the last six Games'
## See Fig 6.7

olyAX


## Fig 28.3 'Newcomb\'s estimates of the speed of light,
## plotted in order (left) and by date (right)'
## See Figs 5.6 and 5.7

(newct1XX + ylab(NULL)) + newct2XX


## Fig 28.4 'Numbers of finishers of the Comrades Ultramarathon
## by age group and sex from 1975 to 2019'
## See Fig 21.2

comX


## Fig 28.5 'Layers building up to a histogram of the bill lengths of Adelie penguins
## with two superimposed density estimates.'

p2 <- penguins
p2a <- p2 %>% filter(species == "Adelie",
                     !(is.na(bill_length_mm)))
names(p2a)[3] <- "bill length (mm)"

g1 <- ggplot() +
  xlab("")
g2 <- ggplot(p2a, aes(`bill length (mm)`)) +
  ylab("") +
  xlim(30, 50)
g3 <- g2 +
      geom_histogram(aes(y = after_stat(density)),
      boundary = 0, binwidth = 1, fill = "grey80") + ylim(0, 0.2)
g4 <- g3 +
      geom_density(colour = "red", linewidth = 1.25, adjust = 1.5)
g5 <- g4 +
      geom_density(colour = "blue", linewidth = 1.25, adjust = 0.5)
Adelg6 <- g5 +
          ggtitle("Adelie penguin bill lengths") +
          theme(plot.title = element_text(vjust = 2))
          
(g1 + g2 + g3) / (g4 + g5 + Adelg6)


## Fig 28.6 'Histogram of the bill lengths of Adelie penguins
## with two superimposed density estimates'

Adelg6


## Fig 28.7 'Rates of draws for the top tiers in Italy (dark blue) since 1934,
## England (light blue) since 1888, and Spain (red) since 1928'
## See Figure 16.2

drEIS


## Fig 28.8 'Scatterplot of age by year of space flight coloured by sex,
## with smooths and their confidence intervals'
## See Figure 10.2

astgB + geom_point(aes(colour = sex), size = 0.5) + geom_smooth(aes(colour = sex))


## Fig 28.9 'Logged spaceflight mission times of individuals
## by year of flight by nationality'
## See Figure 10.7

flx


## Fig 28.10 'Wormchart for season 1954-55'
## The wormchart function is defined in the code for Figure 16.8 and the dataset Zall
## is constructed in Fig 16.5

wormchart(Zall, 1954, 1, 4)


## Fig 28.11 'Layers building up to a wormchart
## for the 1954-55 English League Championship.'

Seax <- 1954
divx <- 1
nsel <- 4
SeasonX <- Zall %>% filter(Season == Seax, division == divx)
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

g1 <- ggplot()
g2 <- ggplot(Sx, aes(gameno, (Cumpts - meanP))) +
  ylab(NULL) +
  xlab("Number of games") +
  xlim(0, ngames + 16)
g3 <- g2 +
      geom_line(aes(group = team), alpha = 0.1)
g4 <- g3 +
      geom_line(data = Sy, aes(group = Team, colour = Team), linewidth = 1.5) + 
      scale_colour_manual(values = pal1) +
      theme(legend.position = "none")
g5 <- g4 +
      geom_text(data = ZY, aes(xx + 3, yy, label = team), size = 2, hjust = 0)
g6 <- g5 +
      geom_segment(data = ZY, aes(x = gameno + 1,
                                  xend = gameno + 5,
                                  y = Cumpts - meanP,
                                  yend = yy,
                                  group = team),
                                  linetype = 3)
g7 <- g6 +
      geom_text(data = ZYt,
                aes(xx + 3, yy, label = team, colour = team), size = 2, hjust = 0)
g8 <- g7 +
      ggtitle(paste0("English League Championship ", SeasonX$Season,
                     "-", SeasonX$Season + 1)) +
      theme(plot.title = element_text(vjust = 2))
      
(g1 + g2) / (g3 + g4) / (g5 + g6) / (g7 + g8)

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
library(openintro)
})

theme_set(theme(plot.title = element_text(hjust = 0.5)))


## Fig 32.1 'Scatterplot of Biacromial diameter against pelvic breadth (Biiliac)
## for 507 adults.'

data(bdims, package = "openintro")

ggplot(bdims, aes(bii_di, bia_di)) +
  geom_point() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  coord_fixed() +
  xlab("Biiliac") +
  ylab("Biacrom")


## Fig 32.2 'Scatterplot of Biacromial diameter (shoulder width) against
## Biiliac (pelvic breadth) for 247 men (in green) and 260 women (in purple).'

ggplot(bdims, aes(bii_di, bia_di, colour = factor(sex))) +
  geom_point() +
  scale_colour_manual(values = c("purple2", "springgreen3")) +
  theme(legend.position = "none",
        axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  coord_fixed() +
  xlab("Biiliac") +
  ylab("Biacrom")


## Fig 32.3 'Winners of Erststimmen seats by political party in Germany 2021'
## See Fig 26.16

btwMX


## Fig 32.4 'Reordered parallel coordinate plot of the Palmer Penguin data'
## See Fig 18.8

PalY


## Fig 32.5 'Gapminder life expectancy data (left) and movie ratings (right)'
## See Figs 2.1 and 3.9

pgmX + vr


## Fig 32.6 'Best times for the 400 m freestyle events for men and women,
## individual (red) and relay (blue)'
## See Fig 20.11

ir400


## Fig 32.7 'Finishing times in the 2019 race for females (above) and males (below),
## dotted lines mark limits for awarding medals other than gold'
## See Fig 21.9

cmvf / cmvm


## Fig 32.8 'Athletics field events for men: percentage differences in
## gold medal performances compared with averages over the last six Games'
## See Fig 6.8

AmdX


## Fig 32.9 'An UpAndDown plot showing absolute changes in party support
## between the last two German elections as area and relative changes as height'
## See Fig 26.4

y2b$uad + guides(fill = guide_legend(nrow = 1))


## Fig 32.10 'Histograms of movie runtimes, the default on the left
## and a varied one on the right (note the different scales)'
## See Fig 3.2

f22x <- ggplot(films22, aes(runtime)) +
  geom_histogram() +
  ylab(NULL) +
  xlab(NULL)
f22x + f22s


## Fig 32.11 'Smooths of chess ratings (left) and football league draws (right)'
## See Figs 8.22 and 16.2

ags1520
drEIS


## Fig 32.12 'Ratings of active and inactive chess players in 2020'
## See Fig 8.3

actInact


## Fig 32.13 'Seat-winning percentages by party in Germany 2021 and in the UK 2019'
## See Fig 26.18

eGER + eUK


## Fig 32.14 'Line plots of support for same-sex marriage at federal level (left)
## and opposition to a Constitutional Amendment (right) by age of respondent
## by males (green) and females (purple)'
## See Fig 9.9

state1 + fed1


## Fig 32.15 'Extra seats in German elections since 1949.
## Each point marks the date of an election.
## The left dashed line marks the first election after reunification in October 1990.
## The right dashed line marks the first election with Ausgleichsmandate.'
## See Fig 26.1

ueP


## Fig 32.16 'Best times in seconds for the four 50 m (left) and
## four 200 m (right) swimming events achieved by men and women in the order
## breaststroke, backstroke, butterfly, freestyle from the top'
## See Figs 20.3 and 20.5

sw50yX + sw200yX

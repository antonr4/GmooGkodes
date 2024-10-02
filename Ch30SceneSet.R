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


## Fig 30.1 'Average speed of the fastest runners (above) and median runners (below)
## in km/hr for the "up" (dark orange) and "down" (black) races, and distances in km
## for the "up" and "down" races, all since 1970'
## See Figs 21.11 and 21.12

comUDpX + profUD


## Fig 30.2 'Numbers of athletics and swimming events for men and women
## at the Summer Olympics from 1896 to 2016'
## See Fig 6.3

olyASnY


## Fig 30.3 'Barcharts of numbers of countries by region and populations
## in millions in 2016'
## See Figs 2.7 and 2.8

reg4X + reg4Y


## Fig 30.4 'Barcharts of population changes by region between 1956 and 2016,
## absolute numbers in millions on the left, percentages on the right.'
## See the Fig 2.8 code for the dataset pLE and the Fig 2.5 code for the colour palette

pLES <- pLE %>%
  group_by(four_regions) %>%
  summarise(n = n(),
            p1956 = sum(`1956`),
            p2016 = sum(`2016`),
            popCh = p2016 - p1956,
            popChp = 100 * popCh / p1956)
                     
c1 <- ggplot(pLES, aes(four_regions, weight = popCh)) +
  geom_bar(aes(fill = four_regions)) +
  xlab(NULL) +
  ylab(NULL) +
  scale_fill_manual(values = colsG) +
  scale_y_continuous(breaks = seq(0, 3000000000, 500000000),
                     labels = c(0, 500, 1000, 1500, 2000, 2500, 3000)) +
  theme(legend.position = "none",
        plot.margin = margin(5.5, 25, 5.5, 5.5, "pt"))
c2 <- ggplot(pLES, aes(four_regions, weight = popChp)) +
  geom_bar(aes(fill = four_regions)) +
  xlab(NULL) +
  ylab(NULL) +
  scale_fill_manual(values = colsG) +
  theme(legend.position = "none",
        plot.margin = margin(5.5, 5.5, 5.5, 25, "pt"))
  
c1 + c2


## Preparing to draw the barchart with four different themes

fr0 <- ggplot(pLES, aes(four_regions)) +
  geom_bar(aes(fill = four_regions, weight = n)) +
  xlab(NULL) +
  ylab(NULL)
frEx <- fr0 + scale_fill_excel_new() + theme_excel_new()
frG <- fr0 + scale_fill_gdocs() + theme_gdocs()
frW <- fr0 + scale_fill_wsj() + theme_wsj()
frEc <- fr0 + scale_fill_economist() + theme_economist()

## Fig 30.5 (above)

frEx
frG

## Fig 30.5 (below) 'Barchart of population by region 2016 using four themes,
## Excel and Google docs (above), Wall Street Journal and Economist (below)'

frW
frEc


## Fig 30.6 'A scatterplot from the diamonds dataset of carat and table'
## See the code for Figure 11.7

gy


## Fig 30.7 'Support for same-sex marriage at state level by sex and race'
## See Fig 9.3

gayGR1


## Fig 30.8 'Scatterplot of responses to the two same-sex marriage questions by state
## with equal and default scales
## (point areas are proportional to the number of respondents in that state)'
## See the code for Fig 9.12

FSfav + (FSfavx + coord_fixed(ratio = 0.5))


## Fig 30.9 'Barchart of numbers of chess players by country and region
## plotted horizontally and vertically'
## See the code for Fig 8.11

ChCtry1 + theme(legend.position = "none") + ChCtry2


## Fig 30.10 'Body and wing lengths for the five species from Isabela Island'
## See Fig 14.2

finch2X


## Fig 30.11 'Scatterplot of numbers of Agriculture and Industry workers
## in France in 1954 plotted two ways'
## See Fig 7.3

a4 + (a4 + coord_flip())


## Fig 30.12 'Scatterplot of responses to the two same-sex marriage questions by state
## plotted two ways
## (point areas are proportional to the number of respondents in that state)'
## See Fig 9.12

FSfav + (FSfav + coord_flip(xlim = c(0, 1), ylim = c(0, 1)))


## Fig 30.13 'Diamond carats by clarity (left) and
## Titanic survival rates for passengers and crew (right)'
## See Figs 11.11 and 25.8

dcbox
titanCX


## Fig 30.14 'Juxtaposed histograms (left) and superposed density estimates (right)
## of ratings for active and inactive chess players'
## See Figs 8.3 and 8.4

actInact + (eloD + theme(legend.position = "none")) + plot_layout(widths = c(2, 1))


## Fig 30.15 'Percentage state shares of delegates at the 1912 and 2020
## Democratic conventions (left) and by region (right)'
## See Figs 4.3 and 4.4

delsS + theme(legend.position = "none")
delsJa + ylab(NULL)


## Fig 30.16 'Age distribution of active players by sex (left) and
## numbers of spaceflight participants by decade and by sex (right)'
## See Figs 8.10 and 10.3

sexAgePP
astbD

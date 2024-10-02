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


## Fig 29.1 'Male swimmer best times for four different strokes'
## See Fig 20.2

swM5x


## Fig 29.2 'Three speed of light plots with three different colour schemes
## because they represent three different situations'
## See Figs 5.1, 5.2, and 5.7

ptmichY
michTY
newct2XY


## Fig 29.3 'Speed of light measurements by Newcomb'
## See Fig 5.4

boxnewcXX


## Fig 29.4 'Boxplots of ratings for each age for active chess players in 2015'
## See Fig 8.19

ratAX


## Fig 29.5 'Rating by age for active players in 2015.  Players with ratings over 2700
## have been coloured red.  An alpha value of 0.01 has been used for the other players.'
## See Fig 8.17

ratAY


## Fig 29.6 'Individual home away points ratios
## for every English football team for every season'
## See Fig 16.4

socHX


## Fig 29.7 'Plots of five species of Darwin\'s finches'
## See Figs 14.1, 14.2, and 14.3

finch1X
finch2X
finch3X


## Fig 29.8 'Plot ensembles for different groups of Penguins
## (using different colours for the selections)'
## See Figs 18.2, 18.3, and 18.4

palens1X
palens2X
palens3X


## Fig 29.9 'Winners of seats by political party in Germany'
## See Fig 26.16

btwMX


## Fig 29.10 'Three uses of a colour blind palette'
## See Figs 9.11, 18.8, and 21.2

FSx
PalX
comX


## Fig 29.11 'Spaceflight mission times coloured by space station programme'
## See Fig 10.5

astroF


## Fig 29.12 'Nine measurements of 9 species on Isabela Island'
## See Fig 14.5

sxpcp


## Fig 29.13 'Graphics drawn with plain backgrounds'
## See Figs 7.1, 25.8, and 14.4

bertX
titanCX
finchpcpX

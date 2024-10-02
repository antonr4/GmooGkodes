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


## Fig 27.1 'Athletics field events for men: percentage differences in
## gold medal performances compared with averages over the last six Games'
## See Fig 6.8

AmdX


## Fig 27.2 'Multiple barcharts of the responses to supporting same-sex marriage
## at state level by the responses to supporting a Constitutional Amendment banning it'
## See Fig 9.11

FSxs


## Fig 27.3 'Histogram of diamond carats'
## See Fig 11.4

dcx


## Fig 27.4 'Use of charging stations, coloured by the type of facility,
## grouped by location, ordered by first installation at location
## and by first date of use, with the end of the testing period marked in red'
## See Fig 13.7

ElecSt2


## Fig 27.5 'Parallel coordinate plot of nine measurements
## of nine Galápagos finch species from Isabela Island'
## See Fig 14.5

sxpcp


## Fig 27.6 'A profile plot for the planned down race starting at Pietermaritzburg
## to the left, finishing at Durban to the right, in 2020'
## See Fig 21.12

ComProfx


## Fig 27.7 'Titanic survival rates by class and crew,
## older dataset on the left, newer on the right'
## See Fig 25.8 for the plot on the right

titan1 <- data.frame(Titanic, package="GmooG")
titan1 <- titan1 %>% mutate(survived = fct_recode(Survived, no = "No", yes = "Yes"),
                            survived = fct_rev(survived))
                            
titanCO <- ggplot(data = titan1) +
  geom_mosaic(aes(x = product(Class), fill = survived, weight = Freq),
              divider = ddecker(), offset = 0.0025) +
  theme(axis.ticks = element_blank()) +
  labs(x = "", y = "") +
  theme(legend.position = "none", panel.background = element_blank()) +
  scale_fill_manual(values = c("red", "grey70")) +
  theme(axis.text.x = element_text(angle = -60))
titanCO | titanCX

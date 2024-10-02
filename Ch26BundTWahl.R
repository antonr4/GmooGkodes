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
library(cartogram)
library(UpAndDownPlots)
library(RColorBrewer)
library(grid)
library(parlitools)
library(ggpcp)
})


theme_set(theme(plot.title = element_text(hjust = 0.5)))


## Fig 26.1 'Extra seats in German elections since 1949.
## Each point marks the date of an election.
## The left dashed line marks the first election after reunification in October 1990.
## The right dashed line marks the first election with Ausgleichsmandate.'

data(GermanExtraSeats, package="GmooG")

ueP <- ggplot(GermanExtraSeats, aes(Year, Number)) +
  geom_point(colour = "grey50", size = 1.2) +
  geom_step(colour = "grey50", linewidth = 1) +
  ylab(NULL) +
  xlab(NULL) +
  geom_vline(xintercept = c(1990, 2013), lty = "dashed",
             colour = "palevioletred", linewidth = 1)
ueP


## Fig 26.2 'Party percentage support (in Zweitstimmen) for the last two elections'

data(GermanElection21, package="GmooG")

btw1v <- GermanElection21 %>%
  group_by(WkNr, Stimme) %>%
  mutate(sum21 = sum(Anzahl),
         sum17 = sum(VorpAnzahl),
         Prozent21 = 100 * Anzahl / sum21,
         Prozent17 = 100 * VorpAnzahl / sum17)
btwV <- btw1v %>%
  ungroup() %>%
  filter(Stimme == 2) %>%
  mutate(Party = fct_lump_n(Partei, w = Anzahl, n = 7))
btwV <- btwV %>% mutate(party = fct_collapse(Party, CDUCSU = c("CDU", "CSU"),
                                                    Linke = "DIE LINKE"))

# Define colour palette for the German political parties
polnames2 <- c("CDUCSU", "SPD", "FDP", "GRÜNE", "AfD", "Linke", "Other")
polpal2 <- c("black", "red", "yellow", "green", "blue", "violet", "grey70")
names(polpal2) <- polnames2

bdiff <- btwV %>%
  group_by(party) %>%
  summarise(s21 = sum(Anzahl), s17 = sum(VorpAnzahl), d4 = s21 - s17)
bdiff <- bdiff %>%
  ungroup() %>%
  mutate(S21 = sum(s21),
         S17 = sum(s17),
         p21 = 100 * s21 / S21,
         p17 = 100 * s17 / S17,
         d4p = p21 - p17)
bdiff <- bdiff %>% mutate(party = fct_reorder(party, -p21))

w <- ggplot(bdiff, aes(party)) +
  scale_fill_manual(values = alpha(polpal2, 0.8)) +
  ylab(NULL) +
  xlab(NULL) +
  ylim(0, 35) +
  theme(plot.title = element_text(vjust = 3),
        legend.position = "none")
w1 <- w + geom_bar(aes(fill = party, weight = p21)) +
          ggtitle("Party support in 2021")
w2 <- w + geom_bar(aes(fill = party, weight = p17)) +
          ggtitle("Party support in 2017")
w1 / w2


## Fig 26.3 'Changes in party percentage support between the last two elections'

wpX <- w + geom_bar(aes(fill = party, weight = d4p)) + ylim(-17.5, 17.5)
wpX


## Fig 26.4 'Relative changes in support between the last two elections'

yy <- ud_prep(bdiff, v1 = "s17", v2 = "s21", levs = c("party"),
              sortLev = c("final"), reverse = TRUE)
y2b <- ud_plot(yy, totperc = "no", vscale = c(-80, 80), labelvar = "party",
               levelColour = "party", ud_control = ud_colours(colours = c("grey75",
               "grey60", "red", "grey60", "green"), gcpal = polpal2))
y2bX <- y2b$uad + theme(legend.position = "none")
y2bX


## Fig 26.5 'Relative changes in support by party and region.'

Bdiff <- btwV %>%
  group_by(Region, party) %>%
  summarise(s21 = sum(Anzahl), s17 = sum(VorpAnzahl))
  
YY <- ud_prep(Bdiff, v1 = "s17", v2 = "s21", levs = c("party", "Region"),
              sortLev = c("final", "base"), reverse = c(TRUE, TRUE))
Yb <- ud_plot(YY, totperc = "no", vscale = c(-120, 120), labelvar = "party",
              levelColour = "party", ud_control = ud_colours(colours = c("grey75",
              "grey60", "red", "grey60", "green"), gcpal = polpal2))
Yb$uad + theme(legend.position = "none")


## Fig 26.6 'Relative changes in support by party and Bundesland.'

Bdiff <- btwV %>%
  group_by(Bundesland, party) %>%
  summarise(s21 = sum(Anzahl), s17 = sum(VorpAnzahl))
  
YY <- ud_prep(Bdiff, v1 = "s17", v2 = "s21", levs = c("party", "Bundesland"),
              sortLev = c("final", "base"), reverse = c(TRUE, TRUE))
Yb <- ud_plot(YY, totperc = "no", vscale = c(-120, 120), labelvar = "party",
              levelColour = "party", ud_control = ud_colours(colours = c("grey75",
              "grey60", "red", "grey60", "green"), gcpal = polpal2))
Yb$uad + theme(legend.position = "none")


## Grouping and summarising election data, and combining with a map of Germany

btwVx <- btwV %>% select(-Land, -Partei, -Stimme, -sum21,
                         -sum17, -Prozent21, -Prozent17, -Party)
btwVx <- btwVx %>%
  group_by(WkNr, WkName, Bundesland, Region, party) %>%
  summarise(Anz = sum(Anzahl),
            VorpAnz = sum(VorpAnzahl))
btwVx <- btwVx %>%
  ungroup() %>%
  group_by(WkNr) %>%
  mutate(s21 = sum(Anz),
         s17 = sum(VorpAnz),
         p21 = 100 * Anz / s21,
         p17 = 100 * VorpAnz / s17)
btwVL <- btwVx %>%
  ungroup() %>%
  select(-s21, -s17) %>%
  pivot_wider(names_from = "party",
              values_from = c(Anz, VorpAnz, p21, p17))
  
data(GermanyMap, package="GmooG")
f1x <- st_sf(data.frame(GermanyMap, btwVL))
f1_union <- GermanyMap %>%
  group_by(LAND_NAME) %>%
  summarize(geometry = st_union(geometry))
  
gr1 <- tm_shape(f1x) +
       tm_polygons(col = "p17_GRÜNE",
                   breaks = seq(0, 60, 10),
                   palette = "Greens",
                   legend.show = FALSE) +
       tm_layout(main.title = "2017",
                 frame = FALSE) +
       tm_shape(f1_union) +
       tm_borders(lwd = 2)
gr2 <- tm_shape(f1x) +
       tm_polygons(col =
                   "p21_GRÜNE",
                   breaks = seq(0, 60, 10),
                   palette = "Greens",
                   legend.show = FALSE) +
       tm_layout(main.title = "2021",
                 frame = FALSE) +
       tm_shape(f1_union) +
       tm_borders(lwd = 2)
gcol <- brewer.pal(6, "Greens")

hh <- ggplot(f1x) +
  ylab(NULL) +
  xlab(NULL) +
  ylim(0, 175) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill = "grey98"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
hg1 <- hh + geom_histogram(aes(p17_GRÜNE), breaks = seq(0, 60, 5),
                           fill = c(gcol[1], gcol[1], gcol[2], gcol[2], gcol[3], gcol[3],
                           gcol[4], gcol[4], gcol[5], gcol[5], gcol[6], gcol[6]))
hg2 <- hh + geom_histogram(aes(p21_GRÜNE), breaks = seq(0, 60, 5),
                           fill = c(gcol[1], gcol[1], gcol[2], gcol[2], gcol[3], gcol[3],
                           gcol[4], gcol[4], gcol[5], gcol[5], gcol[6], gcol[6]))
gr1x <- tmap_grob(gr1)
#gr2x <- tmap_grob(gr2)


## Fig 26.7 'Percentage Green support across the constituencies
## in 2017 (left) and 2021 (right).  Bundesland borders are drawn in thick black.
## The colour scale goes from 0 to 60 in intervals of 10 (histograms below).'

drawGrob <- function(grob, row, col) {
  pushViewport(viewport(layout.pos.row = row, layout.pos.col = col))
  grid.draw(grob)
  upViewport()
}
pushViewport(viewport(layout = grid.layout(nrow = 2, ncol = 2, heights = c(.75, .25))))
print(gr1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(gr2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
drawGrob(ggplotGrob(hg1), 2, 1)
drawGrob(ggplotGrob(hg2), 2, 2)


## Fig 26.8 'Green support plotted against log of population density'

data(GermanDemographics, package="GmooG")

# colour palette for the three regions
polreg <- c("royalblue", "grey40", "lightpink2")
names(polreg) <- c("Berlin", "East", "West")

f1z <- st_sf(data.frame(f1x, GermanDemographics))
f1za <- f1z %>% arrange(fct_rev(Region))

ggplot(f1za, aes(log(PopDensity), p21_GRÜNE)) +
  geom_point(aes(colour = Region), size = 2) +
  ylab("Green support") +
  xlab("log(population density)") +
  scale_colour_manual(values = polreg) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5))


## Fig 26.9 'A parallel coordinate plot of Green support and some structural variables
## (constituencies with over 25\\% Green party votes are coloured green)'

BundZ <- inner_join(btwVL, GermanDemographics, join_by(WkNr == WkrNr))
BundZ <- BundZ %>% mutate(logPopD = log(PopDensity),
                          Greens = p21_GRÜNE,
                          Ghigh = ifelse(Greens > 25, "high", "low"),
                          Univ = Hochschulreife)
                          
BundZ %>%
  arrange(Greens) %>%
  pcp_select(Greens, Foreigners, logPopD, Age1824:Age75up, CarsPerP, Univ, Unemployed) %>%
  pcp_scale(method = "uniminmax") %>%
  ggplot(aes_pcp()) +
  geom_pcp_axes(colour = "white") +
  geom_pcp(aes(colour = Ghigh)) +
  ylab(NULL) +
  xlab(NULL) +
  theme(legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  scale_colour_manual(values = c("green", "grey85"))


## Preparing AfD maps

afd1 <- tm_shape(f1x) +
        tm_polygons(col = "p17_AfD",
                    breaks = seq(0, 60, 10),
                    palette = "Blues",
                    legend.show = FALSE) +
        tm_layout(main.title = "2017",
                  frame = FALSE) +
        tm_shape(f1_union) +
        tm_borders(lwd = 2)
afd2 <- tm_shape(f1x) +
        tm_polygons(col = "p21_AfD",
                    breaks = seq(0, 60, 10),
                    palette = "Blues",
                    legend.show = FALSE) +
        tm_layout(main.title = "2021",
                  frame = FALSE) +
        tm_shape(f1_union) +
        tm_borders(lwd = 2)
        
acol <- brewer.pal(6, "Blues")
ha1 <- hh + geom_histogram(aes(p17_AfD), breaks = seq(0, 60, 5),
                           fill = c(acol[1], acol[1], acol[2], acol[2], acol[3], acol[3],
                           acol[4], acol[4], acol[5], acol[5], acol[6], acol[6]))
ha2 <- hh + geom_histogram(aes(p21_AfD), breaks = seq(0, 60, 5),
                           fill = c(acol[1], acol[1], acol[2], acol[2], acol[3], acol[3],
                           acol[4], acol[4], acol[5], acol[5], acol[6], acol[6]))
ha1x <- tmap_grob(ha1)
#ha2x <- tmap_grob(ha2)


## Fig 26.10 'Percentage AfD support across the constituencies
## in 2017 (left) and 2021 (right).    Bundesland borders are drawn in thick black.
## The colour scale goes from 0 to 60 in intervals of 10, (histograms below).'

pushViewport(viewport(layout = grid.layout(nrow = 2, ncol = 2, heights = c(.75, .25))))
print(afd1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(afd2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
drawGrob(ggplotGrob(ha1), 2, 1)
drawGrob(ggplotGrob(ha2), 2, 2)


## Fig 26.11 'AfD support plotted against percentage of foreigners'

ggplot(f1za, aes(Foreigners, p21_AfD)) +
  geom_point(aes(colour = Region), size = 2) +
  ylab("AfD support") +
  xlab("Percentage foreigners") +
  scale_colour_manual(values = polreg) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5))


## Fig 26.12 'A parallel coordinate plot of AfD support and some structural variables
## (constituencies with over 20\\% AfD party votes are coloured blue)'

BundZ <- BundZ %>% mutate(AfD = p21_AfD,
                          Ahigh = ifelse(AfD > 20, "high", "low"))

BundZ %>%
  arrange(AfD) %>%
  pcp_select(AfD, Foreigners, logPopD, Age1824:Age75up, CarsPerP, Univ, Unemployed) %>%
  pcp_scale(method = "uniminmax") %>%
  ggplot(aes_pcp()) +
  geom_pcp_axes(colour = "white") +
  geom_pcp(aes(colour = Ahigh)) +
  ylab(NULL) +
  xlab(NULL) +
  theme(legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  scale_colour_manual(values = c("blue", "grey85"))


## Preparing CDUCSU maps

cu1 <- tm_shape(f1x) +
       tm_polygons(col = "p17_CDUCSU",
                   breaks = seq(0, 60, 10),
                   palette = "Greys",
                   legend.show = FALSE) +
       tm_layout(main.title = "2017",
                 frame = FALSE) +
       tm_shape(f1_union) +
       tm_borders(lwd = 2)
cu2 <- tm_shape(f1x) +
       tm_polygons(col = "p21_CDUCSU",
                   breaks = seq(0, 60, 10),
                   palette = "Greys",
                   legend.show = FALSE) +
       tm_layout(main.title = "2021",
                 frame = FALSE) +
       tm_shape(f1_union) +
       tm_borders(lwd = 2)
ccol <- brewer.pal(6, "Greys")

hc1 <- hh + geom_histogram(aes(p17_CDUCSU), breaks = seq(0, 60, 5),
                           fill = c(ccol[1], ccol[1], ccol[2], ccol[2], ccol[3], ccol[3],
                           ccol[4], ccol[4], ccol[5], ccol[5], ccol[6], ccol[6]))
hc2 <- hh + geom_histogram(aes(p21_CDUCSU), breaks = seq(0, 60, 5),
                           fill = c(ccol[1], ccol[1], ccol[2], ccol[2], ccol[3], ccol[3],
                           ccol[4], ccol[4], ccol[5], ccol[5], ccol[6], ccol[6]))
hc1x <- tmap_grob(hc1)
#hc2x <- tmap_grob(hc2)


## Fig 26.13 'Percentage CDU and CSU support across the constituencies
## in 2017 (left) and 2021 (right).    Bundesland borders are drawn in thick black.
## The colour scale goes from 0 to 60 in intervals of 10.'

pushViewport(viewport(layout = grid.layout(nrow = 2, ncol = 2, heights = c(.75, .25))))
print(cu1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(cu2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
drawGrob(ggplotGrob(hc1), 2, 1)
drawGrob(ggplotGrob(hc2), 2, 2)


## Fig 26.14 'CDU and CSU support plotted against
## numbers of cars per thousand population.'

ggplot(f1za, aes(CarsPerP, p21_CDUCSU)) +
  geom_point(aes(colour = Region), size = 2) +
  ylab("CDU/CSU support") +
  xlab("Cars per 1000 population") +
  scale_colour_manual(values = polreg) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5))


## Fig 26.15 'A parallel coordinate plot of CDU/CSU support and some structural variables
## (constituencies with over 30\\% party votes are coloured black)'

BundZ <- BundZ %>% mutate(CDUCSU = p21_CDUCSU,
                          Chigh = ifelse(CDUCSU > 30, "high", "low"))
                          
BundZ %>%
  arrange(CDUCSU) %>%
  pcp_select(CDUCSU, Foreigners, logPopD, Age1824:Age75up, CarsPerP, Univ, Unemployed) %>%
  pcp_scale(method = "uniminmax") %>%
  ggplot(aes_pcp()) +
  geom_pcp_axes(colour = "white") +
  geom_pcp(aes(colour = Chigh)) +
  ylab(NULL) +
  xlab(NULL) +
  theme(legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  scale_colour_manual(values = c("black", "grey85"))


## Fig 26.16 'Winners of Erststimmen seats by political party'

btwY <- btw1v %>%
  ungroup() %>%
  filter(Stimme == 1)
btwYwin1 <- btwY %>%
  group_by(Bundesland, WkNr, WkName) %>%
  arrange(desc(Anzahl), .by_group = TRUE) %>%
  slice_head(n = 1)
btwYwin1 <- btwYwin1 %>%
  mutate(WkNr = as.numeric(WkNr)) %>%
  arrange(WkNr)
f1y <- st_sf(data.frame(GermanyMap, btwYwin1))

# Define colour palette
polnames6 <- c("CDU", "CSU", "SPD", "GRÜNE", "AfD", "DIE LINKE")
polpal6 <- c("black", "cadetblue2", "red", "green", "blue", "violet")
names(polpal6) <- polnames6

btwMX <- tm_shape(f1y) +
         tm_polygons(col = "Partei",
                     palette = polpal6,
                     alpha = 0.8) +
         tm_layout(frame = FALSE,
                   legend.outside = TRUE,
                   legend.title.color = "white")
btwMX


## Fig 26.17 'Where political parties won Erststimmen seats'

tm_shape(f1y) +
     tm_polygons(col = "Partei",
                 palette = polpal6,
                 alpha = 0.8,
                 legend.show = FALSE) +
     tm_facets(by = "Partei", free.coords = FALSE, nrow = 2) +
     tm_shape(f1_union) +
     tm_borders(lwd = 0.25)


## Fig 26.18 'Seat-winning percentages by party in Germany 2021 and in the UK 2019'

eGER <- ggplot(btwYwin1, aes(Partei, Prozent21)) +
  geom_point(aes(colour = Partei)) +
  scale_colour_manual(values = polpal6) +
  theme(legend.position = "none") +
  ylab(NULL) +
  xlab("Germany 2021") +
  geom_hline(yintercept = 50, lty = "dotted", colour = "black") +
  ylim(0, 100)
  
# The UK election data are in the package parlitools
uk19 <- bes_2019
uk19 <- uk19 %>%
  rowwise() %>%
  mutate(winV = max(c_across(con_vote_19:other_vote_19), na.rm = TRUE),
         winP = 100 * winV / (total_vote_19 - rejected_vote_19))

# Colour palette for UK political parties
polnames2 <- c("Conservative", "Labour", "Liberal Democrat", "Green",
               "NI parties", "Plaid Cymru", "Scottish National Party")
polpal2 <- c("#0087DC", "#DC241f", "#FDBB30", "#528D6B",
             "orange", "yellowgreen", "yellow4")
names(polpal2) <- polnames2

uk19 <- uk19 %>%
        mutate(win19 = ifelse(winner_19 %in% c("Alliance",
                                               "Democratic Unionist Party",
                                               "Sinn Fein",
                                               "Social Democratic & Labour Party"),
                                               "NI parties", winner_19))
                                                
eUK <- ggplot(uk19 %>% filter(!win19 %in% c("Speaker")), aes(win19, winP)) +
  geom_point(aes(colour = winner_19)) +
  scale_colour_manual(values = polpal2) +
  theme(legend.position = "none") +
  ylab(NULL) +
  xlab("UK 2019") +
  geom_hline(yintercept = 50, lty = "dotted", colour = "black") +
  scale_x_discrete(labels = c("Conservative" = "Con",
                              "Green" = "Green",
                              "Labour" = "Lab", 
                              "Liberal Democrat" = "LibDem",
                              "Plaid Cymru" = "PlaidC",
                              "Scottish National Party" = "SNP")) +
  ylim(0, 100)
  
eGER + eUK


## Fig 26.19 'Erststimmen and Zweitstimmen by major parties
## (brown lines mark equality and
## seats that were won on Erststimmen are coloured by the party\'s colour)'

ctw <- btw1v %>%
  ungroup() %>%
  select(WkName, Partei, Stimme, Anzahl, Bundesland, Region) %>%
  filter(Partei %in% c("CDU", "CSU", "SPD", "GRÜNE", "AfD", "Die Linke", "FDP"))
ctW <- ctw %>%
  group_by(Region, Bundesland, WkName, Partei) %>%
  pivot_wider(values_from = "Anzahl", names_from = "Stimme", names_prefix = "v")
ctW <- ctW %>%
  ungroup() %>%
  group_by(Region, Bundesland, WkName) %>%
  arrange(desc(v1), .by_group = TRUE) %>%
  mutate(Rank = rank(-v1),
         Winner = ifelse(Rank == 1, "Yes", "No"))
ctW <- ctW %>% unite("Farbe", c("Winner", "Partei"), sep = "_", remove = FALSE)

ggplot(ctW %>% arrange(Farbe), aes(v2, v1)) +
  geom_point(aes(colour = Farbe)) +
  facet_wrap(vars(Partei)) +
  geom_abline(intercept = 1, slope = 1, colour = "sienna2") +
  coord_fixed(xlim = c(0, 90000), ylim = c(0, 90000)) +
  theme(legend.position = "none",
        axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  scale_colour_manual(values = c(rep("grey70", 6), "blue", "grey20", "lightblue",
                                 "green", "red")) +
  ylab("Erststimmen") +
  xlab("Zweitstimmen")


## Fig 26.20 'Which party got the most Zweitstimmen in each constituency'

btwY2 <- btw1v %>%
  ungroup() %>%
  filter(Stimme == 2)
btwYwin2 <- btwY2 %>%
  group_by(Bundesland, WkNr, WkName) %>%
  arrange(desc(Anzahl), .by_group = TRUE) %>%
  slice_head(n = 1)
btwYwin2 <- btwYwin2 %>%
  mutate(WkNr = as.numeric(WkNr)) %>%
  arrange(WkNr)
f1y2 <- st_sf(data.frame(GermanyMap, btwYwin2))

tm_shape(f1y2) +
     tm_polygons(col = "Partei",
                 palette = polpal6,
                 alpha = 0.8) +
     tm_layout(frame = FALSE,
               legend.outside = TRUE,
               legend.title.color = "white")


## Fig 26.21 'Consituencies where the party that got the most Zweitstimmen
## did not get the most Erststimmen, coloured by the party with the most Zweitstimmen'

btwYZ <- btw1v %>% ungroup()
btwYwinZ <- btwYZ %>%
  group_by(Bundesland, WkNr, WkName, Stimme) %>%
  arrange(desc(Anzahl), .by_group = TRUE) %>%
  slice_head(n = 1)
btwYwinZ <- btwYwinZ %>%
  mutate(WkNr = as.numeric(WkNr)) %>%
  arrange(WkNr)
btwYwinZ <- btwYwinZ %>% select(WkNr, WkName, Land, Partei, Stimme, Bundesland, Prozent21)
btwYwinZ <- btwYwinZ %>%
              pivot_wider(names_from = "Stimme", values_from = c("Partei", "Prozent21"))
f1xZ <- st_sf(data.frame(GermanyMap, btwYwinZ))
f1xZ <- f1xZ %>% mutate(dx = ifelse(Partei_1 == Partei_2, NA, Partei_2))

tm_shape(f1xZ) +
     tm_polygons(col = "dx",
                 palette = polpal6,
                 alpha = 0.8,
                 textNA = "No difference") +
     tm_layout(frame = FALSE,
               legend.outside = TRUE,
               legend.title.color = "white")

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
library(scales)
library(cartogram)
})

theme_set(theme(plot.title = element_text(hjust = 0.5)))

## Constructing variables for in favour and against

data(SurvGR, package="GmooG")

naesXz2 <- SurvGR %>% mutate(opF = ifelse(valF < 5, valF, NA),
                             opS = ifelse(valS < 5, valS, NA))
naesXz2 <- naesXz2 %>% mutate(Spro = ifelse(valS %in% c(1, 2), "yes", "no"))
naesXz2 <- naesXz2 %>%
  group_by(State) %>%
  mutate(nState = n(), proS =
         sum(Spro == "yes") / (sum(Spro == "yes") + sum(!is.na(opS) & Spro == "no"))) %>%
  ungroup() %>%
  mutate(pState = fct_reorder(State, proS, mean))


## Fig 9.1 'Support for same-sex marriage at state level by respondent age.
## A red dotted line marks 50% support.  Cases with any values missing have
## been excluded and data for ages over 80 combined into one value at 81.
## Point areas are proportional to the number of respondents of that age.'

naesSA <- naesXz2 %>%
  filter(age < 98) %>%
  mutate(age = ifelse(age > 81, 81, age)) %>%
  group_by(age) %>%
  summarise(Spro = sum(valS %in% c(1, 2)), Scon = sum(valS %in% c(3, 4)),
            nn = Spro + Scon, Sfav = Spro / nn)
            
ggplot(naesSA, aes(age, Sfav)) +
  geom_point(aes(size = nn), colour = "dodgerblue3") +
  ylab(NULL) +
  xlab(NULL) +
  scale_y_continuous(labels = percent_format(),
                     limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "firebrick3") +
  theme(legend.position = "none") +
  scale_size_area()


## Fig 9.2 'Support for same-sex marriage at state level by age of respondent
## for females (purple) and males (green)'

naesSAG <- naesXz2 %>%
  filter(age < 98) %>%
  mutate(age = ifelse(age > 81, 81, age)) %>%
  group_by(age, gender) %>%
  summarise(Spro = sum(valS %in% c(1, 2)), Scon = sum(valS %in% c(3, 4)),
            Sfav = Spro / (Spro + Scon), ns = Spro + Scon)
            
ggplot(naesSAG, aes(age, Sfav)) +
  geom_line(aes(colour = gender)) +
  ylab(NULL) +
  xlab(NULL) +
  theme(legend.position = "none") +
  scale_colour_manual(values = c("purple2", "springgreen3")) +
  scale_y_continuous(labels = percent_format(),
                     limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "firebrick3")


## Fig 9.3 'Support for same-sex marriage at state level by sex and race'

naesXz2 <- naesXz2 %>% mutate(spro = fct_rev(Spro),
                              Gender = factor(gender, levels = c("Male", "Female")))
                              
gayGR1 <- ggplot(naesXz2 %>% filter(!is.na(opS))) +
  geom_mosaic(aes(x = product(race, Gender), fill = spro),
                  divider = ddecker(), offset = 0.005) +
  xlab("") +
  ylab("") +
  theme(legend.position = "none",
        panel.background = element_blank(),
        plot.margin = margin(0, 0, 0, 0),
        axis.ticks = element_blank()) +
  scale_fill_manual(values = c("red", "grey80")) +
  scale_x_productlist(labels = paste0(rep(c("Bl", "Hisp", "Oth", "White"), 2), "\n",
                      rep(c(" ", " ", "                    Male", " ", " ", " ", "
                                          Female", " "))), expand = c(0, 0)) +
  scale_y_productlist(expand = c(0, 0))
gayGR1


## Fig 9.4 'Support for same-sex marriage at state level
## on a continuous colour scale from 10\\% to 70\\%'

data(USregions, package="GmooG")

naesSStx <- naesXz2 %>% filter(valS %in% c(1, 2, 3, 4))
naesZ <- naesSStx %>%
  group_by(State) %>%
  summarise(ProStateLaw = mean(proS), nS = n())
USpsl <- full_join(USregions, naesZ)

gaym1 <- tm_shape(USpsl) +
         tm_polygons(col = "ProStateLaw", breaks = seq(0.1, 0.7, 0.01), legend.show = FALSE)
gaym1


## Fig 9.5 'Support for same-sex marriage by state with 95\\% confidence intervals,
## states grouped by region and ordered within region
## by estimated population proportion in favour'

USpsl <- USpsl %>% mutate(p1 = ProStateLaw, hi = 1.96 * (p1 * (1 - p1) / nS)**0.5,
                                            low = p1 - hi, upp = p1 + hi)

# Colour palette for regions
regnames4 <- c("NorthEast", "South", "MidWest", "West")
colblindx <- colorblind_pal()(8)[1:4]
names(colblindx) <- regnames4

USpsl <- USpsl %>% mutate(Region = factor(Region,
                           levels = c("NorthEast", "South", "MidWest", "West")))
                           
gayCIx <- ggplot(USpsl, aes(fct_reorder(NAME, p1), p1)) +
  geom_point(aes(colour = Region), size = 2) +
  geom_errorbar(aes(ymin = low, ymax = upp), width = 0.2) +
  ylab(NULL) +
  xlab(NULL) +
  coord_flip() +
  facet_grid(rows = vars(Region), scales = "free_y", space = "free_y") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), labels = percent) +
  theme(strip.text.y.right = element_text(angle = 0),
        legend.position = "none") +
  scale_colour_manual(values = colblindx)
gayCIx


## Fig 9.6 'The four US regions defined by the Census Bureau'

tm_shape(USpsl) + tm_polygons(col = "Region", palette = colblindx)


## Fig 9.7 'Support for same-sex marriage at state level using a population cartogram'

USxt <- st_transform(USpsl, crs = 2163)
USxC <- cartogram_cont(USxt, "nS")
tm_shape(USxC) +
  tm_polygons(col = "ProStateLaw", breaks = seq(0.1, 0.7, 0.01), legend.show = FALSE)


## Fig 9.8 'Scatterplot of opposition to a Constitutional Amendment
## making same-sex marriage illegal by age of respondent.
## Cases with any values missing have been excluded and data for ages above 80
## have been combined into one value at 81.
## Point areas are proportional to the number of respondents of that age.'

naesFA <- naesXz2 %>%
  filter(age < 98) %>%
  mutate(age = ifelse(age > 81, 81, age)) %>%
  group_by(age) %>%
  summarise(ns = n(), Fpro = sum(valF %in% c(3, 4)),
                      Fcon = sum(valF %in% c(1, 2)), Ffav = Fpro / (Fpro + Fcon))
                      
ggplot(naesFA, aes(age, Ffav)) +
  geom_point(aes(size = ns), colour = "gold3") +
  ylab(NULL) +
  xlab(NULL) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1),
                     breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "firebrick3") +
  theme(legend.position = "none") +
  scale_size_area()


## Fig 9.9 'Line plots of support for same-sex marriage at federal level (left) and
## opposition to a Constitutional Amendment (right) by age of respondent
## by males (green) and females (purple)'

state1 <- ggplot(naesSAG, aes(age, Sfav)) +
  geom_line(aes(colour = gender)) +
  ylab(NULL) +
  xlab(NULL) +
  theme(legend.position = "none") +
  scale_colour_manual(values = c("purple2", "springgreen3")) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1),
                              breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "firebrick3")
  
naesFG <- naesXz2 %>%
  filter(age < 98) %>%
  mutate(age = ifelse(age > 81, 81, age)) %>%
  group_by(age, gender) %>%
  summarise(Fpro = sum(valF %in% c(3, 4)), Fcon = sum(valF %in% c(1, 2)),
                                           Ffav = Fpro / (Fpro + Fcon), ns = Fpro + Fcon)
                                           
fed1 <- ggplot(naesFG, aes(age, Ffav)) +
  geom_line(aes(colour = gender)) +
  ylab(NULL) +
  xlab(NULL) +
  theme(legend.position = "none") +
  scale_colour_manual(values = c("purple2", "springgreen3")) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1),
                              breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "firebrick3")
state1 + fed1


## Fig 9.10 'Barcharts of the full responses to the two questions'

naesXz2 <- naesXz2 %>% mutate(ValF = factor(valF, labels = c("Strongly favor",
                       "Somewhat favor", "Somewhat oppose", "Strongly oppose",
                       "Neither", "Don't know", "Refused")),
                       ValS = factor(valS, labels = c("Strongly favor", "Somewhat favor", 
                       "Somewhat oppose", "Strongly oppose", "Don't know", "Refused")))
                       
gs1 <- ggplot(naesXz2 %>% filter(!is.na(ValS)), aes(ValS)) +
  geom_bar(aes(fill = ValS)) +
  ylab(NULL) +
  xlab(NULL) +
  ggtitle("For same-sex marriage in state") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 3),
        legend.position = "none",
        axis.text.x = element_text(angle = -45)) +
  scale_fill_manual(values = c(rep("dodgerblue3", 4), rep("grey70", 2)))
gs2 <- ggplot(naesXz2, aes(ValF)) +
  geom_bar(aes(fill = ValF)) +
  ylab(NULL) +
  xlab(NULL) +
  ggtitle("For a Constitutional Amendment banning same-sex marriage") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 3),
        legend.position = "none",
        axis.text.x = element_text(angle = -45)) +
  scale_fill_manual(values = c(rep("gold3", 4), rep("grey70", 3)))
gs1 / gs2


## Fig 9.11 'Multiple barcharts of the responses to supporting same-sex marriage
## at state level by the responses to supporting a Constitutional Amendment banning it'

naesXz2 <- naesXz2 %>% mutate(OpF = factor(opF, labels = c("Strongly favor",
                                 "Somewhat favor", "Somewhat oppose", "Strongly oppose")),
                              OpS = factor(opS, labels = c("Strongly favor",
                              "Somewhat favor", "Somewhat oppose", "Strongly oppose")),
                              OpFf = factor(opF, labels = c("VeryFor", "For",
                                                            "Against", "VeryAgainst")),
                              OpSs = factor(opS, labels = c("VeryFor", "For",
                                                            "Against", "VeryAgainst")))
colblindy <- c("red3", colorblind_pal()(8)[c(5, 2, 3)])

FSx <- ggplot(data = naesXz2 %>% filter(!is.na(OpF), !is.na(OpS)), aes(OpS)) +
  geom_bar(aes(fill = OpF)) +
  facet_grid(rows = vars(OpF)) +
  ylab("Constitutional Amendment against same-sex marriage") +
  xlab("State same-sex marriage law") +
  theme(legend.position = "none") +
  scale_fill_manual(values = colblindy)
FSx

FSxs <- ggplot(data = naesXz2 %>% filter(!is.na(OpFf), !is.na(OpSs)), aes(OpSs)) +
  geom_bar(aes(fill = OpFf)) +
  facet_grid(rows = vars(OpFf)) +
  ylab("Constitutional Amendment against same-sex marriage") +
  xlab("State same-sex marriage law") +
  theme(legend.position = "none") +
  scale_fill_manual(values = colblindy)


## Fig 9.12 'Scatterplot of responses to the two same-sex marriage questions by state
## (point areas are proportional to the number of respondents in that state)'

naesSST <- naesXz2 %>%
  group_by(State) %>%
  summarise(Spro = sum(valS %in% c(1, 2)), Scon = sum(valS %in% c(3, 4)),
                       nn = Spro + Scon, Sfav = Spro / nn, Fpro = sum(valF %in% c(3, 4)),
                       Fcon = sum(valF %in% c(1, 2)), mm = Fpro + Fcon, Ffav = Fpro / mm)
                       
FSfav <- ggplot(naesSST, aes(Ffav, Sfav)) +
  ylab("In favour at state level") +
  xlab("Against Constitutional Amendment") +
  geom_point(aes(size = mm), colour = "coral3") +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted", colour = "firebrick3") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2),
        legend.position = "none") +
  ggtitle("Opinions on same-sex marriage by state") +
  scale_size_area()
FSfav

FSfavx <- ggplot(naesSST, aes(Ffav, Sfav)) +
  ylab("In favour at state level") +
  xlab("Against Constitutional Amendment") +
  geom_point(aes(size = mm), colour = "coral3") +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted", colour = "firebrick3") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 3),
        legend.position = "none") +
  ggtitle("Opinions on same-sex marriage by state") +
  scale_size_area()

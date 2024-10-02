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


## Fig 13.1 'Number of chargings carried out by user, by facility type, by location,
## and by charging station  (locations and charging stations are coloured
## by the type of facility where they were located)'

data(ElecCars, package="GmooG")

ElecCars <- ElecCars %>% mutate(Weekday = factor(weekday,
                           levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))
ElecCars <- ElecCars %>%
  mutate(FacilityType = case_when(
    facilityType == 1 ~ "Manufacturing",
    facilityType == 2 ~ "Office",
    facilityType == 3 ~ "R&D",
    facilityType == 4 ~ "Other"
))
ElecCars <- ElecCars %>%
  mutate(DateTime = ymd_hms(created))
ElecCars <- ElecCars %>%
  mutate(day = day(DateTime),
         month = month(DateTime, label = TRUE),
         year = year(DateTime),
         dayD = make_date(year = year, month = month, day = day))
ElecCars <- ElecCars %>%
  mutate(Month = factor(month, levels = c("Nov", "Dec", levels(month)[1:10])))
ElecCars <- ElecCars %>%
  group_by(FacilityType) %>%
  mutate(LocationId = fct_infreq(factor(locationId)))
ElecCars <- ElecCars %>%
  group_by(LocationId) %>%
  mutate(StationId = fct_infreq(factor(stationId)))
ElecCars <- ElecCars %>%
  ungroup() %>%
  mutate(weekNg = interval(min(DateTime), DateTime) %/% weeks(1) + 1)
colblindz <- c(colorblind_pal()(8)[c(3, 2, 7, 6)])

e1 <- ggplot(ElecCars, aes(FacilityType)) +
  geom_bar(aes(fill = FacilityType)) +
  ylab(NULL) +
  xlab(NULL) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2),
        legend.position = "none",
        axis.text.x = element_text(angle = -45)) +
  ggtitle("Use by facility type") +
  ylim(0, 2000) +
  scale_fill_manual(values = colblindz)
e2 <- ggplot(ElecCars, aes(LocationId)) +
  geom_bar(aes(fill = FacilityType)) +
  ylab(NULL) +
  xlab(NULL) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2),
        legend.position = "none",
        axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  ggtitle("Use by location") +
  ylim(0, 600) +
  scale_fill_manual(values = colblindz)
e3 <- ggplot(ElecCars, aes(StationId)) +
  geom_bar(aes(fill = FacilityType)) +
  ylab(NULL) +
  xlab(NULL) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2),
        legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  ggtitle("Use by charging station") +
  ylim(0, 400) +
  scale_fill_manual(values = colblindz)
e4 <- ggplot(ElecCars %>% group_by(userId) %>% summarise(nn = n()), aes(nn)) +
  geom_histogram(breaks = seq(0, 200, 5), fill = "grey70") +
  ylab(NULL) +
  xlab(NULL) +
  ylim(0, 25) +
  ggtitle("How often users charged their cars") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2))
layout <- "
   DDDDDD
   DDDDDD
   DDDDDD
   ##BBBB
   AABBBB
   AACCCC
   ##CCCC
"
e1 + e2 + e3 + e4 + plot_layout(design = layout)


## Fig 13.2 'Numbers of charging stations by facility type and location'

ex <- unique(ElecCars %>% select(FacilityType, LocationId, StationId)) %>%
  group_by(FacilityType, LocationId) %>%
  mutate(nn = n())
ex2 <- unique(ex %>% select(FacilityType, LocationId, nn))
ex3 <- ex2 %>%
  group_by(FacilityType) %>%
  arrange(-nn, .by_group = TRUE) %>%
  ungroup() %>%
  mutate(ix = seq(1, 25),
         LocId = reorder(LocationId, ix))
  
ggplot(ex3, aes(LocId, weight = nn, fill = FacilityType)) +
  geom_bar() +
  ylab(NULL) +
  xlab(NULL) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(r = 2, unit = "cm")),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  ylim(0, 15) +
  scale_fill_manual(values = colblindz)


## Fig 13.3 'Numbers of charges per day with smooth added'

ElecCarsDW <- ElecCars %>%
  group_by(dayD) %>%
  summarise(UsesD = n())
  
ggplot(ElecCarsDW, aes(dayD, UsesD)) +
  geom_line(alpha = 0.3) +
  geom_smooth(se = FALSE, colour = "coral", span = 0.15) +
  ylab(NULL) +
  xlab(NULL) +
  scale_x_date(breaks = function(x) seq.Date(from = min(x), to = max(x), by = "2 months"),
               date_labels = "%e %b %Y", limits = as.Date(c("2014-11-03", "2015-10-13")))


## Fig 13.4 'Numbers of charging stations by FacilityType over the study'

ElecST <- ElecCars %>%
  group_by(StationId, FacilityType) %>%
  summarise(Startd = min(dayD)) %>%
  ungroup() %>%
  arrange(FacilityType, Startd) %>%
  group_by(FacilityType) %>%
  mutate(Number = seq_along(FacilityType))
ES2 <- data.frame(StationId = rep(NA, 4),
                  FacilityType = c("Manufacturing", "Office", "R&D", "Other"),
                  Startd = rep(ymd("2015-10-13"), 4), Number = c(6, 38, 57, 4))
ElecST <- rbind(ElecST, ES2)

ggplot(ElecST, aes(Startd, Number, group = FacilityType, colour = FacilityType)) +
  geom_step() +
  ylab(NULL) +
  xlab(NULL) +
  scale_x_date(breaks = function(x) seq.Date(from = min(x), to = max(x), by = "2 months"),
             date_labels = "%e %b %Y", limits = as.Date(c("2014-11-03", "2015-10-13"))) +
  scale_colour_manual(values = colblindz) +
  theme(legend.position = "bottom",
        legend.title = element_blank())


## Fig 13.5 'When charging stations were used during the study'

ECL <- ElecCars %>%
  group_by(StationId) %>%
  mutate(nn = n())
  
ElecSt1 <- ggplot(ECL, aes(DateTime, StationId)) +
  geom_point()
ElecSt1


## Fig 13.6 'When users charged their cars, sorted by number of charges'

ECu <- ElecCars %>%
  group_by(userId) %>%
  mutate(nn = n()) %>%
  ungroup()
ECu <- ECu %>% mutate(UserId = fct_reorder(factor(userId), nn))

ggplot(ECu, aes(DateTime, UserId)) +
  geom_point(colour = "blue") +
  ylab(NULL) +
  xlab(NULL) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())


## Fig 13.7 'Use of charging stations, coloured by facility type, grouped by location,
## ordered by first installation at location and by first date of use,
## with the end of testing marked in red'

ECL <- ECL %>% mutate(minX = min(DateTime))
ECL <- ECL %>%
  ungroup() %>%
  group_by(LocationId) %>%
  mutate(minY = min(DateTime)) %>%
  ungroup() %>%
  mutate(LocationId = fct_rev(fct_reorder(factor(LocationId), minY)))
ECL <- ECL %>%
  group_by(LocationId) %>%
  mutate(StationId = fct_reorder(factor(StationId), minX))
  
ElecSt2 <- ggplot(ECL, aes(dayD, StationId)) +
  geom_point(aes(colour = FacilityType)) +
  xlab(NULL) +
  ylab(NULL) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(), 
        strip.text.y.right = element_text(angle = 0)) +
  scale_x_date(breaks = function(x) seq.Date(from = min(x), to = max(x), by = "2 months"),
              date_labels = "%e %b %Y", limits = as.Date(c("2014-11-03", "2015-10-13"))) +
  geom_vline(xintercept = as.Date("2015-02-18"),
             colour = "red", lty = "dashed", linewidth = 2) +
  facet_grid(rows = vars(LocationId), scale = "free_y", space = "free_y") +
  scale_colour_manual(values = colblindz)
ElecSt2


## Fig 13.8 'Distribution of charging time in hours'

ggplot(ElecCars %>% filter(chargeTimeHrs < 12, kwhTotal > 0), aes(chargeTimeHrs)) +
  geom_histogram(breaks = seq(0, 12, 0.25), fill = "grey70") +
  geom_vline(xintercept = c(2, 4), lty = "dashed", colour = "red") +
  scale_x_continuous(breaks = seq(0, 12, 2), limits = c(0, 12)) +
  ylab(NULL) +
  xlab(NULL)


## Fig 13.9 'Energy charge in kilowatt-hours by charging time in hours'

ggplot(ElecCars %>% filter(chargeTimeHrs < 12, kwhTotal > 0),
       aes(chargeTimeHrs, kwhTotal)) +
  geom_point(alpha = 0.2, size = 1.5, stroke = 0) +
  ylab(NULL) +
  xlab(NULL) +
  scale_x_continuous(breaks = seq(0, 12, 2), limits = c(0, 12))


## Fig 13.10 'Energy charge in kilowatt-hours by charging time in hours
## where charges higher than 10 kwh have been coloured by the user ID
## (charges less than 10 kwh for those users have not been coloured)'

ElecCars <- ElecCars %>%
  group_by(userId) %>%
  mutate(useF = n(), UserId = factor(userId)) %>%
  ungroup()
colblindv <- colorblind_pal()(8)[c(2:6, 7)]

ggplot(ElecCars %>% filter(chargeTimeHrs < 12, kwhTotal > 0),
       aes(chargeTimeHrs, kwhTotal)) +
  geom_point(alpha = 0.2, size = 1.5, stroke = 0) +
  ylab(NULL) +
  xlab(NULL) +
  scale_x_continuous(breaks = seq(0, 12, 2), limits = c(0, 12)) +
  geom_point(data = ElecCars %>% filter(kwhTotal > 10),
  aes(chargeTimeHrs, kwhTotal, colour = UserId)) +
  scale_colour_manual(values = colblindv) +
  theme(legend.position = "bottom")


## Fig 13.11 'Energy charge in kilowatt-hours by charging time in hours, grouped
## by day of the week and type of facility, excluding charging times over six hours.
## The barchart shows the number of charges by day.'

e5 <- ggplot(ElecCars, aes(Weekday)) +
  geom_bar(aes(fill = Weekday)) +
  ylab(NULL) +
  xlab(NULL) +
  scale_fill_colorblind() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 3),
        legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  ggtitle("Number of charges by day of the week") +
  ylim(0, 800) +
  facet_wrap(vars(Weekday), nrow = 1, scales = "free_x")
e7 <- ggplot(ElecCars %>% filter(!facilityType == "4", chargeTimeHrs < 6),
             aes(chargeTimeHrs, kwhTotal)) +
  geom_point(aes(colour = Weekday), alpha = 0.2) +
  scale_x_continuous(limits = c(0, 6), breaks = c(0, 2, 4, 6)) +
  facet_grid(cols = vars(Weekday), rows = vars(FacilityType)) +
  xlab(NULL) +
  ylab(NULL) +
  scale_colour_colorblind() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 3),
        legend.position = "none") +
  ggtitle("Energy charge in kilowatt-hours by charging time in hours")
  
e7 / e5 + plot_layout(heights = c(4, 1))

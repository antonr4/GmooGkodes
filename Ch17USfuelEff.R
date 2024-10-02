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
library(dichromat)
})

theme_set(theme(plot.title = element_text(hjust = 0.5)))


## Fig 17.1 'Numbers of car models tested by the U.S. Environmental Protection Agency
## by fuel, drive type, and class'

data(VehEffUS, package="GmooG")

Veh1a <- VehEffUS %>% filter(fuelType1 %in% c("Diesel",
                                              "Premium Gasoline",
                                              "Regular Gasoline"))
Veh1b <- Veh1a %>% filter(!is.na(drive),
                          !is.na(displ),
                          !is.na(trany),
                          year < 2022)

# Pick out cars
cars <- c("Compact Cars", "Large Cars", "Midsize Cars", "Minicompact Cars", "Subcompact Cars", "Two Seaters")
Veh1d <- Veh1b %>% filter(VClass %in% cars)

# Sort car classes
Veh1d <- Veh1d %>% mutate(vClass = factor(VClass,
                                   levels = c("Two Seaters",
                                              "Minicompact Cars",
                                              "Subcompact Cars",
                                              "Compact Cars",
                                              "Midsize Cars",
                                              "Large Cars")))
                                              
Veh1e <- Veh1d %>% filter(!drive %in% c("2-Wheel Drive"))
Veh1g <- Veh1e %>% mutate(Drive = fct_lump(drive, 2, other_level = "All-Wheel"))
Veh1h <- Veh1g %>% filter(!atvType %in% c("Bifuel (CNG)"))

ggplot(Veh1h, aes(vClass)) +
  geom_bar(aes(fill = fuelType1)) +
  facet_grid(rows = vars(Drive), cols = vars(fuelType1)) +
  xlab(NULL) +
  ylab(NULL) +
  theme(strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "none") +
  scale_fill_manual(values = c("chocolate4", "darkgoldenrod1", "coral"))


## Fig 17.2 'Fuel efficiency of gasoline cars by fuel, drive type, and class'

ggplot(Veh1h %>% filter(!fuelType1 %in% c("Diesel")), aes(vClass, combined)) +
  geom_boxplot() +
  facet_grid(rows = vars(Drive), cols = vars(fuelType1)) +
  xlab(NULL) +
  ylab(NULL) +
  theme(strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "none")


## Fig 17.3 'Fuel efficiency of hybrid cars compared with the rest
## (boxplot widths are proportional to the square roots of the group sizes)'

Veh1j <- Veh1h %>%
         mutate(Hybrid = ifelse(atvType %in% c("Hybrid", "Plug-in Hybrid"), "Yes", "No"))
                                          
ggplot(Veh1j, aes(Hybrid, combined)) +
  geom_boxplot(varwidth = TRUE) +
  ylab(NULL) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  coord_flip()


## Fig 17.4 'Fuel efficiency of non-hybrid cars over the years'

Veh1i <- Veh1h %>% filter(!atvType %in% c("Hybrid", "Plug-in Hybrid"))

ggplot(Veh1i, aes(factor(year), combined)) +
  geom_boxplot() +
  ylab(NULL) +
  xlab(NULL) +
  theme(axis.text.x = element_text(angle = -45, vjust = 0.5))


## Fig 17.5 'Numbers of cars by class for six car manufacturers'
Veh1l <- Veh1d %>% filter(make %in% c("Rolls-Royce",
                                      "Aston Martin",
                                      "Ferrari",
                                      "Bentley",
                                      "Maserati",
                                      "Lamborghini"))
                                      
ggplot(Veh1l, aes(vClass)) +
  geom_bar(aes(fill = make)) +
  facet_grid(rows = vars(make)) +
  ylab(NULL) +
  xlab(NULL) +
  theme(strip.text.y = element_text(angle = 0),
        legend.position = "none",
        axis.text.x = element_text(angle = -45, hjust = 0.5, vjust = 0.2)) +
  scale_y_continuous(breaks = c(0, 150)) +
  scale_fill_manual(values = c("seagreen1", "darkgreen", "red2",
                               "deepskyblue4", "firebrick3", "slategray2"))


## Fig 17.6 'Numbers of models per year'

Veh1D <- Veh1b %>% filter(VClass %in% cars)
Veh1f <- Veh1b %>% filter(VClass %in% c("Small Sport Utility Vehicle 2WD",
                                        "Small Sport Utility Vehicle 4WD",
                                        "Sport Utility Vehicle - 2WD",
                                        "Sport Utility Vehicle - 4WD",
                                        "Standard Sport Utility Vehicle 2WD",
                                        "Standard Sport Utility Vehicle 4WD",
                                        "Small Station Wagons",
                                        "Midsize Station Wagons",
                                        "Midsize-Large Station Wagons"))
Veh1x <- rbind(Veh1D, Veh1f)
Veh1x <- Veh1x %>% mutate(vClass = factor(VClass,
                                   levels = c("Two Seaters",
                                              "Minicompact Cars",
                                              "Subcompact Cars",
                                              "Compact Cars",
                                              "Midsize Cars",
                                              "Large Cars",
                                              "Small Station Wagons",
                                              "Midsize Station Wagons",
                                              "Midsize-Large Station Wagons",
                                              "Sport Utility Vehicle - 2WD",
                                              "Small Sport Utility Vehicle 2WD",
                                              "Standard Sport Utility Vehicle 2WD",
                                              "Sport Utility Vehicle - 4WD",
                                              "Small Sport Utility Vehicle 4WD",
                                              "Standard Sport Utility Vehicle 4WD")))
models <- c("Two Seaters",
            "Minicompact Cars",
            "Subcompact Cars",
            "Compact Cars",
            "Midsize Cars",
            "Large Cars",
            "Small Station W",
            "Midsize Station W",
            "Midsize-Large SW",
            "SUV 2WD",
            "Small SUV 2WD",
            "Standard SUV 2WD",
            "SUV 4WD",
            "Small SUV 4WD",
            "Standard SUV 4WD")
levels(Veh1x$vClass) <- models
sw15pal <- c("aquamarine", "chocolate1", "cyan2", "coral4", "dodgerblue", "firebrick3",
             colorschemes$Categorical.12[c(1, 6, 2, 7, 9, 10, 8, 11, 12)])
names(sw15pal) <- models
xvc <- Veh1x %>% count(year, vClass)

ggplot(xvc, aes(year, n, group = vClass, colour = vClass)) +
  geom_line(linewidth = 1.25) +
  scale_colour_manual(values = sw15pal) +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab(NULL) +
  facet_wrap(vars(vClass), nrow = 3, dir = "v")


## Fig 17.7 'Numbers of station wagon and SUV models by year with categories
## arranged by station wagons and SUVs and within those groupings by year and size'
swSUnames <- c("Small Station Wagons",
               "Midsize-Large Station Wagons",
               "Midsize Station Wagons",
               "Sport Utility Vehicle - 2WD",
               "Sport Utility Vehicle - 4WD",
               "Small Sport Utility Vehicle 2WD",
               "Small Sport Utility Vehicle 4WD",
               "Standard Sport Utility Vehicle 2WD",
               "Standard Sport Utility Vehicle 4WD")
               
swSUVpal <- colorschemes$Categorical.12[c(1, 2, 6, 7, 8, 9, 10, 11, 12)]
names(swSUVpal) <- swSUnames

Veh1f <- Veh1f %>% mutate(vClass = factor(VClass, levels = swSUnames))
Veh1fx <- Veh1f %>%
  group_by(vClass, year) %>%
  summarise(nn = n())
  
vgf <- ggplot(Veh1fx, aes(year, weight = nn)) +
  geom_bar(aes(fill = vClass)) +
  facet_grid(rows = vars(vClass)) +
  scale_fill_manual(values = swSUVpal) +
  ylab(NULL) +
  xlab(NULL) +
  theme(strip.text.y = element_text(angle = 0),
        legend.position = "none") +
  scale_y_continuous(breaks = c(0, 150))
vgf


## Fig 17.8 'Fuel efficiency of Toyota Corolla models for manual transmission
## (lines with points) and automatic (lines) transmission'

mmtoyC <- Veh1h %>% filter(model == "Corolla",
                           trany %in% c("Manual 5-spd", "Automatic 4-spd"),
                           displ == "1.8", is.na(atvType))
mmtoyCL <- mmtoyC %>%
               pivot_longer(cols = city:combined, names_to = "Type", values_to = "Eff")
               
ggplot(mmtoyCL %>% filter(trany %in% c("Manual 5-spd")),
       aes(year, Eff, group = Type, colour = Type)) +
  geom_point() +
  geom_line() +
  geom_line(data = mmtoyCL %>% filter(trany %in% c("Automatic 4-spd")),
            aes(year, Eff, group = Type, colour = Type)) +
  ylab("mpg  ") +
  xlab(NULL) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5, hjust = 0)) +
  guides(col = guide_legend(reverse = TRUE, title = NULL)) +
  scale_x_continuous(breaks = seq(1993, 2017, 2)) +
  ylim(20, 40)


## Fig 17.9 'Fuel efficiency of Mazda MX-5 models for manual transmission
## (lines with points) and automatic (lines) transmission'

MX5C <- Veh1h %>% filter(model == "MX-5",
                         trany %in% c("Manual 6-spd", "Automatic (S6)"))
MX5CL <- MX5C %>% pivot_longer(cols = city:combined, names_to = "Type", values_to = "Eff")

ggplot(MX5CL %>% filter(trany %in% c("Manual 6-spd")),
       aes(year, Eff, group = Type, colour = Type)) +
  geom_point() +
  geom_line() +
  geom_line(data = MX5CL %>% filter(trany %in% c("Automatic (S6)")),
            aes(year, Eff, group = Type, colour = Type)) +
  ylab("mpg  ") +
  xlab(NULL) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5, hjust = 0)) +
  guides(col = guide_legend(reverse = TRUE, title = NULL)) +
  scale_x_continuous(breaks = seq(2006, 2022, 2), limits = c(2006, 2022)) +
  ylim(20, 40)

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


## Fig 10.1 'Numbers of people by year of flight'

data(astronauts, package="GmooG")

ggplot(astronauts, aes(year_of_mission)) +
  scale_x_continuous(breaks = seq(1960, 2020, 10)) +
  geom_bar(fill = "cyan3") +
  labs(x = NULL, y = NULL)


## Fig 10.2 'Scatterplot of age by year of flight coloured by sex,
## with smooths and their confidence intervals'

astronauts <- astronauts %>% mutate(age = year_of_mission - year_of_birth, 
                                    sex = factor(sex, levels = c("male", "female")))

astgB <- ggplot(astronauts %>% arrange(sex), aes(year_of_mission, age, group = sex)) +
  scale_colour_manual(values = c("springgreen3", "purple2")) +
  theme(legend.title = element_blank(),
        axis.title.y = element_text(angle = 0)) +
  xlab("year of mission")
astgX <- astgB +
         geom_point(aes(colour = sex), size = 1) +
         geom_smooth(aes(colour = sex),
                     linewidth = 2,
                     method = "gam", formula = y ~ s(x, bs = "cs"))
astgX


## Fig 10.3 'Numbers of participants by decade and by sex'

astronauts <- astronauts %>% mutate(decade = factor(10 * (year_of_mission %/% 10)))

astbD <- ggplot(astronauts, aes(decade)) +
  geom_bar(aes(fill = sex)) +
  facet_grid(vars(sex)) +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("springgreen3", "purple2"))
astbD


## Fig 10.4 'Scatterplot of mission time in hours by year of flight'

fttX <- ggplot(astronauts, aes(year_of_mission, hours_mission)) +
  geom_point() +
  labs(x = NULL, y = NULL)
fttX


## Fig 10.5 'Mission time in hours by year of flight coloured by space station programme'

astronauts <- astronauts %>%
  mutate(spst = factor(case_when(
    in_orbit == "ISS" ~ "ISS",
    in_orbit == "Mir" ~ "Mir",
    in_orbit == "Mir EP" ~ "Mir",
    grepl(("Skylab"), in_orbit) ~ "Skylab",
    grepl(("Salyut"), in_orbit) ~ "Salyut",
    grepl(("Saluyt"), in_orbit) ~ "Salyut",
    .default = "none"),
    levels = c("none", "Salyut", "Skylab", "Mir", "ISS")))
  
colblS <- c(colorblind_pal()(8)[c(1, 7, 3, 6, 8)])

astroF <- ggplot(astronauts, aes(year_of_mission, hours_mission)) +
  geom_point(aes(colour = spst)) +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  scale_colour_manual(values = colblS)
astroF


## Fig 10.6'Scatterplot of log mission time by year of flight
## coloured by space station programme'

ftlX <- ggplot(astronauts, aes(year_of_mission, hours_mission)) +
  geom_point(aes(colour = spst)) +
  labs(x = NULL, y = NULL) +
  scale_y_log10(breaks = c(1, 24, 168, 365 * 12, 365 * 24),
                labels = c("1 hour", "1 day", "1 week", "6 months", "1 year")) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  scale_colour_manual(values = colblS)
ftlX


## Fig 10.7 'Scatterplots of log mission time by year of flight by nationality'

astronauts <- astronauts %>%
  mutate(nation = case_when(
    nationality == "U.S." ~ "U.S.",
    nationality == "U.S.S.R/Russia" ~ "U.S.S.R./Russia",
    .default = "Other"
  ), nation = fct_infreq(nation))

flx <- ggplot(astronauts %>% select(-nation), aes(year_of_mission, hours_mission)) +
  geom_point(colour = "grey70", alpha = 0.5) +
  labs(x = NULL, y = NULL) +
  scale_y_log10(breaks = c(1, 24, 168, 365 * 12, 365 * 24),
                labels = c("1 hour", "1 day", "1 week", "6 months", "1 year")) +
  geom_point(data = astronauts,
                    aes(year_of_mission, hours_mission, colour = nation), size = 2) +
            theme(legend.position = "none",
                  panel.spacing.x = unit(1, "lines")) +
            scale_colour_manual(values = c("deepskyblue3", "red3", "darkslategray3")) +
            facet_wrap(vars(nation))
flx


## Fig 10.8 'Boxplot of biggest differences in times for (possibly) the same missions'

asM <- astronauts %>%
  group_by(mission_title, ascend_shuttle, in_orbit, descend_shuttle, year_of_mission) %>%
  summarise(mn = min(hours_mission),
            mx = max(hours_mission),
            dif = mx - mn)
asMg <- ggplot(asM, aes("", dif)) +
  geom_boxplot() +
  labs(x = NULL, y = NULL) +
  coord_flip()
asMg

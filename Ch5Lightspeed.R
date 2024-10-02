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


## Fig 5.1 'Measurements of the speed of light in air by Michelson by date and time with
## dotted lines drawn to separate Stigler\'s artificial groupings.  There is a limited
## amount of overplotting due to exact equality of measurements close together in time.'

data(Mich1879, package="GmooG")

# Organise speed, dates and times
Mich1879 <- Mich1879 %>% mutate(speed = 299000 + Value)
Mich1879 <- Mich1879 %>% mutate(datey = as.Date(dmy(Date)))
Mich1879 <- Mich1879 %>% mutate(t1 = ifelse(Time == "AM", 9, 15))
Mich1879 <- Mich1879 %>% mutate(datez = datey + hours(t1))
Mich1879 <- Mich1879 %>% mutate(Exp = factor(rep(1:5, each = 20)))
Mich1879 <- Mich1879 %>%
  group_by(datez) %>%
  mutate(id = seq(1, n()))
Mich1879 <- Mich1879 %>%
  ungroup() %>%
  mutate(datew = datez + hours(id))
Mich1879 <- Mich1879 %>% mutate(dt = difftime(datew, lag(datew), hours),
                                avt = datew - dt / 2)
colblindM <- c(colorblind_pal()(8)[1:4], "sienna4")

ptmichW <- ggplot(Mich1879, aes(datew, speed)) +
  theme(legend.position = "none",
        axis.title.y = element_text(angle = 0)) +
  ylab("km/s") +
  xlab("1879") +
  geom_vline(lty = "dotted", colour = "red",
  xintercept = c(Mich1879$avt[21], Mich1879$avt[41], Mich1879$avt[61], Mich1879$avt[81]))
ptmichX <- ptmichW +
           geom_point(aes(colour = Exp), size = 2) +
           scale_colour_manual(values = colblindM)
ptmichX
ptmichY <- ptmichW +
           geom_point(aes(colour = Exp), size = 4) +
           scale_colour_manual(values = colblindM)


## Fig 5.2 'Michelson\'s estimates of the speed of light in air by temperature,
## coloured by time of day'

Mich1879 <- Mich1879 %>% mutate(time = ifelse(Time == "Elec", "Night", Time))
colbl3 <- colorblind_pal()(8)[c(3, 1, 8)]

michTW <- ggplot(Mich1879, aes(Temperature, speed)) +
  ylab("Speed of light") +
  xlab("Temperature (°Fahrenheit)") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_text(angle = 0))
michTX <- michTW +
          geom_point(size = 2, aes(colour = time)) +
          scale_colour_manual(values = colbl3)
michTX
michTY <- michTW +
          geom_point(size = 4, aes(colour = time)) +
          scale_colour_manual(values = colbl3)


## Fig 5.3 'Histogram of measurements of the speed of light by Newcomb'

data(newcomb, package="GmooG")
newcomb <- newcomb %>% mutate(speed = 7442420 / Time)

ggplot(newcomb, aes(speed)) +
  geom_histogram(breaks = seq(299600, 300800, 50), fill = "cornflowerblue") +
  scale_x_continuous(breaks = seq(299600, 300800, 200)) +
  ylab(NULL) +
  xlab(NULL) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())


## Fig 5.4 'Boxplot of measurements of the speed of light by Newcomb'

boxnewcXX <- ggplot(newcomb, aes("", speed)) +
  geom_boxplot(outlier.color = "red") +
  scale_y_continuous(limits = c(299600, 300800), n.break = 8) +
  ylab(NULL) +
  xlab(NULL) +
  theme(axis.ticks.y = element_blank()) +
  coord_flip()
boxnewcXX


## Fig 5.5 'Density estimate of measurements of the speed of light by Newcomb
## excluding an outlier.'

ggplot(newcomb %>% filter(speed < 300500), aes(speed)) +
  geom_density(col = "cornflowerblue") +
  scale_x_continuous(limits = c(299600, 300800), n.break = 8) +
  ylab(NULL) +
  xlab("speed of light") +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())


## Fig 5.6 'Measurements of the speed of light by Newcomb displayed
## in the order the experiments were carried out'

newcomb <- newcomb %>% mutate(ID = seq(1, dim(newcomb)[1]))

newct1XX <- ggplot(newcomb, aes(ID, speed)) +
  geom_point() +
  ylim(299600, 300800) +
  xlab("order of experiments") +
  ylab("speed of light") +
  theme(axis.title.y = element_text(angle = 0))
newct1XX


## Fig 5.7 'Measurements of the speed of light by Newcomb in 1882 displayed by date,
## coloured by observer'

newcomb <- newcomb %>% mutate(Year = 1882)
newcomb <- newcomb %>% unite("date", c("Date", "Year"), sep = "-", remove = FALSE)
newcomb <- newcomb %>% mutate(datey = as.Date(dmy(date)))
newct2XW <- ggplot(newcomb, aes(datey, speed)) +
  ylim(299600, 300800) +
  xlab(NULL) +
  ylab(NULL) +
  theme(legend.position = "bottom")
  
newct2XX <- newct2XW +
            geom_point(aes(colour = Observer)) +
            scale_color_manual(values = c("#38598CFF", "#51C56AFF")) +
            guides(colour = guide_legend(reverse = TRUE))
newct2XX
newct2XY <- newct2XW +
            geom_point(aes(colour = Observer), size = 4) +
            scale_color_manual(values = c("#38598CFF", "#51C56AFF")) +
            guides(colour = guide_legend(reverse = TRUE))


## Fig 5.8 'Weights assigned to each observation by Newcomb.'

ggplot(newcomb, aes(Wt)) +
  geom_bar(fill = "brown4") +
  xlab(NULL) +
  ylab(NULL) +
  ylim(0, 45)


## Fig 5.9 'Boxplots and dotplots of estimates of the speed of light in air
## by Michelson in 1879 and by Newcomb in 1882
## with the transformed value of today marked by a dashed blue line'

cMN <- c(Mich1879$speed, newcomb$speed)
cMN <- data.frame(cMN)
cMN$group <- c(rep("Michelson", dim(Mich1879)[1]), rep("Newcomb", dim(newcomb)[1]))
names(cMN) <- c("speed", "Scientist")

ggplot(cMN %>% filter(speed < 300200), aes(fct_rev(Scientist), speed)) +
  geom_boxplot(outlier.colour = "red") +
  geom_point(colour = "grey70", alpha = 0.5) +
  geom_hline(yintercept = 299700, linetype = "dashed", colour = "blue") +
  scale_y_continuous(limits = c(299600, 300200), n.break = 4) +
  ylab(NULL) +
  xlab(NULL) +
  coord_flip()

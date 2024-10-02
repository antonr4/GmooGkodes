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
library(ggrepel)
library(ComradesM)
})


theme_set(theme(plot.title = element_text(hjust = 0.5)))


## Fig 21.1 'Numbers of finishers of the Comrades Marathon and the race direction by year
## (dark orange is up, black is down).'

data(cmAll, package="ComradesM")

cmAllD <- cmAll %>%
  group_by(Direction, Year) %>%
  summarise(N = n()) %>%
  ungroup()
cmAllD <- cmAllD %>% mutate(wt = ifelse(Direction == "Up", 1, -1))

d1 <- ggplot(cmAllD, aes(Year, weight = wt)) +
  geom_bar(aes(fill = Direction)) +
  xlab(NULL) +
  ylab(NULL) +
  theme(legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  ylim(-2, 2) +
  scale_fill_manual(values = c("black", "#E69F00")) +
  scale_x_continuous(breaks = seq(1920, 2020, 10))
  
cmAllfastn <- cmAll %>%
  group_by(Year) %>%
  summarise(N = n())
cmAllfastn <- cmAllfastn %>% complete(Year = full_seq(Year, 1))

d2 <- ggplot(cmAllfastn, aes(Year, N)) +
  geom_line(colour = "deeppink3", lwd = 1.5) +
  ylab(NULL) +
  xlab(NULL) +
  scale_x_continuous(breaks = seq(1920, 2020, 10))
  
d2 / d1 + plot_layout(heights = c(3, 1))


## Fig 21.2 'Numbers of finishers by age group and sex from 1975 to 2019
## (the brown line is for those whose birthyear is not given)'

cmAll <- cmAll %>% separate(Category, c("Sex", "Cat"), sep = " ", remove = FALSE)
cmAll <- cmAll %>% mutate(cat = factor(Cat, levels = c("SEN", "VET", "MAS", "GM")))
cmAllcat <- cmAll %>%
  group_by(Year, Category, Sex, cat) %>%
  summarise(nn = n())
cmAllcat75 <- cmAllcat %>%
  ungroup() %>%
  filter(Year > 1974)
  
comX <- ggplot(cmAllcat75, aes(Year, nn, group = Category)) +
  geom_line(aes(colour = cat, lty = Sex)) +
  scale_linetype_manual(values = c("longdash", "solid")) +
  ylab(NULL) +
  xlab(NULL) +
  theme(legend.position = "none") +
  geom_text_repel(data = cmAllcat %>% filter(Year == 2019),
                  aes(label = Category, x = Year + 0.5, y = nn, colour = cat),
                  nudge_x = 0.5) +
  xlim(1975, 2021) +
  scale_colour_colorblind() +
  geom_line(data = cmAllcat75 %>% filter(is.na(Category)),
            aes(Year, nn), colour = "brown", lty = "solid")
comX


## Fig 21.3 'Numbers of female (purple) and foreign (red) finishers since 1975'

cmAllfastw <- cmAll %>%
  filter(Year > 1974, Sex == "F") %>%
  group_by(Year) %>%
  summarise(N = n())
cmAllfastf <- cmAll %>%
  filter(Year > 1974, !Nation == "RSA") %>%
  group_by(Year) %>%
  summarise(Nf = n())
  
ggplot(cmAllfastw, aes(Year, N)) +
  geom_line(colour = "purple2", lwd = 1.5) +
  ylab(NULL) +
  xlab(NULL) +
  geom_line(data = cmAllfastf, aes(Year, Nf), colour = "indianred1", lwd = 1.5)


## Fig 21.4 'Distributions of finishing times in hours by year'

cmAll <- cmAll %>%
    mutate(timeSecs = 3600 * hour(Time) + 60 * minute(Time) + second(Time))
cmAllx <- cmAll %>% filter(timeSecs <= 12 * 3600)

ggplot(cmAllx, aes(Year, timeSecs, group = Year)) +
  geom_boxplot() +
  ylab("hours") +
  xlab(NULL) +
  scale_y_continuous(breaks = seq(5 * 3600, 11 * 3600, 7200),
                     labels = c("5", "7", "9", "11"),
                     limits = c(5 * 3600, 12 * 3600)) +
  geom_hline(yintercept = c(11 * 3600, 12 * 3600), linetype = "dashed", colour = "red") +
  theme(axis.title.y = element_text(angle = 0))


## Fig 21.5 'Median times (red), best times (blue) and
## numbers of finishers (barchart) from 1946 to 2019'

cmAllstats <- cmAll %>%
  group_by(Year) %>%
  summarise(N = n(),
            best = min(timeSecs, na.rm = TRUE),
            mid = median(timeSecs))
cmAllstats <- cmAllstats %>% complete(Year = full_seq(Year, 1))

h1 <- ggplot(cmAllstats, aes(Year)) +
  geom_line(aes(y = best), colour = "blue") +
  geom_line(aes(y = mid), colour = "red") +
  xlab(NULL) +
  ylab("hours") +
  xlim(1945, 2020) +
  scale_y_continuous(breaks = seq(5 * 3600, 11 * 3600, 7200),
                     labels = c("5", "7", "9", "11"),
                     limits = c(5 * 3600, 12 * 3600)) +
  theme(axis.title.y = element_text(angle = 0))
h2 <- ggplot(cmAllstats, aes(Year, weight = N)) +
  geom_bar(fill = "grey70") +
  xlim(1945, 2020) +
  ylab(NULL) +
  xlab(NULL)
h1 / h2 + plot_layout(heights = c(5, 3))


## Fig 21.6 'Best times for women (purple) and men (green) since 1980'

cmAllsx <- cmAll %>%
  filter(!is.na(Sex), Year > 1979) %>%
  group_by(Year, Sex) %>%
  summarise(N = n(),
            best = min(timeSecs, na.rm = TRUE),
            mid = median(timeSecs))
  
pcmA <- ggplot(cmAllsx, aes(Year, best)) +
  geom_line(aes(colour = Sex)) +
  xlab(NULL) +
  ylab("hours") +
  scale_y_continuous(limits = c(5 * 3600, 8 * 3600),
                     breaks = seq(5 * 3600, 8 * 3600, 3600),
                     labels = c("5", "6", "7", "8")) +
  scale_colour_manual(values = c("purple2", "springgreen3")) +
  theme(legend.position = "none",
        axis.title.y = element_text(angle = 0))
  
cmAllsxW <- cmAllsx %>%
  select(-best, -mid) %>%
  pivot_wider(names_from = Sex, values_from = N)
cmAllsxW <- cmAllsxW %>% mutate(pFemale = F / (F + M))

pcmFperc <- ggplot(cmAllsxW, aes(Year, 100 * pFemale)) +
  geom_line(col = "purple2") +
  xlab(NULL) +
  ylab("% Female") +
  ylim(0, 25) +
  theme(axis.title.y = element_text(angle = 0))
pcmFperc_dim <- get_dim(pcmFperc)
set_dim(pcmA, pcmFperc_dim)


## Fig 21.7 'Percentage of women participants since 1980'
pcmFperc


## Fig 21.8 'Median times for men and women for "up" (dark orange) and "down" (black)
## races since 1980 and the numbers finishing'

cmAlld <- cmAll %>%
  filter(!is.na(Sex),
         Year > 1945) %>%
  group_by(Direction,
           Year,
           Sex) %>%
  summarise(N = n(),
            best = min(timeSecs, na.rm = TRUE),
            mid = median(timeSecs))
cmAlld <- cmAlld %>% mutate(sex = factor(Sex, levels = c("M", "F"),
                                              labels = c("Men", "Women")))
                                              
gm1 <- ggplot(cmAlld %>% ungroup() %>% filter(Year > 1979),
              aes(Year, mid, group = Direction)) +
  geom_line(aes(colour = Direction)) +
  ylab("hours") +
  xlab(NULL) +
  scale_colour_colorblind() +
  facet_wrap(vars(sex)) +
  geom_point(aes(colour = Direction)) +
  scale_y_continuous(breaks = seq(8 * 3600, 11 * 3600, 3600),
                     labels = c("8", "9", "10", "11"),
                     limits = c(8 * 3600, 12 * 3600)) +
  theme(legend.position = "none",
        axis.title.y = element_text(angle = 0))
  
gm2 <- ggplot(cmAlld %>% ungroup() %>% filter(Year > 1979),
              aes(Year, weight = N, group = Direction)) +
  geom_bar(aes(fill = Sex)) +
  ylab(NULL) +
  xlab(NULL) +
  scale_colour_colorblind() +
  theme(legend.position = "none") +
  facet_wrap(vars(sex)) +
  scale_fill_manual(values = c("purple2", "springgreen3"))
  
CM12 <- gm1 / gm2
CM12 + plot_layout(heights = c(3, 1))


## Fig 21.9 'Finishing times in the 2019 race for females (above) and males (below),
## dotted lines mark limits for awarding medals other than gold'

cmAll2019 <- cmAll %>% filter(Year == 2019)
cmAll2019t <- cmAll2019 %>%
  filter(!is.na(Sex)) %>%
  group_by(Sex) %>%
  slice_min(prop = 0.05, order_by = timeSecs)
  
vx <- ggplot() +
  ylab(NULL) +
  xlab("hours") +
  scale_x_continuous(breaks = seq(5 * 3600, 11 * 3600, 7200),
                     labels = c("5", "7", "9", "11"),
                     limits = c(5 * 3600, 12 * 3600)) +
  theme(legend.position = "none") +
  geom_vline(xintercept = c(7.5 * 3600, 9 * 3600, 10 * 3600, 11 * 3600, 12 * 3600),
             linetype = "dotted")
             
cmvf <- vx + geom_histogram(data = cmAll2019 %>% filter(Sex == "F"), aes(timeSecs),
                            fill = "purple2", breaks = seq(5 * 3600, 12 * 3600, 300))
cmvm <- vx + geom_histogram(data = cmAll2019 %>% filter(Sex == "M"), aes(timeSecs),
                            fill = "springgreen3", breaks = seq(5 * 3600, 12 * 3600, 300))
cmvf / cmvm


## Fig 21.10 'Comparisons of best times in hours for the "up" (dark orange)
## and "down" (black) races since 1921'

cmAllS <- cmAll %>%
  group_by(Direction, Year) %>%
  summarise(N = n(),
            best = min(timeSecs, na.rm = TRUE),
            mid = median(timeSecs)) %>%
  ungroup() %>%
  complete(Year = full_seq(Year, 1))
cmAllS[21:25, "Direction"] <- c("Down", "Up", "Down", "Up", "Down")

ggplot() +
  geom_line(data = cmAllS %>% filter(Direction == "Up"),
            aes(Year, best), colour = "#E69F00", ) +
  geom_line(data = cmAllS %>% filter(Direction == "Down"),
            aes(Year, best), colour = "black") +
  xlab(NULL) +
  ylab("hours") +
  scale_y_continuous(breaks = seq(5 * 3600, 10 * 3600, 7200),
                     labels = c("5", "7", "9"),
                     limits = c(5 * 3600, 10 * 3600)) +
  theme(axis.title.y = element_text(angle = 0))


## Fig 21.11 'Average speed of the fastest runners (above)
## and of the median runners (below) in km/hr
## for the "up" (dark orange) and "down" (black) races since 1970'

data(cmDist, package="ComradesM")

cmAD <- left_join(cmDist, cmAllS %>% filter(Year > 1969))
cmAD <- cmAD %>% mutate(paceBest = 3600 * Distance / best,
                        paceMed = 3600 * Distance / mid)
                        
comUDpX <- ggplot() +
  geom_line(data = cmAD %>% filter(Direction == "Up"),
            aes(Year, paceBest), colour = "#E69F00") +
  geom_line(data = cmAD %>% filter(Direction == "Down"),
            aes(Year, paceBest), colour = "black") +
  xlab(NULL) +
  ylab("km/hr") +
  ylim(7.5, 17.5) +
  geom_line(data = cmAD %>% filter(Direction == "Up"),
            aes(Year, paceMed), colour = "#E69F00", ) +
  geom_line(data = cmAD %>% filter(Direction == "Down"),
            aes(Year, paceMed), colour = "black") +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.4))
comUDpX


## Fig 21.12 'Distances in km for the "up" (dark orange) and "down" (black) races
## since 1970 and a profile plot for the down race
## from Pietermaritzburg to Durban that was planned for 2020'

profUD <- ggplot() +
  geom_line(data = cmAD %>% filter(Direction == "Up"),
            aes(Year, Distance), colour = "#E69F00") +
  geom_line(data = cmAD %>% filter(Direction == "Down"),
            aes(Year, Distance), colour = "black") +
  xlab(NULL) +
  ylab("km") +
  ylim(0, 100) +
  geom_point(data = cmAD %>% filter(Direction == "Up"),
             aes(Year, Distance), colour = "#E69F00") +
  geom_point(data = cmAD %>% filter(Direction == "Down"),
             aes(Year, Distance), colour = "black") +
  theme(axis.title.y = element_text(angle = 0))
  
data(cmE, package="ComradesM")

cmE <- cmE %>% mutate(name = Name)
cmE[c(20, 26, 32, 38, 40, 83, 93), "name"] <- NA
cmE[106, "name"] <- "Durban City Hall"

ComProfx <- ggplot(cmE, aes(Distance, Elevation)) +
  geom_area(fill = "green") +
  xlim(0, 100) +
  ylim(0, 1000) +
  ylab("Elevation in metres") +
  xlab("Distance in km") +
  theme_classic()
  
profUD + ComProfx


## Fig 21.13 'Performances of the Nurgalieva identical twins'

cmAllnurg <- cmAll %>% filter(grepl("Nurgalieva", Name))

# Remove false duplicate in 2014
cmAllnurg <- cmAllnurg %>% filter(timeSecs < 30000)
cmAllnurg <- cmAllnurg %>% complete(Year = 2003:2015, nesting(Name))

nb <- ggplot(cmAllnurg, aes(Year, timeSecs, group = Name, colour = Name)) +
  geom_line() +
  scale_x_continuous(breaks = seq(2003, 2015, 2)) +
  ylab("hours") +
  xlab(NULL) +
  theme(legend.position = "none") +
  scale_colour_discrete(labels = c("Elena", "Olesya"))
n1 <- nb + scale_y_continuous(breaks = seq(5 * 3600, 11 * 3600, 7200),
                              labels = c("5", "7", "9", "11"),
                              limits = c(5 * 3600, 12 * 3600))
n2 <- nb + scale_y_continuous(breaks = seq(6 * 3600, 7 * 3600, 3600),
                              labels = c("6", "7"),
                              limits = c(6 * 3600, 7 * 3600))
                              
nurg12 <- n1 + n2 + plot_layout(guides = "collect") &
          theme(legend.position = "bottom",
                legend.title = element_blank(),
                axis.title.y = element_text(angle = 0))
nurg12


## Fig 21.14 'Runners who have finished more than 40 times,
## ordered by the first time they ran the race'

cmAllm <- cmAll %>%
  group_by(Bib, Name) %>%
  mutate(nn = n()) %>%
  filter(nn > 40)
cmAllm <- rbind(cmAllm, cmAll %>% filter(Name == "Dave  Rogers" & Bib == 196))
cmAllm <- rbind(cmAllm, cmAll %>% filter(Name == "Mike  Cowling"))
cmAllm <- cmAllm %>%
  group_by(Name) %>%
  mutate(Year1 = min(Year))
cmAllm <- cmAllm %>% mutate(cat = fct_na_value_to_level(cat, level = "none"))
cmAllm <- cmAllm %>%
  ungroup() %>%
  mutate(Name = factor(Name), Name = fct_reorder(Name, Year1))
cmAllm <- cmAllm %>% complete(Year = 1957:2019, nesting(Name))
colblind2 <- c(colorblind_pal()(4), "brown")

ggplot(cmAllm, aes(Year, timeSecs)) +
  geom_line(aes(colour = cat)) +
  ylab("hours") +
  xlab(NULL) +
  facet_wrap(vars(Name), nrow = 4) +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(5 * 3600, 11 * 3600, 7200),
                     labels = c("5", "7", "9", "11"),
                     limits = c(5 * 3600, 12 * 3600)) +
  geom_point(aes(colour = cat), size = 0.5) +
  scale_colour_manual(values = colblind2, na.translate = F) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_text(angle = 0))

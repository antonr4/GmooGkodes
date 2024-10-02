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
library(ggpcp)
library(lvplot)
})

theme_set(theme(plot.title = element_text(hjust = 0.5)))


## Fig 20.1 'Best times by male swimmers for the 100 m freestyle'

data(All200, package = "GmooG")

ggplot(All200 %>% filter(dist == 50,
                         Gender == "Men",
                         style == "Freestyle"),
       aes(sdate, SwimTime)) +
  geom_point() +
  xlab(NULL) +
  ylab("time (secs)") +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  geom_vline(xintercept = as.Date("2010-01-01"), colour = "red")


## Fig 20.2 'Best times by male swimmers for the four 50 m events'

All200 <- All200 %>% mutate(Style = factor(style, levels = c("Breaststroke",
                                                             "Backstroke",
                                                             "Butterfly",
                                                             "Freestyle",
                                                             "Medley",
                                                             "Medley Relay",
                                                             "Freestyle Relay")))
                                                             
swM5x <- ggplot(All200 %>% filter(dist == 50,
                                  Gender == "Men"),
                aes(sdate, SwimTime)) +
  geom_point(aes(colour = Style)) +
  xlab(NULL) +
  ylab("time (secs)") +
  theme(legend.title = element_blank(),
        axis.title.y = element_text(angle = 0, vjust = 0.5, hjust = 0))
swM5x


## Fig 20.3 'Best times for the four 50 m events achieved by men and women'

sw50y <- ggplot(All200 %>% filter(dist == 50), aes(sdate, SwimTime)) +
  geom_point(aes(colour = Style)) +
  xlab(NULL) +
  ylab("time (secs)") +
  theme(legend.title = element_blank(),
        axis.title.y = element_text(angle = 0, vjust = 0.5, hjust = 0)) +
  facet_wrap(vars(Gender), nrow = 1)
sw50y
sw50yX <- sw50y + theme(legend.position = "none",
                        axis.title.y = element_blank())


## Fig 20.4 'Boxplots of the best times by women for the four 50 m events'

ggplot(All200 %>% filter(dist == 50,
                         Gender == "Women"),
       aes(fct_rev(Style), SwimTime)) +
  geom_boxplot(aes(colour = Style)) +
  xlab(NULL) +
  ylab("time (secs)") +
  theme(legend.position = "none",
        axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  coord_flip()


## Fig 20.5 'Best times for the four 200 m events achieved by men and women'

sw200y <- ggplot(All200 %>% filter(dist == 200,
                                   !style %in% c("Medley")),
                 aes(sdate, SwimTime)) +
  geom_point(aes(colour = Style)) +
  xlab(NULL) +
  ylab("time (secs)") +
  theme(legend.title = element_blank(),
        axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  facet_wrap(vars(Gender), nrow = 1)
sw200y
sw200yX <- sw200y + theme(legend.position = "none",
                          axis.title.y = element_blank())


## Fig 20.6 'Time differences between men and women for the four 100 m events by rank'

All200dif <- All200 %>%
  pivot_wider(names_from = Gender,
              values_from = SwimTime,
              id_cols = c(Style, dist, Rank_Order)) %>%
  mutate(wm = Women - Men)
  
ggplot(All200dif %>% filter(dist == 100), aes(Rank_Order, wm, colour = Style)) +
  geom_line(aes(colour = Style)) +
  theme(legend.title = element_blank(),
        axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  ylab("difference (secs)")


## Fig 20.7 'Katinka Hosszu\'s best times by event (in purple)
## compared with the 200 best in each event'

All200x <- All200 %>% filter(!distance %in% c("4x100", "4x200"))
All200y <- All200x %>%
  group_by(full_name_computed) %>%
  mutate(app = n()) %>%
  ungroup()
All200y <- All200y %>%
  unite(Styledist, c("Style", "dist"), remove = FALSE) %>%
  mutate(yz = 1, Styledist = factor(Styledist, levels = c("Backstroke_50",
                                                          "Backstroke_100",
                                                          "Backstroke_200",
                                                          "Breaststroke_50",
                                                          "Breaststroke_100",
                                                          "Breaststroke_200",
                                                          "Butterfly_50",
                                                          "Butterfly_100",
                                                          "Butterfly_200",
                                                          "Freestyle_50",
                                                          "Freestyle_100",
                                                          "Freestyle_200",
                                                          "Freestyle_400",
                                                          "Freestyle_800",
                                                          "Freestyle_1500",
                                                          "Medley_200",
                                                          "Medley_400")))
                                                          
KHa <- ggplot(All200y %>% filter(Gender == "Women"), aes(yz, SwimTime)) +
  geom_lv(outlier.colour = "grey80", colour = "grey80", fill = "grey80") +
  geom_point(data = All200y %>%
             filter(Gender == "Women",
                    full_name_computed == "HOSSZU, Katinka"),
             aes(yz, SwimTime), colour = "purple2", size = 4) +
  facet_wrap(vars(Styledist), scales = "free", nrow = 1) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        strip.text.x = element_text(angle = -75))
KHa


## Fig 20.8 'Michael Phelps\'s best times by event (in green)
## compared with the 200 best in each event'

ggplot(All200y %>% filter(Gender == "Men"), aes(yz, SwimTime)) +
  geom_lv(outlier.colour = "grey80", colour = "grey80", fill = "grey80") +
  geom_point(data = All200y %>%
             filter(Gender == "Men",
                    full_name_computed == "PHELPS, Michael"),
             aes(yz, SwimTime), colour = "springgreen3", size = 4) +
  facet_wrap(vars(Styledist), scales = "free", nrow = 1) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        strip.text.x = element_text(angle = -75))


## Fig 20.9 'A parallel coordinate plot of the ranked times standardised
## by event across the individual events'

All200p <- All200x %>% unite("event", c("Gender", "distance", "Style"))
All200p <- All200p %>% select(event, SwimTime, Rank_Order)
All200px <- All200p %>% pivot_wider(names_from = event,
                                    values_from = SwimTime)
                                    
All200px %>%
  pcp_select(Women_50_Freestyle:Men_400_Medley) %>%
  pcp_scale(method = "uniminmax") %>%
  ggplot(aes_pcp()) +
  geom_pcp_axes(colour = "white") +
  geom_pcp(alpha = 0.25) +
  ylab(NULL) +
  xlab(NULL) +
  theme(axis.text.x = element_blank()) +
  coord_flip()


## Fig 20.10 'A parallel coordinate plot of the ranked times
## aligning the 100th ranks (the red line) across the individual events,
## sorted by relative fastest times'

All200q <- All200p %>%
  group_by(event) %>%
  arrange(SwimTime) %>%
  mutate(dx200 = nth(SwimTime, 100) - min(SwimTime),
         Range = max(SwimTime) - min(SwimTime), aa = dx200 / Range)
ax <- max(All200q$aa)
All200q <- All200q %>% mutate(stsw = (SwimTime - min(SwimTime)) / Range,
                              st1 = 1 - (1 - stsw) * (1 - ax) / (1 - aa))
All200qX <- All200q %>% select(event, st1, Rank_Order)
All200qX <- All200qX %>%
  ungroup() %>%
  mutate(event = fct_reorder(factor(event), st1, min))
All200qX <- All200qX %>% arrange(event)
All200qY <- All200qX %>% pivot_wider(names_from = event, values_from = st1)

All200qY %>%
  pcp_select(Men_50_Freestyle:Men_200_Breaststroke) %>%
  pcp_scale(method = "raw") %>%
  ggplot(aes_pcp()) +
  geom_pcp_axes(colour = "white") +
  geom_pcp(alpha = 0.25) +
  geom_hline(yintercept = ax, colour = "red") +
  ylab(NULL) +
  xlab(NULL) +
  theme(axis.text.x = element_blank()) +
  coord_flip()


## Fig 20.11 'Best times for the 400 m freestyle events for men and women,
## individual (red) and relay (blue)'

ir400 <- ggplot(All200 %>% filter(dist == 400,
                                  style %in% c("Freestyle", "Freestyle Relay")),
                aes(sdate, SwimTime)) +
  geom_point(aes(colour = Style)) +
  xlab(NULL) +
  ylab("time (secs)") +
  theme(legend.title = element_blank(),
        axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  facet_wrap(vars(Gender), nrow = 1)
ir400


## Fig 20.12 'Numbers of female swimmers in the top 200 plotted against
## numbers of male swimmers in the top 200 for each of 17 events
## by country (the dashed lines represent equality)'

All200r <- All200x %>% unite("comp", c("distance", "Style"))
All200s <- All200r %>%
  group_by(Gender, comp, team_code) %>%
  summarise(nc = n())
All200sw <- All200s %>% pivot_wider(names_from = Gender, values_from = nc)
All200swC <- All200sw %>%
  ungroup() %>%
  group_by(team_code) %>%
  summarise(ttm = sum(Men, na.rm = TRUE),
            ttw = sum(Women, na.rm = TRUE),
            tta = ttm + ttw)
colblind4 <- c(colorblind_pal()(8), "grey60")

ggplot(All200sw %>% filter(team_code %in% c("USA", "JPN", "AUS", "RUS", "CHN",
                                            "CAN", "GER", "GBR", "ITA")),
       aes(Men, Women)) +
  geom_point(aes(colour = team_code)) +
  scale_colour_manual(values = colblind4) +
  geom_abline(slope = 1, colour = "brown", linetype = "dashed") +
  coord_fixed() +
  facet_wrap(vars(team_code)) +
  xlim(0, 55) +
  ylim(0, 55) +
  theme(legend.position = "none",
        axis.title.y = element_text(angle = 0, vjust = 0.5))

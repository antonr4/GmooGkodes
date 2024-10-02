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

## Fig 4.1 'Votes for the candidates over the 46 ballots'

data(DC1912, package="GmooG")

DC1912x <- DC1912 %>%
  group_by(Ballot, Candidate) %>%
  summarise(v = sum(Votes))
  
cand1 <- ggplot(DC1912x, aes(Ballot, v, colour = Candidate)) +
  geom_line() +
  ylab("votes")
cand1


## Fig 4.2 'Numbers of delegates by state and territory
## at the 1912 and 2020 Democratic conventions (different horizontal scales)'

dcs <- DC1912 %>%
  group_by(State, Ballot) %>%
  summarise(bv = sum(Votes))
dcs <- dcs %>%
  ungroup() %>%
  group_by(State) %>%
  summarise(mxb = max(bv))
  
data(DC1912dels, package="GmooG")
delsC <- full_join(DC1912dels, dcs)
delsC <- delsC %>% mutate(Mxb = ifelse(is.na(mxb), 0, mxb))
delsC <- delsC %>% mutate(state = fct_reorder(State, -Mxb))
delsCl <- delsC %>% pivot_longer(cols = c(TotP, mxb),
                                 names_to = "Year",
                                 values_to = "Delegs")
delsCl <- delsCl %>% mutate(year = ifelse(Year == "mxb", "1912", "2020"))

ggplot(delsCl, aes(state, weight = Delegs)) +
  geom_bar(aes(fill = region)) +
  xlab(NULL) +
  ylab(NULL) +
  coord_flip() +
  facet_wrap(vars(year), nrow = 1, scales = "free_x") +
  scale_fill_colorblind()


## Fig 4.3 'Percentage state shares of delegates at the 1912 and 2020 Democratic conventions
## (point sizes are proportional to the number of delegates in 2020)'

delsC <- delsC %>% mutate(all12 = sum(mxb, na.rm = TRUE),
                          mxp = 100 * mxb / all12,
                          all20 = sum(TotP),
                          TotPp = 100 * TotP / all20)
delsCx <- na.omit(delsC) %>% filter(!(state == "Puerto Rico"))

delsS <- ggplot(delsCx, aes(mxp, TotPp)) +
  geom_point(aes(col = region, size = TotP)) +
  xlab("1912") +
  ylab("2020") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_text(angle = 0)) +
  guides(colour = guide_legend(nrow = 1), size = "none") +
  xlim(0, 11) +
  ylim(0, 11) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
  geom_text(data = delsC %>% filter(TotP > 180), aes(label = state), nudge_y = -0.45) +
  scale_colour_colorblind()
delsS


## Fig 4.4 'Percentage state shares of delegates by region'

delsJ <- ggplot(delsCx, aes(mxp, TotPp)) +
  geom_point(aes(col = region, size = TotP)) +
  xlab("1912") +
  ylab("2020") +
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.title.y = element_text(angle = 0)) +
  guides(size = "none") +
  xlim(0, 11) +
  ylim(0, 11) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
  facet_wrap(vars(region)) +
  scale_colour_colorblind()
delsJ
delsJa <- delsJ +
          geom_point(data = delsCx %>% select(-region), aes(size = TotP),
                     colour = alpha("black", 0.1))


## Fig 4.5 'Delegates and Electoral College votes by state in 1912 (left) and 2020 (right)'

data(DC1912evs, package="GmooG")

dcsx <- left_join(dcs, DC1912evs)

gev1 <- ggplot(dcsx, aes(y19121928, mxb)) +
  geom_point() +
  xlab("Electoral College votes") +
  ylab("Convention delegates") +
  xlim(0, 50) +
  ylim(0, 100) +
  theme(legend.position = "none") +
  geom_abline(intercept = 0, slope = 2, linetype = "dotted")
  
EV2020x <- right_join(DC1912dels, DC1912evs)
m1 <- lm(data = EV2020x, TotP ~ y20122020)
gev2 <- ggplot(EV2020x, aes(y20122020, TotP)) +
  geom_point() +
  xlab("Electoral College votes") +
  ylab("Convention delegates") +
  geom_abline(intercept = m1$coefficients[1],
              slope = m1$coefficients[2], linetype = "dotted") +
  geom_text(data = EV2020x %>% filter(TotP > 160),
            aes(label = State), size = 3, colour = "blue", nudge_y = -9, nudge_x = -4)
            
gev1 + gev2


## Fig 4.6 'Votes for the main candidates over the 46 ballots'

dcX <- DC1912 %>% expand(State, Candidate, Ballot)
dcY <- DC1912 %>% right_join(dcX)
dcY[is.na(dcY$Votes), "Votes"] <- 0
dcx <- dcY %>%
  group_by(Ballot, Candidate) %>%
  summarise(v = sum(Votes))
dcx <- dcx %>%
  group_by(Candidate) %>%
  mutate(vm = max(v))
ord45 <- dcx %>%
  filter(Ballot == "45") %>%
  ungroup() %>%
  mutate(candidate = fct_reorder(factor(Candidate), v, max))
dcx <- dcx %>% mutate(candidate = factor(Candidate, levels = levels(ord45$candidate)))

gcv <- ggplot(dcx %>% filter(vm > 35), aes(Ballot, v, group = candidate)) +
  geom_point(aes(col = candidate), size = 0.5) +
  geom_step(aes(col = candidate)) +
  geom_hline(yintercept = 1088 * 2 / 3, linetype = "dashed", colour = "grey70") +
  geom_hline(yintercept = 1088 / 2, linetype = "dotted", colour = "grey50") +
  theme(plot.title = element_text(vjust = 2),
        legend.title = element_blank(),
        legend.text = element_text(size = 10)) +
  ylab(NULL) +
  guides(color = guide_legend(reverse = TRUE)) +
  scale_colour_manual(values = colorblind_pal()(8)[c(2, 3, 4, 7, 1)])
gcv + ggtitle("Wilson won the 1912 Democratic nomination on the 46th ballot")


## Fig 4.6a
gcv + annotate("rect", xmin = 6, xmax = 14, ymin = 400, ymax = 600,
               colour = "yellow", alpha = 0, linewidth = 2)


## Fig 4.6b
gcv + annotate("rect", xmin = 18, xmax = 22, ymin = 325, ymax = 425,
               colour = "yellow", alpha = 0, linewidth = 2) +
               annotate("rect", xmin = 26, xmax = 29.5, ymin = 360, ymax = 460,
               colour = "yellow", alpha = 0, , linewidth = 2)


##  Fig 4.6c
gcv + annotate("rect", xmin = 20, xmax = 27, ymin = -15, ymax = 70,
               colour = "yellow", alpha = 0, linewidth = 2)


##  Fig 4.6d
gcv + annotate("rect", xmin = 41.5, xmax = 44.5, ymin = 450, ymax = 625,
               colour = "yellow", alpha = 0, linewidth = 2)


##  Fig 4.6e
gcv + annotate("rect", xmin = 44.5, xmax = 47.5, ymin = 600, ymax = 1010,
               colour = "yellow", alpha = 0, linewidth = 2)


## Fig 4.7 'Convention voting using estimated ballot times
## with the date labels at midday and adjournment periods shaded grey'

data(DC1912ballots, package="GmooG")
dcxt <- left_join(dcx, DC1912ballots)

data(DC1912adjourns, package="GmooG")

cand2 <- ggplot(dcxt %>% filter(vm > 35), aes(DateT, v, group = candidate)) +
  geom_rect(data = DC1912adjourns,
            mapping = aes(xmin = StartT, xmax = EndT, ymin = -Inf, ymax = Inf),
            fill = "grey", alpha = 0.3, inherit.aes = FALSE) +
  geom_step(aes(col = candidate)) +
  geom_hline(yintercept = 1088 * 2 / 3, linetype = "dashed", colour = "grey70") +
  geom_hline(yintercept = 1088 / 2, linetype = "dotted", colour = "grey50") +
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        plot.title = element_text(vjust = 2),
        legend.title = element_blank()) +
  ylab(NULL) +
  geom_point(aes(col = candidate), size = 0.75) +
  xlab(NULL) +
  scale_colour_manual(values = colorblind_pal()(8)[c(2, 3, 4, 7, 1)]) +
  guides(color = guide_legend(reverse = TRUE)) +
  scale_x_datetime(date_labels = "%e %B",
                   breaks = c(as.POSIXct("1912-06-27 12:00:00 UTC"),
                   as.POSIXct("1912-06-28 12:00:00 UTC"),
                   as.POSIXct("1912-06-29 12:00:00 UTC"),
                   as.POSIXct("1912-06-30 12:00:00 UTC"),
                   as.POSIXct("1912-07-01 12:00:00 UTC"),
                   as.POSIXct("1912-07-02 12:00:00 UTC")))
cand2a <- cand2 +
       ggtitle("Candidates' votes at the 1912 Democratic Convention plotted against time")
cand2a

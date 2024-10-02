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
library(viridis)
})

theme_set(theme(plot.title = element_text(hjust = 0.5)))


## Fig 12.1 'The spineplot top left shows the treatment group was much bigger
## than the placebo group and the dropout rate was small for both groups.
## The scatterplots of PASI and DLQI scores at the start of the study top right
## show the two groups had similar initial distributions.
## The bottom plot shows that average initial scores on the 10 questions were similar
## for the groups (solid lines) and mostly slightly lower for the placebo group
## after 16 weeks (blue dashed line).  For the treatment group average scores were
## substantially less on all questions after 16 weeks (dark orange dashed line).'

data(DLQI, package="GmooG")

dq <- DLQI %>% mutate(TRTx = ifelse(TRT == "A", "Placebo", "Treatment"))
dqx <- dq %>% pivot_wider(id_cols = c(USUBJID, TRTx, PASI_BASELINE),
                          names_from = VISIT, values_from = c(DLQI101:DLQI_SCORE))
dqx <- dqx %>% mutate(
                   Dropout = ifelse(is.na(`DLQI_SCORE_Week 16`), "dropout", "completed"))
                   
p1 <- ggplot(dqx) +
  geom_mosaic(aes(x = product(TRTx), fill = Dropout)) +
  theme(legend.position = "none") +
  xlab("Group") +
  ylab("") +
  scale_fill_manual(values = c("grey70", "red"))
p2 <- ggplot(dqx, aes(PASI_BASELINE, DLQI_SCORE_Baseline)) +
  geom_point(aes(colour = TRTx)) +
  facet_wrap(vars(TRTx), strip.position = "bottom") +
  theme(legend.position = "none") +
  xlab("PASI at baseline") +
  ylab("DLQI score at baseline") +
  xlim(0, 72) +
  scale_colour_manual(values = c("steelblue", "darkorange3"))
  
dqwS <- dqx %>%
  filter(Dropout == "completed") %>%
  group_by(TRTx) %>%
  summarise(across(PASI_BASELINE:`DLQI110_Week 16`, \(x) mean(x, na.rm = TRUE)))
dqws <- dqwS[, 3:22]
tdq <- data.frame(t(dqws))
colnames(tdq) <- c("A", "B")
tdq$g <- rep(c("Base", "w16"), 10)
tdq <- tdq %>% mutate(ID = rep(1:10, each = 2))

p3 <- ggplot(tdq, aes(ID, A)) +
  geom_line(aes(linetype = g), colour = "steelblue", linewidth = 2) +
  geom_line(aes(ID, B, linetype = g), colour = "darkorange3", linewidth = 2) +
  ylim(0, 3) +
  scale_x_continuous(breaks = seq(1, 10)) +
  xlab("DLQI question") +
  ylab(NULL) +
  theme(plot.title = element_text(vjust = 3),
        legend.position = "none") +
  ggtitle("Average question scores by group initially (solid lines)
           and finally (dashed lines)")
           
qlEnsY <- ((p1 + p2 + plot_layout(ncol = 2, widths = c(1, 2))) / p3 +
                      plot_layout(nrow = 2, heights = c(1, 2)))
qlEnsY


## Fig 12.2 'Distributions of answers to question 1 at the start of the trial (top)
## and at the end (bottom): quality of life declines to the right'

ggplot(dq, aes(DLQI101)) +
  geom_bar(aes(fill = TRTx)) +
  facet_grid(rows = vars(VISIT), cols = vars(TRTx)) +
  ylab(NULL) +
  scale_fill_manual(values = c("steelblue", "darkorange3")) +
  theme(legend.position = "none") +
  xlab("DLQI Question 1 responses")


## Fig 12.3 'Answers to question 1 at the start of the trial (rows)
## and at the end (columns within the groups).'

ggplot(dqx %>% filter(Dropout == "completed"), aes(`DLQI101_Week 16`)) +
  geom_bar(aes(fill = TRTx)) +
  facet_grid(cols = vars(fct_rev(TRTx)), rows = vars(DLQI101_Baseline), switch = "y") +
  ylab("DLQI101 response initially") +
  xlab("DLQI response at Week 16") +
  scale_fill_manual(values = c("steelblue", "darkorange3")) +
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        axis.text.y = element_blank())

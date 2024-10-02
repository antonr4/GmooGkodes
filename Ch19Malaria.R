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
library(extracat)
})

theme_set(theme(plot.title = element_text(hjust = 0.5)))


## Creating new variables to give the results of the four tests

data(malaria, package="GmooG")
cuto <- 4.19

malaria <- malaria %>%
  mutate(RDT = RDT1 + RDT2 + RDTb,
         PCR = ifelse(Pf > 0 | Pv > 0, 1, 0),
         LM = ifelse(((LM_Pf > 0) + (LM_Pv > 0) +
                      (LM_Pfg > 0) + (LM_Pvg > 0) + (LM_Pm > 0)) > 0, 1, 0),
         ROMD = ifelse((AveMO > cuto), 1, 0))
         
malp <- malaria %>% filter(!is.na(PCR))
malpT <- malp %>% summarise(RDTtest = sum(RDT),
                            PCRtest = sum(PCR),
                            LMtest = sum(LM),
                            ROMDtest = sum(ROMD))
malpTw <- malpT %>%
  pivot_longer(everything(), names_to = "Test",
                             values_to = "Positives") %>%
  mutate(p = 100 * Positives / dim(malp)[1])


## Fig 19.1 'Percentages of positives for the four tests'

ggplot(malpTw, aes(Test, weight = Positives / dim(malp)[1])) +
  geom_bar(aes(fill = Test)) +
  xlab(NULL) +
  ylab(NULL) +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.5)) +
  theme(legend.position = "none")


## Fig 19.2 'Combinations of individual malaria test results
## (red rectangles are positives)'

malB <- malp %>% count(RDT, PCR, LM, ROMD)
umalB <- untableSet(malB, freqvar = "n")
umalB <- umalB %>% mutate(across(everything(), as.numeric))

visid(umalB, sort = "b", col = c("grey80", "red"), pmax = 0,
      tp = TRUE, opts = list(mar = c(0.025, 0.4)))


## Fig 19.3 'Paired comparisons of the results of the four tests'

malT <- malp %>% select(RDT:ROMD)
malT <- malT %>% mutate(across(RDT:ROMD, function(x) factor(x, labels = c("no", "yes"))))

GGally::ggpairs(malT[, 1:4], mapping = ggplot2::aes(fill = "grey70"),
                diag = list(discrete = "blankDiag"), lower = "blank") +
                scale_fill_manual(values = "grey70")


## Fig 19.4 'Magneto-optical measurements on a log10 scale
## grouped by the corresponding LM test result'

ggplot(malaria %>% filter(!is.na(LM)), aes(AveMO)) +
  geom_histogram(aes(fill = factor(LM))) +
  facet_wrap(vars(LM), ncol = 1,
             labeller = as_labeller(c(`0` = "LM negative", `1` = "LM positive"))) +
  scale_x_log10() +
  xlab("log(MO)") +
  ylab(NULL) +
  scale_fill_manual(values = c("grey80", "red")) +
  theme(legend.position = "none") +
  geom_vline(xintercept = cuto, linetype = "dotted")


## Fig 19.5 'Weight by age and sex of the people tested for malaria'

malaria <- malaria %>% mutate(sex = factor(Sex, levels = c("male", "female")))

ggplot(malaria, aes(Age, Weight, colour = sex)) +
  geom_point() +
  facet_wrap(vars(sex)) +
  ylim(0, 100) +
  theme(legend.position = "none") +
  scale_colour_manual(values = c("springgreen3", "purple2"))

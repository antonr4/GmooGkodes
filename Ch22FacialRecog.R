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


## Fig 22.1 'Numbers of different skin tones in the dataset for both sexes'

data(aFacial)

aFacial <- aFacial %>% mutate(pred = fct_rev(Prediction),
                              sex1 = ifelse(Sex == "Female", "F", "M"),
                              sex = fct_rev(sex1))
                              
ggplot(aFacial, aes(Skin, weight = Freq, fill = sex)) +
  geom_bar() +
  facet_wrap(vars(sex)) +
  ylab(NULL) +
  xlab(NULL) +
  scale_fill_manual(values = c("springgreen3", "purple2")) +
  theme(legend.position = "none")


## Fig 22.2 'Error rates by sex and skin colour for three software systems
## drawn side by side.  In each plot males are to the left, females to the right,
## and skin colour gets darker from left to right within sex.
## The width of each bar is proportional to the size of the group it represents.'

hm <- ggplot(aFacial %>% filter(Software == "Microsoft")) +
  geom_mosaic(aes(weight = Freq, x = product(pred, Skin, sex), fill = pred),
              divider = ddecker(), offset = 0.0025) +
  scale_fill_manual(values = c("darkorange", "grey70")) +
  xlab("") +
  ylab("") +
  scale_x_productlist(labels = paste0(rep(c("I", "II", "III", "IV", "V", "VI"), 2), "\n",
                c(" ", " ", "Male", " ", " ", " ", " ", " ", "Female", " ", " ", " "))) +
  theme(panel.background = element_blank(),
        legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  ggtitle("Microsoft error rates")
  
hi <- ggplot(aFacial %>% filter(Software == "IBM")) +
  geom_mosaic(aes(weight = Freq, x = product(pred, Skin, sex), fill = pred),
              divider = ddecker(), offset = 0.0025) +
  scale_fill_manual(values = c("blue", "grey70")) +
  xlab("") +
  ylab("") +
  scale_x_productlist(labels = paste0(rep(c("I", "II", "III", "IV", "V", "VI"), 2), "\n",
                c(" ", " ", "Male", " ", " ", " ", " ", " ", "Female", " ", " ", " "))) +
  theme(panel.background = element_blank(),
        legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  ggtitle("IBM error rates")
  
hf <- ggplot(aFacial %>% filter(Software == "Face++")) +
  geom_mosaic(aes(weight = Freq, x = product(pred, Skin, sex), fill = pred),
              divider = ddecker(), offset = 0.0025) +
  scale_fill_manual(values = c("lightblue", "grey70")) +
  xlab("") +
  ylab("") +
  scale_x_productlist(labels = paste0(rep(c("I", "II", "III", "IV", "V", "VI"), 2), "\n",
                c(" ", " ", "Male", " ", " ", " ", " ", " ", "Female", " ", " ", " "))) +
  theme(panel.background = element_blank(),
        legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  ggtitle("Face++ error rates")
  
hm + hf + hi


## Fig 22.3 'Error rates by sex and skin colour for three separate software systems.
## Males are on the left, females on the right,
## and skin colour gets darker from left to right within sex.
## The width of each bar is proportional to the size of the group it represents.'

gm <- ggplot(aFacial %>% filter(Software == "Microsoft")) +
  geom_mosaic(aes(weight = Freq, x = product(pred, Skin, sex), fill = pred),
              divider = ddecker(), offset = 0.0025) +
  scale_fill_manual(values = c("darkorange", "grey70")) +
  xlab("") +
  ylab("") +
  xlab("") +
  ylab("") +
  scale_x_productlist(labels = paste0(rep(c("I", "II", "III", "IV", "V", "VI"), 2), "\n",
                c(" ", " ", "Male", " ", " ", " ", " ", " ", "Female", " ", " ", " "))) +
  theme(panel.background = element_blank(),
        legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  ggtitle("Microsoft error rates")
  
gi <- ggplot(aFacial %>% filter(Software == "IBM")) +
  geom_mosaic(aes(weight = Freq, x = product(pred, Skin, sex), fill = pred),
              divider = ddecker(), offset = 0.0025) +
  scale_fill_manual(values = c("blue", "grey70")) +
  xlab("") +
  ylab("") +
  xlab("") +
  ylab("") +
  scale_x_productlist(labels = paste0(rep(c("I", "II", "III", "IV", "V", "VI"), 2), "\n",
                c(" ", " ", "Male", " ", " ", " ", " ", " ", "Female", " ", " ", " "))) +
  theme(panel.background = element_blank(),
        legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  ggtitle("IBM error rates")
  
gf <- ggplot(aFacial %>% filter(Software == "Face++")) +
  geom_mosaic(aes(weight = Freq, x = product(pred, Skin, sex), fill = pred),
              divider = ddecker(), offset = 0.0025) +
  scale_fill_manual(values = c("lightblue", "grey70")) +
  xlab("") +
  ylab("") +
  xlab("") +
  ylab("") +
  scale_x_productlist(labels = paste0(rep(c("I", "II", "III", "IV", "V", "VI"), 2), "\n",
                c(" ", " ", "Male", " ", " ", " ", " ", " ", "Female", " ", " ", " "))) +
  theme(panel.background = element_blank(),
        legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  ggtitle("Face++ error rates")
  
gm / gf / gi


## Fig 22.4 'FDRs by sex and skin colour for three separate software systems.
## Males are on the left, females on the right, and
## skin colour gets darker from left to right within sex.'

aFacial <- aFacial %>% select(-Sex, -sex1, -Prediction)
aF1 <- aFacial %>% pivot_wider(names_from = pred,
                               values_from = Freq)
aF2 <- aF1 %>% pivot_wider(names_from = sex,
                           values_from = c(Correct, Wrong))
aF2 <- aF2 %>% mutate(freqP_M = Correct_M + Wrong_F,
                      freqP_F = Correct_F + Wrong_M)
aF4 <- aF2 %>% pivot_longer(cols = Correct_F:freqP_F,
                            names_to = "Group",
                            values_to = "freq")
aF4 <- aF4 %>% separate(Group, into = c("Type", "Sex"))
aF4x <- aF4 %>% pivot_wider(names_from = Type,
                            values_from = freq)
aF4y <- aF4x %>%
  mutate(Wrong = freqP - Correct) %>%
  select(-freqP)
aF4z <- aF4y %>% pivot_longer(cols = Correct:Wrong,
                              names_to = "Prediction",
                              values_to = "Freq")
aF4z <- aF4z %>% mutate(pred = fct_rev(Prediction),
                        sex = fct_rev(Sex))
                        
km <- ggplot(aF4z %>% filter(Software == "Microsoft")) +
  geom_mosaic(aes(weight = Freq, x = product(pred, Skin, sex), fill = pred),
              divider = ddecker(), offset = 0.0025) +
  scale_fill_manual(values = c("darkorange", "grey70")) +
  xlab("") +
  ylab("") +
  xlab("") +
  ylab("") +
  scale_x_productlist(labels = paste0(rep(c("I", "II", "III", "IV", "V", "VI"), 2), "\n",
                c(" ", " ", "Male", " ", " ", " ", " ", " ", "Female", " ", " ", " "))) +
  theme(panel.background = element_blank(),
        legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  ggtitle("Microsoft FDR")
  
ki <- ggplot(aF4z %>% filter(Software == "IBM")) +
  geom_mosaic(aes(weight = Freq, x = product(pred, Skin, sex), fill = pred),
              divider = ddecker(), offset = 0.0025) +
  scale_fill_manual(values = c("blue", "grey70")) +
  xlab("") +
  ylab("") +
  xlab("") +
  ylab("") +
  scale_x_productlist(labels = paste0(rep(c("I", "II", "III", "IV", "V", "VI"), 2), "\n",
                c(" ", " ", "Male", " ", " ", " ", " ", " ", "Female", " ", " ", " "))) +
  theme(panel.background = element_blank(),
        legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  ggtitle("IBM FDR")
  
kf <- ggplot(aF4z %>% filter(Software == "Face++")) +
  geom_mosaic(aes(weight = Freq, x = product(pred, Skin, sex), fill = pred),
              divider = ddecker(), offset = 0.0025) +
  scale_fill_manual(values = c("lightblue", "grey70")) +
  xlab("") +
  ylab("") +
  xlab("") +
  ylab("") +
  scale_x_productlist(labels = paste0(rep(c("I", "II", "III", "IV", "V", "VI"), 2), "\n",
                c(" ", " ", "Male", " ", " ", " ", " ", " ", "Female", " ", " ", " "))) +
  theme(panel.background = element_blank(),
        legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  ggtitle("Face++ FDR")
    
km / kf / ki

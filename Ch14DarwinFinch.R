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
})

theme_set(theme(plot.title = element_text(hjust = 0.5)))


## Fig 14.1 'Numbers of finch species from Isabela Island
## with at least ten birds and complete data'

data(morphSnodgrassHeller, package = "hypervolume")

mSH <- morphSnodgrassHeller %>% mutate(N.UBkL = as.numeric(as.character(N.UBkL)))
mSH1 <- mSH %>% filter(IslandID == "Isa_Alb")
mSH1 <- mSH1 %>% filter(!TaxonOrig == "Certhidea olivacea olivacea")
mSH1 <- mSH1 %>%
  group_by(TaxonOrig) %>%
  mutate(tot = n()) %>%
  filter(tot > 9)
mSH1 <- mSH1 %>% select(-Plumage, -MToeL)
mSH1 <- na.omit(mSH1)
mSH1 <- mSH1 %>% mutate(species = str_replace(TaxonOrig, "Geospiza", ""))
colblind4 <- c(colorblind_pal()(8), "grey60")
colblind6 <- c(colblind4[2:4], "sienna4", "lawngreen", colblind4[6:8], colblind4[5])
colblind5 <- colblind6[4:8]

finch1X <- ggplot(mSH1, aes(species)) +
  geom_bar(aes(fill = species), alpha = 0.5) +
  xlab(NULL) +
  ylab(NULL) +
  theme(legend.position = "none",
        panel.background = element_blank()) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(as.factor(mSH1$species)))) +
  scale_fill_manual(values = colblind5)
finch1X


## Fig 14.2 'Body and wing lengths for the five species from Isabela Island'

a0 <- ggplot(mSH1) +
  xlab(NULL) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_fill_manual(values = colblind5) +
  coord_flip()
  
a1 <- a0 + geom_boxplot(aes(species, BodyL, fill = species), alpha = 0.5) +
      ylab("Body length (mm)")
a2 <- a0 + geom_boxplot(aes(species, WingL, fill = species), alpha = 0.5) +
      ylab("Wing length (mm)") +
      theme(axis.ticks.y = element_blank(),
            axis.text.y = element_blank())
finch2X <- a1 + a2
finch2X


## Fig 14.3 'Scatterplot of wing length and body length
## for the five species from Isabela Island'

finch3X <- ggplot(mSH1, aes(BodyL, WingL)) +
  geom_point(aes(colour = species), size = 2, alpha = 0.5) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        panel.background = element_blank()) +
  xlab("Body length") +
  ylab("Wing length") +
  scale_colour_manual(values = colblind5) +
  guides(color = guide_legend(nrow = 2))
finch3X


## Fig 14.4 'Parallel coordinate plot of nine measurements of five
## Galápagos finch species from Isabela Island'

finchpcpX <- mSH1 %>%
  pcp_select(BodyL:TarsusL) %>%
  pcp_scale(method = "std") %>%
  ggplot(aes_pcp()) +
  geom_pcp_axes(colour = "white") +
  geom_pcp(aes(colour = species), alpha = 0.5) +
  ylab(NULL) +
  xlab(NULL) +
  theme_tufte() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  scale_colour_manual(values = colblind5)
finchpcpX


## Fig 14.5 'Parallel coordinate plot of nine measurements of nine
## Galápagos finch species from Isabela Island'

snodH <- mSH %>% mutate(Species = str_replace_all(TaxonOrig, " ", "_"),
                        Species = gsub("_$", "", Species))
names(snodH)[18] <- "N.UBkL"
snodH <- snodH %>% mutate(Island = case_when(
  IslandID == "Balt_SS" ~ "Baltra",
  IslandID == "Drwn_Clp" ~ "Darwin",
  IslandID == "Esp_Hd" ~ "Espanola",
  IslandID == "Flor_Chrl" ~ "Floreana",
  IslandID == "Frn_Nrb" ~ "Fernandina",
  IslandID == "Gnov_Twr" ~ "Genovesa",
  IslandID == "Isa_Alb" ~ "Isabela",
  IslandID == "Mrch_Bndl" ~ "Marchena",
  IslandID == "Pnt_Abng" ~ "Pinta",
  IslandID == "Pnz_Dnc" ~ "Pinzon",
  IslandID == "SCris_Chat" ~ "San_Cristobal",
  IslandID == "SCru_Inde" ~ "Santa_Cruz",
  IslandID == "SFe_Brngt" ~ "Santa_Fe",
  IslandID == "Snti_Jams" ~ "Santiago",
  IslandID == "Wlf_Wnm" ~ "Wolf"
))
snod <- snodH %>% mutate(N.UBkL = str_replace(N.UBkL, "8..3", "8.3"),
                         N.UBkL = as.numeric(N.UBkL))
snod <- snod %>% mutate(species = str_replace(Species, "Geospiza_", ""),
                        species = str_replace(species, "Certhidia_", ""))
                        
sxpcp <- snod %>%
  filter(Island == "Isabela") %>%
  pcp_select(BodyL:TarsusL) %>%
  pcp_scale(method = "std") %>%
  ggplot(aes_pcp()) +
  geom_pcp(alpha = 0.05, colour = "black", data = . %>% select(-species)) +
  geom_pcp_axes(colour = "white") +
  geom_pcp(aes(colour = species), linewidth = 0.5, alpha = 0.5) +
  ylab(NULL) +
  xlab(NULL) +
  facet_wrap(vars(species)) +
  theme_tufte() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = -45),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  scale_colour_manual(values = colblind6)
sxpcp


## Fig 14.6 'Map of the Galápagos Islands'
## The map can be found here "pngImages/galapMap.png"


## Fig 14.7 '\\textit{fuliginosa parvula} finches on four islands'

snodFP <- snod %>%
  filter(species %in% c("fuliginosa_parvula")) %>%
  group_by(Island) %>%
  mutate(nn = n()) %>%
  filter(nn > 8)
  
snodFP %>%
  pcp_select(BodyL:TarsusL) %>%
  pcp_scale(method = "std") %>%
  ggplot(aes_pcp()) +
  geom_pcp(alpha = 0.05, colour = "black", data = . %>% select(-Island)) +
  geom_pcp_axes(colour = "white") +
  geom_pcp(colour = colblind6[6]) +
  ylab(NULL) +
  xlab(NULL) +
  facet_wrap(vars(Island)) +
  theme_tufte() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = -45),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  scale_x_discrete(expand = expansion(add = 0.75))


## Fig 14.8 'Boxplots of wing lengths by sex for 4 different species with jittered
## dotplots drawn on top.  Species colours are the same as used earlier.  There are not
## enough specimens of \\textit{fortis-platyrhyncha} for the species to be included.'

SpecSex <- snod %>%
  group_by(species, Sex) %>%
  summarise(nn = n())
SpecSexW <- SpecSex %>% pivot_wider(values_from = "nn",
                                    names_from = "Sex", values_fill = 0)
SpecSexW4 <- SpecSexW %>% filter(F > 9)
snodS <- snod %>%
  filter(species %in% SpecSexW4$species) %>%
  select(Sex, species, BodyL:TarsusL)
snodSlong <- snodS %>% pivot_longer(BodyL:TarsusL,
                                    names_to = "Vars", values_to = "Lengths")
                                    
ggplot(snodSlong %>% filter(Vars == "WingL"), aes(Sex, Lengths)) +
  geom_boxplot(aes(fill = species), colour = "grey50", alpha = 0.5, outlier.size = 0) +
  geom_jitter(colour = "grey60", width = 0.05) +
  ylab(NULL) +
  facet_wrap(vars(species), nrow = 1) +
  scale_fill_manual(values = colblind5[c(1, 3, 4, 5)]) +
  theme(legend.position = "none",
        panel.background = element_blank())

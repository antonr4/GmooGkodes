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
library(cowplot)
library(GGally)
})

theme_set(theme(plot.title = element_text(hjust = 0.5)))


## Fig 11.1 'Histogram of diamond lengths (the bar at zero has been circled in green)'
## the diamonds dataset is in the ggplot2 package

# Rename some of the variables
names(diamonds)[c(5, 8:10)] <- c("depthp", "length", "width", "depth")

# use the cowplot package to ensure the two separate figures are aligned
p1 <- ggplot(diamonds, aes(length)) +
  geom_histogram(fill = "azure3", binwidth = 0.25, boundary = 0, closed = "left") +
  annotate("point", x = 0.125, y = 0, colour = "green", size = 8, shape = 1, stroke = 3) +
  xlab(NULL) +
  ylab(NULL)
p2 <- ggplot(diamonds, aes("", length)) +
  geom_boxplot(outlier.colour = "red") +
  xlab(" ") +
  ylab(NULL) +
  theme(axis.ticks.y = element_blank()) +
  coord_flip()
p12 <- align_plots(p1, p2, align = "v")
ggdraw(p12[[1]])


## Fig 11.2 'Boxplot of diamond lengths (outliers coloured red)'
ggdraw(p12[[2]])


## Fig 11.3 'Histogram of diamond lengths (without the zero-length diamonds)'

ggplot(diamonds %>% filter(!length == 0), aes(length)) +
  geom_histogram(fill = "azure3", binwidth = 0.05, boundary = 0, closed = "left") +
  xlab(NULL) +
  ylab(NULL)


## Fig 11.4 'Histogram of diamond carats'

dcx <- ggplot(diamonds, aes(carat)) +
  geom_histogram(fill = "lightcyan3", binwidth = 0.01, boundary = 0, closed = "left") +
  xlab(NULL) +
  ylab(NULL)
dcx


## Fig 11.5 'Histogram of diamond prices'

ggplot(diamonds, aes(price)) +
  geom_histogram(fill = "blue", binwidth = 1, boundary = 0, closed = "left") +
  xlab(NULL) +
  ylab(NULL)


## Fig 11.6 'A scatterplot matrix of length, width, depth, and carats to the power one
## third for the dataset after removing the zero cases and three extreme high outliers'

d1a <- diamonds %>%
  filter(length > 0,
         width > 0,
         depth > 0,
         width < 20,
         depth < 10) %>%
  mutate(carat3 = carat**(1 / 3))
  
ggpairs(d1a, columns = c(8:11),
             lower = list(continuous = wrap("points", alpha = 0.3, size = 0.5)))


## Fig 11.7 'A scatterplot matrix of carat, percentage depth, table, and price
## for the dataset after removing the zero cases and four extreme outliers'

d1b <- d1a %>% filter(table < 90)

gx <- ggpairs(d1b, columns = c(1, 5:7),
                   lower = list(continuous = wrap("points", alpha = 0.3, size = 0.5)))
gx
gy <- ggplot(d1b, aes(carat, table)) + geom_point()


## Fig 11.8 'A vertically jittered dotplot of diamond table, excluding 12 outlying values,
## showing heaping at integer values'

d1b <- d1b %>% mutate(yrn = runif(dim(d1b)[1]))

ggplot(d1b, aes(table, yrn)) +
  geom_point(size = 0.5) +
  xlab(NULL) +
  ylab(NULL) +
  xlim(50, 70)


## Fig 11.9 'A scatterplot of reported percentage depth (`depthp`) and percentage depth
## calculated from the data (after removing the zero cases and three extreme outliers).
## Cases where the difference is bigger than 1 have been coloured red and drawn last.'

d1a <- d1a %>% mutate(Depthp = round(200 * depth / (length + width), 1),
                      DifDep = depthp - Depthp,
                      difdep = (abs(DifDep) > 1))
                      
ggplot(d1a %>% arrange(difdep), aes(Depthp, depthp)) +
  geom_point(aes(colour = difdep), size = 2) +
  scale_colour_manual(values = c("grey70", "red")) +
  theme(legend.position = "none",
        axis.title.y = element_text(angle = 0, vjust = 0.5, hjust = 0)) +
  coord_fixed(xlim = c(10, 110), ylim = c(10, 110)) +
  ylab("depthp   ") +
  geom_abline(intercept = 0, slope = 1, lty = "dashed", colour = "grey70")


## Fig 11.10 'A scatterplot of price against carat with a red line at \\$19000,
## just above the maximum reported price.'

mp <- round(max(diamonds$price), -3)

ggplot(d1a, aes(carat, price)) +
  geom_point(size = 1, colour = "grey60") +
  geom_hline(yintercept = mp, linewidth = 1, colour = "red") +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5, hjust = 0))


## Fig 11.11 'Boxplots of carat by clarity and by colour'

dcbox <- ggplot(diamonds, aes(clarity, carat)) +
  geom_boxplot() +
  ylab(NULL)
dccbox <- ggplot(diamonds, aes(color, carat)) +
  geom_boxplot() +
  ylab(NULL) +
  xlab("colour")
dcbox + dccbox


## Fig 11.12 'Boxplots of carat by cut'

dcutbox <- ggplot(diamonds, aes(cut, carat)) +
  geom_boxplot() +
  ylab(NULL)
dcutbox

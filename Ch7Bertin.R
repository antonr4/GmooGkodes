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
library(tidygeocoder)
library(cartogram)
})

theme_set(theme(plot.title = element_text(hjust = 0.5)))


## Fig 7.1 'Departments in France in 1954 where agricultural workers
## made up at least 40\\% of the workforce.'

data(F1954, package="GmooG")
data(France54Map, package="GmooG")

# Calculate percentages and densities
F1954 <- F1954 %>% mutate(Total = I.Agriculture + II.Industry + III.Commerce,
                         AgPerc = 100 * I.Agriculture / Total,
                         IndPerc = 100 * II.Industry / Total,
                         CommPerc = 100 * III.Commerce / Total)
F1954 <- F1954 %>% mutate(TotDens = 1000 * Total / Area)
F1954 <- F1954 %>% mutate(AgDens = 1000 * I.Agriculture / Area,
                          IndDens = 1000 * II.Industry / Area,
                          CommDens = 1000 * III.Commerce / Area)
zx <- right_join(F1954, France54Map)
zx <- st_sf(data.frame(zx))
zx <- zx %>% mutate(Ag40 = (AgPerc >= 40))

# Pick out three cities
cities <- data.frame(name = c("Marseille", "Le Havre", "Paris"))
tt1 <- cities %>%
  geocode(city = name, method = "osm") %>%
  st_as_sf(coords = c("long", "lat"))
tt1_geo <- st_set_crs(tt1, 4326)

bertX <- tm_shape(zx) +
         tm_polygons(col = "Ag40", palette = c("white", "orange3"), legend.show = FALSE) +
         tm_shape(tt1_geo) +
         tm_dots(size = 0.4) +
         tm_text("name", just = "left", col = "blue", xmod = c(0, -2), ymod = c(-1, 1))
bertX


## Fig 7.2 'Histograms of the numbers working in the three sectors in 1954'

a0 <- ggplot() +
  xlim(0, 1000) +
  ylim(0, 40) +
  ylab("Departments")
a1 <- a0 + geom_histogram(data = F1954, aes(I.Agriculture), breaks = seq(0, 1000, 25),
                          fill = "orange3") + xlab("Workers in Agriculture, (in '000s)")
b1 <- a0 + geom_histogram(data = F1954, aes(II.Industry), breaks = seq(0, 1000, 25),
                          fill = "orangered") + xlab("Workers in Industry, (in '000s)")
c1 <- a0 + geom_histogram(data = F1954, aes(III.Commerce), breaks = seq(0, 1000, 25),
                          fill = "cyan") + xlab("Workers in Commerce, (in '000s)")
a1 / b1 / c1


## Fig 7.3 'Pairwise scatterplots of the numbers in thousands
## working in the three sectors in France in 1954'

a4 <- ggplot(F1954, aes(I.Agriculture, II.Industry)) +
  geom_point(colour = "blue3", size = 0.8) +
  xlab("Agriculture") +
  ylab("Industry")
b4 <- ggplot(F1954, aes(I.Agriculture, III.Commerce)) +
  geom_point(colour = "blue3", size = 0.8) +
  xlab("Agriculture") +
  ylab("Commerce")
c4 <- ggplot(F1954, aes(II.Industry, III.Commerce)) +
  geom_point(colour = "blue3", size = 0.8) +
  xlab("Industry") +
  ylab("Commerce")
a4 + b4 + c4


## Fig 7.4 'Choropleth map of percentage of agricultural workers in France in 1954'

zx <- zx %>% mutate(Agriculture = AgPerc)

# Pick out two cities
cities2 <- data.frame(name = c("Saint-Malo", "Geneva"))
tt2 <- cities2 %>%
  geocode(city = name, method = "osm") %>%
  st_as_sf(coords = c("long", "lat"))
tt2_geo <- st_set_crs(tt2, 4326)

tm_shape(zx) +
  tm_polygons(col = "Agriculture") +
  tm_shape(tt2_geo) +
  tm_dots(size = 0.4) +
  tm_text("name", just = "left", col = "blue", xmod = c(-4.5, 0.5), ymod = c(1.7, 0))


## Fig 7.5 'Cartogram of working population size shaded by
## the percentage of agricultural workers in France in 1954'

zxC <- cartogram_cont(zx, "Total")
tm_shape(zxC) +
  tm_polygons(col = "Agriculture")


## Fig 7.6 'Maps of the density of the total working population in thousands per square
## kilometre in France in 1954, all departments (left), excluding Seine and Paris (right)'

tmT <- tm_shape(zx) +
       tm_polygons(col = "TotDens", breaks = seq(0, 15000, 2500), palette = "Reds") +
       tm_legend(legend.title.size = 0.01)
zxPS <- zx %>% filter(!Dept %in% c("Seine", "Paris"))
tmPS <- tm_shape(zxPS) +
        tm_polygons(col = "TotDens", breaks = seq(0, 150, 25), palette = "Reds") +
        tm_legend(legend.title.size = 0.01)
tmap_arrange(tmT, tmPS)


## Fig 7.7 'Maps of the log of population density of the total working population
## in thousands per square kilometre in France in 1954,
## default scale (left), more detailed scale (right)'

zx <- zx %>% mutate(lTotDens = log(TotDens))
tmTl1 <- tm_shape(zx) +
         tm_polygons(col = "lTotDens", palette = "Reds") +
         tm_legend(legend.title.size = 0.01)
tmTl2 <- tm_shape(zx) +
         tm_polygons(col = "lTotDens", breaks = seq(0, 10, 1), palette = "Reds") +
         tm_legend(legend.title.size = 0.01)
tmap_arrange(tmTl1, tmTl2)

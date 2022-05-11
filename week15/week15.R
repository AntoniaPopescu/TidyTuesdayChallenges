library(tidyverse)
library(waffle)
library(dplyr)
library(MetBrewer)
library(showtext)
library(tidyverse)
library(showtext)
library(ggplot2)
library(dplyr)

library(geofacet)
#setting up text font
showtext_auto()
sysfonts::font_families_google()
sysfonts::font_add_google("Merriweather", "Merriweather")
sysfonts::font_add_google("Abril Fatface", "Abril Fatface")


#loading week14 data
tuesdata <- tidytuesdayR::tt_load(2022, week = 15)

indoor_pollution <- tuesdata$indoor_pollution

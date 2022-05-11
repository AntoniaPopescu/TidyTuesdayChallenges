library(tidyverse)
library(dplyr)
library(MetBrewer)
library(showtext)
library(tidyverse)
library(showtext)
library(ggplot2)
library(dplyr)

#setting up text font
showtext_auto()
sysfonts::font_families_google()
sysfonts::font_add_google("Merriweather", "Merriweather")
sysfonts::font_add_google("Abril Fatface", "Abril Fatface")


#loading week17 data
tuesdata <- tidytuesdayR::tt_load(2022, week = 17)

hidden_gems <- tuesdata$hidden_gems

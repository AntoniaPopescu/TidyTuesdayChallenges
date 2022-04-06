library(tidyverse)
library(ggplot2)
library(waffle)
library(dplyr)
library(MetBrewer)
library(showtext)

#setting up text font
showtext_auto()
sysfonts::font_families_google()
sysfonts::font_add_google("Merriweather", "Merriweather")
sysfonts::font_add_google("Abril Fatface", "Abril Fatface")


#loading week14 data
tuesdata <- tidytuesdayR::tt_load('2022-04-05')

news_orgs <- tuesdata$news_orgs

#formatting data
news = news_orgs %>%
  select(underrepresented_communities) %>%
  drop_na()

news = sapply(strsplit(as.character(news), ","), c)
news = data.frame(gsub("[[:punct:]]", "", news))

names(news)[1] <- "communities"

news$communities[1] <-
  "Communities with English as a second language"

news = news %>%
  count(communities, sort = T)

communities = c(
  "Low-income communities",
  "People of color",
  "Ethnic communities",
  "Immigrant communities",
  "LGBTQI communities",
  "Communities with English as a second language" ,
  "Indigenous communities"
)

count = c(87, 82, 55, 52, 49, 34, 5)

news = data.frame(communities, count)
news = news %>%
  mutate(tot = sum(count))


#plot data
news <- setNames(news$count, communities)

colors = met.brewer("Klimt", 7)

p = waffle(news, rows = 28)  + theme_void() +
  scale_fill_manual(values = colors) +
  labs(title = "Underrepresented communities \nin the Media",
       subtitle = "Digital Publications that serve underrepresnted communities",
       caption = "Data: Project Oasis | Plot: @AntoniaPopes | #TidyTuesday") +
  theme(
    text = element_text(family = "Merriweather"),
    plot.background = element_rect(fill = "#efedf5", color = NA),
    plot.title = element_text(
      margin = margin(15, 0, 0, 0),
      size = 30,
      family = "Abril Fatface"
    ),
    plot.subtitle = element_text(margin = margin(10, 0, 10, 0), size = 14),
    legend.text = element_text(size = 12, family = "Merriweather"),
    plot.caption = element_text(hjust = 0,
                                margin = margin(10, 0, 10, 0)),
    plot.margin = margin(1, 1, 1, 1, unit = "cm"),
    legend.key.size = unit(2, "cm"),
    legend.key.width = unit(0.5, "cm"),
  )
p


#save
ggsave(
  p,
  filename = "publications.png",
  height = 1350,
  width = 1080,
  units = "px"
)

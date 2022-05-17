#library load
library(tidyverse)
library(MetBrewer)
library(showtext)
library(ggplot2)
library(showtext)


#setting up text font
showtext_auto()
sysfonts::font_families_google()
sysfonts::font_add_google("Merriweather", "Merriweather")
sysfonts::font_add_google("Abril Fatface", "Abril Fatface")


#loading week18 data
wind <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/wind.csv')
solar <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/solar.csv')

WindAndSolar = solar %>%
  full_join(wind, by = "date") %>%
  pivot_longer(cols = c(solar_mwh:wind_capacity)) %>%
  mutate(energy_type = case_when(str_detect(name, "wind") ~ "wind",
                                 TRUE ~ "solar"),
         metric = case_when(str_detect(name, "mwh") ~ "mwh",
                            TRUE ~ "capacity")) %>%
  select(-name) %>%
  pivot_wider(names_from = "metric") %>%
  unnest(cols = c(mwh, capacity))

power_combined = solar %>%
  full_join(wind, by = "date")
WindAndSolar = merge(wind, solar, by = "date", all = T)
WindAndSolar <- WindAndSolar %>%
  pivot_longer(cols = c(solar_mwh:wind_capacity)) %>%
  mutate(energy_type = case_when(str_detect(name, "wind") ~ "wind",
                                 TRUE ~ "solar"),
         metric = case_when(str_detect(name, "mwh") ~ "mwh",
                            TRUE ~ "capacity")) %>%
  select(-name) %>%
  pivot_wider(names_from = "metric") %>%
  unnest(cols = c(mwh, capacity))

chart_colours <- c("solar" = "#ff6f61", "wind" = "#92a8d1")


p <- WindAndSolar %>%
  ggplot(aes(x = date, y = mwh, colour = energy_type, size = capacity,
             fill = energy_type)) +
  geom_point(colour =  "#3A3B3C", shape = 21, stroke = 1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(colour = "#EADDDD"),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 12, margin = margin(15, 0, 0, 0)),
        axis.title.y = element_text(size = 12, margin = margin(0, 15, 0, 0)),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.678, 1),
        legend.direction = "horizontal") +
  scale_fill_manual(values = chart_colours, guide = "none") +
  scale_size(name = "Capacity (gigawatts)", breaks = c(100, 500)) +
  labs(y = "$ per megawatt hour", x = "")

p

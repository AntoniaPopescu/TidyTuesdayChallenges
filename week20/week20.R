#library load
library(tidyverse)
library(MetBrewer)
library(showtext)
library(ggplot2)
library(showtext)
library(rnaturalearth)
library(pacman)
library(viridis)
devtools::install_github("ropenscilabs/rnaturalearthdata")
install.packages("rnaturalearthhires",
                 repos = "http://packages.ropensci.org",
                 type = "source")


#setting up text font
showtext_auto()
sysfonts::font_families_google()
sysfonts::font_add_google("Merriweather", "Merriweather")
sysfonts::font_add_google("Russo One", "Russo One")


#loading week20 data
eurovision <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision.csv')

pacman::p_load(BBmisc, tidyverse, hablar, ggbump, sf, rnaturalearth, feather, janitor, lubridate)


final_eurovision <- eurovision %>% 
  filter(rank == 1, section == "grand-final" | section == "final") %>% 
  group_by(artist_country) %>% 
  summarise(winnings = n())
  

eurovision_geo <- rnaturalearthdata::countries50 %>% 
  st_as_sf() %>% 
  st_crop(xmin = -24, xmax = 31, ymin = 33, ymax = 73)%>% 
  filter(NAME %in% final_eurovision$artist_country) %>% 
  left_join(final_eurovision, by = c("NAME" = "artist_country")) %>% 
  mutate(NAME = case_when(NAME == "United Kingdom" ~ "UK",
                           T ~ NAME))

ranking <- st_geometry(eurovision_geo) %>% 
  st_point_on_surface() %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  bind_cols(tibble(fine_rank = normalize(rank(eurovision_geo$winnings),range = c(40.12161, 66.12161), method = "range"),
                   country = eurovision_geo$NAME,
                   xend = 60,
                   x_axis_start = xend + 10,
                   fine_rank_x = normalize(eurovision_geo$winnings, range = c(first(x_axis_start), 100), method = "range"),
                   val_txt = paste0(format(eurovision_geo$winnings, digits = 0, nsmall = 2)),
                   val_txt2 = if_else(country == "Ireland", paste0(val_txt, " victories"), val_txt))) %>% 
  arrange(desc(fine_rank))


ranking$rank = 66.12161

fine_rank = 66.12161

for (i in 1: 24){
ranking[i, 10] = fine_rank
fine_rank = fine_rank - 1.40541
}

eurovision_geo <- eurovision_geo %>% 
  bind_cols(ranking %>% select(fine_rank))

colors = met.brewer("Java", 6)

ranking$fine_rank = factor(ranking$fine_rank,c("66.12161","64.71620","61.20269","55.58107","49.95945","40.12161"))

paletteFunc <- colorRampPalette(c('red', 'purple'));
palette1     <- paletteFunc(8);

barplot(1:8, col=palette);

ggplot() + 
  geom_sf(data = eurovision_geo, size = .3, fill = "transparent", color = "#4e216e") +
  # Sigmoid from country to start of barchart
  geom_sigmoid(data = ranking, 
               aes(x = X, y = Y, xend = x_axis_start - .2, yend = rank, group = country, color = fine_rank), 
               alpha = .6, smooth = 10, size = 1) + 
  # Line from xstart to value
  geom_segment(data = ranking, 
               aes(x = x_axis_start, y = rank, xend = fine_rank_x, yend = rank, color = fine_rank), alpha = .6, size = 1, 
               lineend = "round") + 
  # dot on centroid of country in map
  geom_point(data = ranking, 
             aes(x = X, y = Y, color = fine_rank), size = 2) +
  # Country text
  geom_text(data = ranking, aes(x = x_axis_start-.5, y = rank, label = country, color = fine_rank), hjust = 1, size = 3.5, nudge_y = .5, fontface='bold') +
  # Value text
  geom_text(data = ranking, aes(x = fine_rank_x, y = rank, label = val_txt2, color = fine_rank), hjust = 0, size = 4, nudge_x = .4) +
  coord_sf(clip = "off") +
  scale_fill_viridis(option="plasma")+
  scale_color_viridis(option="plasma")+
  theme_void() +
  labs(title = "Eurovison winning countries",
       subtitle = "Overall number of victories for each country to have ever won the Eurovision Song contest \nIreland is the country with most victories, having won the contest 7 times since its start in 1956",
       caption = "Data: Post45 Data | Plot: @AntoniaPopes | #TidyTuesday") +
  theme(
    text = element_text(family = "Merriweather", size = 12, color = "#8152a1"),
    plot.title = element_text(family = "Russo One",margin = margin(10, 0, 10, 0), size = 40),
    plot.subtitle = element_text(margin = margin(10, 0, 10, 0), size = 12),
    plot.caption = element_text(hjust = 1,
                                margin = margin(10, 0, 10, 0), size = 10),
    plot.margin = margin(1, 1, 1, 1, unit = "cm"),
    plot.background = element_rect(fill = "#12001e"),
    legend.position = "none"
  )

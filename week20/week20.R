#library load
library(tidyverse)
library(BBmisc)
library(showtext)
library(ggplot2)
library(showtext)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggbump)
library(feather)
library(janitor)
library(viridis)
library(hablar)


#setting up text font
showtext_auto()
sysfonts::font_families_google()
sysfonts::font_add_google("Russo One", "Russo One")


#loading week20 data
eurovision <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision.csv')

#data prep
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

country_rankinging <- st_geometry(eurovision_geo) %>% 
  st_point_on_surface() %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  bind_cols(tibble(country_cluster = normalize(rank(eurovision_geo$winnings),range = c(40.12161, 66.12161), method = "range"),
                   country = eurovision_geo$NAME,
                   xend = 60,
                   x_axis_start = xend + 10,
                   country_cluster_x = normalize(eurovision_geo$winnings, range = c(first(x_axis_start), 100), method = "range"),
                   val_txt = paste0(format(eurovision_geo$winnings, digits = 0, nsmall = 2)),
                   val_txt2 = if_else(country == "Ireland", paste0(val_txt, "  total victories"), val_txt))) %>% 
  arrange(desc(country_cluster))


country_rankinging$country_ranking = 66.12161

country_cluster = 66.12161

for (i in 1: 24){
country_rankinging[i, 10] = country_cluster
country_cluster = country_cluster - 1.40541
}

eurovision_geo <- eurovision_geo %>% 
  bind_cols(country_rankinging %>% select(country_cluster))


#plot
ggplot() + 
  geom_sf(data = eurovision_geo, size = .3, fill = "transparent", color = "#8152a1") +
  # Sigmoid from country to start of barchart
  geom_sigmoid(data = country_rankinging, 
               aes(x = X, y = Y, xend = x_axis_start - .2, yend = country_ranking, group = country, color = country_cluster), 
               alpha = .6, smooth = 10, size = 1.3) + 
  # Line from xstart to value
  geom_segment(data = country_rankinging, 
               aes(x = x_axis_start, y = country_ranking, xend = country_cluster_x, yend = country_ranking, color = country_cluster), alpha = .6, size = 1.5, 
               lineend = "round") + 
  # dot on centroid of country in map
  geom_point(data = country_rankinging, 
             aes(x = X, y = Y, color = country_cluster), size = 5) +
  # Country text
  geom_text(data = country_rankinging, aes(x = x_axis_start-.5, y = country_ranking, label = country, color = country_cluster), hjust = 1, size = 3.5, nudge_y = .5, fontface='bold') +
  # Value text
  geom_text(data = country_rankinging, aes(x = country_cluster_x, y = country_ranking, label = val_txt2, color = country_cluster), hjust = 0, size = 4, nudge_x = .4) +
  coord_sf(clip = "off") +
  scale_fill_viridis(option="plasma")+
  scale_color_viridis(option="plasma")+
  theme_void() +
  labs(title = "Eurovison winning countries",
       subtitle = "Overall number of victories for each country to have ever won the Eurovision Song contest \nIreland is the country with most victories, having won the contest 7 times since its start in 1956",
       caption = "Data: Eurovision curated by @tanya_shapiro & @hrbrmstr | Plot: @AntoniaPopes | #TidyTuesday") +
  theme(
    text = element_text(family = "Russo One", size = 12, color = "#8152a1"),
    plot.title = element_text(margin = margin(10, 0, 10, 0), size = 46),
    plot.subtitle = element_text(margin = margin(10, 0, 10, 0), size = 12),
    plot.caption = element_text(hjust = 1,
                                margin = margin(10, 0, 10, 0), size = 10),
    plot.margin = margin(1, 1, 1, 1, unit = "cm"),
    plot.background = element_rect(fill = "#12001e"),
    legend.position = "none"
  )

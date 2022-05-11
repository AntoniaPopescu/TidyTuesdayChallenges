library(tidyverse)
library(MetBrewer)
library(showtext)
library(ggplot2)
library(showtext)



#setting up text font
showtext_auto()
sysfonts::font_families_google()
#sysfonts::font_add_google("Merriweather", "Merriweather")
sysfonts::font_add_google("Abril Fatface", "Abril Fatface")


#loading week19 data
nyt_titles <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_titles.tsv')
nyt_full <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_full.tsv')

#prep data
books <- nyt_full %>% 
  mutate(decade = (year - 1) %/% 10 * 10) %>% 
  distinct(decade, title_id) %>%
  count(decade) %>% 
  rename(bookCount = n)

authors <- nyt_full %>% 
  mutate(decade = (year - 1) %/% 10 * 10) %>% 
  distinct(decade, author) %>% 
  count(decade) %>% 
  rename(authorCount = n)

decadeCounts = merge(books, authors, by = "decade")  
decadeCounts = decadeCounts %>% 
  mutate(booksPerAuthor = round(bookCount/authorCount, digits = 2))

decadeCounts$decade = factor(decadeCounts$decade,c("1930","1940", "1950","1960","1970","1980","1990","2000", "2010"))

#plot
colors = met.brewer("Signac", 10)
authorColors = sample(colors)

get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}

l <- get_png("NYTimesLogo.png")



authorPlot = ggplot(decadeCounts, aes(x = decade, y = authorCount, fill = decade)) +
  geom_bar(stat = "identity", width = 1, colour = "black", size = 2, show.legend = FALSE) +
  geom_text(aes(label = booksPerAuthor), nudge_y = -20.5, size = 5, fontface='bold')+
  geom_point(
    aes(y = authorCount - 65),
    shape = 23,
    fill = "black",
    stroke = 2,
    size = 15,
    alpha = 0.7)+
  geom_text(aes(label =  authorCount, y = authorCount - 65), color = "white", size = 5, fontface='bold')+
  geom_label(aes(label = decade, y = 0), fill = "white", vjust = 0, nudge_y = 10, size = 5, fontface='bold',  show.legend = FALSE, alpha = 0.5)+
  scale_fill_manual(values = authorColors) +
  theme_void() +
  labs(title = "\n\n\n\n\nNY Times authors through the decades ",
       subtitle = "\n Each book, rapresenting each decade, is annotated with:\n\n   - Top: book per author ratio\n\n   - Middle: number of authors in the list each decade\n\n   - Bottom: decade",
       caption = "Data: Onthesnow.co.uk | Plot: @AntoniaPopes | #TidyTuesday") +
  theme(
    text = element_text(family = "Abril Fatface"),
    plot.subtitle = element_text(margin = margin(10, 0, 10, 0), size = 12),
    plot.caption = element_text(hjust = 1,
                                margin = margin(10, 0, 10, 0), size = 12),
    plot.margin = margin(1, 1, 1, 1, unit = "cm")
  )


authorPlot +
  annotation_custom(l, xmin = 0.5, xmax = 6.5, ymin = 800, ymax = 1100) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = unit(c(1, 1, 3, 1), "lines"))


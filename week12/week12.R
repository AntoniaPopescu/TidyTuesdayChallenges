#loading libraries
library("gridExtra") 
library(dplyr)
library(tidyverse)
library(ggstream)
library(showtext)
library(ggtext)
library(sysfonts)
library(ggplot2)
library(tidyverse)
library(gganimate)
library(directlabels)
library(png)
library(transformr)
library(ggridges)


#setting up text font
showtext_auto()
sysfonts::font_families_google()
sysfonts::font_add_google("Merriweather","Merriweather")


#loading week12 data
tuesdata <- tidytuesdayR::tt_load('2022-03-22')

babynames <- tuesdata$babynames

#most popular girl names 
babyFnames = filter(babynames,sex == "F")

popularF = babyFnames %>%
  filter(year == "2017")%>%
  group_by(name) %>%
  summarise(n=sum(n)) %>%
  arrange(desc(n))%>%
  slice(1:5)

popularF <- subset(babyFnames, name %in% popularF$name)

# Compute percentages
popularF = popularF %>%
  group_by(year) %>%
  mutate(fraction = n/sum(n)) %>%
  mutate(ymax = cumsum(fraction)) %>%
  mutate(ymin = c(0, head(ymax, n=-1)))

#most popular boy names 
babyMnames = filter(babynames,sex == "M")

popularM = babyMnames %>%
  filter(year == "2017")%>%
  group_by(name) %>%
  summarise(n=sum(n)) %>%
  arrange(desc(n))%>%
  slice(1:5)

popularM <- subset(babyMnames, name %in% popularM$name)

# Compute percentages
popularM = popularM %>%
  group_by(year) %>%
  mutate(fraction = n/sum(n)) %>%
  mutate(ymax = cumsum(fraction)) %>%
  mutate(ymin = c(0, head(ymax, n=-1)))

#popularF$label <- paste0(popularF$name, "\n nr of babies: ", popularF$n)
#popularF$labelPosition <- (popularF$ymax + popularF$ymin) / 2

popularF17 = subset(popularF, year == "2017")
# Make the plot

anim = ggplot(popularF, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=name)) +
  geom_rect()+
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  geom_label(
    label=label1,
    nudge_x=0.45, nudge_y=0.1,
    check_overlap=T,
    label.padding=unit(0.55, "lines"),
    label.size=0.4,
    color="white",
    fill=name)+
  labs(title="", 
       subtitle="Year: {closest_state}",
       caption="Data: CRAN archives & Robert Flight | Plot: @AntoniaPopes | #TidyTuesday")+
  theme(text = element_text(family="Merriweather"),
        plot.background = element_rect(fill = "#eff6f8",color=NA),
        plot.title = element_text(size=24, face='bold'))+
 transition_states(year,transition_length = 1,state_length = 2) 

animate(anim, fps = 10, nframes = 500)
anim_save("basic_animation.gif", anim)

plotM = ggplot(popularF17, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=name)) +
  geom_rect()+
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() 
  #labs(title="", 
  #    subtitle="Year: {closest_state}",
  #   caption="Data: CRAN archives & Robert Flight | Plot: @AntoniaPopes | #TidyTuesday")+
  #  theme(text = element_text(family="Merriweather"),
  #       plot.background = element_rect(fill = "#eff6f8",color=NA),
  #      plot.title = element_text(size=24, face='bold'))+
 # transition_states(year, 
  #                  transition_length = 2,
   #                 state_length = 1) 
grid.arrange(plotF, plotM, ncol = 2)  

plotF

ggplot(popularF, aes(y=n, x=year, fill=name))+
  geom_stream(type = "proportion" )+
  #coord_polar()+
  labs(title="TIDYVERSE VIGNETTES", 
       subtitle="CRAN package RMD vignettes for core tidyverse packages by year",
       y="", x="",
       caption="Data: CRAN archives & Robert Flight | Plot: @AntoniaPopes | #TidyTuesday")+
  theme_minimal()

ggplot(popularF17, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=name)) +
  geom_rect()+
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void()


# Ranking by year and number of babies
popularF = popularF %>% 
  arrange(year, n) %>% 
  mutate(order = 1:n())

# Animation
p = popularF %>% 
  ggplot(aes(x = order, y = n, fill = name)) +
  geom_bar(stat = "identity") +
  labs(title='Popular baby names in {closest_state}', x=NULL) +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) +
 #scale_x_continuous(breaks=popularF$order, labels=popularF$name, position = "top") +
  transition_states(year, transition_length = 1, state_length = 1) +
  view_follow(fixed_y=TRUE) +
  ease_aes('cubic-in-out')

animate(p, nframes=600, fps=20)
anim_save("bar_animation.gif", p)

lineplot = popularF %>% 
  ggplot(aes(x = year, y = n, color = name)) +
  geom_point(aes(group = seq_along(year)), # needed, otherwise transition dosen't work
             size = 4, # size of points
             alpha = 0.7) +
  geom_line(aes(lty = name), # line type according to group
            alpha = 0.6) + 
  labs(title='Popular baby names in {closest_state}', x=NULL) +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) 
  #scale_x_continuous(breaks=popularF$order, labels=popularF$name, position = "top") +
  #transition_states(year, transition_length = 1, state_length = 1) +
  #view_follow(fixed_y=TRUE) +
  #ease_aes('cubic-in-out')
lineplot =lineplot +
  transition_reveal(along = year) 

lineplot


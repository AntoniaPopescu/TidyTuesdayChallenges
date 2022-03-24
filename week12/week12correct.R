library(tidyverse)
library(showtext)
library(patchwork)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(showtext)
library(usefunc)
library(patchwork)
library(cowplot)

install.packages("usefunc")
#setting up text font
showtext_auto()
sysfonts::font_families_google()
sysfonts::font_add_google("Merriweather","Merriweather")
sysfonts::font_add_google("Abril Fatface","Abril Fatface")


#loading week12 data
tuesdata <- tidytuesdayR::tt_load('2022-03-22')

babynames <- tuesdata$babynames

#Most changing names

#female
babyFnames = filter(babynames,sex == "F")

changingF = babyFnames %>% 
  group_by(name) %>% 
  mutate(min = min(n)) %>% 
  mutate(max = max(n)) %>% 
  mutate(change = max-min) %>% 
  arrange(desc(change))

topChangingF = changingF %>% 
  ungroup() %>% 
  distinct(name, change) %>% 
  top_n(6)

changingF <- subset(changingF, name %in% topChangingF$name)
changingF$nameR = factor(changingF$name, levels=c('Linda','Mary','Jennifer','Lisa','Patricia','Jessica'))

#male
babyMnames = filter(babynames,sex== "M")

changingM = babyMnames %>% 
  group_by(name) %>% 
  mutate(min = min(n)) %>% 
  mutate(max = max(n)) %>% 
  mutate(change = max-min) %>% 
  arrange(desc(change))

topChangingM = changingM %>% 
  ungroup() %>% 
  distinct(name, change) %>% 
  top_n(6)

changingM <- subset(changingM, name %in% topChangingM$name)
changingM$nameR = factor(changingM$name, levels=c('Michael','James','Robert','David','John','William'))

#plot points
pointsF <- changingF %>%
   group_by(name) %>% 
   slice(which.max(n))

pointsM <- changingM %>%
  group_by(name) %>% 
  slice(which.max(n))

#plots
colorF = rev(c('#c994c7','#df65b0','#e7298a','#ce1256','#980043','#67001f'))
colorM = rev(c('#a6bddb','#74a9cf','#3690c0','#0570b0','#045a8d','#023858'))

pF = ggplot(data = changingF, 
             mapping = aes(x = year, y = n)) + 
  geom_area(aes(group = name, fill = nameR)) +
  facet_wrap(~nameR, nrow = 1, strip.position = "top") +
  scale_fill_manual(values = colorF)+
  scale_colour_manual(values = colorF)+
  scale_y_continuous(limits = c(0, 100000)) +
  scale_x_continuous(breaks = c(1880, 2017)) +
  coord_cartesian(expand = F) +
  labs(x = "", y = "Number of babies\n", ) +
  geom_point(
    data = pointsF, aes(x = year, y = max-5000, color = nameR),
    shape = 21, fill = "white", stroke = 2, size = 15
  ) +
  geom_text(data = pointsF, aes(x = year, y = max-5000, label = max), size = 4) +
  theme_minimal()+
  theme(legend.position = "none", 
        panel.spacing = unit(2, "lines"))
pF

pM = ggplot(data = changingM, 
            mapping = aes(x = year, y = n)) + 
  geom_area(aes(group = name, fill = nameR)) +
  facet_wrap(~nameR, nrow = 1,strip.position="bottom") +
  scale_fill_manual(values = colorM)+
  scale_colour_manual(values = colorM)+
  scale_y_continuous(limits = c(0, 100000)) +
  scale_x_continuous(breaks = c(1880, 2017), position = "top") +
  coord_cartesian(expand = F) +
  labs(x = "", y = "Number of babies\n") +
  geom_point(
    data = pointsM, aes(x = year, y = max-5000, color = nameR),
    shape = 21, fill = "white", stroke = 2, size = 15
  ) +
  geom_text(data = pointsM, aes(x = year, y = max-5000, label = max), size = 4) +
  theme_minimal()+
  theme(legend.position = "none", 
        panel.spacing = unit(2, "lines"))+
  scale_y_reverse()
pM

# join plots
p <- pF /pM +
  plot_annotation(title="Can a name go out of style?", 
                    subtitle="Baby girl & boy names that have seen the highest change in popularity over the last century",
                  caption="Data: {babynames} r package  | Plot: @AntoniaPopes | #TidyTuesday") &
  theme(text = element_text(family="Merriweather"),
        axis.title = element_text(size=12),
        axis.text = element_text(size=12, face = 'bold', hjust = 1),
        plot.background = element_rect(fill = "#efedf5",color=NA),
        plot.title = element_text(margin = margin(20, 0, 10, 0),size=44, family="Abril Fatface"),
        plot.subtitle = element_text(margin = margin(10, 0, 10, 0), size = 14),
        panel.grid.major =element_blank(),
        panel.grid.minor =element_blank())
p

#save
ggsave(p,filename="babynames.png", width = 1350, height = 1080, units = "px")

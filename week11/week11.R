#loading libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggridges)
library(RColorBrewer)
library(ggstream)
library(showtext)
library(ggtext)
library(sysfonts)

#setting up text font
showtext_auto()
sysfonts::font_families_google()
sysfonts::font_add_google("Merriweather","Merriweather")

#loading week11 data
tuesdata = tidytuesdayR::tt_load('2022-03-15')

bioc = tuesdata$bioc
cran = tuesdata$cran

#tidyverse package
tidyPackage <- c('ggplot2','dplyr','tidyr','readr','purrr','tibble','stringr','forcats')

#cleaning up date info
tidyCran <- cran %>%
  filter(package %in% tidyPackage)%>%
  mutate(newDate=as.Date(date, "%Y-%m-%d")) %>%
  filter(!is.na(newDate)) %>%
  mutate(year = year(newDate))

#summary stats for plot
data<-tidyCran%>%
  group_by(package,year)%>%
  summarise(rmdVignettes = sum(rmd))%>%
  merge(tidyCran%>%group_by(package)%>%summarise(last = max(date))%>%arrange(last), by="package")


#CREATE GGPLOT
tidyPalette = c("#f48a95", "#f22324","#682223", "#a45775", "#a0b3cc", "#e1dde7", "#962fbf", "#4f5bd5")
yearLabels = c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")

tidyVignettes = ggplot(data, aes(x = year, y = rmdVignettes, fill = package))+
  geom_stream(type = "proportion" )+
  scale_fill_brewer(palette = "YlGnBu", direction = -1)+
  scale_x_continuous(breaks = 2009:2021,labels= yearLabels)+
  labs(title="VIGNETTES IN TIDYVERSE", 
       subtitle="Yearly proportion of CRAN RMD vignettes for core tidyverse packages",
       caption="Data: CRAN archives & Robert Flight | Plot: @AntoniaPopes | #TidyTuesday",
       y = "Vignettes Proportion")+
  coord_polar()+
  theme_minimal()+
  theme(text = element_text(family="Merriweather"),
        legend.position = "bottom",
        legend.text = element_text(size=12),
        axis.title = element_text(size=12),
        axis.text = element_text(size=12, face = 'bold', hjust = 1),
        plot.background = element_rect(fill = "#eff6f8",color=NA),
        plot.title = element_text(size=24, face='bold'),
        plot.margin = margin(1,1,1,1, unit = "cm"),
        panel.grid.major.x =element_line(color="white", size=1),
        panel.grid.major.y =element_line(color="white", size=0.5, linetype = 2),
        panel.grid.minor = element_blank(), 
        panel.ontop = TRUE)

ggsave(tidyVignettes,filename="tidyVignettes.png")

#other attempted plots  
ggplot(data, aes(x = year, y = rmdVignettes, group = package, fill = package))+
  geom_area(alpha = 0.5,
            color = 1,    
            lwd = 0.5,    
            linetype = 1) +
  labs(title="TIDYVERSE VIGNETTES", 
       subtitle="CRAN package RMD vignettes for core tidyverse packages by year",
       caption="Data: CRAN archives & Robert Flight | Plot: @AntoniaPopes | #TidyTuesday")+
  theme_minimal()

ggplot(data, aes(x = year, y = rmdVignettes, fill = package))+
    geom_stream(type = "ridge",color = 1, lwd = 0.25)+
    scale_fill_brewer(palette = "RdYlBu")+
    labs(title="TIDYVERSE VIGNETTES", 
       subtitle="CRAN package RMD vignettes for core tidyverse packages by year",
       caption="Data: CRAN archives & Robert Flight | Plot: @AntoniaPopes | #TidyTuesday")+
    theme_minimal()


ggplot(data, aes(y=package, x=year))+
   geom_density_ridges(alpha = 0.5) + 
   scale_x_continuous(limits = c(2009, 2021)) +
   labs(title="TIDYVERSE VIGNETTES", 
        subtitle="CRAN package RMD vignettes for core tidyverse packages by year",
        y="", x="",
        caption="Data: CRAN archives & Robert Flight | Plot: @AntoniaPopes | #TidyTuesday")+
   theme_minimal()

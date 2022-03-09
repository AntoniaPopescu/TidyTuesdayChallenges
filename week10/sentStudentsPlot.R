#library
library(magick)
library(ggplot2)
library(grid)
library(rworldmap)
library(tidyverse)
library(tidytuesdayR)
library(rworldxtra)
library(showtext)
library(ggtext)
library(sysfonts)


#Getting data
tuesdata <- tidytuesdayR::tt_load('2022-03-08')
erasmus <- tuesdata$erasmus

#setting up text font
showtext_auto()
sysfonts::font_families_google()
sysfonts::font_add_google("Junge","Junge")

#Setting up map
worldMap <- getMap(resolution = "high", projection = NA )

# Member States of the European Union
europe <- c("Albania","Andorra","Armenia","Austria","Azerbaijan","Belarus","Belgium","Bosnia and Herz.","Bulgaria",
            "Croatia","Cyprus","Czech Rep.","Denmark","Estonia","Finland","France","Georgia","Germany","Greece","Hungary",
            "Iceland","Ireland","Italy","Kazakhstan","Kosovo","Latvia","Liechtenstein","Lithuania","Luxembourg","Malta",
            "Moldova","Monaco","Montenegro","Netherlands","Macedonia","Norway","Poland","Portugal","Romania",
            "San Marino","Serbia","Slovakia","Slovenia","Spain","Sweden","Switzerland","Turkey","Ukraine","United Kingdom")
# Selecting only the index of european countries 
indexEU <- which(worldMap$NAME%in%europe)

# Extract longitude and latitude border's of each country 
europeCoords <- lapply(indexEU, function(i){
  df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$region =as.character(worldMap$NAME[i])
  colnames(df) <- list("long", "lat", "region")
  return(df)
})
europeCoords <- do.call("rbind", europeCoords)

#Adding student numbers per country
europeCoords$nationality <- countryCodes$code[match(europeCoords$region,countryCodes$region)]

#empty list to store yearly plots in 
years = c("2014-2015","2015-2016","2016-2017","2017-2018", "2018-2019", "2019-2020")
listID = list()
plot = 1

#Plotting the map

counter = 1
for (year in years){
  sentStudents = erasmus%>%
    subset(academic_year == year)%>%
    group_by(sending_country_code)%>%
    summarize(participants=sum(na.omit(participants)))
  
  europeCoords$sent = sentStudents$participants[match(europeCoords$nationality,sentStudents$sending_country_code)]
  
  sentMap = ggplot() + 
    geom_polygon(data = europeCoords, aes(x = long, y = lat, group = region, fill = sent),colour = "black", size = 0.01) +
    coord_map(xlim = c(-22, 40),  ylim = c(30, 71)) + 
    scale_fill_viridis_c(name = "Number of Students", option = "magma") +
    labs(title = "Yearly Erasmus Students Sent by Each European Country",
         subtitle = year,
         caption = "\nData: Data.Europa| Plot: @AntoniaPopescu | #TidyTuesday") +
    theme_void()+
    theme(text = element_text(family="Junge"),
          legend.position = "right",
          legend.text = element_text(size=12),
          plot.background = element_rect(fill = "#FAF0F0",color=NA),
          plot.title = element_text(size=20, face='bold'),
          plot.subtitle = element_text(size=14),
          plot.caption = element_text(size=12),
          plot.margin = margin(1,1,1,1, unit = "cm"))
  ggsave(sentMap,filename=paste("sentMap",year,".png",sep=""))
}
#making gif from saved plots
file_names <- list.files(pattern = "sentMap")

image_read(file_names) %>%
  image_animate(fps = 1) %>%
  image_write("European_Erasmus_Students_Yearly.gif")


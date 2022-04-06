library(tidyverse)
library(showtext)
library(ggplot2)
library(dplyr)

library(geofacet)


#setting up text font
showtext_auto()
sysfonts::font_families_google()
sysfonts::font_add_google("Ubuntu", "Ubuntu")
sysfonts::font_add_google("Varela Round", "Varela Round")


#loading week13 data
tuesdata = tidytuesdayR::tt_load('2022-03-29')
sports = tuesdata$sports

exp_data = sports %>%
  select(state_cd, exp_men, exp_women, total_exp_menwomen) %>%
  drop_na() %>%
  group_by(state_cd) %>%
  mutate(total_exp = sum(total_exp_menwomen)) %>%
  mutate(exp_women = (sum(exp_women)) * 100 / total_exp) %>%
  mutate(exp_men = (sum(exp_men)) * 100 / total_exp) %>%
  rename(men = exp_men,
         women = exp_women) %>%
  summarize(across(men:women, mean)) %>%
  pivot_longer(2:3, names_to = "sex", values_to = "exp")

#plot map
p = ggplot(exp_data, aes(x = 1.4, y = exp, fill = sex)) +
  geom_col(color = "white", width = 0.7) +
  coord_polar(theta = "y", start = 0) +
  facet_geo( ~ state_cd) +
  scale_fill_manual(values = c("#11C99F" , "#D83E1B")) +
  theme_void() +
  labs(title = "How does each state split their college sports budget?",
                  subtitle = "Proportion of expenditure in College Sports in each state on women's and men's sports from 2015 to 2019",
                  caption = "Data: Equity in Athletics Data Analysis | Plot: @AntoniaPopes | #TidyTuesday") +
  theme(
    text = element_text(family = "Ubuntu", face = 'bold', colour = "#004D40"),
    plot.background = element_rect(fill = "#f7f7f2", color = NA),
    plot.title = element_text(
      margin = margin(20, 0, 10, 0),
      size = 36,
      family = "Varela Round"
    ),
    plot.subtitle = element_text(margin = margin(10, 0, 10, 0), size = 14),
  )

#save
ggsave(p,filename="sportsExpenditures.png", width = 1350, height = 1080, units = "px")

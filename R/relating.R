library(tidyverse)
library(donsteR)
library(googleAnalyticsR)
library(lubridate)
library(plotly)
library(janitor)
library(readxl)
library(ggrepel)

rel <- read_excel("projects/relating.xlsx") %>% 
  separate(dim_name, into = c("characteristic", "sub_dimension"), sep = "\\: ") %>% 
  mutate(sub_dimension = str_replace_all(sub_dimension,
                                         c("(and |in )" = "\\1\n")),
         relevance = case_when(characteristic == "Sensemaking" ~ relevance -.25,
                               TRUE ~ relevance))

rel %>% 
  ggplot(aes(skill_level, relevance, label = sub_dimension, fill = characteristic)) +
  geom_label() +
  ge_theme(orient = "hor") +
  labs(x = "Skill level",
       y = "Relevance",
       fill = "Characteristic",
       label = "Characteristic",
       title = "Relevance Vs skill level for relating/sensemaking characteristics",
       subtitle = "Donal Phipps") +
  scale_y_continuous(breaks = c(1,2,3),
                     limits = c(0.5, 3.5),
                     labels = c("Slightly Relevant",
                                "Moderately Relevant",
                                "Highly Relevant")) +
  scale_x_continuous(breaks = c(1,2,3,4),
                     limits = c(0.5, 4.5),
                     labels = c("Not at all \nSkilled",
                                "Not Particularly \nSkilled",
                                "Skilled",
                                "Exceptionally \nSkilled")) +
  scale_fill_brewer(type = "div", palette = 1) +
  theme(panel.grid.major.x = element_line(colour = "gray90", size = .1))

ggsave("output/skill_scatter.png", width = 260, height = 160, units = "mm")

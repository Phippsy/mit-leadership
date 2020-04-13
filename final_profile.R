library(tidyverse)
library(donsteR)
library(googleAnalyticsR)
library(lubridate)
library(plotly)
library(janitor)
library(readxl)

props <- read_excel("projects/self-assessment.xlsx",
                    sheet = "My goals") %>% 
  clean_names() %>% 
  rename(score = your_score_1_4) %>% 
  mutate(sub_dimension = str_replace_all(sub_dimension, "\\d\\) ", ""),
         sub_dimension = paste0(ranking, " | ", sub_dimension, " (", str_extract(capability, "^.{2}"), ")")) %>% 
  mutate(sub_dimension = fct_reorder(sub_dimension, -ranking),
         Approach = case_when(
           ranking <= 8 & score <=2 ~ "High Priority, need help",
           ranking <= 8 & score >=3 ~ "High Priority, strength",
           ranking <= 12 & score >=3 ~ "Medium Priority, strength",
           ranking <= 12 & score >=3 ~ "Medium Priority, strength",
           score >= 3 ~ "Low Priority, Strength",
           score <= 3 ~ "Low Priority, need help",
           TRUE ~ "Error"
         ))

props %>% 
  ggplot(aes(sub_dimension, score, fill = Approach)) +
  geom_col() + 
  coord_flip() +
  scale_y_continuous(labels = c("", "\n\n\n\nNot at all \nskilled",
                                "\n\n\n\nNot particularly \nskilled",
                                "\n\n\n\nSkilled",
                                "\n\n\n\nExceptionally \nSkilled")) +
  ge_theme(orient = "mob") +
  labs(
    title = "Personal priorities and leadership approach, Donal Phipps",
    subtitle = "Ranking: order of relevance to role and personal context. 1 = Most relevant, 15 = least relevant.",
    x = "Ranking | Sub-dimension (Capability)",
    y = "Score"
  ) +
  scale_fill_manual(values = c("red3", "dodgerblue4", "lightcoral", "dodgerblue1", "deepskyblue1")) +
  geom_hline(yintercept = 2.5, linetype = "dotted")

ggsave(filename = "projects/personal_leadership_approach.png", width = 300, height = 200, units = "mm")

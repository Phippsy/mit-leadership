library(tidyverse)
library(donsteR)
library(googleAnalyticsR)
library(lubridate)
library(plotly)
library(janitor)
library(readxl)

files <- paste0("R/raw-data/", list.files("R/raw-data"))

getit <- function(file) {
  lead <- read_excel(file) %>% 
    mutate(student = file) %>% 
  filter(!is.na(Notes))
} %>% 
  clean_names

studs <- map_df(files, getit) %>% 
  mutate(student = str_replace_all(student,
                                   c(".*\\/|.xlsx" = "")),
         your_score_1_4 = as.numeric(your_score_1_4),
         sub_dimension = fct_rev(sub_dimension))

studs %>% 
  ggplot(aes(sub_dimension, your_score_1_4, colour = sub_dimension,
             shape = student)) + 
  geom_jitter(width = 0, height = .3) + 
  ge_theme(orient = "hor") + 
  facet_wrap(~capability, scales = "free", ncol = 1) +
  coord_flip() +
  guides(colour = FALSE)  +
  labs(
    title = "Distribution of 4-CAPS+ self-assessments",
    y = "Perceived ability",
    x = "Capability / Sub-dimension",
    shape = "Student:"
  ) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(colour = "gray50", size = 8)) +
  scale_y_continuous(breaks = 1:4, limits = c(0,5),
                     labels = c("Not at\nall skilled",
                                "Not particularly \nskilled",
                                "Skilled",
                                "Exceptionally skilled")) 

ggsave("output/distribution_individuals.png",
       width = 230, height = 200, units = "mm")  


studs %>% 
  ggplot(aes(sub_dimension, your_score_1_4, colour = sub_dimension)) +
  geom_violin() +
  ge_theme(orient = "hor") +
  coord_flip() +
  facet_wrap(~capability, scales = "free", ncol = 1) +
  labs(
    title = "Distribution of 4-CAPS+ self-assessments\nfor MIT February cohort\n",
    y = "Perceived ability",
    x = "Capability / Sub-dimension"
  ) +
  theme(legend.position = "none",
        strip.background = element_rect(colour = "gray50", size = .1),
        axis.text.x = element_text(colour = "gray50", size = 8)) +
  scale_y_continuous(breaks = 1:4, limits = c(0.75,4.25),
                     labels = c("Not at\nall skilled\n\n",
                                "Not particularly \nskilled\n\n",
                                "Skilled",
                                "Exceptionally skilled\n\n"))


ggsave("output/distribution.png",
       width = 230, height = 230, units = "mm")  

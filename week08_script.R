#' ---
#' title: "Report week8"
#' output: html_document
#' date: "2023-10-3"
#' author: Zachary Bunch
#' ---


library(tidyverse)

# read csv

df_anova <- read_csv(here::here("data_raw/data_fish_length_anova.csv"))

#check
distinct(df_anova, lake)

unique(df_anova$lake)

#figure

df_anova %>% 
  ggplot(aes(x=lake, y = length)) +
  geom_violin(draw_quantiles = 0.5, fill = 'salmon', alpha = 0.5) +
  geom_jitter(width = 0.2, height = 0)



#' ---
#' title: "Report week3"
#' output: html_document
#' date: "2023-8-29"
#' author: Zachary Bunch
#' ---


# Visualization -----------------------------------------------------------

library(ggplot2)
library(tidyverse)

# basic plot
iris %>% 
  ggplot(aes(x = Sepal.Length,
             y = Sepal.Width)) +
  geom_point()

# change color by "Species" column
iris %>% 
  ggplot(aes(x = Sepal.Length,
             y = Sepal.Width,
             color = Species)) +
  geom_point()

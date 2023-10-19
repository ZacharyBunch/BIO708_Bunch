#' ---
#' title: "Report week10"
#' output: html_document
#' date: "2023-10-19"
#' author: Zachary Bunch
#' ---

library(tidyverse)
library(dbplyr)

# read data

df_fl <- read_csv(here::here("data_raw/data_fish_length.csv"))

m <- lm(length ~ lake, 
        data = df_fl)

#calculate mean for lake a

mu_a <- df_fl %>% 
  filter(lake == "a") %>% 
  pull(length) %>% 
  mean()

mean <- df_fl %>% group_by(lake) %>% 
  summarize(mu=mean(length))

#calculate mean for lake b

mu_b <- df_fl %>% 
  filter(lake == "b") %>% 
  pull(length) %>% 
  mean()


# difference beten two groups

b <- mu_b - mu_a



#compare with t-test

## lm summary
summary(m)

# apply t-test


a <- df_fl %>% 
  filter(lake == "b") %>% 
  pull(length)

b <- df_fl %>% 
  filter(lake == "b") %>% 
  pull(length)



t.test(b,a, var.equal = true)

#anova

df_anova <- read_csv(here::here("data_Raw/data_fish_length_anova.csv"))

lm(length ~ lake, 
   data = df_anova)

#get difference from lake a

df_mu <- df_anova %>% 
  group_by(lake) %>% 
  summarize(mu = mean(length)) %>% 
  arrange(lake) %>% 
  pull(mu)

# do anova with aov()

m_aov <-aov(length ~ lake, data = df_anova)


# compare the lm() and aov() output

summary(m_aov)


# combine different predictors --------------------------------------------

iris <- as_tibble(iris)


# develop iris model
m_iris <- lm(Petal.Length ~ Petal.Width + Species,
             data = iris)

summary(m_iris)


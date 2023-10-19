#' ---
#' title: "Report week10"
#' output: html_document
#' date: "2023-10-19"
#' author: Zachary Bunch
#' ---

library(tidyverse)
library(dbplyr)

# read data

df_fl <- read_csv(here:here("data_raw/data_fish_length.csv"))

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

b <- mu_b = mu_a



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


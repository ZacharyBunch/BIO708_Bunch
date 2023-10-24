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



t.test(b, a)

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



# prediction --------------------------------------------------------------

# create a data frame for prediction
# variable names must be identical to the original dataframe for analysis
n_rep <- 100
df_pred <- tibble(Petal.Width = rep(seq(min(iris$Petal.Width),
                                        max(iris$Petal.Width),
                                        length = n_rep),
                                    n_distinct(iris$Species)),
                  Species = rep(unique(iris$Species),
                                each = n_rep))

# make prediction based on supplied values of explanatory variables
y_pred <- predict(m_iris,
                  newdata = df_pred)

df_pred <- df_pred %>% 
  mutate(y_pred = y_pred)

print(df_pred)

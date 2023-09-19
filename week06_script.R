#' ---
#' title: "Report week6"
#' output: html_document
#' date: "2023-9-19"
#' author: Zachary Bunch
#' ---

library(tidyverse)

# load csv data on R
df_h0 <- read_csv(here::here("data_raw/data_plant_height.csv"))

df_h0 %>% 
  ggplot(aes(x = height)) + 
  geom_histogram(binwidth = 1) + # specify binwidth
  geom_vline(aes(xintercept = mean(height))) # draw vertical line at the mean

#probability distribution

# vector of x values
# seq() generate min to max values with specified numbers of elements or interval
# the following produce 100 elements
x <- seq(min(df_h0$height), max(df_h0$height), length = 100)

# calculate probability density
mu <- mean(df_h0$height)
sigma <- sd(df_h0$height)
pd <- dnorm(x, mean = mu, sd = sigma)

# figure
tibble(y = pd, x = x) %>% # data frame
  ggplot(aes(x = x, y = y)) +
  geom_line() + # draw lines
  labs(y = "Probability density") # re-label
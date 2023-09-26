#' ---
#' title: "Report week7"
#' output: html_document
#' date: "2023-9-26"
#' author: Zachary Bunch
#' ---

#+ message = F, warning

library(tidyverse) # call add-in packages everytime you open new R session
df_fl <- read_csv(here::here("data_raw/data_fish_length.csv"))
print(df_fl)

# unique returns unique values as a vector
unique(df_fl$lake)

# distinct returns unique values as a tibble
distinct(df_fl, lake)

# group mean and sd
df_fl_mu <- df_fl %>% 
  group_by(lake) %>% # group operation
  summarize(mu_l = mean(length), # summarize by mean()
            sd_l = sd(length)) # summarize with sd()

# plot
# geom_jitter() plot data points with scatter
# geom_segment() draw lines
# geom_point() draw points
df_fl %>% 
  ggplot(aes(x = lake,
             y = length)) +
  geom_jitter(width = 0.1, # scatter width
              height = 0, # scatter height (no scatter with zero)
              alpha = 0.25) + # transparency of data points
  geom_segment(data = df_fl_mu, # switch data frame
               aes(x = lake,
                   xend = lake,
                   y = mu_l - sd_l,
                   yend = mu_l + sd_l)) +
  geom_point(data = df_fl_mu, # switch data frame
             aes(x = lake,
                 y = mu_l),
             size = 3) +
  labs(x = "Lake", # x label
       y = "Fish body length") # y label

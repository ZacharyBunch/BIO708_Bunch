#' ---
#' title: "Report week8"
#' output: html_document
#' date: "2023-10-5"
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
  geom_jitter(width = 0.2, height = 0, alpha = 0.5)

# anova in R

fit <- aov(formula = length ~ lake, data = df_anova)

summary(fit)


# behind anova ------------------------------------------------------------

#whole
mu <- mean(df_anova$length)


#group level information 
x_a <- df_anova %>%
  filter(lake == "a") %>% 
  pull(length)

x_b <- df_anova %>% 
  filter(lake == 'b') %>% 
  pull(length)

x_c <- df_anova %>%  
  filter(lake == 'c') %>% 
  pull(length)

mu_a <- mean(x_a)
mu_b <- mean(x_b)
mu_c <- mean(x_c)

#variablility

ss_ga <- (mu_a -mu)^2 * length(x_a)
ss_gb <- (mu_b -mu)^2 * length(x_b)
ss_gc <- (mu_c -mu)^2 * length(x_c)

ss_g <- ss_ga + ss_gb + ss_gc


# estimate overall mean
mu <- mean(df_anova$length)

# estimate group means and sample size each
df_g <- df_anova %>% 
  group_by(lake) %>% 
  summarize(mu_g = mean(length), # mean for each group
            dev_g = (mu_g - mu)^2, # squared deviation for each group
            n = n()) # sample size for each group

print(df_g)


ss_g <- df_g %>% 
  mutate(ss = dev_g * n) %>% 
  pull(ss) %>% 
  sum()

# within-group variability 

dev_ia <- (x_a - mu_a)^2

dev_ib <- (x_b - mu_b)^2

dev_ic <- (x_c - mu_c)^2

ss_w <- sum(dev_ia) + sum(dev_ib) + sum(dev_ic)


df_i <- df_anova %>% 
  group_by(lake) %>% 
  mutate(mu_g = mean(length)) %>% # use mutate() to retain individual rows
  ungroup() %>% 
  mutate(dev_i = (length - mu_g)^2) # deviation from group mean for each fish


# convert variability into sd

var_g <- ss_g / (3 - 1)
var_w <- ss_w / (150 - 3)

# get F value
f_value <- var_g / var_w


# f distribution  ---------------------------------------------------------

x <- seq(0,10, by = 0.1)
y <- df(x, df1 = 3 - 1, df2 = 150 -3)

tibble(x = x, y = y) %>% 
  ggplot(aes(x=x, y = y)) +
  geom_line() +
  geom_vline(xintercept = f_value, color = "salmon", linetype = "dashed")

p_value <- 1 - pf(f_value, df1 = 3 - 1, df2 = 150 -3)

p_value


# Lab 5.1

#1

PlantGrowth %>% 
  ggplot(aes(x = group,
             y = weight)) +
  geom_violin(draw_quantiles = 0.5, # draw median horizontal line
              alpha = 0.2) + # transparency
  geom_jitter(alpha = 0.2) # transparency


# group mean and sd
plant_fl_mu <- PlantGrowth %>% 
  group_by(group) %>% # group operation
  summarize(mu_l = mean(weight), # summarize by mean()
            sd_l = sd(weight)) # summarize with sd()

PlantGrowth %>% 
  ggplot(aes(x = group,
             y = weight)) +
  geom_jitter(width = 0.1, # scatter width
              height = 0, # scatter height (no scatter with zero)
              alpha = 0.25) + # transparency of data points
  geom_segment(data = plant_fl_mu, # switch data frame
               aes(x = group,
                   xend = group,
                   y = mu_l - sd_l,
                   yend = mu_l + sd_l)) +
  geom_point(data = plant_fl_mu, # switch data frame
             aes(x = group,
                 y = mu_l),
             size = 3) +
  labs(x = "Group", # x label
       y = "Weight") # y label


#2

aov_plant <- aov(formula = weight ~ group, data = PlantGrowth)

summary(aov_plant)

#3 

#P-Value, F-Value, degrees of freedom
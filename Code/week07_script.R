#' ---
#' title: "Report week7"
#' output: html_document
#' date: "2023-9-28"
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

# pull mu_l from tibble as vector
v_mu <- df_fl_mu %>% 
  pull(mu_l)

# lake a
print(v_mu[1])
# lake b
print(v_mu[2])

# difference
v_mu[1] - v_mu[2]

# group mean, variance, and sample size
df_t <- df_fl %>% 
  group_by(lake) %>% # group operation
  summarize(mu_l = mean(length), # summarize by mean()
            var_l = var(length), # summarize with sd()
            n = n()) # count number of rows per group

print(df_t)


# pull values as a vector
v_mu <- pull(df_t, mu_l)
v_var <- pull(df_t, var_l)
v_n <- pull(df_t, n)

var_p <- ((v_n[1] - 1)/(sum(v_n) - 2)) * v_var[1] +
  ((v_n[2] - 1)/(sum(v_n) - 2)) * v_var[2]

t_value <- (v_mu[1] - v_mu[2]) / sqrt(var_p * ((1 / v_n[1]) + (1 / v_n[2])))

print(t_value)

# produce 500 values from -5 to 5 with equal interval
x <- seq(-5, 5, length = 500)

# probability density of t-statistics with df = sum(v_n) - 2
y <- dt(x, df = sum(v_n) - 2)

# draw figure
tibble(x, y) %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line() +
  labs(y = "Probability density",
       x = "t-statistic")

# draw entire range
tibble(x, y) %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line() +
  geom_vline(xintercept = t_value,
             color = "salmon") + # t_value is the observed t_value
  geom_vline(xintercept = abs(t_value),
             color = "salmon") + # t_value is the observed t_value
  labs(y = "Probability density",
       x = "t-statistic") 

# calculate area under the curve from -infinity to t_value
pr_below <- pt(q = t_value, df = sum(v_n) - 2)

# calculate area under the curve from abs(t_value) to infinity
pr_above <- 1 - pt(q = abs(t_value), df = sum(v_n) - 2)


# p_value
p_value <- pr_below + pr_above
print(p_value)

x <- df_fl %>%
  filter(lake == "a") %>%  # subset lake a
  pull(length)

y <- df_fl %>%
  filter(lake == "b") %>% # subset lake b
  pull(length)

t.test(x, y, var.equal = TRUE)

t.test(x, y, var.equal = FALSE)



# 4.4 Laboratory 

xs <- rnorm(10,5,10)
ys <- rnorm(12,5,10)
x1 <- rnorm(10,5,100)
y1 <- rnorm(12,5,100)


t.test(xs, ys)
t.test(x1,y1)


# 4.4.2 

a1 <- c(13.9, 14.9 ,13.4, 14.3, 11.8, 13.9, 14.5, 15.1, 13.3, 13.9)
a2 <- c(17.4, 17.3, 20.1, 17.2, 18.4, 19.6, 16.8, 18.7, 17.8, 18.9)

b1 <- c(10.9, 20.3, 9.6, 8.3, 14.5, 12.3, 14.5, 16.7, 9.3, 22.0)
b2 <- c(26.9, 12.9, 11.1, 16.7, 20.0, 20.9, 16.6, 15.4, 16.2, 16.2)

a1 <- as.data.frame(a1)
a2 <- as.data.frame(a2)
b1 <- as.data.frame(b1)
b2 <- as.data.frame(b2)

a1_long <- pivot_longer(a1,a1)
a2_long <- pivot_longer(a2,a2)
b1_long <- pivot_longer(b1,b1)
b2_long <- pivot_longer(b2,b2)

# Estimate sample means and SDs for each vector. To

x <- tibble(group = c(a1_long$name, a2_long$name,b1_long$name, b2_long$name), value = c(a1_long$value, a2_long$value, b1_long$value, b2_long$value)) %>% 
  group_by(group) %>% 
  summarize(mean = mean(value), sd = sd(value))

#Recreating for t.test

x2 <- tibble(group = c(a1_long$name, a2_long$name,b1_long$name, b2_long$name), value = c(a1_long$value, a2_long$value, b1_long$value, b2_long$value))

#Sectioned group into own variable

a1_test <- x2 %>% group_by(group) %>%   
  filter(group == c("a1"))

a2_test <- x2 %>% group_by(group) %>%   
  filter(group == c("a2"))

b1_test <- x2 %>% group_by(group) %>%   
  filter(group == c("b1"))

b2_test <- x2 %>% group_by(group) %>%   
  filter(group == c("b2"))

#Running the t.tests

t.test(a1_test$value, a2_test$value)

t.test(b1_test$value, b2_test$value)


#4.4.3: Challenge 

t_a1_a2 <- ((x[1,2] - x[2,2]) / sqrt(((x[1,3]^2)/(length(a1_test$value)))+((x[2,3]^2)/(length(a2_test$value)))))

t_a1_a2

t_a1_a2 <- as.numeric(t_a1_a2)

df <- (((x[1,3]^2)/(length(a1_test$value)))+((x[2,3]^2)/(length(a2_test$value))))^2 / ((((((x[1,3]^2)/(length(a1_test$value)))^2))/(length(a1_test$value)-1)) + (((((x[2,3]^2)/(length(a2_test$value)))^2))/(length(a2_test$value)-1)))

df                                    

df <- as.numeric(df)

p_value <- (1-pt(abs(t_a1_a2), df = df)) * 2                                     

p_value

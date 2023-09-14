#' ---
#' title: "Report week4"
#' output: html_document
#' date: "2023-9-23"
#' author: Zachary Bunch
#' ---


# in class ----------------------------------------------------------------
library(tidyverse)

#plant data frame

h <- c(16.9, 20.9, 15.8, 28, 21.6, 15.9, 22.4, 23.7, 22.9, 18.5)

df_h1 <- tibble(plant_id = 1:10, # a vector from 1 to 10 by 1
                height = h, # height
                unit = "cm") # unit
# nrow() returns the number of rows
# while piping, "." refers to the dataframe inherited 
# i.e., nrow(.) counts the number of rows in df_h1
df_h1 <- df_h1 %>% 
  mutate(mu_height = mean(height),
         var_height = sum((height - mu_height)^2) / nrow(.))

print(df_h1)

#another set

h <- c(27.6, 21.9, 16.9, 8.9, 25.6, 19.8, 19.9, 24.7, 24.1, 23)

df_h2 <- tibble(plant_id = 11:20, # a vector from 11 to 20 by 1
                height = h,
                unit = "cm") %>% 
  mutate(mu_height = mean(height),
         var_height = sum((height - mu_height)^2) / nrow(.))

print(df_h2)

#1000 plants
# load csv data on R
df_h0 <- read_csv("data_raw/data_plant_height.csv")

# show the first 10 rows
print(df_h0)

# Summary statistics

mu <- mean(df_h0$height)
sigma2 <- sum((df_h0$height - mu)^2) / nrow(df_h0)

print(mu)

#subsample with sample_n()

df_i <- df_h0 %>% 
  sample_n(size = 10) # size specifies the number of rows to be selected randomly

print(df_i)



# for reproducibility
set.seed(3)

mu_i <- var_i <- NULL # create empty objects

# repeat the work in {} from i = 1 to i = 100
for (i in 1:100) {
  
  df_i <- df_h0 %>% 
    sample_n(size = 10) # random samples of 10 individuals
  
  # save mean for sample set i
  mu_i[i] <- mean(df_i$height)
  
  # save variance for sample set i
  var_i[i] <- sum((df_i$height - mean(df_i$height))^2) / nrow(df_i) 
  
}

print(mu_i)
print(var_i)

#install.packages("patchwork") # install only once
library(patchwork)

df_sample <- tibble(mu_hat = mu_i, var_hat = var_i)

# histogram for mean
g_mu <- df_sample %>% 
  ggplot(aes(x = mu_hat)) +
  geom_histogram() +
  geom_vline(xintercept = mu)

# histogram for variance
g_var <- df_sample %>% 
  ggplot(aes(x = var_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2)

# layout vertically
# possible only if "patchwork" is loaded
g_mu / g_var

#unbiased variance

# for reproducibility
set.seed(3)

# redo simulations ----
mu_i <- var_i <- var_ub_i <- NULL # create empty objects

# repeat the work in {} from i = 1 to i = 100
for (i in 1:100) {
  
  df_i <- df_h0 %>% 
    sample_n(size = 10) # random samples of 10 individuals
  
  # save mean for sample set i
  mu_i[i] <- mean(df_i$height)
  
  # save variance for sample set i
  var_i[i] <- sum((df_i$height - mean(df_i$height))^2) / nrow(df_i) 
  
  var_ub_i[i] <- var(df_i$height)
}


# draw histograms ----
df_sample <- tibble(mu_hat = mu_i,
                    var_hat = var_i,
                    var_ub_hat = var_ub_i)

# histogram for mu
g_mu <- df_sample %>% 
  ggplot(aes(x = mu_hat)) +
  geom_histogram() +
  geom_vline(xintercept = mu)

# histogram for variance
# scale_x_continuous() adjusts scale in x-axis
g_var <- df_sample %>% 
  ggplot(aes(x = var_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits= c(min(c(var_i, var_ub_i)),
                               max(c(var_i, var_ub_i))))

# histogram for unbiased variance
g_var_ub <- df_sample %>% 
  ggplot(aes(x = var_ub_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits= c(min(c(var_i, var_ub_i)),
                               max(c(var_i, var_ub_i))))

g_mu / g_var / g_var_ub


# 2.3 Lab -----------------------------------------------------------------

# We used 10 plants to estimate sample means and variances. Obtain 100 sub-datasets with 50 and 100 measures each, and draw histograms of sample means and unbiased variances (use var()).

set.seed(3)

mu_a <- var_a <- var_ub_a <- NULL # create empty objects

for (x in 1:100) {
  
  df_a <- df_h0 %>% 
    sample_n(size = 50) # random samples of 10 individuals
  
  # save mean for sample set i
  mu_a[x] <- mean(df_a$height)
  
  # save variance for sample set i
  var_a[x] <- sum((df_a$height - mean(df_a$height))^2) / nrow(df_a) 
  
  var_ub_a[x] <- var(df_a$height)
}

set.seed(3)

mu_b <- var_b <- var_ub_b <- NULL # create empty objects

for (x in 1:100) {
  
  df_b <- df_h0 %>% 
    sample_n(size = 100) # random samples of 10 individuals
  
  # save mean for sample set i
  mu_b[x] <- mean(df_b$height)
  
  # save variance for sample set i
  var_b[x] <- sum((df_b$height - mean(df_b$height))^2) / nrow(df_b) 
  
  var_ub_b[x] <- var(df_b$height)
}

#First histrogram set

df_sample_a <- tibble(mu_hat_a = mu_a,
                    var_hat_a = var_a,
                    var_ub_hat_a = var_ub_a)

g_mu_a <- df_sample_a %>% 
  ggplot(aes(x = mu_hat_a)) +
  geom_histogram() +
  geom_vline(xintercept = mu)


# histogram for unbiased variance
g_var_ub_a <- df_sample_a %>% 
  ggplot(aes(x = var_ub_hat_a)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits= c(min(c(var_a, var_ub_a)),
                               max(c(var_a, var_ub_a))))
g_mu_a / g_var_ub_a


#Second Histogram set

df_sample_b <- tibble(mu_hat_b = mu_b,
                      var_hat_b = var_b,
                      var_ub_hat_b = var_ub_b)

g_mu_b <- df_sample_b %>% 
  ggplot(aes(x = mu_hat_b)) +
  geom_histogram() +
  geom_vline(xintercept = mu)



# histogram for unbiased variance
g_var_ub_b <- df_sample_b %>% 
  ggplot(aes(x = var_ub_hat_b)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits= c(min(c(var_b, var_ub_b)),
                               max(c(var_b, var_ub_b))))
g_mu_b / g_var_ub_b

# Sample means and unbiased variances are unbiased if samples are randomly selected. What happens if samples are non-random? Suppose the investigator was unable to find plants less than 10 cm in height - the following code excludes those less than 10 cm in height:
df_h10 <- df_h0 %>% 
  filter(height >= 10)

set.seed(3)

mu_a1 <- var_a1 <- var_ub_a1 <- NULL # create empty objects

for (x in 1:100) {
  
  df_a1 <- df_h10 %>% 
    sample_n(size = 50) # random samples of 10 individuals
  
  # save mean for sample set i
  mu_a1[x] <- mean(df_a1$height)
  
  # save variance for sample set i
  var_a1[x] <- sum((df_a1$height - mean(df_a1$height))^2) / nrow(df_a1) 
  
  var_ub_a1[x] <- var(df_a1$height)
}

set.seed(3)

mu_b1 <- var_b1 <- var_ub_b1 <- NULL # create empty objects

for (x in 1:100) {
  
  df_b1 <- df_h10 %>% 
    sample_n(size = 100) # random samples of 10 individuals
  
  # save mean for sample set i
  mu_b1[x] <- mean(df_b1$height)
  
  # save variance for sample set i
  var_b1[x] <- sum((df_b1$height - mean(df_b1$height))^2) / nrow(df_b1) 
  
  var_ub_b1[x] <- var(df_b1$height)
}

#First histrogram set

df_sample_a1 <- tibble(mu_hat_a1 = mu_a1,
                      var_hat_a1 = var_a1,
                      var_ub_hat_a1 = var_ub_a1)

g_mu_a1 <- df_sample_a1 %>% 
  ggplot(aes(x = mu_hat_a1)) +
  geom_histogram() +
  geom_vline(xintercept = mu)


# histogram for unbiased variance
g_var_ub_a1 <- df_sample_a1 %>% 
  ggplot(aes(x = var_ub_hat_a1)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits= c(min(c(var_a1, var_ub_a1)),
                               max(c(var_a1, var_ub_a1))))
g_mu_a1 / g_var_ub_a1


#Second Histogram set

df_sample_b1 <- tibble(mu_hat_b1 = mu_b1,
                      var_hat_b1 = var_b1,
                      var_ub_hat_b1 = var_ub_b1)

g_mu_b1 <- df_sample_b1 %>% 
  ggplot(aes(x = mu_hat_b1)) +
  geom_histogram() +
  geom_vline(xintercept = mu)



# histogram for unbiased variance
g_var_ub_b1 <- df_sample_b1 %>% 
  ggplot(aes(x = var_ub_hat_b1)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits= c(min(c(var_b1, var_ub_b1)),
                               max(c(var_b1, var_ub_b1))))
g_mu_b1 / g_var_ub_b1


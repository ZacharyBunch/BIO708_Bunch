#' ---
#' title: "Report week11"
#' output: html_document
#' date: "2023-10-12"
#' author: Zachary Bunch
#' ---


# in class ----------------------------------------------------------------

library(tidyverse)

df_count <- read_csv("data_raw/data_garden_count.csv")
print(df_count)

#try linear model

m_normal <- lm(count ~ nitrate,
               df_count)

summary(m_normal)


## draw line

a <- coef(m_normal)[1] # intercept
b <- coef(m_normal)[2] # slope

df_count %>%  ggplot(aes(y = count, x= nitrate)) + 
  geom_point() +
  geom_abline(intercept = a, slope = b)


# extract estimates
alpha <- coef(m_normal)[1] # intercept
beta <- coef(m_normal)[2] # slope

df_count %>% 
  ggplot(aes(x = nitrate,
             y = count)) +
  geom_point() +
  geom_abline(intercept = alpha,
              slope = beta)

## try GLM

m_pois <- glm(count ~ nitrate, 
    data = df_count,
    family = "poisson")


summary(m_pois)

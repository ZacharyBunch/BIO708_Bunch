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


theta <- coef(m_pois)
se <- sqrt(diag(vcov(m_pois)))
z_value <- theta/se


# glm prediction ----------------------------------------------------------

df_pred <- tibble(nitrate = seq(min(df_count$nitrate), max(df_count$nitrate), length = 100)) %>% 
   mutate(y_pois = predict(m_pois, newdata= .) %>% exp(), y_norm = predict(m_normal, newdata = .))


df_count %>% 
  ggplot(aes(y = count, x = nitrate)) +
  geom_point() +
  geom_line(data = df_pred, aes(y = y_norm, x = nitrate)) +
  geom_line(data = df_pred, aes(y = y_pois, x = nitrate))


# in class: binomial ----------------

library(tidyverse)

df_mussel <- read_csv(here::here("data_raw/data_mussel.csv")) %>% 
  mutate(prop_fert = n_fertilized/n_examined)

df_mussel %>%   
  ggplot(aes(x=density, y= prop_fert)) +
  geom_point()

# see how logit function works
# x: produce 100 numbers from -100 to 100 (assume logit scale)
# y: convert with inverse-logit transformation (ordinary scale)
df_test <- tibble(logit_p = seq(-10, 10, length = 100),
                  x = exp(logit_p) / (1 + exp(logit_p)))

df_test %>% 
  ggplot(aes(x = logit_p,
             y = x)) +
  geom_point() +
  geom_line() +
  labs(y = "p",
       x = "log(P / 1 - P)")


## use binomial glm

m_binom <- glm(cbind(n_fertilized, n_examined - n_fertilized) ~ density,
               data = df_mussel,
               family = "binomial")

summary(m_binom)


# Prediction 

df_pred <- tibble(density = seq(min(df_mussel$density),
                                max(df_mussel$density),
                                length = 100)) %>% 
  mutate(logit_y_hat = predict(m_binom, newdata= .), 
         y_hat = exp(logit_y_hat) / (1 + exp(logit_y_hat)), 
         y_hat0 = boot::inv.logit(logit_y_hat))

## plot fertilization prop data vs. density
## overlay the predicted values from the model

df_mussel %>%  ggplot(aes(x = density, y = prop_fert)) +
  geom_point() +
  geom_line(data = df_pred, aes(y = y_hat))

#' ---
#' title: "Report week9"
#' output: html_document
#' date: "2023-10-12"
#' author: Zachary Bunch
#' ---

library(tidyverse)

# in class ----------------------------------------------------------------


df_algae <- read_csv(here::here("data_raw/data_algae.csv"))

df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point()


# fit linear regression to algae biomass

# lm() takes a formula as the first argument
# don't forget to supply your data
m <- lm(biomass ~ conductivity,
        data = df_algae)

summary(m)


# extract parameters

# coef() extracts estimated coefficients
# e.g., coef(m)[1] is (Intercept)

alpha <- coef(m)[1]
beta <- coef(m)[2]

df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point() +
  geom_abline(intercept = alpha,
              slope = beta) # draw the line

# get residuals

y_hat <- alpha + beta * df_algae$conductivity
epsilon <- df_algae$biomass - y_hat

df_algae <- df_algae %>% 
  mutate(y_hat = alpha + beta * conductivity, epsilon = biomass - y_hat)

sd(epsilon)


# se estimate

summary(m)

# extract coefficients
theta <- coef(m)

# extract standard errors
se <- sqrt(diag(vcov(m)))

# t-value
t_value <- theta / se
print(t_value)

# for intercept
# (1 - pt(t_value[1], df = 48)) calculates pr(t > t_value[1])
# pt(-t_value[1], df = 48) calculates pr(t < -t_value[1])
p_alpha <- (1 - pt(t_value[1], df = 48)) + pt(-t_value[1], df = 48)

# for slope
p_beta <- (1 - pt(t_value[2], df = 48)) + pt(-t_value[2], df = 48)

print(p_alpha)

# residual

eps <- resid(m)


# add error column
df_algae <- df_algae %>% 
  mutate(eps = eps)

# visualization of errors
df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point() +
  geom_abline(intercept = alpha,
              slope = beta) + 
  geom_segment(aes(x = conductivity, # start-coord x
                   xend = conductivity, # end-coord x
                   y = biomass, # start-coord y
                   yend = biomass - eps), # end-coord y
               linetype = "dashed")


# Lab  --------------------------------------------------------------------

library(tidyverse)

head(iris)

setosa <- iris %>% filter(Species == "setosa")
versicolor <- iris %>% filter(Species == "versicolor")
virginica <- iris %>% filter(Species == "virginica")

#Petal.Width

setosa_reg <- lm(Sepal.Width ~ Petal.Width,
        data = setosa)
summary(setosa_reg)

versicolor_reg <- lm(Sepal.Width ~ Petal.Width,
                 data = versicolor)
summary(versicolor_reg)

virginica_reg <- lm(Sepal.Width ~ Petal.Width,
                     data = virginica)
summary(virginica_reg)

#Petal.Length

setosa_reg2 <- lm(Sepal.Width ~ Petal.Length + Petal.Width,
                 data = setosa)
summary(setosa_reg2)

versicolor_reg2 <- lm(Sepal.Width ~ Petal.Length + Petal.Width,
                     data = versicolor)
summary(versicolor_reg2)

virginica_reg2 <- lm(Sepal.Width ~ Petal.Length + Petal.Width,
                    data = virginica)
summary(virginica_reg2)



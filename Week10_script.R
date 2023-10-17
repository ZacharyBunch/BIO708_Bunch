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

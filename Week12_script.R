#' ---
#' title: "Report week12"
#' output: html_document
#' date: "2023-11-7"
#' author: Zachary Bunch
#' ---

#+ message = F, warning = F

library(tidyverse)

# calculate the probability of y = 3
# with lambda = 3.5

pr <- dpois(3, lambda =3.5)

# write the formula to confirm
(p <- (3.5^3 * exp(-3.5)) / factorial(3))

# possible lambda values
lambda <- seq(0, 10, by = 0.01)

df_pr <- tibble(y = 3, lambda = lambda, pr = dpois(3, lambda = lambda))

dpois(3, lambda = lambda)

#draw figure, y = pr, x = lambda
# ggplot()

ggplot(data = df_pr, aes(y = pr, x = lambda)) + 
  geom_line() +
  geom_point()

# find best lambda

df_pr %>%  
  arrange(desc(pr))

# calculate probability of observing y = c(3,2, 5)

pr <- prod(dpois(c(3,2,5), lambda = 3))
prod(pr)


#find the best lambda y = c(3,2, 5)


likieli <- NULL
for (i in 1:length(lambda)) {
  pr0 <- dpois(c(3,2,5), lambda = lambda[i])
  likieli[i] <- prod(pr0)                                      
                                          
                                          }

## Figure

df_pr1 <- tibble(lambda = lambda, pr = likieli)
df_pr1 %>% 
  ggplot(aes(x = lambda, y = pr)) +
  geom_line()

## find the best vale of lambda using df_pr1

df_pr1 %>% 
  arrange(desc(pr)) %>% 
  slice(1) %>% #retrieve row 1
  pull(lambda) # pull out lambda column as vector/ scaler


mean(c(3, 2, 5))


# Lab

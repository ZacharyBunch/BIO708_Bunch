#' ---
#' title: "Report week4"
#' output: html_document
#' date: "2023-9-23"
#' author: Zachary Bunch
#' ---

library(tidyverse)

# construct vectors x and y
x <- c(15.9, 15.1, 21.9, 13.3, 24.4)
y <- c(15.9, 15.1, 21.9, 53.3, 24.4)

x

y

# arithmetic mean
# for vector x
n_x <- length(x) # the number of elements in x = the number of data points
sum_x <- sum(x) # summation for x
mu_x <- sum_x / n_x # arithmetic mean
print(mu_x) # print calculated value


# for vector y; we can calculate directly too
mu_y <- sum(y) / length(y)
print(mu_y) # print calculated value

#mean()
print(mean(x))
print(mean(y))

#Geometric mean ####

# for vector x
prod_x <- prod(x) # product of vector x; x1 * x2 * x3...
n_x <- length(x)
mug_x <- prod_x^(1 / n_x) # ^ means power
print(mug_x)

# for vector y
mug_y <- prod(y)^(1 / length(y))
print(mug_y)

#Median ####

# for vector x
x <- sort(x) # sort x from small to large
index <- (length(x) + 1) / 2 # (N + 1)/2 th index as length(x) is an odd number
med_x <- x[index]
print(med_x)

# for vector y
y <- sort(y) # sort y from small to large
med_y <- y[(length(y) + 1) / 2]
print(med_y)


print(median(x))

print(median(y))


# variation  ------------------------------------------------------------

# for x
sqd_x <- (x - mean(x))^2 # squared deviance
sum_sqd_x <- sum(sqd_x)
var_x <- sum_sqd_x / length(x)
print(var_x)

# for y
var_y <- sum((y - mean(y))^2) / length(y)
print(var_y)


# Standard deviation  -----------------------------------------------------

# for x
sd_x <- sqrt(var_x) # sqrt(): square root
print(sd_x)

# for y
sd_y <- sqrt(var_y)
print(sd_y)

# coeffient of variation 

# for x
cv_x <- sd_x / mean(x)
print(cv_x)

# for y
cv_y <- sd_y / mean(y)
print(cv_y)

# IQR

# for x
x_l <- quantile(x, 0.25) # quantile(): return quantile values, 25 percentile
x_h <- quantile(x, 0.75) # quantile(): return quantile values, 75 percentile
iqr_x <- abs(x_l - x_h) # abs(): absolute value
print(iqr_x)


# for y
y_q <- quantile(y, c(0.25, 0.75)) # return as a vector
iqr_y <- abs(y_q[1] - y_q[2]) # y_q[1] = 25 percentile; y_q[2] = 75 percentile
print(iqr_y)

# MAD

# for x
ad_x <- abs(x - median(x))
mad_x <- median(ad_x)
print(mad_x)

# for y
mad_y <- median(abs(y - median(y)))
print(mad_y)

# MAD/ MEDIAN

# for x
mm_x <- mad_x / median(x)
print(mm_x)

# for y
mm_y <- mad_y / median(y)
print(mm_y)


# 1.3 Lab -----------------------------------------------------------------

# Step 1: Create a vector z with length 1000
z <- exp(rnorm(n = 1000, mean = 0, sd = 0.1))

arithmetic_mean <- mean(z)
geometric_mean <- exp(mean(log(z)))  # Geometric mean calculation
median_value <- median(z)


# Step 2 + 3: Draw a histogram of z using ggplot2 
# Draw vertical lines of arithmetic mean, geometric mean, and median on the histogram with different colors using a function geom_vline() 
library(tibble)
library(ggplot2)

data <- tibble(z = z)

ggplot(data, aes(x = z)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
  geom_vline(xintercept = arithmetic_mean, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = geometric_mean, color = "green", linetype = "dashed", size = 1) +
  geom_vline(xintercept = median_value, color = "purple", linetype = "dashed", size = 1) +
  labs(title = "Histogram of z",
       x = "Value",
       y = "Frequency")


# Step 4: Compare the values of the central tendency measures.
# Just a table?


summary_table <- data.frame(
  Measure = c("Arithmetic Mean", "Geometric Mean", "Median"),
  Value = c(arithmetic_mean, geometric_mean, median_value)
)

summary_table


# Step 5: Create a new vector z_rev and repeat steps 1-4
z_rev <- -z + max(z) + 0.1


arithmetic_mean <- mean(z_rev)
geometric_mean <- exp(mean(log(z_rev)))  # Geometric mean calculation
median_value <- median(z_rev)

#REPEAT
# Step 2 + 3: Draw a histogram of z_rev using ggplot2 
# Draw vertical lines of arithmetic mean, geometric mean, and median on the histogram with different colors using a function geom_vline() 
library(tibble)
library(ggplot2)

data <- tibble(z_rev = z_rev)

ggplot(data, aes(x = z_rev)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
  geom_vline(xintercept = arithmetic_mean, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = geometric_mean, color = "green", linetype = "dashed", size = 1) +
  geom_vline(xintercept = median_value, color = "purple", linetype = "dashed", size = 1) +
  labs(title = "Histogram of z_rev",
       x = "Value",
       y = "Frequency")

#REPEAT
# Step 4: Compare the values of the central tendency measures.
# Just a table?


summary_table <- data.frame(
  Measure = c("Arithmetic Mean", "Geometric Mean", "Median"),
  Value = c(arithmetic_mean, geometric_mean, median_value)
)

summary_table



#### 1.3.2 ####


w <- rnorm(100, mean = 10, sd = 1)
head(w) # show first 10 elements in w



# Step 1: Convert the unit to milligrams and create a new vector m

m <- w * 1000  # 1 gram = 1000 milligrams

# Step 2: Calculate SD and MAD for w and m.

sd_w <- sd(w)
mad_w <- mad(w)
sd_m <- sd(m)
mad_m <- mad(m)

# Step 3: Calculate CV and MAD/Median for w and m.

cv_w <- sd_w / mean(w)
mad_median_w <- mad(w) / median(w)
cv_m <- sd_m / mean(m)
mad_median_m <- mad(m) / median(m)

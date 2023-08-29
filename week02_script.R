#' ---
#' title: "Report week2"
#' output: html_document
#' date: "2023-8-22"
#' author: Zachary Bunch
#' ---

# vector -----------------------------------------------------------------------

#ex.1a manually create a vector using c()
x <- c(1,3,4,8)
x

#ex.1b character
x <- c("a", "b", "c")
x


#ex.1c logical
x <- c(TRUE, FALSE, FALSE)
x


#ex.2 sequence of numbers
x <- 1:5
x

#ex.3a replicate same numbers or characters
x <- rep(2, times = 5) # replicate 2 five times
x

#ex.3b replicate same numbers or characters
x <- rep("a", 5) # replicate "a" five times
x


#ex.4a use seq() function
x <- seq(1, 5, by = 1)
x


#ex.4b use seq() function
x <- seq(1, 5, by = 0.1)
x

# check features ---------------------------------------------------------------

x <- c(1.2, 3.1, 4.0, 8.2)
x

class(x)
typeof(x)
length(x)
sum(x)
mean(x)

y <- c("a", "b", "c")
class(y)
length(y)
# acesss -----------------------------------------------------------------------

x <- c(2,2,3,2,5)
x[2] # access element #2


x[c(2,4)] # access elements #2 and 4


x[2:4] # access elements #2-4


# equation ---------------------------------------------------------------------

# creating a vector
x <- c(2,2,3,2,5)

# ex.1a equal
x == 2


# ex.1b larger than
x > 2 

# ex.2a equal
x[x == 2]

# ex.2b larger than
x[x > 2]

# ex.3a equal
which(x == 2) # returns which elements are equal to 2


# ex.3b larger than
which(x > 2)


# matrix ------------------------------------------------------------------

#ctrl-shift-r = new section

#ex.1 cbind: combine objects by column
x <- cbind(c(1,2,3), c(4,5,6))
x

#ex.2 rbind: combine objects by row
x <- rbind(c(1,2,3), c(4,5,6))
x

#ex.3 matrix: specify elements and the number of rows (nrow) and columns (ncol)
x <- matrix(1:9, nrow = 3, ncol = 3)
x


# check features ----------------------------------------------------------

x <- matrix(1:9, nrow = 3, ncol = 3)
x

class(x)
typeof(x)
dim(x)

y <- matrix(c("a","b", "c", "d", "e", "f"), nrow = 3, ncol = 2)
y



# acesss ------------------------------------------------------------------

x <- matrix(1:9, nrow = 3, ncol = 3)
x

x[2,3] # access an element in row #2 and colum #3

x[2,] # access elements in row #2

x[c(2,3),] # access elements in rows #2 and 3

x[,c(2,3)] # access elements in columns #2 and 3

x == 2 # equal

x > 2 # larger than

x[x == 2] # equal

x[x > 2] # larger than

which(x == 2, arr.ind = TRUE)

which(x > 2, arr.ind = TRUE)


# data frame --------------------------------------------------------------

# Create data frame
x <- c("Pristine", "Pristine", "Disturbed", "Disturbed", "Pristine") # Lake type
y <- c(1.2, 2.2, 10.9, 50.0, 3.0) # TSS: total suspended solids (mg/L)
df0 <- data.frame(LakeType = x, TSS = y) # x is named as "LakeType" while y is named as "TSS"
df0

colnames(df0) # call column names

df0$LakeType # access LakeType

df0$TSS # access TSS

df0[,1] # access column #1
df0[1,] # access row #1
df0[c(2,4),] # access row #2 and 4
df0[c(2,4),] # access row #2 and 4



# exercise ----------------------------------------------------------------

####Vector #### 

##a) Create three numeric vectors with length 3, 6 and 20, respectively. Each vector must be created using different functions in R.


#1

vector_3 <- c(1, 2, 3)

vector_3

#2

vector_6 <- seq(1,6)

vector_6

#3

vector_20 <- rep(1:20,1)

vector_20


##b) Create three character vectors with length 3, 6 and 20, respectively. Each vector must be created using different functions in R.

#1

cvector_3 <- c("a","b","c")

cvector_3

#2 

cvector_6 <- character(6)

cvector_6

#3

cvector_20 <- rep(c("hello","its me"),10)

cvector_20

##c) Copy the following script to your R script and perform the following analysis:

set.seed(1)
x <- rnorm(100)

# Identify element IDs of x that are greater than 2.0

x_over_2 <- which(x > 2.0)

# Identify element values of x that are greater than 2.0

x_over_2_2 <- x[x > 2.0]

#### Matrix #####

## a) Create a numeric matrix with 4 rows and 4 columns. Each column must contain identical elements.

matrix <- matrix(1:4, ncol = 4, nrow = 4)

##b) Create a numeric matrix with 4 rows and 4 columns. Each row must contain identical elements.

matrix_1 <- matrix(1:4, ncol=4, nrow = 4, byrow = TRUE)

##c) Create a character matrix with 4 rows and 4 columns. Each column must contain identical elements.

matrix_2 <- matrix(c("A","B","C","D"), nrow = 4, ncol = 4)

##d) Create a character matrix with 4 rows and 4 columns. Each row must contain identical elements.

matrix_3 <- matrix(c("A","B","C","D"), nrow = 4, ncol = 4, byrow= TRUE)

##e) Copy the following script to your R script and perform the following analysis:

set.seed(1)
x <- matrix(rnorm(100), nrow = 10, ncol = 10)

#Identify element IDs of x that are greater than 2.0 (specify row and column IDs)

greater_than_2 <- which(x > 2.0, arr.ind = TRUE)

#Identify element IDs of x that are greater than 2.0 (specify row and column IDs)

values_greater_than_2 <- x[x > 2.0]

mean_of_values_greater_than_2 <- mean(values_greater_than_2)


#### Data Frame ####

#a) Create a data frame of 3 variables with 10 elements (name variables as x, y and z. x must be character while y and z must be numeric.


data_frame <- data.frame(x = rep("char", 10),y = 1:10, z = 11:20)

#b) Check the data structure (higher-level) of x, y and z

class(data_frame)

x_class <- class(data_frame$x)
y_class <- class(data_frame$y)
z_class <- class(data_frame$z)

#c) Copy the following script to your R script and perform the following analysis:

#Calculate the means of temperature and abundance for states VA and NC separately.

set.seed(1)
x <- rnorm(100, mean = 10, sd = 3)
y <- rpois(100, lambda = 10)
z <- rep(c("VA", "NC"), 50)
df0 <- data.frame(temperature = x, abundance = y, state = z)


mean_temperature_VA <- mean(df0$temperature[df0$state == "VA"])
mean_temperature_NC <- mean(df0$temperature[df0$state == "NC"])

mean_abundance_VA <- mean(df0$abundance[df0$state == "VA"])
mean_abundance_NC <- mean(df0$abundance[df0$state == "NC"])



mean_temperature_VA 
mean_temperature_NC 

mean_abundance_VA
mean_abundance_NC


# Tidyverse ---------------------------------------------------------------

library(tidyverse)

iris <- as_tibble(iris)
iris

# single match "=="
filter(iris, Species == "virginica")

# multiple match "%in%"
filter(iris, Species %in% c("virginica", "versicolor"))

# except "!="
filter(iris, Species != "virginica")

# except multiple "!(x %in% c("a", "b"))
filter(iris, !(Species %in% c("virginica", "versicolor")))

# greater than ">"
filter(iris, Sepal.Length > 5)

# equal & greater than ">="
filter(iris, Sepal.Length >= 5)

# less than "<"
filter(iris, Sepal.Length < 5)

# equal & less than "<="
filter(iris, Sepal.Length <= 5)


# Arrange -----------------------------------------------------------------

# arrange in an ascending order
arrange(iris, Sepal.Length)

# arrange in an descending order
arrange(iris, desc(Sepal.Length))


# Select ------------------------------------------------------------------

# select one column
select(iris, Sepal.Length)

# select multiple columns
select(iris, c(Sepal.Length, Sepal.Width))

# remove one column
select(iris, -Sepal.Length)

# remove multiple columns
select(iris, -c(Sepal.Length, Sepal.Width))

# select/remove multiple columns with a start rule
# starts_with("x")
select(iris, starts_with("Sepal"))
select(iris, -starts_with("Sepal"))

# select/remove multiple columns with an end rule
# ends_with("x")
select(iris, ends_with("Width"))
select(iris, -ends_with("Width"))


# Mutate ------------------------------------------------------------------

# add a new column
x <- 1:150
mutate(iris, x = x)
mutate(iris, SL2 = 2 * Sepal.Length)


# Piping ------------------------------------------------------------------

# the following codes produce the same data frame
# apply functions separately
df_vir <- filter(iris, Species == "virginica")
df_vir_sl <- select(df_vir, Sepal.Length)
print(df_vir_sl)

# piping
iris %>% 
  filter(Species == "virginica") %>% 
  select(Sepal.Length)


# group operation ---------------------------------------------------------

# grouping by "Species", then take means "Speal.Length" for each species
iris %>% 
  group_by(Species) %>% 
  summarize(mu_sl = mean(Sepal.Length))

# grouping by "Species", then take means & SD "Speal.Length" for each species
iris %>% 
  group_by(Species) %>% 
  summarize(mu_sl = mean(Sepal.Length),
            sd_sl = sd(Sepal.Length))

# grouping by "Species", then take means "Speal.Length" for each species
iris %>% 
  group_by(Species) %>% 
  mutate(mu_sl = mean(Sepal.Length)) %>% 
  ungroup()


# Left join ---------------------------------------------------------------

# matching by a single column
## left join by "Species": one to one
df1 <- tibble(Species = c("A", "B", "C"),
              x = c(1, 2, 3))

df2 <- tibble(Species = c("A", "B", "C"),
              y = c(4, 5, 6))

left_join(x = df1,
          y = df2,
          by = "Species")



# Exercise tidyverse  --------------------------------------------------------------

library(ggplot2movies)
data('movies')
library(tidyverse)


#1a
 
data <- movies

data1 <- data  %>% mutate(new_var_name = rating - mean(rating))

#1b

data2 <- data %>%  filter(year >= 2000)

#1c

data3 <- data %>% select(title, year, budget, length, rating, votes)

#1d

data4 <- data %>%  rename(length_in_min = length)

#### Tidyverse Exercise 2 ####

data5 <- data %>% 
  group_by(year) %>% 
  summarize(avg_budget = mean(budget,na.rm = TRUE)) 


#### Tidvverse Exercise 3 ####


dat = tibble(id = 1:10,
             x = rnorm(10),
             y = rnorm(10))

dat1 <- dat %>%
  pivot_longer(cols = c(x, y), names_to = "variable", values_to = "value")


#### Tidyverse Exercise 4 ####

#Now put several actions together in one set of piped operations.

#Filter movies released after 1990
#select the same variables as before but also the mpaa, Action, and Drama variables
#group by mpaa and (your choice) Action or Drama
#get the average rating
# It should spit out something like the following:


big_data <- data %>%  filter(year > 1990) %>% 
  select(title, year, budget, length, rating, votes, mpaa, Action, Drama) %>% 
  group_by(mpaa, Drama) %>% 
  summarize(avg_rating = mean(rating,na.rm = TRUE)) 


big_data




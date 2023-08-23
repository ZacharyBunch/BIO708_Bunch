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

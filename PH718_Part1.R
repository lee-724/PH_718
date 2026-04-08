# PH718 Data Management and Visualization in R
# Part 1: R Basic Syntax
# Zhiyang Zhou (zhou67@uwm.edu, zhiyanggeezhou.github.io)

# This is a R script with ".r" as the file extension, commonly used to save R code.
# However, Rstudio may handle other file formats,
# providing additional features like a script editor, console, debugging tools, and more.
# That is why RStudio is an integrated development environment (IDE).

# Words following pound signs (#) are comments, which provide annotations or explanations in your code.

#------------------------------------------------------------------------------------------
## Generating an EHR-style dataset with the given prompt at a GenAI platform
#------------------------------------------------------------------------------------------
# "Generate a realistic simulated electronic health records (EHR) dataset for 1,000 patients 
# with demographic, clinical, behavioral, and mental health variables. 
# Include a binary outcome related to substance use disorder. 
# Introduce realistic missing values in selected variables. 
# Save the dataset in CSV format."

# My generated dataset is saved as "sim_ehr_1000" on Canvas.

#------------------------------------------------------------------
## Working directory: where inputs are found and outputs are sent
#------------------------------------------------------------------
getwd() # The current working directory

# Change the current working directory using function setwd('C:/file/path')
setwd('c:/PH717') # Error due to the nonexisting path "c:/PH717"
setwd('c:/PH718')
getwd()

# ------------------------------------------------------------------
## Load simulated EHR dataset
# ------------------------------------------------------------------
# Read data (place sim_ehr_1000.csv in your working directory)
setwd('/Users/jasminelee/Desktop/MSDS/2026 Spring/PH_718')
ehr <- read.csv("sim_ehr_1000.csv", header = TRUE, stringsAsFactors = FALSE)
ehr_2 <- read.csv("sim_ehr_1000.csv", header = FALSE, stringsAsFactors = FALSE)

# First look at the data
dim(ehr) # num of rows and columns 
head(ehr) # first 6 rows but if head(ehr, 10), first 10 rows
summary(ehr)

#------------------------------------------------------------------
## How to help yourself when exploring unfamiliar data
#------------------------------------------------------------------
# Help manual of specific functions

?dim
?str
?summary
?read.csv

#------------------------------------------------------------------
## Assigning values to objects
#------------------------------------------------------------------
# The left arrow (<-) or equals sign (=) assigns the value at the right-hand side to the left-hand side.
n_patients <- nrow(ehr)
n_patients = nrow(ehr)
n_vars <- ncol(ehr) # variables

n_patients
n_vars

#------------------------------------------------------------------
## Data types: let str() or class() tell you
#------------------------------------------------------------------

str(ehr)
class(ehr$age)
class(ehr$'age') # class(ehr'age') doesn't work due to syntax error
class(ehr$sex)
class(ehr$sud_dx)

# Convert to factors where appropriate
ehr$sex <- factor(ehr$sex)
levels(ehr$sex)
ehr$sud_dx_f <- factor(ehr$sud_dx, levels = c(0,1), labels = c("No SUD","SUD"))
class(ehr$sud_dx_f)

#------------------------------------------------------------------
## Data Structures: let str() or class() tell you
#------------------------------------------------------------------
# Data frame: each column is a variable, each row is an observation
class(ehr)

ehr[1, ] # the 1st row
ehr[, 1] # the 1st column
ehr[1, 1]
ehr[1, 2]
ehr[, "age"] # the column with name == "age", means to print out all rows in the "age" column, how many prints out per line isn't imp.
ehr$age # the column with name == "age"
ehr[1:10, c("age","sex","sud_dx")]

# Vector: entries should share the SAME data type
# vector of characters
ehr[, 1]
str(ehr[, 1])
class(ehr[, 1])

# vector of numbers
ehr[, "age"]
str(ehr[, "age"])
class(ehr[, "age"])

# Matrix: entries should share the SAME data type
m1 <- ehr[, c("age","bmi","sbp")]
m1
dim(m1)
class(m1)
str(m1)

m2 <- matrix(ehr[, c("age","bmi","sbp")])
m2
dim(m2)
class(m2)
str(m2)

m3 <- as.matrix(ehr[, c("age","bmi","sbp")])
m3
dim(m3)
class(m3)
str(m3)

m1[1,3] # the (1,3)-entry
m1[1, ] # return the 1st row
m1[, 2] # return the 2nd column

m2[1,3]
m2[1, ]
m2[, 2]

m3[1,3]
m3[1, ]
m3[, 2]

#------------------------------------------------------------------
## Checking for missing values
#------------------------------------------------------------------
anyNA(ehr)
summary(ehr)
is.na(ehr$sbp)
!is.na(ehr$sbp)
ehr_complete_bmi <- ehr[!is.na(ehr$sbp), ] # remove rows with missing bmi; exclamation mark "!" means "not"
ehr_complete <- ehr[complete.cases(ehr), ] # remove rows with any missing values
sum(ehr$sbp)
sum(ehr$sbp, na.rm = TRUE) # remove missing values when computing the sum
# ehr4 <- ehr$sbp, na.rm = TRUE # doesn't work because the format is for getting the sum
ehr_complete_2 <- ehr_complete
unique(ehr_complete$alcohol_use)
ehr_complete$alcohol_use[ehr_complete$alcohol_use == ""] = NA
ehr_complete_2 <- ehr_complete[complete.cases(ehr_complete), ]

#------------------------------------------------------------------
## Exporting data
#------------------------------------------------------------------
write.csv(ehr_complete, "ehr_complete.csv", row.names = FALSE)
write.csv(ehr_complete_2, "ehr_complete_2.csv", row.names = FALSE)

#------------------------------------------------------------------
## Logic and control
#------------------------------------------------------------------
# Simple logical operations: 
# exclamation mark "!" means "not"
# ampersand '&' means 'and'
# vertical bar '|' means 'or'

ehr$age >= 65
ifelse(ehr$age >= 65, 1, 0)
ifelse(ehr$age[1] >= 65, 1, 0)
ehr$older_adult <- ifelse(ehr$age >= 65, 1, 0)
table(ehr$older_adult)

ehr$sbp >= 130
ehr$dbp >= 80
ehr$sbp >= 130 | ehr$dbp >= 80
ehr$sbp[1] >= 130
ehr$dbp[1] >= 80
ehr$sbp[1] >= 130 | ehr$dbp[1] >= 80
ehr$high_bp <- ifelse(ehr$sbp >= 130 | ehr$dbp >= 80, 1, 0)
table(ehr$high_bp)

ehr$sex <- factor(ehr$sex)
ehr$sud_dx_f <- factor(ehr$sud_dx, levels = c(0,1), labels = c("No SUD","SUD"))
ehr$sud_dx_f == "SUD"
ehr$sud_dx_f[10] == "SUD"
ehr_sud <- ehr[ehr$sud_dx_f == "SUD", ]
summary(ehr_sud$age)

ehr[ehr$age > 50 & ehr$high_bp == 1, ]
ehr[ehr$age > 50 & ehr$high_bp == 1, c("age","sbp","dbp")]
# how do we exclude a column?
ehr[ehr$age > 50 & ehr$high_bp == 1, c("age")]
ehr[ehr$age > 50 & ehr$high_bp == 1, names(ehr) != "age"] # all columns only except "age"
ehr[ehr$age > 50 & ehr$high_bp == 1, !names(ehr) %in% c("sbp","dbp")] # all columns except "sbp" and "dbp", we can exclude multiple columns

#------------------------------------------------------------------
## Defining functions
#------------------------------------------------------------------
# A simple example
flag_high_bp <- function(sbp, dbp) {
  ifelse(sbp >= 130 | dbp >= 80, 1, 0) # result will be 1 or 0
}

flag_high_bp(140, 85)

ehr$high_bp2 <- flag_high_bp(ehr$sbp, ehr$dbp)
table(ehr$high_bp2)
View(ehr$high_bp2)

flag_high_bp3 <- function(arg1, arg2) {
  ifelse(arg1 >= 130 | arg2 >= 80, 1, 0) # result will be 1 or 0
}
flag_high_bp3(arg1 = 120, arg2 = 80)
flag_high_bp3(120, 80)
flag_high_bp3(arg2 = 120, arg1 = 80)

#------------------------------------------------------------------
## Loops: for-loop
#------------------------------------------------------------------
# If you need to replicate something for multiple times, then
# a loop is readable and save your time in coding.

# Example 1: Create a simple SUD risk score using a for-loop

(-2):10
(-2.5):10
(-2.5):10.5 # the step goes by 1

risk_score <- rep(NA, nrow(ehr))  # pre-allocate space for risk_score
for (i in 1:nrow(ehr)) {
  score <- 0
  
  # Add points based on conditions (adjust variables to match your dataset)
  if (!is.na(ehr$age[i]) && ehr$age[i] < 25) score <- score + 1
  if (!is.na(ehr$phq9[i]) && ehr$phq9[i] >= 10) score <- score + 1
  if (!is.na(ehr$alcohol_use[i]) && ehr$alcohol_use[i] %in% c("Moderate", "Heavy")) score <- score + 1
  if (!is.na(ehr$smoking[i]) && ehr$smoking[i] == "Current") score <- score + 1
  
  risk_score[i] <- score
}
ehr$risk_score <- risk_score
table(ehr$risk_score)

# Example 2: Count missing values per column
na_count <- rep(NA, ncol(ehr))
names(na_count) <- colnames(ehr)

for (j in 1:ncol(ehr)) {
  na_count[j] <- sum(is.na(ehr[, j]))
}
sort(na_count, decreasing = TRUE) # return the sorted na_count in decreasing order

for (lp_idx in 1:ncol(ehr)) {
  na_count[lp_idx] <- sum(is.na(ehr[, lp_idx]))
}
sort(na_count, decreasing = TRUE)

# Example 3: Nested for-loops to build a contingency table (sex x sud_dx)
sex_levels <- sort(unique(ehr$sex))
out_levels <- sort(unique(ehr$sud_dx))

tab <- matrix(0, nrow = length(sex_levels), ncol = length(out_levels),
              dimnames = list(sex = sex_levels, sud_dx = out_levels))
View(tab)

for (r in 1:length(sex_levels)) {
  for (c in 1:length(out_levels)) {
    tab[r, c] <- sum(ehr$sex == sex_levels[r] & ehr$sud_dx == out_levels[c], na.rm = TRUE)
  }
}
tab
prop.table(tab, margin = 1)  # row proportions

# Example 4: Compute BMI category but skip rows with missing BMI using `next`
bmi_cat <- rep(NA, nrow(ehr))

for (i in 1:nrow(ehr)) {
  if (is.na(ehr$bmi[i])) next  # skip if missing
  
  if (ehr$bmi[i] < 18.5) bmi_cat[i] <- "Underweight"
  else if (ehr$bmi[i] < 25) bmi_cat[i] <- "Normal"
  else if (ehr$bmi[i] < 30) bmi_cat[i] <- "Overweight"
  else bmi_cat[i] <- "Obese"
}
ehr$bmi_cat <- factor(bmi_cat, levels = c("Underweight","Normal","Overweight","Obese"))
View(ehr$bmi_cat)
table(ehr$bmi_cat)

# Example 5: Find the first patient with very high PHQ-9 AND SUD
first_index <- NA
for (i in 1:nrow(ehr)) {
  if (!is.na(ehr$phq9[i]) && ehr$phq9[i] >= 20 && ehr$sud_dx[i] == 1) {
    first_index <- i
    break
  }
}
first_index
ehr[first_index, ]

second_index <- NA
for (i in 1:nrow(ehr)) {
  if (!is.na(ehr$phq9[i]) && ehr$phq9[i] >= 10 && ehr$sud_dx[i] == 1) {
    second_index <- i
  }
}
second_index # no break statement, so it finds the last index that meets the condition

#------------------------------------------------------------------
## Loops: while-loop
#------------------------------------------------------------------
# Find a minimum sample size where mean value of SUD stabilizes
n <- 1
prev_old <- mean(ehr$sud_dx[1:n] == 1, na.rm = TRUE)
diff <- 1
while (diff > 0.005 && n < nrow(ehr)) {
  n <- n + 1
  prev_new <- mean(ehr$sud_dx[1:n] == 1, na.rm = TRUE)
  diff <- abs(prev_new - prev_old)
  prev_old <- prev_new
}
n

n <- 1
mean_old <- mean(ehr$sud_dx[1:n], na.rm = TRUE)
diff <- 1
while (diff > 0.005 & n < nrow(ehr)) {
  n <- n + 1
  mean_new <- mean(ehr$sud_dx[1:n] == 1, na.rm = TRUE)
  diff <- abs(mean_new - mean_old)
  mean_old <- mean_new
}
n

n = 4
ehr$sud_dx[1:n]

n = 10
ehr$sud_dx[1:n]# 1/10 of proportion 

n = 10
mean(ehr$sud_dx[1:n])

# Find a minimum sample size where the proportion of SUD patients stabilizes
n <- 10
mean_old <- mean(ehr$sud_dx[1:n], na.rm = TRUE)
diff <- 1
while (diff > 0.005 & n < nrow(ehr)) {
  n <- n + 1
  mean_new <- mean(ehr$sud_dx[1:n] == 1, na.rm = TRUE)
  diff <- abs(mean_new - mean_old)
  mean_old <- mean_new
}
n

while (n > 1) {
  n <- n + 1
}

while (n > 1) {
  n <- n - 1
}

#------------------------------------------------------------------
## Vectorization outperforming loops
#------------------------------------------------------------------
# In R, many loops can be implemented faster by using vectorization.
# Simply speaking, R has been optimized for vectorization.
# Example 1
# via loop
A <- matrix(1:2e5, nrow=500, ncol=400)
B <- matrix((2e5+1):4e5, nrow=500, ncol=400)
system.time({
  matsum <- matrix(0, nrow=500, ncol=400)
  for(i in 1:500) {
    for(j in 1:400) {
      matsum[i,j] <- A[i,j] + B[i,j]
    }
  }
})
# via vectorization
system.time({
  matsum <- A + B
})

# Example 2
# via loop
system.time({
  x <- 1:1e6
  result <- numeric(length(x))
  for (i in x) {
    result[i] <- x[i] * 2
  }
})
# via vectorization
system.time(
  {
    x <- 1:1e6
    result <- x*2
  }
)

# Example 3: sum_{i=1}^{10^4} i^2
result1 = 0 
for (i in 1:10^4){
  result1 = result1 + i^2
}
result1
## The summation may be implemented by the following vectorization
result2 = sum((1:1e4)^2)
result2

#------------------------------------------------------------------
## Vectorized arithmetic
#------------------------------------------------------------------
# For an arbitrary vector, say
x <- c(0.5, 3.6, 3)

x + 3 # Addition
4 - x # Subtraction
7 * x # Multiplication
3/x  # Division
2^x  # Exponentiation
x^(-5)  # Exponentiation
x %% 3  # Modulus (remainder of division)

# For one more vector, say
y <- c(4.1, 5, 6)

x + y
x - y
x * y
x / y

# Entry-wise operations are allowed btw one scalar and one vector (or two vectors of the same length)
# Entry-wise operations are allowed btw one scalar and one matrix (or two matrices of the same dimension)

x = matrix(1:6, 2, 3)
y = matrix(3:8, 2, 3)

x + y
x - y
x * y
x / y

x %*% t(y)

# Mathematical functions
sqrt(x)  # Square root
x^(0.5)
abs(x)  # Absolute value
log(x)  # Natural logarithm 
exp(x)  # Exponentiation (with base e)
round(x, 2)  # Rounding numbers

#------------------------------------------------------------------
## Debugging
#------------------------------------------------------------------
# What is the error/warning in each of the following examples? 
# How should the code be fixed?

# Example 1
data(cars)
plot(cars[,2], cars[,3])
View(plot(cars[,1]))
View(cars)

# Example 2
new_data <- cars[which(cars$speed == 4)] # equiv. cars[c(1,2)] equiv. cars[, c(1,2)]
new_data_2 <- cars[which(cars$speed == 4), ]
plot(new_data$speed, new_data$dist)
     
# Example 3
index <- which(cars$speed < 20 & cars$dist > 100)
new_data2 <- cars[index,]
plot(new_data2$speed, new_data2$dist) # new_data2 is empty, so the plot is empty; the condition is too strict, so no rows meet the condition

# Example 4
find_column <- which(colnames(cars) == "Speed")
find_column

# Example 5
data(iris)
View(iris)
data_bind <- rbind(iris, cars)

# Example 6
install.packages("ggplot2")
library(ggplot2)
ggplot(cars) + geom_point(aes(x=speed, y=dist))
qplot(mpg, wt, data=mtcars)

# Example 7
-1:10
log(-1:10) # -Inf = neg. infinity
     
# Example 8: Debugging in user-defined functions
# Example 8a
my_fun <- function(x) {
  y <- x^2 
  # browser() # Interrupt the execution and allow the inspection of the environment
  y <- y*2 
  # browser()
  y 
}
my_fun(x = 7)
 
# Example 8b
fcn <- function(x, y) {
  z <- x * y
  # browser()
  z1 <- z
  # browser()
  z2 <- z1
  # browser()
  z3 <- z2 + "a"
  # browser()
  return(z3 * exp(z3))
}
fcn(2, 1.3)
     

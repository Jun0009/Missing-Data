# Basic R examples

# Create a vector of numbers
x = c(4, 3, 5, 6, 20)

# Manipulate a vector
x^2; 1/x; x + 3; 2*x; 

# Or use built-in functions
sqrt(x); sum(x); length(x);

# Logical vectors 
x < 20
x == 20 # Compares each element of a vector to 20 (note the use of the double equal sign!)

# Creating a vector of sequential numbers
3:40

# Getting specific elements 
x[2]; x[-2]; x[2:4];x[x<20]

# Creating matrices 
A = matrix(1:6, 3, 2)

# Multiplying vectors and matrices 
y = c(1,2); A%*%y

# Combining vectors into a matrix
z = c(3,4); cbind(y,z)

# Data are stored into data frames
# There are built-in datasets available:
data()

# head functions shows just the first 6 rows
head(ChickWeight)

# Make names of columns visible: 
ChickWeight$weight
attach(ChickWeight)
weight
detach(ChickWeight)
# INSTALLING AND USING PACKAGES 
# Packages are collections of R functions, data, and compiled code bundled 
# up into a well-defined format that makes them easily importable into R.

library() # shows you which packages are available.

### Search the internet for details about package "car". Then install it.
install.packages("car") 
### If you haven't done so already, R will ask you which CRAN mirror
### to download from. After that it will be your default.
### The package car (companion to applied regression) will be installed, 
### along with any dependent packages it requires. At this point, although 
### we just *installed* the package, we have not yet *loaded* it into the 
### current workspace.

library(car)

### Once the packcage has been loaded you can use its functions and access
### its help and data examples (if any)

data() 
# Shows all example data sets currently loaded.
# Including in the packages!



# Simple examples with missing data

# Create vector with 3 elements
x = c(4, 6, 2)
mean(x)

# NA means "missing"
x = c(4, 6, NA)
mean(x)

# na.rm = T tells the function to ignore missing
mean(x, na.rm = T)

# How can missing be obtained as an answer?
var(3)
x[4]

# Some operations work and some don't
x + 1
2*x
sum(x)
length(x)


# How to create missing data matrix R?
# First create 4x3 data matrix
Y = matrix(1:12, 4, 3)
# Make some missing elements
Y[2,3] = Y[3, 2] = Y[3, 3] = NA
Y

# is.na function returns TRUE or FALSE, depending if missing or observed
is.na(Y)
# ! negates a statement
!is.na(Y)
# Converting TRUE FALSE to 1 0:
R = (!is.na(Y))*1
R

# Use which to find out missing indices:
which(is.na(Y))

# A simple MCAR data generator as in class
# Generate the full dataset
# sample size
n = 1000
y = rnorm(n)
# Choose the "observed" 80%:
R = sample(1:n, 800)
# Plot "observed" and "missing"
hist(y[R], col = "white", border = "blue")
hist(y[-R], col = "white", border = "red", add = TRUE)

# Example
# Read data from the file eatingattitudes.dat
d = read.table(file.choose())
head(d)
tail(d)
# the data contain the responses from 400 college-aged women
# on 10 questions from the Eating Attitudes Test, BMI, WSB, ANX

# Change missing from -99 to NA
d[d == -99] = NA
# Remove the first variable, which is just an index number
d = d[,-1]

summary(d)
# Notice many missing
dim(d)

# Listwise deletion of cases with at least one missing entry:
dim(na.omit(d))
# We lost almost half of the dataset!!


install.packages("BaylorEdPsych")
install.packages( "mvnmle")
# Note these packages have been "discontinued"
library(BaylorEdPsych)
# Test for MCAR
LittleMCAR(d)
# TS = 643.3141 which is highly significant and rejects Ho: MCAR
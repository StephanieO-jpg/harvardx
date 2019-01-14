library(dplyr)
library(purrr)
library(rvest)
library(tidyverse)
library(stringr)
library(dslabs)
library(caret)
library(matrixStats)


x <- matrix(rnorm(100*10), 100, 10)
dim(x)
nrow(x)
ncol(x)

#following lines of code would add the scalar 1 to row 1, the scalar 2 to row 2, and so on, for the matrix x
x <- sweep(x, 1, 1:nrow(x),"+")
x <- x + seq(nrow(x))

#Both these functions will add a sequence to each column
x <- matrix(rnorm(100*10), 100, 10)
x[,1]

#following lines of code would add the scalar 1 to column 1, the scalar 2 to column 2, and so on, for the matrix x
x <- sweep(x, 2, 1:ncol(x), FUN = "+")
x[,1]



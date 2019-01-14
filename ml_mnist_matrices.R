library(dplyr)
library(purrr)
library(rvest)
library(tidyverse)
library(stringr)
library(dslabs)
library(caret)
library(matrixStats)


data("mnist_27")
mnist <-read_mnist()

#Take the first 1000 records
x <- mnist$train$images[1:1000,] 
y <- mnist$train$labels[1:1000]

#Convert a vector to a matrix
my_vector <- 1:15
mat <- matrix(my_vector, 5, 3)
mat

#This transposes the vector to 3 row ----
mat_t <- matrix(my_vector, 3, 5, byrow = TRUE)
mat_t
t(mat)  #the fuction t also does this

#Take the third row and transform to a grid
#R flips images so you need to use the [,28:1] to resort when displaying the image
grid <- matrix(x[3,], 28, 28)
image(1:28, 1:28, grid[,28:1]) 

#Row and Column Summaries and Apply ----
# - Total pixel darkness
sums <- rowSums(x)
avg <- rowMeans(x)

data.frame(labels = as.factor(y), row_averages = avg) %>%
  ggplot(aes(labels, row_averages)) +
  geom_boxplot()
#From this plot we see that, not surprisingly, 1s use less ink than the other digits.
# Apply function
# - matrix, 1 for rows 2 for columns, the function to be applied
avgs <- apply(x,1,mean) #average for each row
sds <- apply(x,2,sd) #SD for each column

#Filtering Columns Based on Summaries ----
#Study the variation of each pixel and remove columns that do not vary much
#Each column represents a pixel
sds <- colSds(x)
qplot(sds, bins = "30", color = I("black"))
image(1:28, 1:28, matrix(sds, 28, 28)[, 28:1])
#We see that there is little variation in the corners.We typically write in the centre of a box
new_x <- x[ ,colSds(x) >60]
dim(new_x)
#Indexing with Matrices and Binarizing the Data ----
qplot(as.vector(x), bins = "30", color = I("black"))
#values below 25 are smudges
new_x <- x
new_x[new_x < 50] <- 0
#Binarize the data. Pixels are either ink or not
bin_x <- x
bin_x[bin_x < 255/2] <- 0
bin_x[bin_x > 255/2] <- 1
#Vectorization for Matrices and Matrix Algebra Operations ----
#In R, we subtract a vector from a matrix,
#the first element of each vector is subtracted
#from the first row of the matrix.
#The second element from the vector is subtracted from the second row
#of the matrix and so on.

#Sweep - deletes a value from all rows or columns
# - matrix, 1 = row 2 = column, function, default is subtraction
X_mean_0 <- sweep(x, 2, colMeans(x))
X_standarize <- sweep(X_mean_0, 2, colSds(x), FUN ="/")

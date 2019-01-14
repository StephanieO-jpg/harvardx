library(dplyr)
library(purrr)
library(tidyverse)
library(dslabs)
library(caret)

#Generate a set of random predictors and outcomes using the following code----
set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

#Train function----
#Because x and y are completely independent, you should not be able to predict y using x 
#with accuracy greater than 0.5. 
#Confirm this by running cross-validation using logistic regression to fit the model. 
#Because we have so many predictors, we selected a random sample x_subset. 
#Use the subset when training the model.
#Needed to install randomForest to support the train function
fit <- train(x_subset, y, method = "glm")
fit$results

#Select the predictor that is the most accurate----
source("https://bioconductor.org/biocLite.R")
#Using the biocLite function, install the “genefilter” library, you would type:
biocLite("genefilter")
library(genefilter)
# x is the matrix (must not contain NAs)
# y is the factor which codes the grouping to be tested
tt <- colttests(x, y) #t-tests and F-tests for rows or columns of a matrix
glimpse(tt)

#This lists the predictors that have a good p_value based on the t-test----
p_vals <- tt$p.value
ind <- which(p_vals < 0.01)
length(ind)
x_subset_signf <- as.matrix(x[,ind])
#Now re-run the cross-validation after redefinining x_subset to be the subset of x 
#defined by the columns showing "statistically significant" association with y.
fit_x_significant <- train(x_subset_signf, y, method = "glm")
fit_x_significant$results

#Run cross validation using KNN method----
fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)


#KNN method gene expression----
#Use the train function to predict tissue from gene expression in the
#tissue_gene_expression dataset. Use kNN.
data("tissue_gene_expression")
x <- tissue_gene_expression$x
y <- tissue_gene_expression$y

data("tissue_gene_expression")
fit <- with(tissue_gene_expression, train(x, y, method = "knn", tuneGrid = data.frame( k = seq(1, 7, 2))))
ggplot(fit)
fit$results

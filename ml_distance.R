library(dplyr)
library(purrr)
library(rvest)
library(tidyverse)
library(stringr)
library(dslabs)
library(caret)
library(matrixStats)

data("tissue_gene_expression")
dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)

d <- dist(tissue_gene_expression$x)

#Compare the distances between observations 1 and 2 (both cerebellum), 
#observations 39 and 40 (both colon), and observations 73 and 74 (both endometrium).
as.matrix(d)[1:2,1:2] #Need to coerce the dist object into a matrix
as.matrix(d)[39:40,39:40] #Need to coerce the dist object into a matrix
as.matrix(d)[73:74,73:74] #Need to coerce the dist object into a matrix
as.matrix(d)[1:2,c(1:2, 39:40,73:74)]
as.matrix(d)[39:40,c(1:2, 39:40,73:74)]
as.matrix(d)[73:74,c(1:2, 39:40,73:74)]

ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind,ind]
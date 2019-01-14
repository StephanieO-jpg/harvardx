library(dplyr)
library(ggrepel)
library(purrr)
library(rvest)
library(tidyverse)
library(stringr)
library(dslabs)
library(caret)
library(matrixStats)

set.seed(1993)
data("tissue_gene_expression")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])   #Drops unused levels in a factor
x <- tissue_gene_expression$x[ind, ]        # At this point there are 500 predictors
x <- x[, sample(ncol(x), 10)] # This code picks out 10 predictors are random

#LDA----
train_lda <- train(x, y, 
                   method = "lda",
                   preProcess = "center")
train_lda$results
train_lda$finalModel #This lists the coefficients of linear discriminants
means <- data.frame(t(train_lda$finalModel$means)) 
means <- means %>% mutate(gene = as.factor(rownames(means)))

means %>% ggplot(aes(x = cerebellum, y = hippocampus, colour = gene, label = gene)) +
  ggtitle("LDA Means - Cerebellum vs Hippocampus") +
  geom_point() +
  geom_text_repel(aes(label=gene)) +
  theme(legend.position="none")  


t(fit_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()


#QDA----
train_qda <- train(x, y, 
                   method = "qda")
train_qda$results
train_qda$finalModel #This lists the coefficients of linear discriminants
t(train_qda$finalModel$means) # t() - Matrix transpose. Really nice function to take the levels and transpose.
colMeans(t(train_qda$finalModel$means))


means <- data.frame(t(train_qda$finalModel$means)) 
means <- means %>% mutate(gene = as.factor(rownames(means)))

means %>% ggplot(aes(x = cerebellum, y = hippocampus, colour = gene, label = gene)) +
  ggtitle("QDA Means - Cerebellum vs Hippocampus") +
  geom_point() +
  geom_text_repel(aes(label=gene)) +
  theme(legend.position="none")  

# This is the solution proposed by Harvardx.
t(fit_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()



#Assess all tissue types----
set.seed(1993)
data("tissue_gene_expression")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

train_lda <- train(x, y, 
                   method = "lda")
train_lda$results
train_lda$finalModel #This lists the coefficients of linear discriminants
means <- data.frame(t(train_lda$finalModel$means)) 
means <- means %>% mutate(gene = as.factor(rownames(means)))

means %>% ggplot(aes(x = cerebellum, y = hippocampus, colour = gene, label = gene)) +
  ggtitle("LDA Means - Cerebellum vs Hippocampus") +
  geom_point() +
  geom_text_repel(aes(label=gene)) +
  theme(legend.position="none")  
# Assignment-3

---
title: "Assignment 3"
author: "Victoria Restivo"
date: '2018-11-05'
output: html_document
---

#question 2
rna_counts <- read.csv("~/Desktop/eXpress_dm_counts.csv", h = T)

dim(rna_counts)

mean_function <- function (x=rna_counts, y=FALSE) {
  if (y == TRUE) {
    x <- (log2(x+0.00000001))} 
  return(mean(x))}    

mean_function(rna_counts[,2])  
mean_function(rna_counts[,2], y = TRUE)  

#question 3
dim_data <- ncol(rna_counts)
stored_means <- rep(0, dim_data-1) 

for (i in 1:dim_data) {
  stored_means[i-1] <- mean_function(rna_counts[,i], y=FALSE) 
  print(colnames(rna_counts[i]))
  print(stored_means[i-1])
}

#question 4
sapply(rna_counts[,2:56], mean_function) 

TimeTest2 <- sapply(rna_counts[,2:56], mean_function) 

TimeTest1 <- for (i in 1:dim_data) {
                stored_means[i-1] <- mean_function(rna_counts[,i], y=FALSE) 
                print(colnames(rna_counts[i]))
                print(stored_means[i-1])
              }

library(microbenchmark)
microbenchmark(TimeTest2)
microbenchmark(TimeTest1)
#The sapply function (question 4) ran faster than the for loop (question 3)

#question 5
colMeans(rna_counts[,2:56])


#question 6
nrow(rna_counts)
stored_gene_means <- rowMeans(rna_counts[,2:56])
print(stored_gene_means)


#question 7 
lg_male <- subset(rna_counts, select = (grepl("lg_male_hdhorn*", names(rna_counts))))
sm_male <- subset(rna_counts, select = (grepl("sm_male_hdhorn*", names(rna_counts)))) 

gene_difference <- function (x) {
  sm_male <- subset(x, select = (grepl("sm_male_hdhorn*", names(x)))) 
  small_mean <- rowMeans(sm_male) 
  lg_male <- subset(rna_counts, select = (grepl("lg_male_hdhorn*", names(x))))
  large_mean <- rowMeans(lg_male)
  gene_difference <- large_mean - small_mean
}
difference <- gene_difference(rna_counts)
print(difference)

#question 8 
library(ggplot2)
theme_update(plot.title = element_text(hjust = 0.5))

ggplot(rna_counts, aes(x=stored_gene_means, y=difference)) + geom_point() + xlab("Mean Gene Expression") + ylab("Mean Gene Expression Difference") + ggtitle("Mean gene expression vs mean gene difference in large and small male Drosophila headhorns")

ggplot(rna_counts, aes(x=stored_gene_means, y=difference)) + geom_point() + xlab("Mean Gene Expression") + ylab("Mean Gene Expression Difference") + scale_x_continuous(trans='log2') +
  scale_y_continuous(trans='log2') + ggtitle("Transformed mean gene expression vs mean gene difference in large and small male Drosophila melanogaster headhorns")




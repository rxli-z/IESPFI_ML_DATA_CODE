# K-Means Clustering

# Load libraries
library(tidyverse)
library(tidymodels)
library(janitor)
library(xgboost)
library(vip)
library(yardstick)
library(rpart)
library(rpart.plot)
library(polycor)
library(ggplot2)
library(reshape2)
library(EGAnet)
library(factoextra)
library(NbClust)
library(cluster)
library(nnet)
library(glmnet)
library(caret)
library(readr)
library(stringr)
library(labelled)
library(hardhat)
library(themis)

# Load the dataset
pfi_data_clean <- read_csv("pfi_data_01.csv")
ega_net_scores <- read_csv("pfi_data_02.csv")

# Perform K-means clustering
set.seed(123)  # For reproducibility

# Determine best k using NbClust
cluster_model<-FALSE

if(cluster_model){
  nb <- NbClust(ega_net_scores, distance = "euclidean",
                min.nc = 2, max.nc = 10,
                method = "kmeans")
  save(nb,file="pfi_nb.Rdata")
} else{
  load("pfi_nb.Rdata")
}

cat("Number of criteria suggesting the number of clusters:\n")
print(table(nb$Best.nc[1, ]))

kmeans_result <- kmeans(ega_net_scores, centers = 3, nstart = 25)
kmeans_result$size
kmeans_result$centers

# Visualize the clustering result
fviz_cluster(kmeans_result, data = ega_net_scores)

# Adding cluster into dataset
FPP <- kmeans_result$cluster
pfi_data_final <- cbind(pfi_data_clean, as.data.frame(FPP))

write.table(pfi_data_final, file = "pfi_data_03.csv")
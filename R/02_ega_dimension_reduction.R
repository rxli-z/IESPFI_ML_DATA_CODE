# EGA Dimension Reduction

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
pfi_data_sub <- pfi_data_clean[20:42]

# Correlations
cor_matrix <- hetcor(as.data.frame(pfi_data_sub))$correlations

# Reshape correlation matrix into long format
cor_matrix_long <- melt(cor_matrix)

# Add a column for significance
cor_matrix_long$Significance <- ifelse(abs(cor_matrix_long$value) > 0.7, "*", "")

# Plot the heatmap
cor_heatmap <- ggplot(cor_matrix_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab") +
  geom_text(aes(label = Significance), color = "black", size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8),
        axis.title = element_blank()) +
  labs(title = "Correlation Heatmap", fill = "Correlation")
cor_heatmap

# Remove highly correlated
pfi_data_sub <- pfi_data_sub %>%
  select(-FCSTDS)

# Perform UVA
pfi_uva <- UVA(pfi_data_sub)
pfi_uva

# Estimate EGA
pfi_fit <- EGA.fit(data = pfi_data_sub, algorithm = "walktrap")
summary(pfi_fit)

# Compute Node Strength
pfi_loadings <- net.loads(pfi_fit, loading.method = "experimental") 
summary(pfi_loadings) 
#network loading of .2 would generally equate to .4 in terms of PCA metrics

# Remove low loading variables
pfi_data_sub_load <- pfi_data_sub %>%
  select(-FSCOUNSLR)

# Redo EGA
pfi_fit <- EGA.fit(data = pfi_data_sub_load, algorithm = "walktrap")
summary(pfi_fit)

# Compute New Node Strength
pfi_loadings <- net.loads(pfi_fit, loading.method = "experimental")
summary(pfi_loadings)

# Bootstrap to check stability
pfi_fit_boot <- bootEGA(data = pfi_data_sub_load, algorithm = "walktrap", 
                        EGA.type = "EGA.fit", seed = 123, 
                        plot.itemStability = FALSE)
dimensionStability(pfi_fit_boot)

# Get Network (factor) Scores
ega_net_scores <- net.scores(data = pfi_data_sub_load, pfi_fit, 
                             loading.method = "experimental")

# Rename columns to be more descriptive
ega_net_scores <- as.data.frame(ega_net_scores$scores$std.scores) %>% 
  rename(communication = 1, information = 2, satisfaction = 3, involvement = 4, homework = 5)

write.table(ega_net_scores, file = "pfi_data_02.csv")
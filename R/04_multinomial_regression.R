# Multinomial Regression

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
ega_net_scores <- read_csv("pfi_data_02.csv")
pfi_data_final <- read_csv("pfi_data_03.csv")

#Prepare data
pfi_data_final <- cbind(pfi_data_final, ega_net_scores)

pfi_data_final_sc <- pfi_data_final %>% 
  na.omit() %>% #ensure rows with NAs are removed
  mutate(FPP = as.factor(FPP)) %>% #change outcome to factor
  mutate(FPP = recode(FPP, "1" = "Low All", "2" = "High I&S", "3" = "High All")) %>%
  mutate(FPP = factor(FPP, levels = c("Low All", "High I&S", "High All")))

# Set seed for replication purposes
set.seed(1234)

# Set High All as reference for FPP and White as reference for Race
pfi_data_final_sc$FPP <- relevel(pfi_data_final_sc$FPP, ref = "High All")
pfi_data_final_sc$RACEETH <- relevel(pfi_data_final_sc$RACEETH, ref = "White")

# Create new variable in tibble for division into training and test sets
pfi_data_final_sc_t_t <- pfi_data_final_sc %>% 
  mutate(id = row_number())

# 70% of data as training set 
train_set <- pfi_data_final_sc_t_t %>% 
  sample_frac(0.70) #selects 70% of observations

# 30% of data test set 
test_set  <- anti_join(pfi_data_final_sc_t_t, train_set, by = 'id') 
#anti_join, take remaining observations not in train_set

# Remove unnecessary variables
train_set <- train_set[, c(1:19, 43)]
test_set <- test_set[, c(1:19, 43)]

# Select features for training model
model_train_pfi <- train(FPP ~ .,
                         data = train_set,
                         method="multinom", #multinomial regression
                         stepwise = TRUE, #step-wise
                         direction="both", #both backwards and foward
                         trControl = trainControl(method = "cv", number = 10)) 
#cross validation with 10x/folds

# Training Results
summary(model_train_pfi)
model_train_pfi$finalModel

# Extract coefficients and standard errors
coef_matrix <- summary(model_train_pfi$finalModel)$coefficients
se_matrix <- summary(model_train_pfi$finalModel)$standard.errors

# Calculate z-scores and p-values
z_scores <- coef_matrix / se_matrix
p_values <- (1 - pnorm(abs(z_scores), 0, 1)) * 2  # Two-tailed p-values

# Combine values together into dataframe
train_pfi_descrip <- as.data.frame(t(round(coef_matrix, 3))) %>%
  rename_with(~ paste0("Coef_", rownames(coef_matrix)), everything()) %>%
  bind_cols(as.data.frame(t(round(se_matrix, 3))) %>%
              rename_with(~ paste0("SE_", rownames(se_matrix)), everything())) %>%
  bind_cols(as.data.frame(t(round(p_values, 3))) %>%
              rename_with(~ paste0("P_", rownames(p_values)), everything())) %>%
  rownames_to_column("Predictor")

# View the final model descriptive stats
train_pfi_descrip

# Evaluate the model performance on the test data
test_predictions <- predict(model_train_pfi, newdata = test_set)

# Add and recode predicted values in tibble
test_set_pred <- test_set %>% 
  mutate (predicted = test_predictions) %>% #add in predicted vector
  mutate(
    act_num = ifelse(FPP == "Low All", 1,
                     ifelse(FPP == "High I&S", 2, 3)),
    #ifelse(FPP == "High Com", 3, 4))),  # numeric var for actual FPP
    pred_num = ifelse(predicted == "Low All", 1,
                      ifelse(predicted == "High I&S", 2, 3))) %>%
  #ifelse(predicted == "High Com", 3, 4)))) %>%  # numeric var for predicted FPP
  mutate_at(c("act_num", "pred_num"), as.factor) #factor for confusion matrix

# Check the data
glimpse(test_set_pred)
table(test_set_pred$act_num)

# create confusion matrix using CARET
confusionMatrix(test_set_pred$act_num, test_set_pred$pred_num,
                mode = "everything", #reported stats
                positive = "1")

# Create table with actual and predicted values
mosaic_table <- table(test_set_pred$act_num, test_set_pred$pred_num)
mosaic_table #check table

# simple mosaic plot
mosaicplot(mosaic_table,
           main = "Confusion matrix for multinomial regression", #title
           sub = "Accuracy of prediction", #description
           xlab = "Predicted", #x axis label
           ylab = "Actual", #y axis label
           color = c("blue4", "purple4", "hotpink4"),
           border = "black")


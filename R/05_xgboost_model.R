# XGBoost Model

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

# Prepare data
pfi_data_final <- cbind(pfi_data_final, ega_net_scores)

pfi_data_final_sc <- pfi_data_final %>% 
  na.omit() %>% #ensure rows with NAs are removed
  mutate(FPP = as.factor(FPP)) %>% #change outcome to factor
  mutate(FPP = recode(FPP, "1" = "Low All", "2" = "High I&S", "3" = "High All")) %>%
  mutate(FPP = factor(FPP, levels = c("Low All", "High I&S", "High All")))

pfi_split <- initial_split(pfi_data_final_sc[, c(1:19, 43)])
pfi_train <- training(pfi_split)
pfi_test <- testing(pfi_split)

pfi_train <- pfi_train %>%
  mutate(FPP = factor(FPP, levels = c("Low All", "High I&S", "High Com", "High All")))

w_table <- table(pfi_train$FPP)
maj_n   <- max(w_table)
w_map   <- maj_n/as.numeric(w_table)
names(w_map) <- names(w_table)
w_map

pfi_train <- pfi_train %>%
  mutate(wts = unname(w_map[as.character(FPP)])) %>%
  mutate(wts = importance_weights(wts))

# Model Specification
xgb_spec <- boost_tree(
  trees = 100,
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  mtry = tune(),
  learn_rate = tune()
) %>%
  set_engine("xgboost",
             objective   = "multi:softprob",
             eval_metric = "mlogloss" ) %>%
  set_mode("classification")

# Final Model Spec
xgb_spec_final <- boost_tree(
  trees = 1500,
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  mtry = tune(),
  learn_rate = tune()
) %>%
  set_engine("xgboost",
             objective = "multi:softprob",
             eval_metric = "mlogloss",
             early_stopping_rounds = 50
  ) %>%
  set_mode("classification")

# Recipe and Workflow
pfi_formula <- as.formula("FPP ~ .")

pfi_rec <- recipe(pfi_formula, pfi_train) %>%
  step_other(all_nominal_predictors(), threshold = .05) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) #%>%
#step_smote(FPP, neighbors = 5)

pfi_wf <- workflow() %>%
  add_recipe(pfi_rec) %>%
  add_model(xgb_spec) #%>%
#add_case_weights(wts)

# Prep once to compute the right number of predictors
pfi_prep <- prep(pfi_rec)
pfi_juice <- juice(pfi_prep)

# Hyperparameter Tuning
xgb_grid <- grid_space_filling(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), pfi_juice),
  learn_rate(),
  size = 30
)

#xgb_grid_focus <- grid_regular(
#  tree_depth(range = c(3L, 6L)),
#  learn_rate(range = c(0.02, 0.10)),
#  loss_reduction(range = c(0.05, 1.0)),
#  min_n(range = c(5L, 40L)),
#  sample_prop(range = c(0.6, 0.9)),
#  levels = c(4, 5, 5, 4, 4)
#)

# Resamples
pfi_rs <- vfold_cv(pfi_train)

fit_model<-FALSE

if(fit_model){
  xg_tune_res <- tune_grid(
    pfi_wf,
    grid = xgb_grid_focus,
    resamples = pfi_rs,
  )
  save(xg_tune_res,file="pfi_xg_tune_res.Rdata")
} else{
  load("pfi_xg_tune_res.Rdata")
}


# Evaluation Metrics
collect_metrics(xg_tune_res) %>% 
  dplyr::filter(.metric %in% c("accuracy", "mn_log_loss", "roc_auc")) %>% 
  arrange(.metric, mean)
show_best(xg_tune_res, metric = "accuracy", n = 5)

# Variable Importance
final_xgb <- finalize_workflow(pfi_wf, select_best(xg_tune_res, metric = "roc_auc")) # For ROC/AUC Reporting
final_xgb <- finalize_workflow(pfi_wf, select_best(xg_tune_res, metric = "accuracy")) # For Accuracy Reporting

# Fit final data
final_xgb %>%
  fit(data = pfi_train) %>%
  extract_fit_parsnip() %>%
  vip::vip(geom = "point") +
  labs(title = "Variable Importance for Predicting FPP (Multiclass XGBoost)")


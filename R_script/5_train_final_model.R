# Final Project ----
# Stat 301-2
# Train final model

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(xgboost)


# handle common conflicts
tidymodels_prefer()


# load workflow
load(here("workflow_fitting/complex/bt_tune_complex.rda"))

# load training
load(here("data_splits/hhs_train.rda"))

# set seed
set.seed(878)

## Boosted Tree (Complex Recipes) are the best model 
# Update final workflow ----
final_wflow <- bt_tune_complex |> 
  extract_workflow(bt_tune_complex) |>  
  finalize_workflow(select_best(bt_tune_complex, 
                                metric = "f_meas"))

# final fitting -----
hhs_final_fit <- fit(final_wflow, hhs_train)

## save final fitting
save(hhs_final_fit, 
     file = here("final_fitting/hhs_final_fit.rda"))


# Hyperparameters ----
bt_hyperameter <- select_best(bt_tune_complex, metric = "roc_auc")|>
  select(trees, mtry, min_n, learn_rate)

## save hyperparameters
save(bt_hyperameter, 
     file = here("final_fitting/bt_hyperameter.rda"))

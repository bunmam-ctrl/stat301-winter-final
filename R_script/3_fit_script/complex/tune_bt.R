# Final Project ----
# Stat 301-2
# Define, fit, and tune boosted tree model (xgboost) (complex recipe)

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(doMC)
library(xgboost)


# handle common conflicts
tidymodels_prefer()

# parallel processing ----
num_cores <- detectCores(logical = FALSE)
registerDoMC(cores = num_cores)


# Load recipe and data
load(here("recipes/hhs_recipe_tree_complex.rda"))
load(here("data_splits/hhs_folds.rda"))
load(here("data_splits/keep_wflow.rda"))
load(here("data_splits/my_metrics.rda"))

# set seed
set.seed(5987)

# model specifications ----
bt_model <- boost_tree( mtry = tune(),trees = tune(),
                        min_n = tune(), learn_rate = tune())|>
  set_engine("xgboost")|>
  set_mode("classification")


# define workflows ----
bt_wflow_complex <- workflow()|>
  add_recipe(hhs_recipe_tree_complex)|>
  add_model(bt_model)


# hyperparameter tuning values ----
## check ranges for hyperparameter
extract_parameter_set_dials(bt_model)

bt_params <- extract_parameter_set_dials(bt_wflow_complex)|>
  update(
    mtry = mtry(c(1,5)),
    trees = trees(c(100,2000)),
    min_n = min_n(c(2,40)),
    learn_rate = learn_rate(range = c(-5,-0.2))
  )

bt_grid <- grid_regular(bt_params, levels = c(5, 6, 4, 10))


# fit workflow/model ----
## Tuning resample
bt_tune_complex <- bt_wflow_complex|>
  tune_grid(
    resamples = hhs_folds,
    grid = bt_grid,
    metrics = my_metrics,
    control = keep_wflow
  )

# write out results (fitted/trained workflows) ----
save(bt_tune_complex,
     file = here("workflow_fitting/complex/bt_tune_complex.rda"))

save(bt_wflow_complex, 
     file = here("workflow_fitting/complex/bt_wflow_complex.rda"))

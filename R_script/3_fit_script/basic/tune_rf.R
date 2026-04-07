# Final Project ----
# Stat 301-2
# Define, fit, and tune random forest model (basic recipe)

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(doMC)
library(hardhat)

# handle common conflicts
tidymodels_prefer()

# parallel processing ----
num_cores <- detectCores(logical = FALSE)
registerDoMC(cores = num_cores)

# Load recipe and data
load(here("recipes/hhs_recipe_tree_basic.rda"))
load(here("data_splits/hhs_folds.rda"))
load(here("data_splits/keep_wflow.rda"))
load(here("data_splits/my_metrics.rda"))

# set seed
set.seed(1756)

# model specifications ----
rf_model <- rand_forest(
  trees = tune(),
  mtry = tune(),
  min_n = tune()
)|>
  set_engine("ranger")|>
  set_mode("classification")


# define workflows ----
rf_wflow_basic <- workflow()|>
  add_recipe(hhs_recipe_tree_basic)|>
  add_model(rf_model)


# hyperparameter tuning values ----
## check ranges for hyperparameter
extract_parameter_set_dials(rf_model)

rf_params <- extract_parameter_set_dials(rf_wflow_basic)|>
  update(
    mtry = mtry(c(1,33)),
    trees = trees(c(500,2000)),
    min_n = min_n(c(2,40))
  )

rf_grid <- grid_regular(rf_params, levels = c(5,4,4))


# fit workflow/model ----
## Tuning resample
rf_tune_basic <- rf_wflow_basic|>
  tune_grid(
    resamples = hhs_folds,
    grid = rf_grid,
    control = keep_wflow,
    metrics = my_metrics
  )

# write out results (fitted/trained workflows) ----
save(rf_tune_basic,
     file = here("workflow_fitting/basic/rf_tune_basic.rda"))

save(rf_wflow_basic, 
     file = here("workflow_fitting/basic/rf_wflow_basic.rda"))







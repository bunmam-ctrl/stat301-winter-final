# Final Project ----
# Stat 301-2
# Define and fit K-nearest neighbors (complex recipes)

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(doMC)

# handle common conflicts
tidymodels_prefer()

# Load recipe and data
load(here("recipes/hhs_recipe_tree_complex.rda"))
load(here("data_splits/hhs_folds.rda"))
load(here("data_splits/keep_wflow.rda"))
load(here("data_splits/my_metrics.rda"))


# parallel processing ----
num_cores <- detectCores(logical = FALSE)
registerDoMC(cores = num_cores)

# set seed
set.seed(2222)


# model specifications ----
knn_model <- nearest_neighbor(neighbors = tune())|>
  set_engine("kknn")|>
  set_mode("classification")

# define workflows ----
knn_wflow_complex <- workflow()|>
  add_recipe(hhs_recipe_tree_complex)|>
  add_model(knn_model)

# hyperparameter tuning values ----
## check ranges for hyperparameter
extract_parameter_set_dials(knn_model)

knn_params <- extract_parameter_set_dials(knn_wflow_complex)|>
  update(
    neighbors = neighbors(c(1,60))
  )

knn_grid <- grid_regular(knn_params, levels = 20)

# fit workflow/model ----
## Tuning resample
knn_tune_complex <- knn_wflow_complex|>
  tune_grid(
    resamples = hhs_folds,
    grid = knn_grid,
    metrics = my_metrics,
    control = keep_wflow
  )

# write out results (fitted/trained workflows) ----
save(knn_tune_complex,
     file = here("workflow_fitting/complex/knn_tune_complex.rda"))

save(knn_wflow_complex,
     file = here("workflow_fitting/complex/knn_wflow_complex.rda"))


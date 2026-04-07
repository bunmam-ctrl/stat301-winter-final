# Final Project ----
# Stat 301-2
# Define and fit null model (complex recipe)

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)

# handle common conflicts
tidymodels_prefer()

# Load recipe and data
load(here("recipes/hhs_recipe_tree_complex.rda"))
load(here("data_splits/hhs_folds.rda"))
load(here("data_splits/keep_wflow.rda"))
load(here("data_splits/my_metrics.rda"))


## Model specification ----
null_model <- null_model()|>
  set_engine("parsnip")|>
  set_mode("classification")

## Create workflow
null_wflow_complex <- workflow()|>
  add_recipe(hhs_recipe_tree_complex)|>
  add_model(null_model)

## null fitting
null_fit_complex <- null_wflow_complex|>
  fit_resamples(
    resamples = hhs_folds,
    control = keep_wflow,
    metrics = my_metrics)

## Save workflow and fitting ----
save(null_wflow_complex,
     file = here("workflow_fitting/complex/null_wflow_complex.rda"))

save(null_fit_complex,
     file = here("workflow_fitting/complex/null_fit_complex.rda"))

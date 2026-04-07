# Final Project ----
# Stat 301-2
# Define and fit null model (basic recipe)

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)

# handle common conflicts
tidymodels_prefer()

# Load recipe and data
load(here("recipes/hhs_recipe_tree_basic.rda"))
load(here("data_splits/hhs_folds.rda"))
load(here("data_splits/keep_wflow.rda"))
load(here("data_splits/my_metrics.rda"))


## Model specification ----
null_model <- null_model()|>
  set_engine("parsnip")|>
  set_mode("classification")

## Create workflow
null_wflow_basic <- workflow()|>
  add_recipe(hhs_recipe_tree_basic)|>
  add_model(null_model)

## null fitting
null_fit_basic <- null_wflow_basic|>
  fit_resamples(
    resamples = hhs_folds,
    control = keep_wflow,
    metrics = my_metrics)

## Save workflow and fitting ----
save(null_wflow_basic,
     file = here("workflow_fitting/basic/null_wflow_basic.rda"))

save(null_fit_basic,
     file = here("workflow_fitting/basic/null_fit_basic.rda"))




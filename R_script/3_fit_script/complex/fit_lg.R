# Final Project ----
# Stat 301-2
# Define and fit logistic regression (complex recipe)

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)

# handle common conflicts
tidymodels_prefer()


# Load recipe and data
load(here("recipes/hhs_recipe_lm_complex.rda"))
load(here("data_splits/hhs_folds.rda"))
load(here("data_splits/keep_wflow.rda"))
load(here("data_splits/my_metrics.rda"))


# Model specifications ----
lg_model <- logistic_reg()|>
  set_engine("glm")|>
  set_mode("classification")

# define workflows ----
lg_wflow_complex <- workflow()|>
  add_recipe(hhs_recipe_lm_complex)|>
  add_model(lg_model)

## logistic regression fitting
lg_fit_complex <- lg_wflow_complex|>
  fit_resamples(
    resamples = hhs_folds,
    control = keep_wflow,
    metrics = my_metrics)

## Save workflow and fitting ----
save(lg_fit_complex,
     file = here("workflow_fitting/complex/lg_fit_complex.rda"))

save(lg_wflow_complex,
     file = here("workflow_fitting/complex/lg_wflow_complex.rda"))

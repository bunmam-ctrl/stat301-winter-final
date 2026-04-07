# Final Project ----
# Stat 301-2
# Define and fit elastic net (basic recipe)

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(doMC)

# handle common conflicts
tidymodels_prefer()


# Load recipe and data
load(here("recipes/hhs_recipe_lm_basic.rda"))
load(here("data_splits/hhs_folds.rda"))
load(here("data_splits/keep_wflow.rda"))
load(here("data_splits/my_metrics.rda"))


# parallel processing ----
num_cores <- detectCores(logical = FALSE)
registerDoMC(cores = num_cores)



# model specifications ----
en_model <- logistic_reg(mixture = tune(), penalty = tune())|>
  set_engine("glmnet")|>
  set_mode("classification")


# define workflows ----
en_wflow_basic <-  workflow()|>
  add_recipe(hhs_recipe_lm_basic)|>
  add_model(en_model)

# hyperparameter tuning values ----
## check ranges for hyperparameter
extract_parameter_set_dials(en_model)

en_params <- extract_parameter_set_dials(en_wflow_basic)|>
  update(
    mixture = mixture(range = c(0,1)),
    penalty = penalty(range = c(-3,0))
  )

en_grid <- grid_regular(en_params, levels = 10)

# fit workflow/model ----
en_tune_basic <- en_wflow_basic|>
  tune_grid(
    resamples = hhs_folds,
    grid = en_grid,
    metrics = my_metrics,
    control = keep_wflow
  )

# write out results (fitted/trained workflows) ----
save(en_tune_basic,
     file = here("workflow_fitting/basic/en_tune_basic.rda"))

save(en_wflow_basic,
     file = here("workflow_fitting/basic/en_wflow_basic.rda"))



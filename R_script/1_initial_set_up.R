# Final Project ----
# Stat 301-2
# Initial data checks & data splitting

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)

# handle common conflicts
tidymodels_prefer()

# load data
hhs_tidy <- read_rds("data/hhs_tidy.rds")

# seed set
set.seed(309)

# initial split ----
hhs_split <- initial_split(hhs_tidy, prop = 0.80, 
                           strata = quality_star_category)

## Training dataset
hhs_train <- training(hhs_split)

## Testing dataset
hhs_test <- testing(hhs_split)



# folding data (resamples) ----
# set seed 
set.seed(605)

hhs_folds <- vfold_cv(
  hhs_train, 
  v = 10,
  repeats = 5,
  strata = quality_star_category
)

# set up controls for fitting resamples ----
keep_wflow <- control_resamples(save_workflow = TRUE)
my_metrics <- metric_set(accuracy, f_meas, roc_auc)


# write out split, train, test and folds ----
save(hhs_split, 
     file = here("data_splits/hhs_split.rda"))

save(hhs_train, 
     file = here("data_splits/hhs_train.rda"))

save(hhs_test, 
     file = here("data_splits/hhs_test.rda"))

save(hhs_folds, 
     file = here("data_splits/hhs_folds.rda"))

save(keep_wflow, 
     file = here("data_splits/keep_wflow.rda"))

save(my_metrics, 
     file = here("data_splits/my_metrics.rda"))






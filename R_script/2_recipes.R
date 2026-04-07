# Final Project ----
# Stat 301-2
# Setup pre-processing/recipes/feature engineering

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)

# handle common conflicts
tidymodels_prefer()

# load training data ----
load(here("data_splits/hhs_train.rda"))


# basic recipe----
## build lm recipe ----
hhs_recipe_lm_basic <- recipe(quality_star_category ~ ., 
                          data = hhs_train) |>
  step_rm("State", "CMS Certification Number (CCN)", "Telephone Number",
          "Provider Name", "Address", "City/Town", "ZIP Code", 
          "Certification Date")|>
  step_zv(all_predictors()) |>
  step_impute_mean(all_numeric_predictors()) |>  
  step_impute_mode(all_nominal_predictors()) |>  
  step_dummy(all_nominal_predictors()) |> 
  step_zv(all_predictors()) |> 
  step_normalize(all_numeric_predictors())  

### check recipe
hhs_recipe_lm_basic |> 
  prep() |> 
  bake(new_data = NULL) |> 
  glimpse()

### write out recipe(s) ----
save(hhs_recipe_lm_basic,
     file = here("recipes/hhs_recipe_lm_basic.rda"))


## build tree recipe ----
hhs_recipe_tree_basic <- recipe(quality_star_category ~ ., 
                                 data = hhs_train) |>
  step_rm("State", "CMS Certification Number (CCN)", "Telephone Number",
          "Provider Name", "Address", "City/Town", "ZIP Code", 
          "Certification Date")|>
  step_zv(all_predictors()) |>
  step_impute_mean(all_numeric_predictors()) |>  
  step_impute_mode(all_nominal_predictors()) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |> 
  step_normalize(all_numeric_predictors())   

### check recipe
hhs_recipe_tree_basic |> 
    prep() |> 
    bake(new_data = NULL) |> 
    glimpse()

## write out recipe(s) ----
save(hhs_recipe_tree_basic,
  file = here("recipes/hhs_recipe_tree_basic.rda"))



# Complex recipe----
## build lm recipe ----
hhs_recipe_lm_complex <- recipe(quality_star_category ~ ., 
                        data = hhs_train) |>
  step_rm("State", "CMS Certification Number (CCN)", "Telephone Number",
          "Provider Name", "Address", "City/Town", "ZIP Code", 
          "Certification Date")|>
  step_zv(all_predictors()) |>
  step_impute_mean(all_numeric_predictors()) |>  
  step_impute_mode(all_nominal_predictors()) |>  
  step_mutate(
    num_episodes = log10(num_episodes))|>
  step_interact(
    terms = ~timely_care : improve_walking +
      timely_care : improve_bed_transfer +
      timely_care : improve_medicine +
      timely_care : improve_bathing +
      timely_care : improve_breathing +
      timely_care : discharge_score + 
      timely_care : starts_with("pp") +
#-----------------------------------------------------------------------------------    
      flu_shot_check : improve_bed_transfer +
      flu_shot_check : improve_medicine +
      flu_shot_check : improve_breathing +
      flu_shot_check : med_issue_resolved + 
      flu_shot_check : starts_with("transfer_to_") +
      flu_shot_check : dtc_rate +
#-----------------------------------------------------------------------------------         
      improve_walking : improve_medicine +
      improve_walking : improve_bathing +    
      improve_walking : improve_breathing +
      improve_walking : discharge_score +
      improve_walking : med_issue_resolved +
      improve_walking : starts_with("transfer_to_") +
      improve_walking : ends_with("_rate") + 
      improve_walking : num_episodes + 
#-----------------------------------------------------------------------------------    
      improve_bed_transfer : improve_bathing +
      improve_bed_transfer : improve_breathing +
      improve_bed_transfer : med_issue_resolved +
      improve_bed_transfer :  discharge_score + 
      improve_bed_transfer : transfer_to_patient +
      improve_bed_transfer : ends_with("_rate") +
      improve_bed_transfer: medicare_spending_index +
#-----------------------------------------------------------------------------------    
      improve_bathing : improve_breathing +
      improve_bathing : improve_medicine +
      improve_bathing : med_issue_resolved + 
      improve_bathing : discharge_score +
      improve_bathing : starts_with("transfer_to_") +
      improve_bathing : ends_with("_rate") +
      improve_bathing : num_episodes +
#-----------------------------------------------------------------------------------    
      improve_breathing : med_issue_resolved +
      improve_breathing : discharge_score +
      improve_breathing : pph_rate + 
      improve_breathing : dtc_rate + 
      improve_breathing : starts_with("transfer_to_") +
      improve_breathing : num_episodes +
#-----------------------------------------------------------------------------------    
      improve_medicine : discharge_score +
      improve_medicine : ends_with("_rate") +
      improve_medicine : starts_with("transfer_to_") +
#-----------------------------------------------------------------------------------    
      improve_skin :ppr_rate +
      improve_skin : num_episodes +
      improve_skin : discharge_score +
      improve_skin : starts_with("transfer_to_") +
      improve_skin : med_issue_resolved + 
#-----------------------------------------------------------------------------------    
      med_issue_resolved : discharge_score +
#-----------------------------------------------------------------------------------    
      fall_injury_rate : transfer_to_patient +
      fall_injury_rate : dtc_rate +
#-----------------------------------------------------------------------------------    
      discharge_score : transfer_to_patient +
      discharge_score : dtc_rate +
      discharge_score : pph_rate +
      discharge_score : medicare_spending_index +
      discharge_score : num_episodes +
#-----------------------------------------------------------------------------------    
      transfer_to_provider : num_episodes +
#-----------------------------------------------------------------------------------    
      transfer_to_patient : num_episodes +
#-----------------------------------------------------------------------------------   
      dtc_rate : num_episodes +
#-----------------------------------------------------------------------------------    
      ppr_rate : medicare_spending_index +
      ppr_rate : num_episodes +
#-----------------------------------------------------------------------------------    
      medicare_spending_index : num_episodes
  )|>
  step_dummy(all_nominal_predictors()) |> 
  step_zv(all_predictors()) |> 
  step_normalize(all_numeric_predictors())  

### check recipe
hhs_recipe_lm_complex|> 
  prep() |> 
  bake(new_data = NULL) |> 
  glimpse()


## write out recipe(s) ----
save(hhs_recipe_lm_complex,
     file = here("recipes/hhs_recipe_lm_complex.rda"))


## build tree recipe
hhs_recipe_tree_complex <- recipe(quality_star_category ~ ., 
                                data = hhs_train) |>
  step_rm("State", "CMS Certification Number (CCN)", "Telephone Number",
          "Provider Name", "Address", "City/Town", "ZIP Code", 
          "Certification Date")|>
  step_zv(all_predictors()) |>
  step_impute_mean(all_numeric_predictors()) |>  
  step_impute_mode(all_nominal_predictors()) |>  
  step_mutate(
    num_episodes = log10(num_episodes)) |>
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |> 
  step_normalize(all_numeric_predictors())   

### check recipe
hhs_recipe_tree_complex|> 
  prep() |> 
  bake(new_data = NULL) |> 
  glimpse()

## write out recipe(s) ----
save(hhs_recipe_tree_complex,
     file = here("recipes/hhs_recipe_tree_complex.rda"))


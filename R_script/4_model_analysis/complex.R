# Final Project ----
# Stat 301-2
# Model selection/comparison & analysis (complex recipe)

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(flextable)


# load fitting model
list.files (
  here("workflow_fitting/complex/"),
  pattern = "complex.rda",
  full.names = TRUE
)|>
  map(load, envir = .GlobalEnv)


### F1, accuracy, and ROC have the same best model----------------------------------------------------------
# Compile table 
model_results <- as_workflow_set(
  null = null_fit_complex,
  lg = lg_fit_complex, 
  en = en_tune_complex, 
  knn = knn_tune_complex,
  rf = rf_tune_complex,
  bt = bt_tune_complex
)

# Create Evaluation table----
## Select top model based on F1 
top_5_accuracy_models <- model_results |> 
  collect_metrics() |>
  filter(.metric == "f_meas") |>  
  slice_max(mean, by = wflow_id)|>
  select(wflow_id, .config)

## Retrieve Accuracy, F1 Score & ROC AUC for these models  
evaluate_complex <-  model_results|>
  collect_metrics() |>
  inner_join(top_5_accuracy_models, by = c("wflow_id",".config") )|> 
  filter(.metric %in% c("accuracy", "roc_auc")) |>  
  select(wflow_id, .metric, mean, std_err)|>
  mutate(
    mean = round(mean, 3), 
    std_err = formatC(std_err, format = "e", digits = 2),  # Scientific notation
    .metric = case_when(
      .metric %in%  "accuracy"~ "Accuracy",
      .metric %in% "roc_auc" ~"ROC-AUC"
    ),
    wflow_id = case_when(
      wflow_id %in%  "null"~ "Null",
      wflow_id %in%  "lg"~ "Logistic regession",
      wflow_id %in%  "en"~ "Elastic net",
      wflow_id %in%  "rf"~ "Random forest",
      wflow_id %in%  "knn"~ "K-nearest neighbors",
      wflow_id %in%  "bt"~ "Boosted tree"
    )
  )|>
  rename(
    "Model" = wflow_id,  
    "Metric" = .metric, 
    "Mean" = mean, 
    "Standard Error" = std_err
  )


# Reformat table ----
## Identify last row of each model for adding a separator line
line_positions <- which(!duplicated(evaluate_complex$Model, 
                                    fromLast = TRUE))

evaluate_complex_reformat <- evaluate_complex |>
  flextable() |>
  merge_v(j = "Model") |>  # Merge Model type column
  theme_booktabs() |>  # Improve table styling
  align(j = c("Model", "Metric"), align = "left") |>  # Align text
  align(j = c("Mean", "Standard Error"), align = "center") |>
  bold(part = "header") |>  # Bold headers
  padding(padding = 5, part = "all") |>  # Increase padding for better spacing
  hline(i = line_positions, border = fp_border_default(width = 2))   # Add bold horizontal line between models


evaluate_complex_reformat <- autofit(evaluate_complex_reformat)

# Write accuracy table ----
save_as_image(evaluate_complex_reformat, 
              path = "metrics_table/evaluate_complex_reformat.png")

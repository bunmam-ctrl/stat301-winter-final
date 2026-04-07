# Final Project ----
# Stat 301-2
# Model selection/comparison & analysis (basic recipe)

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(flextable)


# load fitting model
list.files (
  here("workflow_fitting/basic/"),
  pattern = "basic.rda",
  full.names = TRUE
            )|>
  map(load, envir = .GlobalEnv)


### ROC and accuracy has the same best model----------------------------------------------------------
# Compile table 
model_results <- as_workflow_set(
  null = null_fit_basic,
  lg = lg_fit_basic, 
  en = en_tune_basic, 
  knn = knn_tune_basic,
  rf = rf_tune_basic,
  bt = bt_tune_basic
)


# Create ROC-AUC Score table----
## Select top model based on roc_auc
top_5_roc_models <- model_results |> 
  collect_metrics() |>
  filter(.metric == "roc_auc") |>  
  slice_max(mean, by = wflow_id)|>
  select(wflow_id, .config)

## Retrieve Accuracy, F1 Score & ROC AUC for these models  
roc_evaluate_basic <-  model_results|>
  collect_metrics() |>
  inner_join(top_5_accuracy_models,
             by = c("wflow_id",".config") )|> 
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
      wflow_id %in%  "knn"~ "K-nearest neighbors",
      wflow_id %in%  "rf"~ "Random Forest",
      wflow_id %in%  "bt"~ "Boosted tree"
    )
  )|>
  rename(
    "Model" = wflow_id,  
    "Metric" = .metric, 
    "Mean" = mean, 
    "Standard Error" = std_err
  )

## Identify last row of each model for adding a separator line
line_positions <- which(!duplicated(roc_evaluate_basic$Model, 
                                    fromLast = TRUE))

## Reformat table ----
roc_basic_reformat <- roc_evaluate_basic |>
  flextable() |>
  merge_v(j = "Model") |>  
  theme_booktabs() |> 
  align(j = c("Model", "Metric"), align = "left") |>
  align(j = c("Mean", "Standard Error"), align = "center") |>
  bold(part = "header") |>  # Bold headers
  padding(padding = 5, part = "all") |> 
  hline(i = line_positions, border = fp_border_default(width = 2))   


roc_basic_reformat <- autofit(roc_basic_reformat)

# Write ROC-AUC table ----
save_as_image(roc_basic_reformat, 
              path = "metrics_table/roc_basic_reformat.png")

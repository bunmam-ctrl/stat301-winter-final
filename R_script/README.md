The `R_script/` directory contains all R scripts used in the project, covering **data preparation, exploratory analysis, model training, and evaluation**. The scripts are numbered to reflect the **workflow sequence** from setup to final model assessment.  

### Files in Folder  

#### Data Preparation & EDA
- `data_tidy.R` – Cleans and transforms raw data into a structured format.  
- `target_analysis.R` – Performs exploratory data analysis (EDA) on the target variable.  
- `EDA_predictor.R` – Conducts EDA on predictor variables.  
- `predictor_check.R` – Checks for missing values and distributions of predictor variables.  
- `interact_predict.R` – Analyzes interactions between predictors.  

#### Model Training & Evaluation
- `1_initial_set_up.R` – Initial data split & formation of resamples.
- `2_recipes.R` – Data preprocessing and feature engineering for various models.
- [`3_fit_script/`](3_fit_script/) – Directory containing scripts for training various models based on recipes.
- [`4_model_analysis/`](4_model_analysis/) – Directory containing scripts for analyzing model performance based on recipes.
- `5_train_final_model.R` – Trains the final selected model.  
- `6_assess_final_model.R` – Evaluates the final model’s performance and metrics.  


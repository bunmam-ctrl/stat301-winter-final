The `3_fit_script/` directory contains scripts for **training and tuning machine learning models** based on different preprocessing recipes. It is structured into two subdirectories:  

- [`basic/`](basic/) – Scripts for models trained using a **basic recipe** (minimal preprocessing).  
- [`complex/`](complex/) – Scripts for models trained using a **complex recipe** (advanced feature transformations).  

Both folders contain the same types of scripts, differing only in the preprocessing approach used.  

### Files in Folder
Each file follows the naming convention **`<process>_<model>.R`**, where:  
- `<process>` indicates whether the script is used for **model tuning (`tune_`)** or **fitting (`fit_`)**.  
- `<model>` represents the machine learning model used.  

#### Model Tuning & Fitting Scripts 
- `tune_rf.R` – Hyperparameter tuning for **Random Forest**.  
- `tune_knn.R` – Hyperparameter tuning for **k-Nearest Neighbors**.  
- `tune_en.R` – Hyperparameter tuning for **Elastic Net**.  
- `tune_bt.R` – Hyperparameter tuning for **Boosted Trees**.  
- `tune_plot.R` – Generates tuning plots for different models.  
- `fit_null.R` – Fits a **null model** as a baseline for comparison.  
- `fit_lg.R` – Fits a **Logistic Regression model**.  


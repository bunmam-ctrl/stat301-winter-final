The `workflow_fitting/` directory contains results from training and fitting models to resamples. It is structured into two subdirectories:

- [`basic/`](basic/) – Stores results from models trained using a **basic recipe**, which includes minimal preprocessing and feature engineering.
- [`complex/`](complex/) – Stores results from models trained using a **complex recipe**, which incorporates advanced preprocessing techniques and feature transformations.

### Files in Folder
Each file follows the naming convention **`<model>_<type>_[recipe].rda`**, where:  

- `<model>` represents the machine learning model used.  
- `<type>` indicates whether the file stores the workflow (`wflow`), fitted model (`fit`), or tuning results (`tune`).  
- `[recipe]` is either **basic** or **complex**, indicating the preprocessing approach used.  

#### Model Files
- `rf_wflow_[recipe].rda` – Random Forest model workflow  
- `rf_tune_[recipe].rda` – Random Forest tuning results  
- `null_wflow_[recipe].rda` – Null model workflow  
- `null_fit_[recipe].rda` – Null model fitted results  
- `lg_wflow_[recipe].rda` – Logistic Regression model workflow  
- `lg_fit_[recipe].rda` – Logistic Regression fitted results  
- `knn_wflow_[recipe].rda` – k-Nearest Neighbors model workflow  
- `knn_tune_[recipe].rda` – k-Nearest Neighbors tuning results  
- `en_wflow_[recipe].rda` – Elastic Net model workflow  
- `en_tune_[recipe].rda` – Elastic Net tuning results  
- `bt_wflow_[recipe].rda` – Boosted Trees model workflow  
- `bt_tune_[recipe].rda` – Boosted Trees tuning results  


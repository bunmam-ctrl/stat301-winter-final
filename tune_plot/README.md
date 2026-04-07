The `tune_plot/` directory contains visualizations of model tuning results, structured into two subdirectories:  

- [`basic/`](basic/) – Stores tuning plots for models trained using a **basic recipe**, which includes minimal preprocessing and feature engineering.  
- [`complex/`](complex/) – Stores tuning plots for models trained using a **complex recipe**, which incorporates advanced preprocessing techniques and feature transformations.  

### Files in Folder
Each file follows the naming convention **`<model>_<metric>_[recipe].png`**, where:  

- `<model>` represents the machine learning model used.  
- `<metric>` indicates whether the plot shows **ROC curves (`roc`)** or **accuracy (`accuracy`)**.  
- `[recipe]` is either **basic** or **complex**, indicating the preprocessing approach used.  

#### Tuning Plots 
- `rf_roc_[recipe].png` – Random Forest ROC curve  
- `rf_accuracy_[recipe].png` – Random Forest accuracy plot  
- `knn_roc_[recipe].png` – k-Nearest Neighbors ROC curve  
- `knn_accuracy_[recipe].png` – k-Nearest Neighbors accuracy plot  
- `en_roc_[recipe].png` – Elastic Net ROC curve  
- `en_accuracy_[recipe].png` – Elastic Net accuracy plot  
- `bt_roc_[recipe].png` – Boosted Trees ROC curve  
- `bt_accuracy_[recipe].png` – Boosted Trees accuracy plot  


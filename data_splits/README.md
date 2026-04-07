Each file in `data_splits/` contains **data partitioning artifacts** used for model training, validation, and testing. These files facilitate **cross-validation, performance evaluation, and reproducibility** in predictive modeling.  

#### Files in Folder  

- `hhs_folds.rda` – Cross-validation folds for training.  
- `hhs_split.rda`– Data split object containing training and testing partitions.  
- `hhs_test.rda`– The test dataset used for model evaluation.  
- `hhs_train.rda` – The training dataset used for model fitting.  
- `keep_wflow.rda` – Specifies additional controls for saving workflow during tuning and training on a regular grid.  
- `my_metrics.rda` – Stores performance metrics generated during model tuning and training.
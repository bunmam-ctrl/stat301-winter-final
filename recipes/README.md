The `recipes/` directory contains **preprocessing and feature engineering objects** used to prepare the data for modeling. These files define the transformations applied to the dataset before training.  

### Files in Folder
Each file follows the naming convention **`hhs_recipe_<model_group>_<recipe>.rda`**, where:  
- `<model_group>` specifies the type of models the recipe is designed for:  
  - `tree` → Used for *Random Forest, Boosted Trees, and k-Nearest Neighbors* models.  
  - `lm` → Used for *Logistic Regression, Elastic Net, and other regression models*.  
- `<recipe>` indicates whether the recipe is **basic** (minimal preprocessing) or **complex** (advanced feature transformations).  

#### Recipe Files 
- `hhs_recipe_tree_basic.rda` – Preprocessing recipe for tree-based models (basic).  
- `hhs_recipe_tree_complex.rda` – Preprocessing recipe for tree-based models (complex).  
- `hhs_recipe_lm_basic.rda` – Preprocessing recipe for regression-based models (basic).  
- `hhs_recipe_lm_complex.rda` – Preprocessing recipe for regression-based models (complex).  

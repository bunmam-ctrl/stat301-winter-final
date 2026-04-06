## Descrition

This project, developed as the **final project for STAT 301-2**, builds a **predictive model** to classify home health agencies as `High` or `Low` quality based on key performance indicators like patient outcomes, service effectiveness, and compliance. Using **machine learning classification**, it analyzes agency characteristics, care metrics, safety compliance, and financial performance to provide actionable insights.  

Leveraging the **CMS Home Health Care Agencies Dataset**, the model transforms the **1–5 star rating** into a binary classification (`High` vs. `Low`). It serves **patients**, **healthcare providers**, and **regulators**, enhancing transparency and supporting data-driven healthcare decisions.

### Directories

- [`EDA_predict_var/`](EDA_predict_var/): Stores EDA plots of predictor variables after data splitting.
- [`R_script/`](R_script/): Includes all R scripts, from exploratory data analysis to model-building scripts.  
- [`data/`](data/): Contains raw data (`.csv`), transformed data (`.rds`), and a tidy version of transformed data (`.rds`), along with the codebook.  
- [`data_splits/`](data_splits/): Contains the setup of data—initial split, resamples, and controls for fitting to resamples.  
- [`final_fitting/`](final_fitting/): Stores outputs of the final model fitting and evaluation.  
- [`interact_predict/`](interact_predict/): Holds interaction plots showing one predictor against the remaining predictors.  
- [`memos/`](memos/): Contains all `.qmd` and `.html` files for two progress memos.  
- [`metrics_table/`](metrics_table/): Stores tables summarizing the performance metrics of the best-performing models across different types.  
- [`predictor_check/`](predictor_check/): Contains outputs for checking missing values in predictor variables.  
- [`recipes/`](recipes/): Stores all preprocessing and feature engineering objects.  
- [`target_check/`](target_check/): Stores all outputs from target exploratory data analysis (EDA).  
- [`tune_plot/`](tune_plot/): Contains all tuning plots from the model tuning process.  
- [`workflow_fitting/`](workflow_fitting/): Holds results from training and fitting models to resamples.  

### Project Reports & Summaries

- `Tran_Chau_final_report.qmd` & `Tran_Chau_final_report.html`
  - Comprehensive report detailing methodology, data analysis, model results, and conclusions.

- `Tran_Chau_executive_summary.qmd` & `Tran_Chau_executive_summary.html`
  - A high-level overview summarizing key insights, findings, and implications of the study.

- `Tran_Chau_appendix.qmd` 
  - Supplementary materials, additional analyses, extended tables, and supporting visualizations.
  -`Tran_Chau_appendix.html` exceeds GitHub's 100MB upload limit and cannot be uploaded directly.

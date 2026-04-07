# Final Project ----
# Stat 301-2
# Plotting tuning parameter (complex recipe)

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)

## Elastic net----
### Accuracy
load(here("workflow_fitting/complex/en_tune_complex.rda"))

select_best(en_tune_complex, metric = "accuracy")

en_accuracy_complex <-  autoplot(en_tune_complex, 
                               metric = "accuracy") + 
  theme_minimal() +
  labs(
    title = "Effect of Regularization and \nLasso Proportion on Model Accuracy",
    x = "Amount of Regularization",
    y = "Accuracy") +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 16, face= "bold", hjust = 0.5),
    legend.text = element_text(size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
  )


ggsave(en_accuracy_complex,
       filename = here("tune_plot/complex/en_accuracy_complex.png"))



### ROC-AUC
select_best(en_tune_complex, metric = "roc_auc")
en_roc_complex <-  autoplot(en_tune_complex, 
                          metric = "roc_auc") + 
  theme_minimal() +
  labs(
    title = "Effect of Regularization and \nLasso Proportion on ROC-AUC",
    x = "Amount of Regularization",
    y = "ROC-AUC")+
  theme(
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 16, face= "bold", hjust = 0.5),
    legend.text = element_text(size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
  )

ggsave(en_roc_complex,
       filename = here("tune_plot/complex/en_roc_complex.png"))


## K-nearest Neighbors----
load(here("workflow_fitting/complex/knn_tune_complex.rda"))
### Accuracy
select_best(knn_tune_complex, metric = "accuracy")
knn_accuracy_complex <-  autoplot(knn_tune_complex, 
                                metric = "accuracy") +
  theme_minimal()+
  labs(
    title = "Effect of k on KNN Model Accuracy",
    x = "Number of Nearest Neighbors (k)",
    y = "Accuracy"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 16, hjust = 0.5),
    legend.text = element_text(size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
  )

ggsave(knn_accuracy_complex,
       filename = here("tune_plot/complex/knn_accuracy_complex.png"))


### ROC-AUC
select_best(knn_tune_complex, metric = "roc_auc")
knn_roc_complex <-  autoplot(knn_tune_complex, 
                           metric = "roc_auc") + 
  theme_minimal() +
  labs(
    title = "Effect of k on KNN Model ROC-AUC",
    x = "Number of Nearest Neighbors (k)",
    y = "ROC-AUC"
  )+
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 16, hjust = 0.5),
    legend.text = element_text(size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
  )

ggsave(knn_roc_complex,
       filename = here("tune_plot/complex/knn_roc_complex.png"))



## Random forest----
### Accuracy
load(here("workflow_fitting/complex/rf_tune_complex.rda"))
select_best(rf_tune_complex, metric = "accuracy")
rf_accuracy_complex <-  autoplot(rf_tune_complex, 
                               metric = "accuracy") + 
  theme_minimal() +
  labs(
    title = " Effect of Minimal Node Size and Randomly \nSelected Predictors on Accuracy in Random Forest",
    x =  "Number of Randomly Selected Predictors",
    y = "Accuracy"
  )+
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    strip.text = element_text(size = 16, face = "italic"),
    legend.title = element_text(size = 16, hjust = 0.5),
    legend.text = element_text(size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
  )

ggsave(rf_accuracy_complex,
       filename = here("tune_plot/complex/rf_accuracy_complex.png"))



### ROC-AUC
select_best(rf_tune_complex, metric = "roc_auc")
rf_roc_complex <-  autoplot(rf_tune_complex, 
                          metric = "roc_auc") + 
  theme_minimal() +
  labs(
    title = " Effect of Minimal Node Size and Randomly \nSelected Predictors on ROC-AUC in Random Forest",
    x =  "Number of Randomly Selected Predictors",
    y = "ROC-AUC"
  )+
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    strip.text = element_text(size = 16, face = "italic"),
    legend.title = element_text(size = 16, hjust = 0.5),
    legend.text = element_text(size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
  )

ggsave(rf_roc_complex,
       filename = here("tune_plot/complex/rf_roc_complex.png"))



## Boosted tree----
load(here("workflow_fitting/complex/bt_tune_complex.rda"))
select_best(bt_tune_complex, metric = "accuracy")
bt_accuracy_complex <-  autoplot(bt_tune_complex, 
                               metric = "accuracy") +
  theme_minimal() +
  labs(
    title = "Effect of Different Tuning Hyperparameters \non Accuracy in Boosted Tree Models",
    x = "Learning Rate",
    y = "Accuracy") +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    strip.text = element_text(size = 12, face = "italic"),
    legend.title = element_text(size = 16, hjust = 0.5),
    legend.text = element_text(size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
  )


ggsave(bt_accuracy_complex,
       filename = here("tune_plot/complex/bt_accuracy_complex.png"),
       width = 15)

select_best(bt_tune_complex, metric = "roc_auc")
bt_roc_complex <-  autoplot(bt_tune_complex, 
                          metric = "roc_auc") +
  theme_minimal() +
  labs(
    title = "Effect of Different Tuning Hyperparameters \non ROC-AUC in Boosted Tree Models",
    x = "Learning Rate",
    y = "ROC-AUC"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    strip.text = element_text(face = "italic", size = 12),
    legend.title = element_text(size = 16, hjust = 0.5),
    legend.text = element_text(size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
  )
  

ggsave(bt_roc_complex,
       filename = here("tune_plot/complex/bt_roc_complex.png"),
       width = 15)

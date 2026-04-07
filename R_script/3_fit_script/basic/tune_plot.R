# Final Project ----
# Stat 301-2
# Plotting tuning parameter (basic recipe)

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)

## Elastic net----
### Accuracy
load(here("workflow_fitting/basic/en_tune_basic.rda"))
select_best(en_tune_basic, metric = "accuracy")
en_accuracy_basic <-  autoplot(en_tune_basic, 
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


ggsave(en_accuracy_basic,
       filename = here("tune_plot/basic/en_accuracy_basic.png"))



### ROC-AUC
select_best(en_tune_basic, metric = "roc_auc")
en_roc_basic <-  autoplot(en_tune_basic, 
                   metric = "roc_auc") + 
  theme_minimal() +
  labs(
    title = "Effect of Regularization and \nLasso Proportion on ROC-AUC",
    x = "Amount of Regularization",
    y = "ROC-AUC")+
  theme(
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
  )

ggsave(en_roc_basic,
       filename = here("tune_plot/basic/en_roc_basic.png"))


## K-nearest Neighbors----
### Accuracy
load(here("workflow_fitting/basic/knn_tune_basic.rda"))
select_best(knn_tune_basic, metric = "accuracy")
knn_accuracy_basic <-  autoplot(knn_tune_basic, 
                         metric = "accuracy")  +
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

ggsave(knn_accuracy_basic,
       filename = here("tune_plot/basic/knn_accuracy_basic.png"))



### ROC-AUC
select_best(knn_tune_basic, metric = "roc_auc")
knn_roc_basic <-  autoplot(knn_tune_basic, 
                        metric = "roc_auc")+ 
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

ggsave(knn_roc_basic,
       filename = here("tune_plot/basic/knn_roc_basic.png"))


## Random forest----
### Accuracy 
load(here("workflow_fitting/basic/rf_tune_basic.rda"))
select_best(rf_tune_basic, metric = "accuracy")
rf_accuracy_basic <-  autoplot(rf_tune_basic, 
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

ggsave(rf_accuracy_basic,
       filename = here("tune_plot/basic/rf_accuracy_basic.png"))




### ROC-AUC
select_best(rf_tune_basic, metric = "roc_auc")
rf_roc_basic <-  autoplot(rf_tune_basic, 
                           metric = "roc_auc")+
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

ggsave(rf_roc_basic,
       filename = here("tune_plot/basic/rf_roc_basic.png"))


## Boosted tree----
### Accuracy
load(here("workflow_fitting/basic/bt_tune_basic.rda"))
select_best(bt_tune_basic, metric = "accuracy")
bt_accuracy_basic <-  autoplot(bt_tune_basic, 
                               metric = "accuracy") +
  theme_minimal() +
  labs(
    title = "Effect of Different Tuning Hyperparameters \non Accuracy in Boosted Tree Models",
    x = "Learning Rate",
    y = "Accuracy") +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    strip.text = element_text(face = "italic", size = 12),
    legend.title = element_text(size = 16, hjust = 0.5),
    legend.text = element_text(size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
  )


ggsave(bt_accuracy_basic,
       filename = here("tune_plot/basic/bt_accuracy_basic.png"),
       width = 15)

### ROC-AUC
select_best(bt_tune_basic, metric = "roc_auc")
bt_roc_basic <-  autoplot(bt_tune_basic, 
                          metric = "roc_auc")+
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


ggsave(bt_roc_basic,
       filename = here("tune_plot/basic/bt_roc_basic.png"),
       width = 15, 
       units = "in")




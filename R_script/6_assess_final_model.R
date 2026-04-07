# Final Project ----
# Stat 301-2
# Assess final model

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(patchwork)

# handle common conflicts
tidymodels_prefer()


# load fitting/dataset
load(here("final_fitting/hhs_final_fit.rda"))
load(here("data_splits/hhs_test.rda"))
load(here("data_splits/my_metrics.rda"))


# Assess the model performance ----
## Prediction ----
hhs_pred <-  hhs_test|>
  select(quality_star_category)|>
  bind_cols(
    predict(hhs_final_fit, 
            new_data = hhs_test),
    predict(hhs_final_fit,
            new_data = hhs_test,
            type = "prob")
  )

##  Metrics ----
hhs_final_metrics <- hhs_pred|>
  my_metrics(truth = quality_star_category, 
             estimate  = .pred_class, 
              .pred_High)|>
  filter(.metric %in% c("accuracy", "roc_auc"))|>
  mutate(.metric = case_when(
    .metric == "accuracy" ~ "Accuracy",
    .metric == "roc_auc" ~ "ROC-AUC"
  ))|>
  select(.metric, .estimate)|>
  rename(
    "Performance Metric" = .metric,
    "Estimate Value" = .estimate)


save(hhs_final_metrics,
     file = here("final_fitting/hhs_final_metrics.rda")
)


## ROC_AUC curve ----
### High quality
high_roc_auc <-roc_curve(hhs_pred,quality_star_category , .pred_High)|> 
  ggplot(aes(x = (1- specificity), y = sensitivity))+ 
  geom_line(color = "#E41A1C", 
            linewidth = 1.2) +  
  geom_abline(linetype = "dashed", 
              color = "darkgray",
              linewidth = 1.2) + 
  theme_minimal() + 
  labs(
      title = "ROC Curve for Predicting \nHigh Quality of Patient Care Categor",
      subtite = "(Boosted Tree Model, trees = 2000, mtry = 5, min_n = 2)",
      x = "False Positive Rate",
      y = "True Positive Rate"
  ) +
  theme(
    plot.title = element_text(face = "bold", 
                              size = 18,
                              hjust = 0.5),
    plot.subtitle = element_text(face = "italic", 
                                 size = 16 ),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 10, hjust = 1),
    axis.text.y = element_text(size = 10)
  )


  
ggsave(high_roc_auc,
       filename = "final_fitting/high_roc_auc.png")



  

## Confusion matrix ----
hhs_conf_mat <- hhs_pred|>
  conf_mat(quality_star_category, .pred_class)|>
  autoplot(type = "heatmap")+
  labs(
    title = "Confusion Matrix for Quality of Patient Care Star Category",
    subtitle = "(Boosted Tree Model, trees = 2000, mtry = 5, min_n = 2)",
    x = "Actual Quality Star Category",
    y = "Predicted Quality Star Category"
  )+
  scale_fill_gradient(low = "#EEEED1", high = "#87CEEB") +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, face = "italic", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 10, hjust = 1),
    axis.text.y = element_text(size = 10)
  )

### Save the figure
ggsave(hhs_conf_mat, 
       filename = "final_fitting/hhs_conf_mat.png")


  
# Final Project ----
# Stat 301-2
# EDA predictors

# load packages ----
library(tidyverse)
library(here)
library(patchwork)

# load training data ----
load(here("data_splits/hhs_train.rda"))

# Timeliness and Care Delivery Measures----
## timely_care histogram
timely_care_dis <- hhs_train|>
  ggplot(aes(x = timely_care))+
  geom_histogram(color = "white", 
                 fill = "#2C3E50",  
                 alpha = 0.8)+
  theme_minimal()+
  coord_cartesian(xlim = c(50, 100),
                  ylim = c(0, 3000))+
  labs(
    title = "Timely Patient Care (%)",
    x = NULL,
    y = NULL
    
  )+
  theme(
    plot.title = element_text(size = 16, 
                                 face = "italic",
                                 hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )

## flu_shot histogram
flu_shot_dis <- hhs_train|>
  ggplot(aes(x = flu_shot_check))+
  geom_histogram(color = "white", 
                 fill = "#2C3E50",  
                 alpha = 0.8)+
  theme_minimal()+
  coord_cartesian(xlim = c(0, 100),
                  ylim = c(0, 450))+
  labs(
    title = "Flu Shot Assessment Rate (%)",
    x = NULL,
    y = NULL
    
  )+
  theme(
    plot.title = element_text(size = 16,
                              hjust = 0.5, 
                              face = "italic"),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 10, hjust = 1),
    axis.text.y = element_text(size = 10)
  )


## combine plot
delivery_dis <- timely_care_dis / flu_shot_dis +
  plot_annotation( title = "Distribution of Timeliness and Care Delivery Measures",
                   theme = theme(
                     plot.title = element_text(face = "bold", 
                                               size = 18,
                                               hjust = 0.5)
                   ))

## Write plot
ggsave(delivery_dis, 
       filename = here("EDA_predict_var/delivery_dis.png"))

# Patient Improvement and Functional Outcomes----
## Table that include improve metrics
improve_metrics <- hhs_train|>
  select(starts_with("improve"))|>
  pivot_longer(
    cols = starts_with("improve"),
    names_to = "category",
    values_to = "percentage"
  )|>
  mutate(category = str_remove(category, "improve_"),
         category = factor(category))

## Box plot for improve metrics (without skin integrity)
improve_labels <- c(
  "bathing" = "Improved Bathing (%)",
  "breathing" = "Improved Breathing (%)",
  "walking" = "Improved Walking (%)",
  "bed_transfer" = "Improved Bed Transfer (%)",
  "medicine" = "Improved Medication Use (%)"
)

improve_patient_dis <- improve_metrics|>
  filter(category != "skin")|>
  ggplot(aes(x = percentage,
             y = fct_reorder(category, percentage, 
                             .fun = mean, .na_rm = TRUE),
             fill = category)) +
  geom_boxplot(alpha = 0.5, outlier.shape = 16, outlier.size = 2) +
  scale_fill_manual(values = c(
    "bathing" = "#E57373",
    "breathing" = "#81C784",
    "walking" = "#BA68C8",
    "bed_transfer" = "#FBC02D",
    "medicine" = "#64B5F6"
  ), labels = improve_labels) +
  labs(
    title = "Distribution of Patient \nFunctional Improvement",
    x = "Improvement Percentage (%)",
    y = NULL,
    fill = "Patient Outcome \nCategories"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, face = "italic", hjust = 0.5),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 16, hjust = 0.5),
    legend.text = element_text(size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_blank()
  )


## Due to the skewness, skin integrity boxplot is separated
improve_skin_dis <- improve_metrics|>
  filter(category == "skin")|>
  ggplot(aes(x = percentage))+
  geom_boxplot(alpha = 0.5, 
               outlier.shape = 16,
               outlier.size = 2,
               fill = "#4DB6AC") +
  theme_minimal() +
  labs(
    title = "Distribution of Skin Integrity \nChanges After Acute Care",
    x = "Post-Acute Care Skin Integrity (%)"
    ) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, face = "italic", hjust = 0.5),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_blank()
  )

## Combine plot
improve_dis <- improve_patient_dis/improve_skin_dis+
  plot_layout(heights = c (2,1))

## Write plot
ggsave(improve_dis,
       filename = here("EDA_predict_var/improve_dis.png"))

# Medication and Safety Compliance----
## med_issue_resolved
med_res_dis <- hhs_train|>
  ggplot(aes(x = med_issue_resolved))+
  geom_histogram(color = "white", 
                 fill = "#2C3E50",  
                 alpha = 0.8)+
  theme_minimal()+
  labs(
    x = "Timely Physician-Recommended \nActions (%)",
    y = NULL
  )+
  theme(
  plot.title = element_text(size = 16,
                            hjust = 0.5, 
                            face = "italic"),
  axis.title = element_text(size = 14),
  axis.text.x = element_text(size = 10, hjust = 1),
  axis.text.y = element_text(size = 10)
)


###fall_injury_rate
fall_dis <- hhs_train|>
  ggplot(aes(x = fall_injury_rate))+
  geom_histogram(color = "white", 
                 fill = "#2C3E50",  
                 alpha = 0.8)+
  theme_minimal()+
  labs(
    x = "Residents with Falls \nResulting in Major Injury (%)",
    y = NULL
  )+
  theme(
    plot.title = element_text(size = 16,
                              hjust = 0.5, 
                              face = "italic"),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 10, hjust = 1),
    axis.text.y = element_text(size = 10)
  )

## Combine plot
med_safe_dis <- med_res_dis + fall_dis+
  plot_annotation(title = "Distribution of Medication and Safety Compliance Measures",
                   theme = theme(
                     plot.title = element_text(face = "bold", 
                                               size = 18,
                                               hjust = 0.5)
                   ))

## Write plot
ggsave(med_safe_dis, 
       filename = here("EDA_predict_var/med_safe_dis.png"))

# Care Coordination and Information Transfer----
## Histogram of discharge_score
discharge_score_dis <- hhs_train|> 
  ggplot(aes(x = discharge_score))+
  geom_histogram(color = "white", 
                 fill = "#2C3E50",  
                 alpha = 0.8)+
  theme_minimal()+
  labs(
    subtitle = "Discharge Function Scores",
    x = "Score",
    y = NULL
  )+
  theme(
    plot.subtitle = element_text(size = 16, face = "italic"),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 10, hjust = 1),
    axis.text.y = element_text(size = 10)
  )

## Table that include Information Transfer 
transfer_to_metrics <- hhs_train|>
  select(starts_with("transfer_to"))|>
  pivot_longer(
    cols = starts_with("transfer_to"),
    names_to = "category",
    values_to = "percentage"
  )|>
  mutate(category = str_remove(category, "transfer_to_"),
         category = factor(category))


## Box plot of transform metrics
transform_label <-  c(
  "provider" = "To provider",
  "patient" = "To patient"
  )

transform_to_dis <- transfer_to_metrics|>
  ggplot(aes(x = percentage,
             y = fct_reorder(category, percentage, 
                             .fun = mean, .na_rm = TRUE),
             fill = category)) +
  geom_boxplot(alpha = 0.5, outlier.shape = 16, outlier.size = 2)+
  scale_fill_manual(values = c(
    "provider" =  "#81C784",
    "patient" ="#BA68C8"), 
    labels = transform_label)+
  theme_minimal()+
  labs(
    subtitle = "Health Information Transfer",
    x = "Percentage (%)",
    y = NULL,
    fill = "Type of Health \nInformation Transfer"
  ) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(size = 16, face = "italic"),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 16, hjust = 0.5),
    legend.text = element_text(size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_blank()
  )

## Combine plot 
care_trans_dis <- discharge_score_dis/transform_to_dis + 
  plot_annotation( title = "Distribution of Care Coordination and Information Transfer",
                   theme = theme(
                     plot.title = element_text(face = "bold", 
                                               size = 18,
                                               hjust = 0.5)
                   ))

## Write plot
ggsave(care_trans_dis, 
       filename = here("EDA_predict_var/care_trans_dis.png"))

# Performance Metrics----
## Table that include Performance Metric 
perform_metrics <- hhs_train|>
  select(ends_with("_rate"))|>
  pivot_longer(
    cols = ends_with("_rate"),
    names_to = "category",
    values_to = "rate"
  )|>
  mutate(category = str_remove(category, "_rate"),
         category = factor(category))|>
  filter(category != "fall_injury")

## Box plot of Performance metrics 
### PPR distribution
ppr_dis <- perform_metrics|>
  filter(category == "ppr")|>
  ggplot(aes(x = rate)) +
  geom_boxplot(alpha = 0.5, outlier.shape = 16, 
               outlier.size = 2,  fill = "#81C784")+ 
  labs(
    subtitle = "Potentially preventable readmissions",
    x = "Rate",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(size = 16, face = "italic"),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 16, hjust = 0.5),
    legend.text = element_text(size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_blank()
  )


### PPH distribution
pph_dis <- perform_metrics|>
  filter(category == "pph")|>
  ggplot(aes(x = rate)) +
  geom_boxplot(alpha = 0.5, outlier.shape = 16, 
               outlier.size = 2, fill = "#64B5F6")+ 
  labs(
    subtitle = "Potentially preventable hospitalization",
    x = "Rate",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(size = 16, face = "italic"),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 16, hjust = 0.5),
    legend.text = element_text(size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_blank()
  )

### DTC distribution  (separate because it skews the data, ≠ x-scale)
dtc_dis <- perform_metrics|>
  filter(category == "dtc")|>
  ggplot(aes(x = rate)) +
  geom_boxplot(alpha = 0.5, outlier.shape = 16, 
               outlier.size = 2,fill = "#BA68C8")+ 
  labs(
    subtitle = "Discharge to community",
    x = "Rate",
    y = NULL,
    fill = "Metrics Categories"
  ) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(size = 16, face = "italic"),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 16, hjust = 0.5),
    legend.text = element_text(size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_blank()
  )
  
## Combine plot 
perform_dis <- ppr_dis/pph_dis/dtc_dis +
  plot_annotation( title = "Distribution of Performance Metrics ",
                   theme = theme(
                     plot.title = element_text(face = "bold", 
                                               size = 18,
                                               hjust = 0.5)
                   ))

## Write plot
ggsave(perform_dis, 
       filename = here("EDA_predict_var/perform_dis.png"))


# Financial metric ----
## medicare index
medicare_index_dis <- hhs_train|> 
  ggplot(aes(x = med_issue_resolved))+
  geom_histogram(color = "white", fill = "#2C3E50",  
                 alpha = 0.8, bins = 50)+
  theme_minimal()+
  labs(
    title = "Distribution of Medicare Spending per\n Episode of Care Relative to the National Average",
    x = "Cost Index Relative to National Average",
    y = NULL
  )+
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 10, hjust = 1),
    axis.text.y = element_text(size = 10)
  )

ggsave(medicare_index_dis, 
       filename = here("EDA_predict_var/medicare_index_dis.png"))

### Episodes (before)
episode_before_dis <- hhs_train|> 
  ggplot(aes(x = num_episodes))+
  geom_histogram(color = "white", 
                 fill = "#2C3E50",  
                 alpha = 0.8, 
                 bins = 50)+
  theme_minimal()+
  labs(
    subtitle = "Original Scale",
    x = "Count of Episodes",
    y = NULL
  )+
  theme(
    plot.subtitle = element_text(size = 16, face = "italic"),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 10, hjust = 1),
    axis.text.y = element_text(size = 10)
  )

### Episodes after log 10 
episode_after_dis <- hhs_train|> 
  ggplot(aes(x = log10(num_episodes)))+
  geom_histogram(color = "white", 
                 fill = "#2C3E50",  
                 alpha = 0.8,
                 bins = 50)+
  theme_minimal()+
  labs(
    subtitle = "After Log Transformation",
    x = "Log Count of Episodes",
    y = NULL
  )+
  theme(
    plot.subtitle = element_text(size = 16, face = "italic"),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 10, hjust = 1),
    axis.text.y = element_text(size = 10)
  )


finance_dis <-  (episode_before_dis / episode_after_dis) +
  plot_annotation(title = "Distribution of Episodes Used in Medicare Cost Calculations",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 18,
                                              hjust = 0.5)))

### Write plot
ggsave(finance_dis, 
       filename = here("EDA_predict_var/finance_dis.png"))



# Agency and Service Character -----
## There is no interaction with the categorical variable 

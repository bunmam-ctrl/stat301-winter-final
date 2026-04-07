# Final Project ----
# Stat 301-2
# Predictor check

## Load library ----
library(tidyverse)
library(naniar)
library(here)
library(patchwork)

## Load data ----
hhs_provider <- read_rds("data/hhs_provider.rds")


# Make NA table ----
## Agency and Service Characteristics ----
agency_service  <- hhs_provider|>
  select(ownership_type,nursing_services, physical_therapy, 
         occupational_therapy, speech_pathology, medical_social, home_aide)
na_agency_service <- agency_service|>
  gg_miss_var()+
  labs(
    title = "Missing Values in Agency \nand Service Characteristics",
    x = NULL)+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )


## Write the graph -----
ggsave(na_agency_service,
       file = here("predictor_check/na_agency_service.png"))
## Timeliness and Care Delivery Measures ----
timely_delivery <- hhs_provider|>
  select(timely_care, flu_shot_check)

na_timely_delivery <- timely_delivery|>
  gg_miss_var()+
  labs(
    title = "Missing Values in Timeliness \nand Care Delivery Measures",
    x = NULL) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )


## Write the graph -----
ggsave(na_timely_delivery,
       file = here("predictor_check/na_timely_delivery.png"))

## Patient Improvement and Functional Outcomes ----
improve_outcome <- hhs_provider|>
  select(improve_walking, improve_bed_transfer, improve_bathing,
         improve_breathing, improve_medicine, improve_skin)

na_improve_outcome <- improve_outcome|>
  gg_miss_var()+
  labs(
    title = "Missing Values in Patient Improvement \nand Functional Outcomes",
    x = NULL) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 20, face = "bold" , hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )


## Write the graph -----
ggsave(na_improve_outcome,
       file = here("predictor_check/na_improve_outcome.png"))

## Medication and Safety Compliance ----
medication_safety <- hhs_provider|>
  select(med_issue_resolved, fall_injury_rate)

na_medication_safety <- medication_safety|>
  gg_miss_var()+
  labs(
   title = "Missing Values in Medication \nand Safety Compliance",
   x = NULL) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )


## Write the graph -----
ggsave(na_medication_safety,
       file = here("predictor_check/na_medication_safety.png"))

## Care Coordination and Information Transfer ----
coordination_information <- hhs_provider|>
  select(discharge_score, transfer_to_provider, transfer_to_patient)

na_coordination_information <- coordination_information|>
  gg_miss_var()+
  labs(
    title = "Missing Values in Care Coordination \nand Information Transfer",
    x = NULL) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )


## Write the graph -----
ggsave(na_coordination_information,
       file = here("predictor_check/na_coordination_information.png"))

## Financial and Performance Metrics ----
finance_perform <- hhs_provider|>
  select(dtc_rate, ppr_rate, pph_rate, medicare_spending_index, num_episodes)

na_finance_perform <- finance_perform|>
  gg_miss_var()+
  labs(
    title = "Missing Values in Financial \nand Performance Metrics",
    x = NULL) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )


## Write the graph -----
ggsave(na_finance_perform,
       file = here("predictor_check/na_finance_perform.png"))



# Summary table of hhs_provider  ----
hhs_summary <- hhs_provider|>
  summarize(
    observations_number = nrow(hhs_provider),
    variables_number = ncol(hhs_provider),
    numerical_variables_number = sum(
      sapply(hhs_provider, is.numeric)
    ),
    categorical_variables_number = sum(
      sapply(hhs_provider, is.character)
    ),
    missing_values_total_number = sum(
      is.na(hhs_provider)
    ),
    percentage_missing_number = round(
      sum(
        is.na(hhs_provider)) / (nrow(hhs_provider) * ncol(hhs_provider))
      * 100)
  )|>
  rename(
    "Observation" = observations_number,
    "Variables" = variables_number,
    "Numerical variables" =  numerical_variables_number,
    "Categorical variables" =  categorical_variables_number,
    "Missing values" =  missing_values_total_number,
    "Missing values percentage" = percentage_missing_number
  )|>
  pivot_longer(
    cols = everything(),
    names_to = "Metric",
    values_to = "Number"
  )|>
  mutate(
    Number = as.integer(Number)
  )

### Write out table 
save(hhs_summary, 
     file = here("predictor_check/hhs_summary.rda"))

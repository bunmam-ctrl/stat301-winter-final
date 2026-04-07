# Final Project ----
# Stat 301-2
# Data tidy

## Load library ----
library(tidyverse)

## Load data ----
hhs_provider <- read_csv("data/HHS_provider_raw.csv", na = "-")|>
# Select relevant columns 
  select(-contains(c("Numerator","Denominator", "Footnote", "Observed Rate", 
                     "Categorization", "Limit"))
  )|>
# Change names
  rename(
    "quality_star_rating" = "Quality of patient care star rating",
    "ownership_type" = "Type of Ownership",
    "nursing_services" =  "Offers Nursing Care Services",
    "physical_therapy" = "Offers Physical Therapy Services",
    "occupational_therapy" = "Offers Occupational Therapy Services",
    "speech_pathology" = "Offers Speech Pathology Services",
    "medical_social" = "Offers Medical Social Services",
    "home_aide" = "Offers Home Health Aide Services",
    "timely_care" = "How often the home health team began their patients' care in a timely manner",
    "flu_shot_check" = "How often the home health team determined whether patients received a flu shot for the current flu season",
    "improve_walking" = "How often patients got better at walking or moving around",
    "improve_bed_transfer" = "How often patients got better at getting in and out of bed",
    "improve_bathing" = "How often patients got better at bathing",
    "improve_breathing" = "How often patients' breathing improved",
    "improve_medicine" = "How often patients got better at taking their drugs correctly by mouth",
    "improve_skin" = "Changes in skin integrity post-acute care: pressure ulcer/injury",
    "med_issue_resolved" = "How often physician-recommended actions to address medication issues were completely timely",
    "fall_injury_rate" = "Percent of Residents Experiencing One or More Falls with Major Injury",
    "discharge_score" = "Discharge Function Score",
    "transfer_to_provider" = "Transfer of Health Information to the Provider",
    "transfer_to_patient" = "Transfer of Health Information to the Patient",
    "medicare_spending_index" = "How much Medicare spends on an episode of care at this agency, compared to Medicare spending across all agencies nationally",
    "num_episodes" = "No. of episodes to calc how much Medicare spends per episode of care at agency, compared to spending at all agencies (national)",
    "dtc_rate" =  "DTC Risk-Standardized Rate",
    "ppr_rate" =  "PPR Risk-Standardized Rate",
    "pph_rate" =  "PPH Risk-Standardized Rate",
    )|>
  ## Redefine the target variable ----
  ### change if quality_star_rating ≥ 3.5 (High); ≤ 3.5 (Low)
  mutate (
    quality_star_category = if_else(quality_star_rating >= 3.5,
                                    "High", "Low", missing = NULL),
    ownership_type = factor(ownership_type),
    nursing_services = factor(nursing_services),
    physical_therapy = factor(physical_therapy),
    occupational_therapy = factor(occupational_therapy),
    speech_pathology = factor(speech_pathology),
    medical_social = factor(medical_social),
    home_aide = factor(home_aide)
  )


# Remove NA from target variable and drop quality_star_rating
hhs_tidy <- hhs_provider|>
  drop_na(quality_star_category)|>
  mutate(quality_star_category = factor(quality_star_category, 
                                        levels = c("High", "Low")))|>
  select(-quality_star_rating)
  

# write out modified data(s) ----
write_rds(hhs_provider, 
          file = here::here("data/hhs_provider.rds"))
write_rds(hhs_tidy, 
          file = here::here("data/hhs_tidy.rds"))

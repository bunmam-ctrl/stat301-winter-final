# Final Project ----
# Stat 301-2
# Interaction predictor

# load packages ----
library(tidyverse)
library(here)
library(patchwork)

# load training data ----
load(here("data_splits/hhs_train.rda"))

# Creating function to check interaction----
interact_hhs <- function(df, x, y, z) {
  ggplot(df, aes(x = {{ x }}, y = {{ y }})) +
    geom_point(aes(color = {{ z }}), 
               alpha = 0.4, 
               size = 2) +
    geom_smooth(method = "lm", 
                se = FALSE,
                formula = y~x,
                color = "#A65628", 
                linetype = "dashed", 
                linewidth = 3.0)+
    facet_grid(rows = vars({{ z }})) +
    scale_color_brewer(palette = "Accent") +
    theme_minimal()+
    theme(
      legend.position = "none",
      strip.text = element_text(size = 32, face = "italic"),
      axis.text.y.right = element_blank(),
      plot.subtitle =element_text(size = 40, face = "italic"),
      axis.text.x = element_text(size = 25),
      axis.text.y = element_text(size = 25)
    )
  }

# timely_care ----
## timely_care vs flu_shot_check ----
### There is no interaction
timely_flu <- interact_hhs(df = hhs_train, 
                           x = timely_care, 
                           y = flu_shot_check,
                           z = quality_star_category) +
  labs(
    subtitle = "Flu Shot Status Checked \nby Home Health Team (%)",
    x = NULL,
    y = NULL
  )


## timely_care vs improve_walking ----
### There is interaction
timely_walk <- interact_hhs(df = hhs_train, 
                            x = timely_care, 
                            y = improve_walking,
                            z = quality_star_category)+
  labs(
    subtitle = "Walking Ability (%)",
    y = NULL,
    x = NULL
  )


## timely_care vs improve_bed_transfer ----
### There is interaction
timely_bed <- interact_hhs(df = hhs_train, 
                          x = timely_care, 
                          y = improve_bed_transfer,
                          z = quality_star_category)+
  labs(
    subtitle = "Bed Transfer (%)",
    y = NULL,
    x = NULL
  )




## timely_care vs improve_bathing ----
### There is interaction 
timely_bath <- interact_hhs(df = hhs_train, 
                            x = timely_care, 
                            y = improve_bathing,
                            z = quality_star_category)+
  labs(
    subtitle = "Bathing Ability (%)",
    y = NULL,
    x = NULL
  )

## timely_care vs improve_breathing ----
### There is interaction 
timely_breath <- interact_hhs(df = hhs_train, 
                            x = timely_care, 
                            y = improve_breathing,
                            z = quality_star_category) +
  labs(
    subtitle = "Breathing Ability (%)",
    y = NULL,
    x = NULL
  )


## timely_care vs improve_medicine ----
### There is interaction 
timely_medicine <- interact_hhs(df = hhs_train, 
                                x = timely_care, 
                                y = improve_medicine,
                                z = quality_star_category) +
  labs(
    subtitle = "Medication Adherence (%)",
    y = NULL,
    x = NULL
  )
  



## timely_care vs improve_skin ----
### There is interaction 
timely_skin <- interact_hhs(df = hhs_train, 
                            x = timely_care, 
                            y = improve_skin,
                            z = quality_star_category) +
  labs(
    subtitle = "Skin Integrity (%)",
    y = NULL, 
    x = NULL
  )

## timely_care vs med_issue_resolved  ----
### There is no interaction 
timely_resolved <- interact_hhs(df = hhs_train, 
                                x = timely_care, 
                                y = med_issue_resolved,
                                z = quality_star_category) +
  labs(
    subtitle = "Timely Completion \nof Medication Actions (%)",
    y = NULL,
    x = NULL
  )


## timely_care vs fall_injury_rate  ----
### There is interaction 
timely_injury <- interact_hhs(df = hhs_train, 
                              x = timely_care, 
                              y = fall_injury_rate,
                              z = quality_star_category) +
  labs(
    subtitle = "Residents with Major Injury \nfrom Falls (%)", 
    x = NULL,
    y = NULL
  )

## timely_care vs discharge_score ----
### There is interaction 
timely_discharge <- interact_hhs(df = hhs_train, 
                                 x = timely_care, 
                                 y = discharge_score,
                                 z = quality_star_category) +
  labs(
    subtitle = "Discharge Function Score",
    y = NULL, 
    x = NULL
  ) 

## timely_care vs transfer_to_provider ----
### There is no interaction 
timely_provider <- interact_hhs(df = hhs_train, 
                                x = timely_care, 
                                y = transfer_to_provider,
                                z = quality_star_category) +
  labs(
    subtitle = "Transfer of Health Information \nto the Provider (%)",
    y = NULL, 
    x = NULL
  )
  


## timely_care vs transfer_to_patient ----
### There is no interaction 
timely_patient <- interact_hhs(df = hhs_train, 
                               x = timely_care, 
                               y = transfer_to_patient,
                               z = quality_star_category) +
  labs(
    subtitle = "Transfer of Health Information \nto the Patient (%)",
    y = NULL, 
    x = NULL
  )

## timely_care vs dtc_rate  ----
### There is no interaction 
timely_dtc <- interact_hhs(df = hhs_train, 
                          x = timely_care, 
                          y = dtc_rate,
                          z = quality_star_category) +
  labs(
    subtitle = "Discharge to Community Rate",
    y = NULL, 
    x = NULL
  )

## timely_care vs pph_rate ----
### There is interaction 
timely_pph <- interact_hhs(df = hhs_train, 
                           x = timely_care, 
                           y = pph_rate,
                           z = quality_star_category) +
  labs(
    subtitle = "Potentially Preventable \nHospitalization Rate",
    y = NULL, 
    x = NULL
  )


## timely_care vs ppr_rate ----
### There is interaction 
timely_ppr <- interact_hhs(df = hhs_train, 
                           x = timely_care, 
                           y = ppr_rate,
                           z = quality_star_category) +
  labs(
    subtitle = "Potentially Preventable \nReadmissions Rate",
    y = NULL, 
    x = NULL
  )


## timely_care vs medicare_spending_index ----
### There is interaction 
timely_medicare <- interact_hhs(df = hhs_train, 
                               x = timely_care, 
                               y = medicare_spending_index,
                               z = quality_star_category) +
  labs(
    subtitle = "Medicare Spending per Episode \n(Relative to National Average)",
    y = NULL, 
    x = NULL
  )


## timely_care vs log10(num_episodes)  ----
### There is interaction 
timely_episodes <- interact_hhs(df = hhs_train, 
                                x = timely_care, 
                                y = log10(num_episodes),
                                z = quality_star_category) +
  labs(
    y = NULL, 
    x = NULL,
    subtitle = "Log10 of Episodes Used \nfor Medicare Cost Calculation"
  )

## Interaction plot ----
### Improvement metrics
timely_improve <- (timely_walk | timely_bed) / 
                   (timely_bath | timely_breath) /
                   (timely_medicine ) +
  plot_annotation(title = "Interaction Effect of Timely Care \nand Patient Improvement",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )

ggsave(timely_improve, 
       filename = here("interact_predict/timely_care/timely_improve.png"),
       width = 18,
       height = 30, 
       units = "in")

### Care quality 
timely_quality <- ( timely_discharge) +
  plot_annotation(title = "Interaction Effect of Timely Care \nand Care Quality",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )

ggsave(timely_quality, 
       filename = here("interact_predict/timely_care/timely_quality.png"),
       width = 15,
       height = 15,
       units = "in")

                   
### Performance metrics
timely_performance <- (timely_pph | timely_ppr)  +
plot_annotation(title = "Interaction Effect of Timely Care \nand Performance Metrics",
                theme = theme(
                  plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                            )
                )

ggsave(timely_performance, 
       filename = here("interact_predict/timely_care/timely_performance.png"),
       width = 20,
       height = 15,
       units = "in")


### Financial metrics
timely_finance <- (timely_medicare | timely_episodes)  +
  plot_annotation(title = "Interaction Effect of Timely Care \nand Financial Metrics",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )

ggsave(timely_finance, 
       filename = here("interact_predict/timely_care/timely_finance.png"),
       width = 20,
       height = 15,
       units = "in")



  
## No interaction plot ----
timely_non_interact <-  (timely_flu | timely_resolved)/
                        (timely_skin | timely_injury) /
                        (timely_provider | timely_patient)/ 
                        (timely_medicare | timely_episodes)/
                        timely_dtc +
  plot_annotation(title = "Non-Interaction Effect of Timely Care and Various Predictors",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )
                  

ggsave(timely_non_interact, 
       filename = here("interact_predict/timely_care/timely_non_interact.png"),
       width = 19,
       height = 45)
  
### timely_care interact with starts_with("improve_")
### timely_care interact with fall_injury_rate
### timely_care interact with discharge_score
### timely_care interact with ppr_rate & pph_rate
### timely_care interact with medicare_spending_index
### timely_care interact with num_episodes

##-----------------------------------------------------------------------

# flu_shot_check ----
## flu_shot_check vs improve_walking ----
### There is no interaction 
flu_walking <- interact_hhs(df = hhs_train, 
                           x = flu_shot_check, 
                           y = improve_walking,
                           z = quality_star_category) +
  labs(
    subtitle = "Improvement in Walking Ability (%)",
    y = NULL,
    x = NULL
  )



## flu_shot_check vs improve_bed_transfer ----
### There is interaction 
flu_bed <- interact_hhs(df = hhs_train, 
                        x = flu_shot_check, 
                        y = improve_bed_transfer,
                        z = quality_star_category) + 
  labs(
    subtitle = "Bed Transfer (%)",
    y = NULL,
    x = NULL
  )


## flu_shot_check vs improve_bathing ----
### There is no interaction 
flu_bath <- interact_hhs(df = hhs_train, 
                         x = flu_shot_check, 
                         y = improve_bathing,
                         z = quality_star_category) + 
  labs(
    subtitle = "Improvement in Bathing Ability (%)",
    y = NULL,
    x = NULL
  )



## flu_shot_check vs improve_breathing ----
### There is interaction 
flu_breath <- interact_hhs(df = hhs_train, 
                            x = flu_shot_check, 
                            y = improve_breathing,
                            z = quality_star_category) + 
  labs(
    subtitle = "Breathing Ability (%)",
    y = NULL,
    x = NULL
  )


## flu_shot_check vs improve_medicine ----
### There is interaction
flu_medicine <- interact_hhs(df = hhs_train, 
                              x = flu_shot_check, 
                              y = improve_medicine,
                              z = quality_star_category) + 
  labs(
    subtitle = "Medication Adherence (%)",
    y = NULL,
    x = NULL
  )



## flu_shot_check vs improve_skin ----
### There is no interaction
flu_skin <- interact_hhs(df = hhs_train, 
                         x = flu_shot_check, 
                         y = improve_skin,
                         z = quality_star_category) +
  labs(
    subtitle = "Improve in Skin Integrity (%)",
    y = NULL, 
    x = NULL
  )



## flu_shot_check vs med_issue_resolved ----
### There is interaction
flu_resolved <- interact_hhs(df = hhs_train, 
                             x = flu_shot_check, 
                             y = med_issue_resolved,
                             z = quality_star_category) +
  labs(
    subtitle = "Timely Completion of \nMedication Actions (%)",
    y = NULL,
    x = NULL
  )



## flu_shot_check vs fall_injury_rate ----
### There is no interaction
flu_injury <-  interact_hhs(df = hhs_train, 
                            x = flu_shot_check, 
                            y = fall_injury_rate,
                            z = quality_star_category) +
  labs(
    subtitle = "Residents with Major Injury from Falls (%)", 
    x = NULL,
    y = NULL
  )
  


## flu_shot_check vs discharge_score ----
### There is no interaction
flu_discharge <-  interact_hhs(df = hhs_train, 
                               x = flu_shot_check, 
                               y = discharge_score,
                               z = quality_star_category) +
  labs(
    subtitle = "Discharge Function Score",
    y = NULL, 
    x = NULL
  ) 



## flu_shot_check vs transfer_to_provider ----
### There is interaction 
flu_provider <- interact_hhs(df = hhs_train, 
                             x = flu_shot_check, 
                             y = transfer_to_provider,
                             z = quality_star_category) +
  labs(
    subtitle = "Transfer of Health Information \nto the Provider (%)",
    y = NULL, 
    x = NULL
  )



## flu_shot_check vs transfer_to_patient ----
### There is interaction 
flu_patient <- interact_hhs(df = hhs_train, 
                            x = flu_shot_check, 
                            y = transfer_to_patient,
                            z = quality_star_category) +
  labs(
    subtitle = "Transfer of Health Information \nto the Patient (%)",
    y = NULL, 
    x = NULL
  )



## flu_shot_check vs dtc_rate ----
### There is interaction 
flu_dtc <- interact_hhs(df = hhs_train, 
                        x = flu_shot_check, 
                        y = dtc_rate,
                        z = quality_star_category) +
  labs(
    subtitle = "Discharge to Community Rate",
    y = NULL, 
    x = NULL
  )

  


## flu_shot_check vs pph_rate ----
### There is no interaction 
flu_pph <- interact_hhs(df = hhs_train, 
                        x = flu_shot_check, 
                        y = pph_rate,
                        z = quality_star_category) +
  labs(
    subtitle = "Potentially Preventable Hospitalization Rate",
    y = NULL, 
    x = NULL
  )


## flu_shot_check vs ppr_rate ----
### There is no interaction 
flu_ppr <- interact_hhs(df = hhs_train, 
                        x = flu_shot_check, 
                        y = ppr_rate,
                        z = quality_star_category) +
  labs(
    subtitle = "Potentially Preventable Readmissions Rate",
    y = NULL, 
    x = NULL
  )

## flu_shot_check vs medicare_spending_index ----
### There is no interaction 
flu_medicare <- interact_hhs(df = hhs_train, 
                             x = flu_shot_check, 
                             y = medicare_spending_index,
                             z = quality_star_category) +
  labs(
    subtitle = "Medicare Spending per Episode \n(Relative to National Average)",
    y = NULL, 
    x = NULL
  )


## flu_shot_check vs log10(num_episodes) ----
### There is no interaction 
flu_episodes <- interact_hhs(df = hhs_train, 
                             x = flu_shot_check, 
                             y = log10(num_episodes),
                             z = quality_star_category) +
  labs(
    y = NULL, 
    x = NULL,
    subtitle = "Log10 of Episodes Used \nfor Medicare Cost Calculation"
  )



## Interaction plot -----
### Improvement metrics 
flu_improve <- (flu_bed / flu_breath / flu_medicine) + 
  plot_annotation(title = "Interaction Effect of Flu Shot Checked \nand Patient Improvement",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )
  

ggsave(flu_improve, 
       filename = "interact_predict/flu_shot_check/flu_improve.png",
       width = 18,
       height = 30,
       units = "in")

### Information Transfer
flu_transfer<- ( flu_provider | flu_patient) +
  plot_annotation(title = "Interaction Effect of Flu Shot Checked \nand Information Transfer",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )

ggsave(flu_transfer, 
       filename = here("interact_predict/flu_shot_check/flu_transfer.png"),
       width = 25,
       height = 20,
       units = "in")
  

### Care Quality 
flu_care<- (flu_resolved | flu_dtc) +
  plot_annotation(title = "Interaction Effect of Flu Shot Checked \nand Care Quality",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )

ggsave(flu_care, 
       filename = here("interact_predict/flu_shot_check/flu_care.png"),
       width = 25,
       height = 20,
       units = "in")


## No interaction plot ----
flu_no_interact <-  (flu_walking | flu_bath |flu_skin) /
                     (flu_injury | flu_discharge | flu_pph) /
                     (flu_ppr | flu_medicare | flu_episodes) +
  plot_annotation(title = "Non-Interaction Effect of Flu Shot Checked and Various Predictors",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )


ggsave(flu_no_interact, 
       filename = here("interact_predict/flu_shot_check/flu_no_interact.png"),
       width = 40,
       height = 30)
                   


### flu_shot_check interact with improve_bed_transfer & improve_medicine * improve_breathing
### flu_shot_check interact with med_issue_resolved
### flu_shot_check interact with starts_with("transfer_to")
### flu_shot_check interact with dtc_rate

#-----------------------------------------------------------------------

# improve_walking ----
## improve_walking vs improve_bed_transfer
### There is no interaction 
walking_bed <- interact_hhs(df = hhs_train, 
                           x = improve_walking, 
                           y = improve_bed_transfer,
                           z = quality_star_category) + 
  labs(
    subtitle = "Improvement in Bed Transfer (%)",
    y = NULL,
    x = NULL
  )

## improve_walking vs improve_bathing ----
### There is interaction 
walking_bath <- interact_hhs(df = hhs_train, 
                             x = improve_walking, 
                             y = improve_bathing,
                             z = quality_star_category) +
  labs(
    subtitle = "Bathing Ability (%)",
    y = NULL,
    x = NULL
  )



## improve_walking vs improve_breathing ----
### There is interaction 
walking_breath <- interact_hhs(df = hhs_train, 
                              x = improve_walking, 
                              y = improve_breathing,
                              z = quality_star_category) + 
  labs(
    subtitle = "Breathing Ability (%)",
    y = NULL,
    x = NULL
  )


## improve_walking vs improve_medicine ----
### There is interaction
walking_medicine <- interact_hhs(df = hhs_train, 
                                 x = improve_walking, 
                                 y = improve_medicine,
                                 z = quality_star_category) +
  labs(
    subtitle = "Medication Adherence (%)",
    y = NULL,
    x = NULL
  )



## improve_walking vs improve_skin ----
### There is interaction
walking_skin <- interact_hhs(df = hhs_train, 
                             x = improve_walking, 
                             y = improve_skin,
                             z = quality_star_category) +
  labs(
    subtitle = "Skin Integrity (%)",
    y = NULL, 
    x = NULL
  )



## improve_walking vs med_issue_resolved ----
### There is no interaction
walking_resolved <-  interact_hhs(df = hhs_train, 
                                  x = improve_walking, 
                                  y = med_issue_resolved,
                                  z = quality_star_category) +
  labs(
    subtitle = "Timely Completion of Medication Actions (%)",
    y = NULL,
    x = NULL
  )



## improve_walking vs fall_injury_rate ----
### There is interaction
walking_injury <- interact_hhs(df = hhs_train, 
                               x = improve_walking, 
                               y = fall_injury_rate,
                               z = quality_star_category) +
  labs(
    subtitle = "Residents with Major Injury from Falls (%)", 
    x = NULL,
    y = NULL
  )



## improve_walking vs discharge_score ----
### There is interaction
walking_discharge <- interact_hhs(df = hhs_train, 
                               x = improve_walking, 
                               y = discharge_score,
                               z = quality_star_category) +
  labs(
    subtitle = "Discharge Function Score",
    y = NULL, 
    x = NULL
  ) 



## improve_walking vs transfer_to_provider ----
### There is interaction 
walking_provider <- interact_hhs(df = hhs_train, 
                                 x = improve_walking, 
                                 y = transfer_to_provider,
                                 z = quality_star_category) +
  labs(
    subtitle = "Transfer of Health Information \nto the Provider (%)",
    y = NULL, 
    x = NULL
  )



## improve_walking vs transfer_to_patient ----
### There is interaction 
walking_patient <- interact_hhs(df = hhs_train, 
                                x = improve_walking, 
                                y = transfer_to_patient,
                                z = quality_star_category) +
  labs(
    subtitle = "Transfer of Health Information \nto the Patient (%)",
    y = NULL, 
    x = NULL
  )


## improve_walking vs dtc_rate ----
### There is no interaction 
walking_dtc <- interact_hhs(df = hhs_train, 
                            x = improve_walking, 
                            y = dtc_rate,
                            z = quality_star_category) +
  labs(
    subtitle = "Discharge to Community Rate",
    y = NULL, 
    x = NULL
  )



## improve_walking vs pph_rate ----
### There is interaction 
walking_pph <- interact_hhs(df = hhs_train, 
                            x = improve_walking, 
                            y = pph_rate,
                            z = quality_star_category) +
  labs(
    subtitle = "Potentially Preventable Hospitalization Rate",
    y = NULL, 
    x = NULL
  )



## improve_walking vs ppr_rate ----
### There is interaction 
walking_ppr <- interact_hhs(df = hhs_train, 
                             x = improve_walking, 
                             y = ppr_rate,
                             z = quality_star_category) +
  labs(
    subtitle = "Potentially Preventable Readmissions Rate",
    y = NULL, 
    x = NULL
  )

## improve_walking vs medicare_spending_index ----
### There is interaction 
walking_medicare <- interact_hhs(df = hhs_train, 
                                  x = improve_walking, 
                                  y = medicare_spending_index,
                                  z = quality_star_category) +
  labs(
    subtitle = "Medicare Spending per Episode \n(Relative to National Average)",
    y = NULL, 
    x = NULL
  )

## improve_walking vs log10(num_episodes) ----
### There is interaction 
walking_episodes <- interact_hhs(df = hhs_train, 
                                  x = improve_walking, 
                                  y = log10(num_episodes),
                                  z = quality_star_category) +
  labs(
    y = NULL, 
    x = NULL,
    subtitle = "Log10 of Episodes Used \nfor Medicare Cost Calculation"
  )

## Interaction plot ----
### Improvement metrics
walk_improve <- (walking_bath / walking_breath / walking_medicine ) +
  plot_annotation(title = "Interaction Effect of Improvement in Walking \nand Other Patient Outcomes",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )


ggsave(walk_improve, 
       filename = here("interact_predict/improve_walking/walk_improve.png"),
       width = 15,
       height = 30,
       units = "in")


### Care quality
walk_quality <- (walking_resolved | walking_discharge) +
  plot_annotation(title = "Interaction Effect of Walking Improvement \nand Care Quality",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )

ggsave(walk_quality, 
       filename = here("interact_predict/improve_walking/walk_quality.png"),
       width = 25,
       height = 20,
       units = "in")

### Information Transfer
walk_transfer<- (walking_provider | walking_patient) +
  plot_annotation(title = "Interaction Effect of Walking Improvement \nand Information Transfer",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )


ggsave(walk_transfer, 
       filename = here("interact_predict/improve_walking/walk_transfer.png"),
       width = 25,
       height = 20,
       units = "in")

### Performance metrics
walk_performance <- (walking_pph / walking_ppr /walking_dtc ) + 
  plot_annotation(title = "Interaction Effect of Walking Improvement \nand Performance metrics",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )

ggsave(walk_performance, 
       filename = here("interact_predict/improve_walking/walk_performance.png"),
       width = 16,
       height = 30,
       units = "in")


### Financial metrics
walk_finance <- (walking_episodes) +
  plot_annotation(title = "Interaction Effect of Walking Improvement \nand Financial Metrics",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5)
                  )
  )

ggsave(walk_finance, 
       filename = here("interact_predict/improve_walking/walk_finance.png"))


## No interaction plot ----
walk_no_interact <-  (walking_skin / walking_injury/ walking_medicare) +
  plot_annotation(title = "Non-Interaction Effect of Walking Improvement \nand Various Predictors",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )


ggsave(walk_no_interact, 
       filename = here("interact_predict/improve_walking/walk_no_interact.png"),
       width = 18,
       height = 30, 
       units = "in")


        

### improve_walking interact with improve_skin & improve_medicine & improve_bathing & improve breathing
### improve_walking interact with fall_injury_rate
### improve_walking interact with discharge_score
### improve_walking interact with starts_with("transfer_to_")
### improve_walking interact with ppr_rate & pph_rate
### improve_walking interact with medicare_spending_index
### improve_walking interact with log10(num_episodes)

##-----------------------------------------------------------------------
# improve_bed_transfer ----
## improve_bed_transfer vs improve_bathing----
### There is interaction 
bed_bath <- interact_hhs(df = hhs_train, 
                          x = improve_bed_transfer, 
                          y = improve_bathing,
                          z = quality_star_category) +
  labs(
    subtitle = "Bathing Ability (%)",
    y = NULL,
    x = NULL
  )



## improve_bed_transfer vs improve_breathing----
### There is interaction
bed_breath <- interact_hhs(df = hhs_train, 
                            x = improve_bed_transfer, 
                            y = improve_breathing,
                            z = quality_star_category) + 
  labs(
    subtitle = "Breathing Ability (%)",
    y = NULL,
    x = NULL
  )



## improve_bed_transfer vs improve_medicine----
### There is no interaction
bed_medicine <- interact_hhs(df = hhs_train, 
                              x = improve_bed_transfer, 
                              y = improve_medicine,
                              z = quality_star_category) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Medication Adherence Improvement (%)"
  )



## improve_bed_transfer vs improve_skin----
### There is interaction
bed_skin <- interact_hhs(df = hhs_train, 
                          x = improve_bed_transfer, 
                          y = improve_skin,
                          z = quality_star_category) +
  labs(
    subtitle = "Improvement in Skin Integrity (%)",
    y = NULL, 
    x = NULL
  )


## improve_bed_transfer vs med_issue_resolved----
### There is interaction
bed_resolved <- interact_hhs(df = hhs_train, 
                              x = improve_bed_transfer, 
                              y = med_issue_resolved,
                              z = quality_star_category) +
  labs(
    subtitle = "Timely Completion of Medication Actions (%)",
    y = NULL,
    x = NULL
  )


## improve_bed_transfer vs fall_injury_rate----
### There is interaction
bed_injury <- interact_hhs(df = hhs_train, 
                            x = improve_bed_transfer, 
                            y = fall_injury_rate,
                            z = quality_star_category) +
  labs(
    subtitle = "Residents with Major Injury from Falls (%)", 
    x = NULL,
    y = NULL
  )



## improve_bed_transfer vs discharge_score----
### There is interaction
bed_discharge <- interact_hhs(df = hhs_train, 
                                  x = improve_bed_transfer, 
                                  y = discharge_score,
                                  z = quality_star_category) +
  labs(
    subtitle = "Discharge Function Score",
    y = NULL, 
    x = NULL
  ) 



## improve_bed_transfer vs transfer_to_provider----
### There is interaction 
bed_provider <- interact_hhs(df = hhs_train, 
                             x = improve_bed_transfer, 
                             y = transfer_to_provider,
                             z = quality_star_category) +
  labs(
    subtitle = "Transfer of Health Information \nto the Provider (%)",
    y = NULL, 
    x = NULL
  )



## improve_bed_transfer vs transfer_to_patient----
### There is interaction 
bed_patient <- interact_hhs(df = hhs_train, 
                                x = improve_bed_transfer, 
                                y = transfer_to_patient,
                                z = quality_star_category) +
  labs(
    subtitle = "Transfer of Health Information to the Patient (%)",
    y = NULL, 
    x = NULL
  )


## improve_bed_transfer vs dtc_rate----
### There is interaction 
bed_dtc <- interact_hhs(df = hhs_train, 
                            x = improve_bed_transfer, 
                            y = dtc_rate,
                            z = quality_star_category) +
  labs(
    subtitle = "Discharge to Community Rate",
    y = NULL, 
    x = NULL
  )



## improve_bed_transfer vs pph_rate----
### There is interaction 
bed_pph <- interact_hhs(df = hhs_train, 
                       x = improve_bed_transfer, 
                       y = pph_rate,
                       z = quality_star_category) +
  labs(
    subtitle = "Potentially Preventable Hospitalization Rate",
    y = NULL, 
    x = NULL
  )




## improve_bed_transfer vs ppr_rate----
### There is interaction 
bed_ppr <- interact_hhs(df = hhs_train, 
                            x = improve_bed_transfer, 
                            y = ppr_rate,
                            z = quality_star_category) +
  labs(
    subtitle = "Potentially Preventable Readmissions Rate",
    y = NULL, 
    x = NULL
  )

## improve_bed_transfer vs medicare_spending_index----
### There is interaction 
bed_medicare <- interact_hhs(df = hhs_train, 
                                 x = improve_bed_transfer, 
                                 y = medicare_spending_index,
                                 z = quality_star_category) +
  labs(
    subtitle = "Medicare Spending per Episode \n(Relative to National Average)",
    y = NULL, 
    x = NULL
  )


## improve_bed_transfer vs log10(num_episodes)----
### There is interaction 
bed_episodes <-interact_hhs(df = hhs_train, 
                                x = improve_bed_transfer, 
                                y = log10(num_episodes),
                                z = quality_star_category) +
  labs(
    y = NULL, 
    x = NULL,
    subtitle = "Log10 of Episodes Used for Medicare Cost Calculation"
  )


## Interaction plot----
### Improvement metrics
bed_improve <- (bed_bath | bed_breath) +
  plot_annotation(title = "Interaction Effect of Improvement in Bed Transfer \nand Other Patient Outcomes",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )


ggsave(bed_improve, 
       filename = here("interact_predict/improve_bed_transfer/bed_improve.png"),
       width = 25,
       height = 15,
       units = "in")

### Care quality
bed_quality <- (bed_resolved + bed_discharge) +
  plot_annotation(title = "Interaction Effect of Bed Transfer Improvement \nand Care Quality",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )

ggsave(bed_quality, 
       filename = here("interact_predict/improve_bed_transfer/bed_quality.png"),
       width = 25,
       height = 15,
       units = "in")


### Information Transfer
bed_transfer<- (bed_patient) +
  plot_annotation(title = "Interaction Effect of Bed Transfer Improvement \nand Information Transfer",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )

ggsave(bed_transfer, 
       filename = here("interact_predict/improve_bed_transfer/bed_transfer.png"),
       width = 15,
       height = 15,
       units = "in")

### Performance metrics
bed_performance <- (bed_dtc/ bed_pph / bed_ppr) + 
  plot_annotation(title = "Interaction Effect of Bed Transfer Improvement \nand Performance metrics",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )

ggsave(bed_performance, 
       filename = here("interact_predict/improve_bed_transfer/bed_performance.png"),
       width = 15,
       height = 30,
       units = "in")
  

### Financial metrics
bed_finance <- (bed_medicare) +
  plot_annotation(title = "Interaction Effect of Bed Transfer Improvement \nand Financial Metrics",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5)
                  )
  )

ggsave(bed_finance, 
       filename = here("interact_predict/improve_bed_transfer/bed_finance.png"),
       width = 16,
       height = 15,
       units = "in")

## No interaction plot -----
bed_no_interact <-  (bed_medicine | bed_provider) /
                    (bed_injury | bed_skin) /
                    (bed_episodes)+
  plot_annotation(title = "Non-Interaction Effect of Bed Transfer Improvement \nand Medication Adherence",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )


ggsave(bed_no_interact, 
       filename = here("interact_predict/improve_bed_transfer/bed_no_interact.png"),
       width = 22,
       height = 38, 
       units = "in")


### improve_bed_transfer interact with improve_skin & improve_bathing & improve breathing
### improve_bed_transfer interact with med_issue_resolved
### improve_bed_transfer interact with fall_injury_rate
### improve_bed_transfer interact with discharge_score
### improve_bed_transfer interact with starts_with("transfer_to_")
### improve_bed_transfer interact with ends_with("_rate")
### improve_bed_transfer interact with medicare_spending_index
### improve_bed_transfer interact with log10(num_episodes)

##-----------------------------------------------------------------------
# improve_bathing ----
## improve_bathing vs improve_breathing----
### There is interaction
bathing_breath <- interact_hhs(df = hhs_train, 
                           x = improve_bathing, 
                           y = improve_breathing,
                           z = quality_star_category) +
  labs(
    subtitle = "Breathing Ability (%)",
    y = NULL,
    x = NULL
  )

## improve_bathing vs improve_medicine----
### There is interaction
bathing_medicine <- interact_hhs(df = hhs_train, 
                             x = improve_bathing, 
                             y = improve_medicine,
                             z = quality_star_category) +
  labs(
    subtitle = "Medication Adherence (%)",
    y = NULL,
    x = NULL
  )



## improve_bathing vs improve_skin----
### There is no interaction
bathing_skin <- interact_hhs(df = hhs_train, 
                         x = improve_bathing, 
                         y = improve_skin,
                         z = quality_star_category) +
  labs(
    subtitle = "Improve in Skin Integrity (%)",
    y = NULL, 
    x = NULL
  )


## improve_bathing vs med_issue_resolved----
### There is interaction
bathing_resolved <- interact_hhs(df = hhs_train, 
                             x = improve_bathing, 
                             y = med_issue_resolved,
                             z = quality_star_category) +
  labs(
    subtitle = "Timely Completion of \nMedication Actions (%)",
    y = NULL,
    x = NULL
  )



## improve_bathing vs fall_injury_rate----
### There is interaction
bathing_injury <- interact_hhs(df = hhs_train, 
                           x = improve_bathing, 
                           y = fall_injury_rate,
                           z = quality_star_category) +
  labs(
    subtitle = "Residents with Major Injury from Falls (%)", 
    x = NULL,
    y = NULL
  )



## improve_bathing vs discharge_score----
### There is interaction
bathing_discharge <- interact_hhs(df = hhs_train, 
                              x = improve_bathing, 
                              y = discharge_score,
                              z = quality_star_category) +
  labs(
    subtitle = "Discharge Function Score",
    y = NULL, 
    x = NULL
  ) 



## improve_bathing vs transfer_to_provider----
### There is interaction 
bathing_provider <- interact_hhs(df = hhs_train, 
                             x = improve_bathing, 
                             y = transfer_to_provider,
                             z = quality_star_category) +
  labs(
    subtitle = "Transfer of Health Information \nto the Provider (%)",
    y = NULL, 
    x = NULL
  )



## improve_bathing vs transfer_to_patient----
### There is no interaction 
bathing_patient <- interact_hhs(df = hhs_train, 
                            x = improve_bathing, 
                            y = transfer_to_patient,
                            z = quality_star_category) +
  labs(
    subtitle = "Transfer of Health Information \nto the Patient (%)",
    y = NULL, 
    x = NULL
  )


## improve_bathing vs dtc_rate----
### There is interaction 
bathing_dtc <- interact_hhs(df = hhs_train, 
                        x = improve_bathing, 
                        y = dtc_rate,
                        z = quality_star_category) +
  labs(
    subtitle = "Discharge to Community Rate",
    y = NULL, 
    x = NULL
  )



## improve_bathing vs pph_rate----
### There is interaction 
bathing_pph <- interact_hhs(df = hhs_train, 
                        x = improve_bathing, 
                        y = pph_rate,
                        z = quality_star_category) +
  labs(
    subtitle = "Potentially Preventable Hospitalization Rate",
    y = NULL, 
    x = NULL
  )



## improve_bathing vs ppr_rate----
### There is interaction 
bathing_ppr <- interact_hhs(df = hhs_train, 
                        x = improve_bathing, 
                        y = ppr_rate,
                        z = quality_star_category) +
  labs(
    subtitle = "Potentially Preventable Readmissions Rate",
    y = NULL, 
    x = NULL
  )


## improve_bathing vs medicare_spending_index----
### There is interaction 
bathing_medicare <- interact_hhs(df = hhs_train, 
                             x = improve_bathing, 
                             y = medicare_spending_index,
                             z = quality_star_category) +
  labs(
    subtitle = "Medicare Spending per Episode \n(Relative to National Average)",
    y = NULL, 
    x = NULL
  )


## improve_bathing vs log10(num_episodes)----
### There is interaction 
bathing_episodes <-interact_hhs(df = hhs_train, 
                            x = improve_bathing, 
                            y = log10(num_episodes),
                            z = quality_star_category) +
  labs(
    y = NULL, 
    x = NULL,
    subtitle = "Log10 of Episodes Used \nfor Medicare Cost Calculation"
  )

## Interaction plot----
### Improvement metrics
bath_improve <- (bathing_breath | bathing_medicine) +
  plot_annotation(title = "Interaction Effect of Improvement in Bathing \nand Other Patient Outcomes",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5)
                  )
  )

ggsave(bath_improve, 
       filename = here("interact_predict/improve_bathing/bath_improve.png"),
       width = 20,
       height = 15,
       units = "in")

### Information Transfer
bath_transfer <- (bathing_patient| bathing_provider) +
  plot_annotation(title = "Interaction Effect of Bathing Improvement \nand Information Transfer",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )


ggsave(bath_transfer, 
       filename = here("interact_predict/improve_bathing/bath_transfer.png"),
       width = 20,
       height = 15,
       units = "in")


### Care Quality
bath_quality <- (  bathing_discharge  | bathing_resolved) +
  plot_annotation(title = "Interaction Effect of Bathing Improvement \nand Care Quality",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
                  )

ggsave(bath_quality, 
       filename = here("interact_predict/improve_bathing/bath_quality.png"),
       width = 20,
       height = 15,
       units = "in")

### Performance metric
bath_performance <- (bathing_dtc / bathing_pph / bathing_ppr) + 
  plot_annotation(title = "Interaction Effect of Bathing Improvement \nand Performance metrics",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )

ggsave(bath_performance, 
       filename = here("interact_predict/improve_bathing/bath_performance.png"),
       width = 15,
       height = 30,
       units = "in")

### Financial metrics
bath_finance <- (bathing_episodes) +
  plot_annotation(title = "Interaction Effect of Bathing Improvement \nand Financial Metrics",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5)
                  )
  )

ggsave(bath_finance, 
       filename = here("interact_predict/improve_bathing/bath_finance.png"),
       width = 20,
       height = 15,
       units = "in")


##No interaction plot -----
bath_no_interact <-  (bathing_injury/ bathing_skin / bathing_medicare) +
  plot_annotation(title = "Non-Interaction Effect of Bathing Improvement \nand Various Predictors",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )


ggsave(bath_no_interact, 
       filename = here("interact_predict/improve_bathing/bath_no_interact.png"),
       width = 18,
       height = 30, 
       units = "in")



##--------------------------------------------------------------------------------
# improve_breathing----
## improve_breathing vs improve_medicine ----
### There is no interaction
breathing_medicine <- interact_hhs(df = hhs_train, 
                               x = improve_breathing, 
                               y = improve_medicine,
                               z = quality_star_category) +
  labs(
    subtitle = "Improvement in Medication Adherence (%)",
    y = NULL,
    x = NULL
  )



## improve_breathing vs improve_skin ----
### There is no interaction
breathing_skin <- interact_hhs(df = hhs_train, 
                               x = improve_breathing, 
                               y = improve_skin,
                               z = quality_star_category) +
  labs(
    subtitle = "Improvement in Skin Integrity (%)",
    y = NULL, 
    x = NULL
  )


## improve_breathing vs med_issue_resolved ----
### There is interaction
breathing_resolved <- interact_hhs(df = hhs_train, 
                                   x = improve_breathing, 
                                   y = med_issue_resolved,
                                   z = quality_star_category) +
  labs(
    subtitle = "Timely Completion of \nMedication Actions (%)",
    x = NULL,
    y = NULL
  )


## improve_breathing vs fall_injury_rate ----
### There is interaction
breathing_injury <- interact_hhs(df = hhs_train, 
                                 x = improve_breathing, 
                                 y = fall_injury_rate,
                                 z = quality_star_category) +
  labs(
    subtitle = "Residents with Major Injury from Falls (%)", 
    x = NULL,
    y = NULL
  )



## improve_breathing vs discharge_score----
### There is interaction
breathing_discharge <- interact_hhs(df = hhs_train, 
                                    x = improve_breathing, 
                                    y = discharge_score,
                                    z = quality_star_category) +
  labs(
    subtitle = "Discharge Function Score",
    y = NULL, 
    x = NULL
  ) 



## improve_breathing vs transfer_to_provider ----
### There is interaction 
breathing_provider <- interact_hhs(df = hhs_train, 
                                   x = improve_breathing, 
                                   y = transfer_to_provider,
                                   z = quality_star_category) +
  labs(
    subtitle = "Transfer of Health Information \nto the Provider (%)",
    y = NULL, 
    x = NULL
  )



## improve_breathing vs transfer_to_patient ----
### There is interaction 
breathing_patient <- interact_hhs(df = hhs_train, 
                                  x = improve_breathing, 
                                  y = transfer_to_patient,
                                  z = quality_star_category) +
  labs(
    subtitle = "Transfer of Health Information \nto the Patient (%)",
    y = NULL, 
    x = NULL
  )


## improve_breathing vs dtc_rate ----
### There is no interaction 
breathing_dtc <- interact_hhs(df = hhs_train, 
                              x = improve_breathing, 
                              y = dtc_rate,
                              z = quality_star_category) +
  labs(
    subtitle = "Discharge to Community Rate",
    y = NULL, 
    x = NULL
  )



## improve_breathing vs pph_rate ----
### There is interaction 
breathing_pph <- interact_hhs(df = hhs_train, 
                              x = improve_breathing, 
                              y = pph_rate,
                              z = quality_star_category) +
  labs(
    subtitle = "Potentially Preventable \nHospitalization Rate",
    y = NULL, 
    x = NULL
  )



## improve_breathing vs ppr_rate ----
### There is no interaction 
breathing_ppr <- interact_hhs(df = hhs_train, 
                              x = improve_breathing, 
                              y = ppr_rate,
                              z = quality_star_category) +
  labs(
    subtitle = "Potentially Preventable \nReadmissions Rate",
    y = NULL, 
    x = NULL
  )


## improve_breathing vs medicare_spending_index ----
### There is interaction 
breathing_medicare <- interact_hhs(df = hhs_train, 
                                   x = improve_breathing, 
                                   y = medicare_spending_index,
                                   z = quality_star_category) +
  labs(
    subtitle = "Medicare Spending per Episode (Relative to National Average)",
    y = NULL, 
    x = NULL
  )


## improve_breathing vs log10(num_episodes) ----
### There is interaction 
breathing_episodes <-interact_hhs(df = hhs_train, 
                                  x = improve_breathing, 
                                  y = log10(num_episodes),
                                  z = quality_star_category) +
  labs(y = NULL, 
       x = NULL,
       subtitle = "Log10 of Episodes Used for Medicare Cost Calculation")


## Interaction plot ----
### Care quality
breath_quality <- (breathing_resolved  | breathing_discharge) +
  plot_annotation(title = "Interaction Effect of Breathing Improvement \nand Care Quality",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )

ggsave(breath_quality, 
       filename = here("interact_predict/improve_breathing/breath_quality.png"),
       width = 20,
       height = 15,
       units = "in")

### Information Transfer
breath_transfer <- (breathing_provider| breathing_patient) +
  plot_annotation(title = "Interaction Effect of Breathing Improvement \nand Information Transfer",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )


ggsave(breath_transfer, 
       filename = here("interact_predict/improve_breathing/breath_transfer.png"),
       width = 20,
       height = 15,
       units = "in")

### Performance metrics
breath_performance <- (breathing_dtc | breathing_pph) + 
  plot_annotation(title = "Interaction Effect of Breathing Improvement \nand Performance metrics",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )

ggsave(breath_performance, 
       filename = here("interact_predict/improve_breathing/breath_performance.png"),
       width = 20,
       height = 15,
       units = "in")


### Financial metrics
breath_finance <- (breathing_episodes) +
  plot_annotation(title = "Interaction Effect of Breathing Improvement \nand Financial Metrics",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5)
                  )
  )

ggsave(breath_finance, 
       filename = here("interact_predict/improve_breathing/breath_finance.png"),
       width = 20, 
       height = 15,
       units = "in")


## Non interaction plot ----
breath_no_interact <- (breathing_medicine | breathing_skin) /
                      (breathing_injury | breathing_ppr)/
                      (breathing_medicare) +
  plot_annotation(title = "Non-Interaction Effect of Breathing Improvement \nand Various Predictors",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )


ggsave(breath_no_interact, 
       filename = here("interact_predict/improve_breathing/breath_no_interact.png"),
       width = 22,
       height = 38,
       units = "in")

##-----------------------------------------------------------------------
# improve_medicine----
## improve_medicine vs improve_skin----
### There is no interaction
medicine_skin <- interact_hhs(df = hhs_train, 
                              x = improve_medicine, 
                              y = improve_skin,
                              z = quality_star_category) +
  labs(
    subtitle = "Improvement in Skin Integrity (%)",
    y = NULL, 
    x = NULL
  )


## improve_medicine vs med_issue_resolved ----
### There is no interaction
medicine_resolved <- interact_hhs(df = hhs_train, 
                                  x = improve_medicine, 
                                  y = med_issue_resolved,
                                  z = quality_star_category) +
  labs(
    subtitle = "Timely Completion of \nMedication Actions (%)",
    x = NULL,
    y = NULL
  )


## improve_medicine vs fall_injury_rate ----
### There is no interaction
medicine_injury <- interact_hhs(df = hhs_train, 
                                x = improve_medicine, 
                                y = fall_injury_rate,
                                z = quality_star_category) +
  labs(
    subtitle = "Residents with Major Injury from Falls (%)", 
    x = NULL,
    y = NULL
  )



## improve_medicine vs discharge_score ----
### There is interaction
medicine_discharge <- interact_hhs(df = hhs_train, 
                                   x = improve_medicine, 
                                   y = discharge_score,
                                   z = quality_star_category) +
  labs(
    subtitle = "Discharge Function Score",
    y = NULL, 
    x = NULL
  ) 



## improve_medicine vs transfer_to_provider ----
### There is interaction 
medicine_provider <- interact_hhs(df = hhs_train, 
                                  x = improve_medicine, 
                                  y = transfer_to_provider,
                                  z = quality_star_category) + 
  labs(
    subtitle = "Transfer of Health Information \nto the Provider (%)",
    y = NULL, 
    x = NULL
  )



## improve_medicine vs transfer_to_patient ----
### There is interaction 
medicine_patient <- interact_hhs(df = hhs_train, 
                                 x = improve_medicine, 
                                 y = transfer_to_patient,
                                 z = quality_star_category) +
  labs(
    subtitle = "Transfer of Health Information \nto the Patient (%)",
    y = NULL, 
    x = NULL
  )


## improve_medicine vs dtc_rate ----
### There is interaction 
medicine_dtc <- interact_hhs(df = hhs_train, 
                             x = improve_medicine, 
                             y = dtc_rate,
                             z = quality_star_category) +
  labs(
    subtitle = "Discharge to Community Rate",
    y = NULL, 
    x = NULL
  )



## improve_medicine vs pph_rate ----
### There is interaction 
medicine_pph <- interact_hhs(df = hhs_train, 
                             x = improve_medicine, 
                             y = pph_rate,
                             z = quality_star_category) +
  labs(
    subtitle = "Potentially Preventable Hospitalization Rate",
    y = NULL, 
    x = NULL
  )



## improve_medicine vs ppr_rate ----
### There is interaction 
medicine_ppr <- interact_hhs(df = hhs_train, 
                             x = improve_medicine, 
                             y = ppr_rate,
                             z = quality_star_category) +
  labs(
    subtitle = "Potentially Preventable Readmissions Rate",
    y = NULL, 
    x = NULL
  )


## improve_medicine vs medicare_spending_index ----
### There is interaction 
medicine_medicare <- interact_hhs(df = hhs_train, 
                                  x = improve_medicine, 
                                  y = medicare_spending_index,
                                  z = quality_star_category) +
  labs(
    subtitle = "Medicare Spending per Episode \n(Relative to National Average)",
    y = NULL, 
    x = NULL
  )


## improve_medicine vs log10(num_episodes) ----
### There is interaction 
medicine_episodes <-interact_hhs(df = hhs_train, 
                                 x = improve_medicine, 
                                 y = log10(num_episodes),
                                 z = quality_star_category) +
  labs(
    y = NULL, 
    x = NULL,
    subtitle = "Log10 of Episodes Used for Medicare Cost Calculation"
  )

## Interaction plot ----

### Performance metric
medicine_performance <- (medicine_dtc / medicine_pph / medicine_ppr) + 
  plot_annotation(title = "Interaction Effect of Medical Adherence \nand Performance metrics",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )

ggsave(medicine_performance, 
       filename = here("interact_predict/improve_medicine/medicine_performance.png"),
       width = 15,
       height = 30,
       units = "in")

### Information Transfer
medicine_transfer <- (medicine_patient| medicine_provider) +
  plot_annotation(title = "Interaction Effect of Medical Adherence \nand Information Transfer",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )


ggsave(medicine_transfer, 
       filename = here("interact_predict/improve_medicine/medicine_transfer.png"),
       width = 20,
       height = 15,
       units = "in")


## Non-interaction plot----
medicine_non_interact <- (medicine_skin | medicine_resolved) /
                        (medicine_injury | medicine_medicare) /
                        (medicine_episodes) +
  plot_annotation(title = "Non-Interaction Effect of Medical Adherence \nand Various Predictors",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )


ggsave(medicine_non_interact, 
       filename = here("interact_predict/improve_medicine/medicine_non_interact.png"),
       width = 22,
       height = 38,
       units = "in")
  

##-------------------------------------------------------------------------------------
# improve_skin----
## improve_skin vs med_issue_resolved -----
### There is interaction
skin_resolved <- interact_hhs(df = hhs_train, 
                                  x = improve_skin, 
                                  y = med_issue_resolved,
                                  z = quality_star_category) +
  labs(
    subtitle = "Timely Completion of Medication Actions (%)",
    x = NULL,
    y = NULL
  )
  


## improve_skin vs fall_injury_rate----
### There is no interaction
skin_injury <- interact_hhs(df = hhs_train, 
                                x = improve_skin, 
                                y = fall_injury_rate,
                                z = quality_star_category) +
  labs(
    subtitle = "Residents with Major \nInjury from Falls (%)", 
    x = NULL,
    y = NULL
  )



## improve_skin vs discharge_score----
### There is interaction
skin_discharge <- interact_hhs(df = hhs_train, 
                                   x = improve_skin, 
                                   y = discharge_score,
                                   z = quality_star_category) +
  labs(
    subtitle = "Discharge Function Score",
    y = NULL, 
    x = NULL
  ) 



## improve_skin vs transfer_to_provider ----
### There is interaction 
skin_provider <- interact_hhs(df = hhs_train, 
                                  x = improve_skin, 
                                  y = transfer_to_provider,
                                  z = quality_star_category) +
  labs(
    subtitle = "Transfer of Health Information \nto the Provider (%)",
    y = NULL, 
    x = NULL
  )



## improve_skin vs transfer_to_patient ----
### There is interaction 
skin_patient <- interact_hhs(df = hhs_train, 
                                 x = improve_skin, 
                                 y = transfer_to_patient,
                                 z = quality_star_category) +
  labs(
    subtitle = "Transfer of Health Information \nto the Patient (%)",
    y = NULL, 
    x = NULL
  )


## improve_skin vs dtc_rate ----
### There is interaction 
skin_dtc <- interact_hhs(df = hhs_train, 
                             x = improve_skin, 
                             y = dtc_rate,
                             z = quality_star_category) +
  labs(
    subtitle = "Discharge to Community Rate",
    y = NULL, 
    x = NULL
  )



## improve_skin vs pph_rate ----
### There is no interaction 
skin_pph <- interact_hhs(df = hhs_train, 
                             x = improve_skin, 
                             y = pph_rate,
                             z = quality_star_category) +
  labs(
    subtitle = "Potentially Preventable Hospitalization Rate",
    y = NULL, 
    x = NULL
  )



## improve_skin vs ppr_rate----
### There is interaction 
skin_ppr <- interact_hhs(df = hhs_train, 
                             x = improve_skin, 
                             y = ppr_rate,
                             z = quality_star_category) +
  labs(
    subtitle = "Potentially Preventable Readmissions Rate",
    y = NULL, 
    x = NULL
  )


## improve_skin vs medicare_spending_index----
### There is no interaction 
skin_medicare <- interact_hhs(df = hhs_train, 
                                  x = improve_skin, 
                                  y = medicare_spending_index,
                                  z = quality_star_category) +
  labs(
    subtitle = "Medicare Spending per Episode \n(Relative to National Average)",
    y = NULL, 
    x = NULL
  )


## improve_skin vs log10(num_episodes) ----
### There is interaction 
skin_episodes <-interact_hhs(df = hhs_train, 
                                 x = improve_skin, 
                                 y = log10(num_episodes),
                                 z = quality_star_category) +
  labs(
    y = NULL, 
    x = NULL,
    subtitle = "Log10 of Episodes Used \nfor Medicare Cost Calculation"
  )


## Interaction plot ----
### Performance metrics
skin_perform <- (skin_resolved/ skin_discharge / skin_ppr) +
  plot_annotation(title = "Interaction Effect of Skin Integrity Improvement \nand Performance Metrics",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5)
                  )
  )

ggsave(skin_perform, 
       filename = here("interact_predict/improve_skin/skin_perform.png"),
       width = 15,
       height = 30,
       units = "in")


### Finance metrics
skin_finance <- (skin_episodes) +
  plot_annotation(title = "Interaction Effect of Skin Integrity Improvement \nand Financial Metrics",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5)
                  )
  )

ggsave(skin_finance, 
       filename = here("interact_predict/improve_skin/skin_finance.png"),
       width = 18,
       height = 15,
       units = "in")

### Information Transfer
skin_transfer <- (skin_patient| skin_provider) +
  plot_annotation(title = "Interaction Effect of Skin Integrity Improvement \nand Information Transfer",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )


ggsave(skin_transfer, 
       filename = here("interact_predict/improve_skin/skin_transfer.png"),
       width = 20,
       height = 15,
       units = "in")



## No interaction----
skin_no_interact <- (skin_dtc| skin_pph) / (skin_injury|skin_medicare) + 
  plot_annotation(title = "Non-Interaction Effect of Skin Integrity Improvement\n and Various Predictors",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )


ggsave(skin_no_interact, 
       filename = here("interact_predict/improve_skin/skin_no_interact.png"),
       width = 24,
       height = 24)



### improve_skin interact with dtc_rate & pph_rate
### improve_skin interact with log10(num_episodes)

##-------------------------------------------------------------------------------------
# med_issue_resolved----
## med_issue_resolved vs fall_injury_rate----
### There is no interaction
med_injury <- interact_hhs(df = hhs_train, 
                            x = med_issue_resolved, 
                            y = fall_injury_rate,
                            z = quality_star_category) + 
  labs(
    subtitle = "Residents with Major Injury from Falls (%)", 
    x = NULL,
    y = NULL
  )



## med_issue_resolved vs discharge_score----
### There is interaction
med_discharge <- interact_hhs(df = hhs_train, 
                               x = med_issue_resolved, 
                               y = discharge_score,
                               z = quality_star_category) +
  labs(
    subtitle = "Discharge Function Score",
    y = NULL, 
    x = NULL
  ) 



## med_issue_resolved vs transfer_to_provider----
### There is no interaction 
med_provider <- interact_hhs(df = hhs_train, 
                              x = med_issue_resolved, 
                              y = transfer_to_provider,
                              z = quality_star_category) +
  labs(
    subtitle = "Transfer of Health Information \nto the Provider (%)",
    y = NULL, 
    x = NULL
  )



## med_issue_resolved vs transfer_to_patient----
### There is no interaction 
med_patient <- interact_hhs(df = hhs_train, 
                             x = med_issue_resolved, 
                             y = transfer_to_patient,
                             z = quality_star_category) +
  labs(
    subtitle = "Transfer of Health Information \nto the Patient (%)",
    y = NULL, 
    x = NULL
  )


## med_issue_resolved vs dtc_rate-----
### There is no interaction 
med_dtc <- interact_hhs(df = hhs_train, 
                         x = med_issue_resolved, 
                         y = dtc_rate,
                         z = quality_star_category) +
  labs(
    subtitle = "Discharge to Community Rate",
    y = NULL, 
    x = NULL
  )



## med_issue_resolved vs pph_rate----
### There is no interaction 
med_pph <- interact_hhs(df = hhs_train, 
                         x = med_issue_resolved, 
                         y = pph_rate,
                         z = quality_star_category) +
  labs(
    subtitle = "Potentially Preventable \nHospitalization Rate",
    y = NULL, 
    x = NULL
  )



## med_issue_resolved vs ppr_rate-----
### There is no interaction 
med_ppr <- interact_hhs(df = hhs_train, 
                         x = med_issue_resolved, 
                         y = ppr_rate,
                         z = quality_star_category) +
  labs(
    subtitle = "Potentially Preventable \nReadmissions Rate",
    y = NULL, 
    x = NULL
  )


## med_issue_resolved vs medicare_spending_index----
### There is no interaction 
med_medicare <- interact_hhs(df = hhs_train, 
                              x = med_issue_resolved, 
                              y = medicare_spending_index,
                              z = quality_star_category) +
  labs(
    subtitle = "Medicare Spending per Episode \n(Relative to National Average)",
    y = NULL, 
    x = NULL
  )


## med_issue_resolved vs log10(num_episodes)----
### There is no interaction 
med_episodes <-interact_hhs(df = hhs_train, 
                             x = med_issue_resolved, 
                             y = log10(num_episodes),
                             z = quality_star_category) +
  labs(
    y = NULL, 
    x = NULL,
    subtitle = "Log10 of Episodes Used \nfor Medicare Cost Calculation"
  )

## Interaction Plot----
### Care quality
med_care <- (med_discharge) +
  plot_annotation(title = "Interaction Effect of Timely Medical Reccomendation \nand Care Quality",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5)
                  )
  )

ggsave(med_care, 
       filename = here("interact_predict/med_issue_resolved/med_care.png"),
       width = 18,
       height = 15,
       units = "in")

## Non-interaction plot----
med_non_interact <- (med_injury | med_provider) /
                    (med_patient | med_dtc) /
                    (med_pph | med_ppr) /
                    (med_episodes | med_medicare) +
  plot_annotation(title = "Non-Interaction Effect of Timely Medical Reccomendation\n and Various Predictors",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )


ggsave(med_non_interact, 
       filename = here("interact_predict/med_issue_resolved/med_non_interact.png"),
       width = 25,
       height = 30)
  

##-------------------------------------------------------------------------------------
# fall_injury_rate----
## fall_injury_rate vs discharge_score----
### There is no interaction
injury_discharge <- interact_hhs(df = hhs_train, 
                              x = fall_injury_rate, 
                              y = discharge_score,
                              z = quality_star_category) +
  labs(
    subtitle = "Discharge Function Score",
    y = NULL, 
    x = NULL
  ) 



## fall_injury_rate vs transfer_to_provider----
### There is interaction 
injury_provider <- interact_hhs(df = hhs_train, 
                             x = fall_injury_rate, 
                             y = transfer_to_provider,
                             z = quality_star_category) +
  labs(
    subtitle = "Transfer of Health Information \nto the Provider (%)",
    y = NULL, 
    x = NULL
  )



## fall_injury_rate vs transfer_to_patient----
### There is no interaction 
injury_patient <- interact_hhs(df = hhs_train, 
                            x = fall_injury_rate, 
                            y = transfer_to_patient,
                            z = quality_star_category) +
  labs(
    subtitle = "Transfer of Health Information \nto the Patient (%)",
    y = NULL, 
    x = NULL
  )


## fall_injury_rate vs dtc_rate----
### There is interaction 
injury_dtc <- interact_hhs(df = hhs_train, 
                        x = fall_injury_rate, 
                        y = dtc_rate,
                        z = quality_star_category) +
  labs(
    subtitle = "Discharge to Community Rate",
    y = NULL, 
    x = NULL
  )



## fall_injury_rate vs pph_rate----
### There is no interaction 
injury_pph <- interact_hhs(df = hhs_train, 
                        x = fall_injury_rate, 
                        y = pph_rate,
                        z = quality_star_category) +
  labs(
    subtitle = "Potentially Preventable Hospitalization Rate",
    y = NULL, 
    x = NULL
  )



## fall_injury_rate vs ppr_rate----
### There is interaction 
injury_ppr <- interact_hhs(df = hhs_train, 
                        x = fall_injury_rate, 
                        y = ppr_rate,
                        z = quality_star_category) +
  labs(
    subtitle = "Potentially Preventable Readmissions Rate",
    y = NULL, 
    x = NULL
  )


## fall_injury_rate vs medicare_spending_index
### There is no interaction 
injury_medicare <- interact_hhs(df = hhs_train, 
                             x = fall_injury_rate, 
                             y = medicare_spending_index,
                             z = quality_star_category) +
  labs(
    subtitle = "Medicare Spending per Episode \n(Relative to National Average)",
    y = NULL, 
    x = NULL
  )


## fall_injury_rate vs log10(num_episodes)
### There is interaction 
injury_episodes <-interact_hhs(df = hhs_train, 
                            x = fall_injury_rate, 
                            y = log10(num_episodes),
                            z = quality_star_category) +
  labs(
    y = NULL, 
    x = NULL,
    subtitle = "Log10 of Episodes Used \nfor Medicare Cost Calculation"
  )

## Interaction plot----
injury_interact <- (injury_provider | injury_dtc) /
                    (injury_ppr | injury_episodes)  +
  plot_annotation(title = "Interaction Effect of Residents with \nMajor Injury Falls and Various Predictors",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )


ggsave(injury_interact, 
       filename = here("interact_predict/fall_injury_rate/injury_interact.png"),
       width = 24,
       height = 24)

## Non interaction plot ----
injury_non_interact <- (injury_discharge | injury_patient) /
                        (injury_pph | injury_medicare)  +
  plot_annotation(title = "Non-Interaction Effect of Residents with \nMajor Injury Falls and Various Predictors",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )


ggsave(injury_non_interact, 
       filename = here("interact_predict/fall_injury_rate/injury_non_interact.png"),
       width = 24,
       height = 24)


##-------------------------------------------------------------------------------------
# discharge_score----
## discharge_score vs transfer_to_provider----
### There is no interaction 
discharge_provider <- interact_hhs(df = hhs_train, 
                                x = discharge_score, 
                                y = transfer_to_provider,
                                z = quality_star_category) +
  labs(
    subtitle = "Transfer of Health Information to the Provider (%)",
    y = NULL, 
    x = NULL
  )



## discharge_score vs transfer_to_patient----
### There is interaction 
discharge_patient <- interact_hhs(df = hhs_train, 
                               x = discharge_score, 
                               y = transfer_to_patient,
                               z = quality_star_category) +
  labs(
    subtitle = "Transfer of Health Information \nto the Patient (%)",
    y = NULL, 
    x = NULL
  )


## discharge_score vs dtc_rate----
### There is interaction 
discharge_dtc <- interact_hhs(df = hhs_train, 
                           x = discharge_score, 
                           y = dtc_rate,
                           z = quality_star_category) +
  labs(
    subtitle = "Discharge to Community Rate",
    y = NULL, 
    x = NULL
  )



## discharge_score vs pph_rate----
### There is no interaction 
discharge_pph <- interact_hhs(df = hhs_train, 
                           x = discharge_score, 
                           y = pph_rate,
                           z = quality_star_category) +
  labs(
    subtitle = "Potentially Preventable Hospitalization Rate",
    y = NULL, 
    x = NULL
  )



## discharge_score vs ppr_rate----
### There is no interaction 
discharge_ppr <- interact_hhs(df = hhs_train, 
                           x = discharge_score, 
                           y = ppr_rate,
                           z = quality_star_category) +
  labs(
    subtitle = "Potentially Preventable Readmissions Rate",
    y = NULL, 
    x = NULL
  )


## discharge_score vs medicare_spending_index----
### There is no interaction 
discharge_medicare <- interact_hhs(df = hhs_train, 
                                x = discharge_score, 
                                y = medicare_spending_index,
                                z = quality_star_category) +
  labs(
    subtitle = "Medicare Spending per Episode \n(Relative to National Average)",
    y = NULL, 
    x = NULL
  )


## discharge_score vs log10(num_episodes)----
### There is no interaction 
discharge_episodes <-interact_hhs(df = hhs_train, 
                               x = discharge_score, 
                               y = log10(num_episodes),
                               z = quality_star_category) +
  labs(
    y = NULL, 
    x = NULL,
    subtitle = "Log10 of Episodes Used \nfor Medicare Cost Calculation"
  )

## Interaction plot----
discharge_interact <- (discharge_patient | discharge_dtc) + 
  plot_annotation(title = "Interaction Effect of Discharge Function Score \n and Various Predictors",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )


ggsave(discharge_interact, 
       filename = here("interact_predict/discharge_score/discharge_interact.png"),
       width = 20,
       height = 15)

## Non-interaction plot----
discharge_non_interact <- (discharge_provider | discharge_pph) /
                          (discharge_ppr | discharge_medicare) /
                          (discharge_medicare | discharge_episodes) + 
  plot_annotation(title = "Non-Interaction Effect of Discharge Function Score \n and Various Predictors",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )


ggsave(discharge_non_interact, 
       filename = here("interact_predict/discharge_score/discharge_non_interact.png"),
       width = 25,
       height = 30)

  

##-------------------------------------------------------------------------------------
# transfer_to_provider----
## transfer_to_provider vs transfer_to_patient----
### There is no interaction 
provider_patient <- interact_hhs(df = hhs_train, 
                                  x = transfer_to_provider, 
                                  y = transfer_to_patient,
                                  z = quality_star_category) +
  labs(
    subtitle = "Transfer of Health Information \nto the Patient (%)",
    y = NULL, 
    x = NULL
  )


## transfer_to_provider vs dtc_rate----
### There is no interaction 
provider_dtc <- interact_hhs(df = hhs_train, 
                              x = transfer_to_provider, 
                              y = dtc_rate,
                              z = quality_star_category) +
  labs(
    subtitle = "Discharge to Community \nRate",
    y = NULL, 
    x = NULL
  )



## transfer_to_provider vs pph_rate----
### There is no interaction 
provider_pph <- interact_hhs(df = hhs_train, 
                              x = transfer_to_provider, 
                              y = pph_rate,
                              z = quality_star_category) +
  labs(
    subtitle = "Potentially Preventable \nHospitalization Rate",
    y = NULL, 
    x = NULL
  )



## transfer_to_provider vs ppr_rate----
### There is no interaction 
provider_ppr <- interact_hhs(df = hhs_train, 
                              x = transfer_to_provider, 
                              y = ppr_rate,
                              z = quality_star_category) +
  labs(
    subtitle = "Potentially Preventable\n Readmissions Rate",
    y = NULL, 
    x = NULL
  )


## transfer_to_provider vs medicare_spending_index----
### There is no interaction 
provider_medicare <- interact_hhs(df = hhs_train, 
                                   x = transfer_to_provider, 
                                   y = medicare_spending_index,
                                   z = quality_star_category) +
  labs(
    subtitle = "Medicare Spending per Episode (Relative to National Average)",
    y = NULL, 
    x = NULL
  )


## transfer_to_provider vs log10(num_episodes)----
### There is interaction 
provider_episodes <-interact_hhs(df = hhs_train, 
                                  x = transfer_to_provider, 
                                  y = log10(num_episodes),
                                  z = quality_star_category) +
  labs(
    y = NULL, 
    x = NULL,
    subtitle = "Log10 of Episodes Used for Medicare Cost Calculation"
  )

## Interaction plot----
provider_interact <- (provider_episodes) + 
  plot_annotation(title = "Interaction Effect of Information Trasnfer to \n  Provider and Various Predictors",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )


ggsave(provider_interact, 
       filename = here("interact_predict/transfer_to_provider/provider_interact.png"),
       width = 17,
       height = 15)

## Non-interaction plot----
provider_non_interact <- (provider_patient | provider_dtc) /
                         (provider_pph | provider_ppr) /
                         (provider_medicare) + 
  plot_annotation(title = "Non-Interaction Effect of Information Trasnfer to \n  Provider and Various Predictors",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )


ggsave(provider_non_interact, 
       filename = here("interact_predict/transfer_to_provider/provider_non_interact.png"),
       width = 17,
       height = 30)


##-------------------------------------------------------------------------------------
# transfer_to_patient----
## transfer_to_patient vs dtc_rate----
### There is no interaction 
patient_dtc <- interact_hhs(df = hhs_train, 
                             x = transfer_to_patient, 
                             y = dtc_rate,
                             z = quality_star_category) +
  labs(
    subtitle = "Discharge to Community Rate",
    y = NULL, 
    x = NULL
  )



## transfer_to_patient vs pph_rate----
### There is no interaction 
patient_pph <- interact_hhs(df = hhs_train, 
                             x = transfer_to_patient, 
                             y = pph_rate,
                             z = quality_star_category) +
  labs(
    subtitle = "Potentially Preventable Hospitalization Rate",
    y = NULL, 
    x = NULL
  )



## transfer_to_patient vs ppr_rate----
### There is no interaction 
patient_ppr <- interact_hhs(df = hhs_train, 
                             x = transfer_to_patient, 
                             y = ppr_rate,
                             z = quality_star_category) +
  labs(
    subtitle = "Potentially Preventable Readmissions Rate",
    y = NULL, 
    x = NULL
  )


## transfer_to_patient vs medicare_spending_index----
### There is no interaction 
patient_medicare <- interact_hhs(df = hhs_train, 
                                  x = transfer_to_patient, 
                                  y = medicare_spending_index,
                                  z = quality_star_category) +
  labs(
    subtitle = "Medicare Spending per Episode \n(Relative to National Average)",
    y = NULL, 
    x = NULL
  )


## transfer_to_patient vs log10(num_episodes)
### There is interaction 
patient_episodes <-interact_hhs(df = hhs_train, 
                                 x = transfer_to_patient, 
                                 y = log10(num_episodes),
                                 z = quality_star_category) +
  labs(
    y = NULL, 
    x = NULL,
    subtitle = "Log10 of Episodes Used for Medicare Cost Calculation"
  )

## Interaction plot----
patient_interact <- (patient_episodes) + 
  plot_annotation(title = "Interaction Effect of Information Trasnfer to \n  Patient and Various Predictors",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )


ggsave(patient_interact, 
       filename = here("interact_predict/transfer_to_patient/patient_interact.png"),
       width = 17,
       height = 15)

## Non-interaction plot----
patient_non_interact <- (patient_dtc | patient_pph) /
                        (patient_ppr | patient_medicare) +
  plot_annotation(title = "Non-Interaction Effect of Information Trasnfer to \n  Patient and Various Predictors",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )


ggsave(patient_non_interact, 
       filename = here("interact_predict/transfer_to_patient/patient_non_interact.png"),
       width = 24,
       height = 24)


##-------------------------------------------------------------------------------------
# dtc_rate----
## dtc_rate vs pph_rate----
### There is no interaction 
dtc_pph <- interact_hhs(df = hhs_train, 
                            x = dtc_rate, 
                            y = pph_rate,
                            z = quality_star_category) +
  labs(
    subtitle = "Potentially Preventable Hospitalization Rate",
    y = NULL, 
    x = NULL
  )



## dtc_rate vs ppr_rate----
### There is no interaction 
dtc_ppr <- interact_hhs(df = hhs_train, 
                            x = dtc_rate, 
                            y = ppr_rate,
                            z = quality_star_category) +
  labs(
    subtitle = "Potentially Preventable Readmissions Rate",
    y = NULL, 
    x = NULL
  )


## dtc_rate vs medicare_spending_index----
### There is no interaction 
dtc_medicare <- interact_hhs(df = hhs_train, 
                                 x = dtc_rate, 
                                 y = medicare_spending_index,
                                 z = quality_star_category) +
  labs(
    subtitle = "Medicare Spending per Episode \n(Relative to National Average)",
    y = NULL, 
    x = NULL
  )


## dtc_rate vs log10(num_episodes)----
### There is interaction 
dtc_episodes <-interact_hhs(df = hhs_train, 
                                x = dtc_rate, 
                                y = log10(num_episodes),
                                z = quality_star_category) +
  labs(
    y = NULL, 
    x = NULL,
    subtitle = "Log10 of Episodes Used for Medicare Cost Calculation"
  )

## Interaction plot----
dtc_interact <- (dtc_episodes) + 
  plot_annotation(title = "Interaction Effect of Discharge to Community \n and Various Predictors",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )


ggsave(dtc_interact, 
       filename = here("interact_predict/dtc_rate/dtc_interact.png"),
       width = 17,
       height = 15)

## Non-interaction plot----
dtc_non_interact <- (dtc_pph / dtc_ppr/ dtc_medicare) +
  plot_annotation(title = "Non-Interaction Effect of  Discharge to Community \n and Various Predictors",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )


ggsave(dtc_non_interact, 
       filename = here("interact_predict/dtc_rate/dtc_non_interact.png"),
       width = 18,
       height = 30)



##-------------------------------------------------------------------------------------
# pph_rate----
## pph_rate vs ppr_rate ----
### There is no interaction 
pph_ppr <- interact_hhs(df = hhs_train, 
                        x = pph_rate, 
                        y = ppr_rate,
                        z = quality_star_category) +
    labs(
    subtitle = "Potentially Preventable Readmissions Rate",
    y = NULL, 
    x = NULL
  )


## pph_rate vs medicare_spending_index----
### There is no interaction 
pph_medicare <- interact_hhs(df = hhs_train, 
                             x = pph_rate, 
                             y = medicare_spending_index,
                             z = quality_star_category) +
  labs(
    subtitle = "Medicare Spending per Episode (Relative to National Average)",
    y = NULL, 
    x = NULL
  )


## pph_rate vs log10(num_episodes) ----
### There is no interaction 
pph_episodes <-interact_hhs(df = hhs_train, 
                            x = pph_rate, 
                            y = log10(num_episodes),
                            z = quality_star_category) +
  labs(
    y = NULL, 
    x = NULL,
    subtitle = "Log10 of Episodes Used for Medicare Cost Calculation"
  )

## Non-interaction plot----
pph_non_interact <- (pph_ppr / pph_medicare/ pph_episodes) +
  plot_annotation(title = "Non-Interaction Effect of Potentially Preventable \nHospitalization and Various Predictors",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )


ggsave(pph_non_interact, 
       filename = here("interact_predict/pph_rate/pph_non_interact.png"),
       width = 18,
       height = 30)

##-------------------------------------------------------------------------------------
# ppr_rate----
## ppr_rate vs medicare_spending_index----
### There is interaction 
ppr_medicare <- interact_hhs(df = hhs_train, 
                             x = ppr_rate, 
                             y = medicare_spending_index,
                             z = quality_star_category) +
  labs(
    subtitle = "Medicare Spending per Episode \n(Relative to National Average)",
    y = NULL, 
    x = NULL
  )


## pph_rate vs log10(num_episodes)-----
### There is interaction 
ppr_episodes <-interact_hhs(df = hhs_train, 
                            x = ppr_rate, 
                            y = log10(num_episodes),
                            z = quality_star_category) +
  labs(
    y = NULL, 
    x = NULL,
    subtitle = "Log10 of Episodes Used \nfor Medicare Cost Calculation"
  )

## Interaction plot----
ppr_interact <- (ppr_medicare | ppr_episodes) +
  plot_annotation(title = "Interaction Effect of Potentially Preventable \nReadmissions and Various Predictors",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )


ggsave(ppr_interact, 
       filename = here("interact_predict/ppr_rate/ppr_interact.png"),
       width = 20,
       height = 15)

##-------------------------------------------------------------------------------------
# medicare_spending_index----
## medicare_spending_index vs log10(num_episodes)
### There is interaction 
medicare_episodes <-interact_hhs(df = hhs_train, 
                            x = medicare_spending_index, 
                            y = log10(num_episodes),
                            z = quality_star_category) +
  labs(
    y = NULL, 
    x = NULL,
    subtitle = "Log10 of Episodes Used for Medicare Cost Calculation"
  )

## Interaction plot----
medicare_interact <- (medicare_episodes) +
  plot_annotation(title = "Interaction Effect of Medicare Spending Indez\n and Various Predictors",
                  theme = theme(
                    plot.title = element_text(face = "bold", 
                                              size = 45,
                                              hjust = 0.5),
                  )
  )


ggsave(medicare_interact, 
       filename = here("interact_predict/medicare_spending_index/medicare_interact.png"),
       width = 17,
       height = 15)

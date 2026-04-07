# Final Project ----
# Stat 301-2
# Target analysis

## Load library ----
library(tidyverse)
library(naniar)
library(plotrix)
library(here)

## Load data ----
hhs_tidy <- read_rds("data/hhs_tidy.rds")
hhs_provider <- read_rds("data/hhs_provider.rds")


## Analyzing target distribution ----
rating_vis <- hhs_provider|>
  ggplot(aes(x = quality_star_rating))+
  geom_bar(fill = "darkcyan")+
  scale_x_continuous(breaks = seq(1, 5, by = 0.5))+
  scale_fill_brewer(palette = "Dark2")+
  theme_minimal()+
  labs(
    y = NULL,
    x = "Quality of Patient Care Star Rating",
    title = "Distribution of Quality of Patient Care Star",
    subtitle = "1–5 star rating assigned \nby CMS based on various performance measures"
  )+
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, face = "italic", hjust = 0.5),
    axis.title = element_text(size = 14, vjust = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )
  
ggsave(filename = "rating_vis.png",
       path = "target_check")

## NA table ----
na_target <- hhs_provider|>
  summarize(
    missing_values_total_number = sum(is.na(quality_star_rating)),
    percentage_missing_number = round(sum(is.na(quality_star_rating)) / nrow(hhs_provider) * 100, 2)
  )|>
  rename(
    "Missing values" =  missing_values_total_number,
    "Missing values percentage" = percentage_missing_number
  )
  
save(na_target, 
     file = here("target_check/na_target.rda"))



## Analyzing category distribution (after changing to binary) ----

### Calculate percent of each category
catgory_percent <- hhs_tidy|>
  count(quality_star_category)|>
  mutate(
    percent = round((n/sum(n))*100, 2)
  )|>
  pull(percent)

### Set up for pie chart
category_labels <- c("High", "Low")
category_color <- c("#FCCDE5", 	"#80B1D3")


### Set output file
png(here("target_check/category_pie.png"),
    width = 1400, height = 1600, res = 200)


### Create pie chart
pie3D(catgory_percent, 
      labels = paste0(category_labels, " (", catgory_percent, "%)"), 
      col = category_color, 
      explode=0.1,
      labelcex = 1.6, 
      shade = 0.6, 
      theta = 1,
      main = " Distribution of Quality of \nPatient Care Star Category Ratings",
      cex.main = 2.2,
      sub = "Based on CMS 1–5 Star Rating System \n(≥ 3.5 is High, < 3.5 is Low)",
      cex.sub = 1.8
      )

### Close the file
dev.off()


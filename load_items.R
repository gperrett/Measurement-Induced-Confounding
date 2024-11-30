library(redivis)

user <- redivis::user("datapages")
dataset <- user$dataset("item_response_warehouse:as2e:v11_25")
table <- dataset$table("depression_anxiety_stress:zsyg")

# Load table as tidyverse tibble
data <- table$to_tibble()
dass <- data |>
  tidyr::pivot_wider(id_cols = id,
                     names_from = item, 
                     values_from = resp, 
                     names_prefix = 'item'
  )

dass <- dass[, 18:59]

write_csv(dass, 'depression_anxiety_stress.csv')

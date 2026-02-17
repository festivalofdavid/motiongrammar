require(tidyverse)

## Test GPS data set
devtools::load_all()

gps_test2 <- initiate(source = 'guess_csv',
session = '/Users/david/Downloads/Adams_25_50657_13_10_09_.csv',
verbose = FALSE)

metadata_report(gps_test2)
quality_report(gps_test2)

|> 
  coordinate(norm = TRUE) |> 
  interpolate(max_gap_frames = 30) |> 
  filtrate() |> 
  derivate(window = 10) |> 
  designate() |> 
  allocate() |> 
  quantitate(allocation  = 'profile_1',
  designation = 'section_label',
  duration_s  = n(),
  mean_velocity  = mean(velocity, na.rm = TRUE),
  mean_angular_velocity = mean(angular_velocity, na.rm = TRUE),
  max_angular_velocity = quantile(angular_velocity, probs = 0.99),
  total_distance = sum(distance, na.rm = TRUE)
) |> 
  glimpse()


library(ggplot2)

gps_test3 <- initiate()
require(tidyverse)

## Test GPS data set
gps_test <- initiate(source = 'catapult_replay',
session = '/Users/david/Downloads/Adams_25_50657_13_10_09_.csv',
verbose = FALSE) |> 
  coordinate(norm = TRUE) |> 
  interpolate(max_gap_frames = 20) |> 
  filtrate() |> 
  derivate(window = 5) |> 
  glimpse()


## Test on my strava defaults
strava_test <- initiate(verbose = FALSE) |> 
  coordinate(norm = TRUE) |> 
  filtrate() |> 
  derivate(window = 5) |> 
  glimpse()

gps_test |> 
  ggplot(aes(x = unix_time,
  y = velocity,
color = is_interpolated))+
  geom_point()

require(tidyverse)

## Test GPS data set
gps_test <- initiate(source = 'catapult_replay',
session = '/Users/david/Downloads/Adams_25_50657_13_10_09_.csv',
verbose = FALSE) |> 
  coordinate(norm = TRUE)

## Test on my strava defaults
strava_test <- initiate(verbose = FALSE) |> 
  coordinate(norm = TRUE) |> 
  glimpse() |> 
  filtrate(cutoff = 0.001) |> 
  glimpse()


strava_test |> 
  ggplot(aes(x = unix_time, y = x_filt),
color = 'red',
alpha = 0.76)+
  geom_point()+
  geom_point(aes(x = unix_time, y = x),
color = 'blue',
alpha = 0.76)

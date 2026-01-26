

## Test GPS data set
gps_test <- initiate(source = 'catapult_replay',
session = '/Users/david/Downloads/Adams_25_50657_13_10_09_.csv') |> 
  coordinate()

## Test on my strava defaults
strava_test <- initiate() |> 
  coordinate()


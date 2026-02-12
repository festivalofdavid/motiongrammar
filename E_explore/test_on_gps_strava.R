require(tidyverse)
require(patchwork)

## Test GPS data set
devtools::load_all()
gps_test <- initiate(source = 'catapult_replay',
session = '/Users/david/Downloads/Adams_25_50657_13_10_09_.csv',
verbose = FALSE) |> 
  coordinate(norm = TRUE) |> 
  interpolate(max_gap_frames = 25) |> 
  filtrate() |> 
  derivate(window = 25) |> 
  designate() |> 
  allocate() |> 
quantitate(
  allocation  = 'profile_1',
  designation = 'section_label',
  duration_s  = n(),
  mean_velocity  = mean(velocity, na.rm = TRUE)
) |> 
  glimpse()


glimpse(gps_test)


# A functional approach to create individual audit plots
create_band_plot <- function(.data, y_var, color_var, title_label) {
  ggplot2::ggplot(.data, aes(x = unix_time, y = !!sym(y_var), color = !!sym(color_var))) +
    # Use linetype to visually audit the interpolation flag
    ggplot2::geom_line(aes(linetype = is_interpolated), size = 0.8) +
    ggplot2::scale_color_viridis_d(option = 'viridis', name = 'Intensity') +
    ggplot2::scale_linetype_manual(values = c('FALSE' = 'solid', 'TRUE' = 'dotted')) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = title_label, x = NULL, y = 'Magnitude') +
    ggplot2::theme(legend.position = 'none')
}

# Generate the three panels
v_p  <- create_band_plot(gps_test, 'velocity', 'velocity_band', 'Velocity Profile')
a_p  <- create_band_plot(gps_test, 'acceleration', 'acceleration_band', 'Acceleration Profile')
av_p <- create_band_plot(gps_test, 'angular_velocity', 'angularvelocity_band', 'Angular Velocity Profile')

# Combine using patchwork for an instant dashboard
# We collect the guides to keep the legend clean at the bottom
(v_p / a_p / av_p) + 
  patchwork::plot_layout(guides = 'collect') & 
  theme(legend.position = 'bottom') &
  labs(caption = 'Solid = Real Data | Dotted = Interpolated (is_interpolated flag)')










## Test on my strava defaults
strava_test <- initiate(verbose = FALSE) |> 
  coordinate(norm = TRUE) |> 
  interpolate() |> 
  filtrate() |> 
  derivate(window = 5) |> 
  designate()

strava_test |> 
  filter(velocity < 10) |> 
  ggplot(aes(x = unix_time,
  y = velocity))+
  geom_point()

gps_test |> 
  ggplot(aes(x = unix_time,
  y = velocity,
color = is_interpolated))+
  geom_point()

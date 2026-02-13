require(tidyverse)

## Test GPS data set
devtools::load_all()

gps_test2 <- initiate(source = 'guess_csv',
session = '/Users/david/Downloads/Adams_25_50657_13_10_09_.csv',
verbose = FALSE) |> 
  coordinate(norm = TRUE) |> 
  interpolate(max_gap_frames = 25) |> 
  filtrate() |> 
  derivate(window = 25) |> 
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

# 1. Path colored by angular velocity
ggplot(gps_test2, aes(x = f_x, y = f_y, color = angular_velocity * (180/pi))) +
  geom_path(size = 1) +
  scale_color_viridis_c(option = "plasma") +
  labs(title = "Path colored by Angular Velocity",
       color = "Angular Vel\n(deg/s)") +
  coord_equal() +
  theme_minimal()

# 2. Scatter plot: angular velocity vs velocity
ggplot(gps_test2, aes(x = velocity, y = angular_velocity * (180/pi))) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Angular Velocity vs Linear Velocity",
       x = "Velocity (m/s)",
       y = "Angular Velocity (deg/s)") +
  theme_minimal()

# 3. Time series - see the turns
ggplot(gps_test2, aes(x = unix_time, y = angular_velocity * (180/pi))) +
  geom_line() +
  geom_hline(yintercept = 90, linetype = "dashed", color = "red") +
  labs(title = "Angular Velocity over Time",
       x = "Time",
       y = "Angular Velocity (deg/s)") +
  theme_minimal()

# 4. Highlight sharp turns on path
sharp_turns <- gps_test2 |> 
  filter(angular_velocity * (180/pi) > 45)  # >45 deg/s

ggplot(gps_test2, aes(x = f_x, y = f_y)) +
  geom_path(color = "gray50") +
  geom_point(data = sharp_turns, aes(color = angular_velocity * (180/pi)), size = 3) +
  scale_color_viridis_c(option = "plasma") +
  labs(title = "Sharp Turns (>45°/s)",
       color = "Angular Vel\n(deg/s)") +
  coord_equal() +
  theme_minimal()
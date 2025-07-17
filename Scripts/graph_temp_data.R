library(tidyverse)
library(lubridate)

experiment_starts <- c(
  "Fri Jun 13 14:56:00 2025",
  "Sat Jun 14 12:23:00 2025",
  "Mon Jun 16 12:05:00 2025",
  "Tue Jun 17 11:52:00 2025",
  "Fri Jun 20 13:38:00 2025",
  "Fri Jun 27 16:23:00 2025"
)

incubator_settings <- tibble(
  experiment_date = c("06_13_2025", "06_14_2025", "06_16_2025", "06_17_2025", "06_20_2025", "06_27_2025"),
  incubator_temp = c("40.5 °C Incubator Temp", "30.5 °C Incubator Temp", "35.0 °C Incubator Temp", 
                     "38.5 °C Incubator Temp", "42.5 °C Incubator Temp", "44.5 °C Incubator Temp")
)

experiment_info <- tibble(
  experiment_start = as.POSIXct(experiment_starts, format = "%a %b %d %H:%M:%S %Y", tz = "America/New_York")
) %>%
  mutate(experiment_date = format(experiment_start, "%m_%d_%Y"))


# Function to process a single file
process_file <- function(file, experiment_start, experiment_date) {
  mission_start_line <- readLines(file, n = 4)[4]
  mission_start <- str_extract(mission_start_line, "\\w{3} \\w{3} \\d+ \\d{2}:\\d{2}:\\d{2} EDT \\d{4}")
  mission_start <- sub(" EDT", "", mission_start)
  mission_start <- as.POSIXct(mission_start, format = "%a %b %d %H:%M:%S %Y", tz = "America/New_York")
  
  # Read the CSV skipping metadata
  data <- read_csv(file, skip = 14)
  
  # Parse time and filter from mission start only
  data <- data %>%
    mutate(time = mdy_hms(`Date/Time`, tz = "America/New_York")) %>%
    filter(time >= experiment_start) %>%
    mutate(mission_start = mission_start) %>%  
    mutate(experiment_start = experiment_start) %>%
    mutate(experiment_date = experiment_date) %>%
    mutate(filename = file) %>%
    mutate(elapsed_minutes = as.numeric(difftime(time, experiment_start, units = "mins")))
  
  return(data)
}


# Process all files
# all_data <- list.files(folder, pattern = "\\.csv$", full.names = TRUE) %>%
#   map_dfr(process_file)

# Process all experiments
all_data <- map_dfr(1:nrow(experiment_info), function(i) {
  exp_start <- experiment_info$experiment_start[i]
  exp_date <- experiment_info$experiment_date[i]
  folder <- paste0("./Raw_Data/temp_data/", exp_date)
  
  list.files(folder, pattern = "\\.csv$", full.names = TRUE) %>%
    map_dfr(process_file, experiment_start = exp_start, experiment_date = exp_date)
})

all_data <- all_data %>%
  mutate(short_name = str_sub(filename, 3)) %>%
  left_join(incubator_settings, by = "experiment_date")


# Filter to only first 2 hours
filtered_data <- all_data %>%
  filter(elapsed_minutes <= 120)

temp_plot <- ggplot(filtered_data, aes(x = elapsed_minutes, y = Value, color = as.factor(incubator_temp))) +
  geom_smooth(se = FALSE) +
  labs(
    x = "Elapsed Time (minutes)",
    y = "Average Temperature (°C)",
    color = "Incubator Temp (°C)",
    title = "First Two Hours of Temperature Logger Readings"
  ) +
  scale_y_continuous(limits = c(20, 45)) +
  scale_x_continuous(limits = c(0, 120)) +
  theme_minimal()

temp_plot


ggsave(
  filename = "temp_plot_all.png",
  plot = temp_plot,
  path = "./Figures"
)

  # ggsave(
  #   filename = paste0("temp_plot_", experiment_date, "_seperate.png"),
  #   plot = seperated_plot,
  #   path = paste0("./Figures/", experiment_date, "_exp/temp_graphs")
  # )


# # Average by time and mission_start
# averaged_data <- filtered_data %>%
#   group_by(mission_start, elapsed_minutes) %>%
#   summarize(avg_temp = mean(Value), .groups = "drop")
# 
# combined_plot <- ggplot(averaged_data, aes(x = elapsed_minutes, y = avg_temp)) +
#   geom_line(size = 1) +
#   labs(x = "Elapsed Time (minutes)", y = "Average Temperature (°C)", color = "Start Time") +
#   scale_y_continuous(limits = c(20, 45)) +
#   scale_x_continuous(limits = c(0, 120)) +
#   ggtitle("06_20_25 Temperature Logger Readings")
#   theme_minimal()
#   
# combined_plot
#   ggsave(
#     filename = paste0("temp_plot_", experiment_date, "_combined.png"),
#     plot = seperated_plot,
#     path = paste0("./Figures/", experiment_date, "_exp/temp_graphs")
#   )
  



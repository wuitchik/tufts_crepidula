library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(stringr)


read_crep_data <- function(file_path) {
  column_names <- c("Time", paste0("Crep_", 1:10))
  first_line <- readLines(file_path, n = 1)
  
  if (grepl("^2025", first_line)) {
    #Files with no headers are the files that contain the data from when the 
    #experiment started to the end of that hour
    data <- read.csv(file_path, header = FALSE)
  } else {
    #Files with annoying headers you need to skip 34 lines
    #These files begin from each new hour after the experiment started
    data <- read.csv(file_path, skip = 34, header = FALSE)
  }
  colnames(data) <- column_names
  
  # Add a filename column to track origin
  # data$SourceFile <- basename(file_path)
  
  return(data)
}

#The day the experiment started
date <- "06_27_2025"
#The associated temperature the incubator was set at 
temp <- "42.5"
sensor_names = paste("Crepidula Sensor", 1:10)

# Folder containing your CSVs
folder_path <- paste0("./Raw_Data/heart_data/", date, "_exp/original_format/")

# List all .csv files in the folder
file_list <- list.files(folder_path, pattern = "(?i)\\.csv$", full.names = TRUE)

# Apply the function and combine results
combined_data <- bind_rows(lapply(file_list, read_crep_data))

# Convert the first column (timestamp) to POSIXct
times <- as.POSIXct(combined_data[[1]], format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")

# Convert to mins since the first timestamp
combined_data$elapsed_minutes <- as.numeric(difftime(times, times[1], units = "mins"))

# Filter to only first 2 hours
combined_data <- combined_data %>%
  filter(elapsed_minutes <= 120)

colnames(combined_data) <- c(
  "Time",
  sensor_names,
  "Elapsed_Time"
)

long_data <- pivot_longer(
  data = combined_data, 
  cols = c(sensor_names),
  names_to = "Sensor",
  values_to = "Signal"
)

long_data$Sensor <- factor(long_data$Sensor, levels = sensor_names)


ggplot(long_data, aes(x = Elapsed_Time, y = Signal)) +
  geom_line(color = "steelblue") +
  scale_x_continuous(limits = c(0, 120)) +
  scale_y_continuous(limits = c(0, 4000)) +
  facet_wrap(~Sensor, ncol = 2, dir = "v") +
  theme_classic() +
  theme (
    plot.title = element_text(hjust = 0.5),
    strip.background = element_blank()
  ) +
  labs(title = paste0(temp, "°C Incubator Temperature"),
       x = "Elapsed Time (Minutes)", y = "Signal")


# plots <- 
#   ggplot(combined_data, aes(x = timestamp_minutes, y = .data[[col]])) +
#     geom_line(color = "steelblue", size = 0.3) +
#     labs(title = paste0("Crepidula Sensor ", str_sub(col, 6)),
#          x = "Elapsed Time (Minutes)", y = "Signal") +
#     scale_x_continuous(limits = c(0, 120)) +
#     scale_y_continuous(limits = c(0, 4000)) +
#     theme_minimal() +
    # theme (
    #   plot.title = element_text(hjust = 0.5),
    #   panel.grid.major = element_blank(),
    #   panel.grid.minor = element_blank(),
    #   axis.line = element_line()
    # )



# List of heartbeat column names
# crep_cols <- paste0("Crep_", 1:10)

# Use lapply to plot each heartbeat signal
# plots <- lapply(crep_cols, function(col) {
#   ggplot(combined_data, aes(x = timestamp_minutes, y = .data[[col]])) +
#     geom_line(color = "steelblue", size = 0.3) +
#     labs(title = paste0("Crepidula Sensor ", str_sub(col, 6)),
#          x = "Elapsed Time (Minutes)", y = "Signal") +
#     scale_x_continuous(limits = c(0, 120)) +
#     scale_y_continuous(limits = c(0, 4000)) +
#     theme_minimal() +
#     theme (
#       plot.title = element_text(hjust = 0.5),
#       panel.grid.major = element_blank(),
#       panel.grid.minor = element_blank(),
#       axis.line = element_line()
#     )
# })
# #Save the 10 plots for that date
# for (i in seq_along(plots)) {
#   ggsave(
#     filename = paste("Time_vs", crep_cols[i], date, temp, ".png", sep="_"),
#     plot = plots[[i]],
#     width = 8, height = 4, dpi = 300
#   )
# }



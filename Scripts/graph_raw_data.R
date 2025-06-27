library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork)

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
  data$SourceFile <- basename(file_path)
  
  return(data)
}

#The day the experiment started
date <- "06_20_25"
#The associated temperature the incubator was set at 
temp <- "42.5"

# Folder containing your CSVs
#Note this might change based on how you set up your computer 
folder_path <- paste0("./Crepidula Data 2025/", date, "/")

# List all .csv files in the folder
file_list <- list.files(folder_path, pattern = "(?i)\\.csv$", full.names = TRUE)

# Apply the function and combine results
combined_data <- bind_rows(lapply(file_list, read_crep_data))

# Convert the first column (timestamp) to POSIXct
times <- as.POSIXct(combined_data[[1]], format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")

# Convert to hours since the first timestamp
combined_data$timestamp_hours <- as.numeric(difftime(times, times[1], units = "hours"))

# List of heartbeat column names
crep_cols <- paste0("Crep_", 1:10)

# Use lapply to plot each heartbeat signal
plots <- lapply(crep_cols, function(col) {
  ggplot(combined_data, aes(x = timestamp_hours, y = .data[[col]])) +
    geom_line(color = "steelblue") +
    labs(title = paste("Time vs", col),
         x = "Time (Hrs)", y = "Signal") +
    theme_minimal()
})

# #Save the 10 plots for that date
# for (i in seq_along(plots)) {
#   ggsave(
#     filename = paste("Time_vs", crep_cols[i], date, temp, ".png", sep="_"),
#     plot = plots[[i]],
#     width = 8, height = 4, dpi = 300
#   )
# }



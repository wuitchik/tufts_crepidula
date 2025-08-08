
library(ggplot2)
library(patchwork)
library(zoo)
library(tidyverse)



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
date <- "06_17_2025"
#The associated temperature the incubator was set at 
temp <- "38.5"
sensor_names = paste("Crepidula Sensor", 1:10)

# Folder containing your CSVs
folder_path <- paste0("./Raw_Data/heart_data/", date, "_exp/subsetted_original_format/")

# List all .csv files in the folder
file_list <- list.files(folder_path, pattern = "(?i)\\.csv$", full.names = TRUE)

# Apply the function and combine results
combined_data <- bind_rows(lapply(file_list, read_crep_data))

# Convert the first column (timestamp) to POSIXct
times <- as.POSIXct(combined_data[[1]], format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")

# Convert to mins since the first timestamp
combined_data$elapsed_minutes <- as.numeric(difftime(times, times[1], units = "mins"))

# Filter to only first 2 hours
# combined_data <- combined_data %>%
#   filter(elapsed_minutes <= 120)

#Function to find time of death based on if heart sd sits below threshold 
#for at least 1 min. Note threshold is defined by average heartbeat sd of last hour of exp. 

find_threshold <- function(curr_crep, threshold, threshold_duration = 1) {
  below_threshold <- curr_crep[["amplitude_sd"]] < threshold
  n <- nrow(curr_crep)
  
  i <- 1
  while (i <= (n - 1)) {
    if (isTRUE(below_threshold[i])) {
      # Get the time where it dips
      dip_time <- curr_crep[["elapsed_minutes"]][i]
      
      # Check if it stays below threshold for next 1 minute
      window_end <- dip_time + threshold_duration
      threshold_window <- curr_crep[curr_crep[["elapsed_minutes"]] >= dip_time & curr_crep[["elapsed_minutes"]] <= window_end, ]
      
      if (all(threshold_window[["amplitude_sd"]] < threshold, na.rm = TRUE)) {
        return(i)  # Return the row index
      } else {
        # Find the first point in the window where it went back above the threshold
        above_sd <- which(threshold_window[["amplitude_sd"]] >= threshold)
        
        if (length(above_sd) > 0) {
          # Get the above threshold time
          time_back_above <- threshold_window[["elapsed_minutes"]][min(above_sd)]
          
          # Skip to just after that point
          i <- which(curr_crep[["elapsed_minutes"]] > time_back_above)[1]
          if (is.na(i)) {
            return(NA)
          }
        }
      }
    } else {
      i <- i + 1
    }
  }
  return(NA)  # No valid dip found
}

#Skip Crep_5
crep_cols <- c(paste0("Crep_", 1:4), paste0("Crep_", 6:10))
# crep_cols <- c(paste0("Crep_", 1:2))

#5 minute window size
window_size <- 6000

all_plots <- list()

# Loop through each column
for (colname in crep_cols) {
  
  curr_crep <- combined_data %>%
    select(elapsed_minutes, amplitude = all_of(colname)) %>%
    mutate(
      amplitude_mean = rollapply(amplitude, width = window_size, FUN = mean, align = "right", fill = NA),
      amplitude_sd   = rollapply(amplitude, width = window_size, FUN = sd, align = "right", fill = NA)
    )
  
  scale_factor <- max(curr_crep$amplitude_mean, na.rm = TRUE) / max(curr_crep$amplitude_sd, na.rm = TRUE)
  print(scale_factor)
  #No threshold graph
  
  # p <- ggplot(curr_crep, aes(x = elapsed_minutes)) +
  #   geom_line(aes(y = amplitude), color = "blue") +
  #   geom_line(aes(y = amplitude_sd * scale_factor), color = "red") +
  #   scale_y_continuous(
  #     name = "Amplitude",
  #     sec.axis = sec_axis(~ . / scale_factor, name = "Standard Deviation")
  #   ) +
  #   labs(title = paste(colname, temp, date)) +
  #   xlab("Time Since Experiment Start (Minutes)") +
  #   theme_minimal() +
  #   theme(
  #     axis.title.y.left = element_text(color = "blue"),
  #     axis.title.y.right = element_text(color = "red")
  #   )
  

  
  max_time <- max(curr_crep$elapsed_minutes, na.rm = TRUE)
  last_hour <- curr_crep[curr_crep$elapsed_minutes >= (max_time - 60), ]
  threshold <- mean(last_hour$amplitude_sd, na.rm = TRUE)
  
  #threshold_index <- find_threshold(curr_crep = curr_crep, threshold = threshold, threshold_duration = .25)
  threshold_index <- min(which(curr_crep[["amplitude_sd"]] < threshold))
  # View the point if found
  if (!is.na(threshold_index)) {
    threshold_point <- curr_crep[threshold_index, ]
    threshold_time <- threshold_point$elapsed_minutes
    
    force(scale_factor)
    force(threshold_time)
    force(threshold)
    
    
    p <- ggplot(curr_crep, aes(x = elapsed_minutes)) +
      geom_line(aes(y = amplitude), color = rgb(0, 0, 1, 0.2)) +
      geom_line(aes(y = amplitude_sd * scale_factor), color = "red") +
      scale_y_continuous(
        name = "Amplitude",
        sec.axis = sec_axis(~ . / scale_factor, name = "SD")
      ) +
      geom_vline(
       xintercept = threshold_time,
        color = "darkgreen",
       linewidth = 1.0,
       linetype = "dashed"
      ) +
      annotate("text", x = threshold_time + 80, y = 1800, 
                                  label = paste("Threshold:", round(threshold), 
                                        "\nTime:", round(threshold_time)), size = 3) +
      labs(title = paste(colname), 
           x = "Time (Min)") +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        plot.title = element_text(size = 10),
        axis.line.y.left = element_line(color="grey"),
        axis.line.y.right = element_line(color="grey"),
        axis.line.x = element_line(color = "black"),
        axis.ticks.y.left = element_line(color = "blue"),
        axis.ticks.y.right = element_line(color = "red"),
        axis.title.y.left = element_text(color = "blue"),
        axis.title.y.right = element_text(color = "red")
      )
    
    all_plots[[colname]] <- p + NULL
    
    # ggsave(filename = paste(colname, temp, date, "test.png", sep="_"),
    #        path = paste0("./Figures/", date, "_exp/analysis_raw_data_graphs"),
    #        plot = p, bg = "white")
    # 
  } else {
    print(paste0("No valid sustained dip below threshold in ", colname, " found."))
  }
  

}

third_plots <- all_plots[1:9]

p0 <- ggplot() + labs(title=paste(date, temp), x = "Time (Min)")

p1 <- wrap_plots(third_plots, ncol=3)


final_plot <- p1 + plot_layout(axes = "collect_x", axis_titles = "collect")

final_plot



ggsave(filename = paste(temp, date, "all_combined_threshold_firstencounter_analysis_raw.png", sep="_"),
       path = paste0("./Figures/", date, "_exp/analysis_raw_data_graphs"),
       plot = final_plot, bg = "white")

all_plots[[1]]



# library(pracma)
# 
# peaks <- findpeaks(temp_data$Crep_1, npeaks = 500)
# peaks_df <- temp_data[peaks, ]
# 
# 
# temp_data <- combined_data %>% select(Time, Crep_1, elapsed_minutes)
# 
# temp_data <- mutate(temp_data, Crep_1 = log(Crep_1))
# 
# temp_data <- temp_data[temp_data$Crep_1 > 0, ]
# 
# 
# ggplot(temp_data, aes(x = elapsed_minutes, y = Crep_11)) +
#   geom_line(color = "blue") +  # all data
#   labs(title = "Amplitude with Detected Peaks", x = "Time", y = "Amplitude") +
#   ylim(0, 9)
#   theme_minimal()

devtools::load_all("Scripts/heartbeatr-main")
library(heartbeatr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)

paths <- dir("Raw_Data/heart_data/06_14_2025_exp/subsetted_heartbeatr_format", full.names = TRUE)
paths <- pulse_example()

# pulse_data <- pulse_read(
#   paths, 
#   msg = FALSE
# )
# 
# pulse_data_split <- pulse_split(
#   pulse_data,
#   window_width_secs = 30,
#   window_shift_secs = 60,
#   min_data_points = 0.8, 
#   msg = FALSE
# )
# 
# 
# pulse_data_optimized <- pulse_optimize(
#   pulse_data_split,
#   interpolation_freq = 40,
#   bandwidth = 0.75,
#   raw_v_smoothed = FALSE,
#   multi = TRUE
# )
# 
# 
# heart_rates <- pulse_heart(
#   pulse_data_optimized
# )
# 
# 
# heart_rates <- pulse_doublecheck(
#   heart_rates
# )
# 
# 
# heart_rates <- pulse_choose_keep(
#   heart_rates
# )




heart_rates2 <- PULSE(
  paths,
  window_width_secs  = 60,
  window_shift_secs  = 5,
  min_data_points    = 0.8,
  interpolation_freq = 40,
  bandwidth          = 0.75,
  raw_v_smoothed     = TRUE
)

#Get BPM
heart_rates2$bpm <- heart_rates2$hz * 60

#Using built in graph making from package

# # Loop through IDs c01 to c10
# for (i in 1:10) {
#   # Construct ID string with leading zero
#   id <- sprintf("c%02d", i)
#   
#   # Create the plot
#   p <- pulse_plot(heart_rates2, ID = id, bpm = TRUE) +
#     xlab("time") +
#     ggtitle(paste("Crepidula", i, "06_20_25 Exp Dataset"))
#   
#   # Save the plot
#   ggsave(
#     filename = paste0("heartbeatr_plot_06_20_2025 Subsetted", id, ".png"),
#     plot = p,
#     path = "./Figures/06_20_2025_exp/heartbeatr_graphs"
#   )
# }

heart_rates2 <- heart_rates2 %>%
  mutate(
    experiment_start = min(time, na.rm = TRUE),
    hours_since_start = as.numeric(difftime(time, experiment_start, units = "hours")),
    mins_since_start = as.numeric(difftime(time, experiment_start, units = "mins"))
  )


#Option 1: Have one graph with bpm and bpm sd as a gradient 

# List of IDs from c01 to c10
id_list <- sprintf("c%02d", 1:10)

# Loop through each ID
for (id in id_list) {
  filtered_data <- heart_rates2 %>%
    filter(id == !!id, keep == TRUE)
  
  p <- ggplot(filtered_data, aes(x = mins_since_start, y = bpm, color = sd)) +
    geom_point() +
    geom_smooth(color = "green", se = FALSE) +
    scale_color_gradient(low = "blue", high = "red", limits = c(0, .8) ) +
    labs(
      x = "Time Since Experiment Start (Minutes)",
      y = "Beats Per Minute",
      color = "SD"
    ) +
    xlim(0, 120) + 
    ylim(0, 160)
    theme_minimal()
  
  # Save plot
  ggsave(filename = paste0("gradient_", id, "width60_shift5.png"), plot = p, bg = "white", path = "./Figures/06_20_2025_exp/heartbeatr_graphs")
}

#Option 2: Have bpm sd and bpm on seperate graphs

custom_limits <- tibble::tibble(
  variable = c("bpm", "sd"),
  ymin = c(0, 0),      # Replace with your desired mins
  ymax = c(160, .8)    # Replace with your desired maxes
)


for (id in id_list) {
  filtered_data2 <- heart_rates2 %>%
    filter(id == !!id, keep == TRUE) %>%
    select(mins_since_start, bpm, sd) %>%
    pivot_longer(cols = c(bpm, sd), names_to = "variable", values_to = "value") %>%
    left_join(custom_limits, by = "variable")
  

  
  p <- ggplot(filtered_data2, aes(x = mins_since_start, y = value)) +
    geom_point(data = filtered_data2 %>% filter(variable == "bpm"), color = "blue") +
    geom_line(data = filtered_data2 %>% filter(variable == "sd"), color = "red") +
    geom_blank(aes(y = ymin)) + 
    geom_blank(aes(y = ymax)) + 
    facet_wrap(~variable, ncol = 1, scales = "free_y") +
    labs(
         x = "Time Since Experiment Start (Minutes)",
         y = "Beats Per Minute") +
    theme_minimal() + 
    theme(strip.text = element_blank())
  
  # Save plot
  ggsave(filename = paste0("seperate_graph_", id, "width60_shift5.png"), plot = p, bg = "white", path = "./Figures/06_20_2025_exp/heartbeatr_graphs")
}



# only_c01 <- heart_rates2[heart_rates2$id == "c01",] 
# 
# new_data <- pulse_summarise(heart_rates2, FUN = stats::median, span_mins = 2)
# 
# pulse_plot(new_data, ID = "c01", bpm=TRUE)
# 
# pulse_plot_raw(heart_rates2, ID = "c01", target_i = 2, range = 1)
# 
# pulse_plot_raw(heart_rates2, ID = "c01", target_i = 1)

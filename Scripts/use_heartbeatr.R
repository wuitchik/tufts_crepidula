devtools::load_all("Scripts/heartbeatr-main")
library(heartbeatr)
library(ggplot2)

paths <- dir("Heart_Rate/raw_heart_data/06_20_2025_exp/heartbeatr_format", full.names = TRUE)
# paths <- pulse_example()

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
  window_width_secs  = 30,
  window_shift_secs  = 30,
  min_data_points    = 0.8,
  interpolation_freq = 40,
  bandwidth          = 0.75,
  raw_v_smoothed     = TRUE
)

#Get BPM
heart_rates2$bpm <- heart_rates2$hz * 60

# Loop through IDs c01 to c10
for (i in 1:10) {
  # Construct ID string with leading zero
  id <- sprintf("c%02d", i)
  
  # Create the plot
  p <- pulse_plot(heart_rates2, ID = id, bpm = TRUE) +
    xlab("time") +
    ggtitle(paste("Crepidula", i, "06_20_25 Exp Dataset"))
  
  # Save the plot
  ggsave(
    filename = paste0("heartbeatr_plot_06_20_25 Subsetted", id, ".png"),
    plot = p,
    path = "./Figures/06_20_25_exp/heartbeatr_graphs"
  )
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

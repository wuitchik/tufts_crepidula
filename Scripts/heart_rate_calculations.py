# %%
# Section 1: Load Libraries
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy.signal import find_peaks

# %%
# Section 2: Load Data
file_path = '/Users/danielwuitchik/Documents/Experiments/Crepidula_tufts/Heart_rate/Day_2/Day_2_combined.csv'
df = pd.read_csv(file_path)

# %%
# Section 3: Preprocess Data (Add Time Column and Set Index)
# Create the Time column in hours (Time in seconds / 3600 to convert to hours)
df['Time'] = df.index / (20 * 60 * 60)  # 20 Hz frequency, converting to hours

# Set the Time column as the index (similar to row names in R)
df.set_index('Time', inplace=True)

# %%
# Section 4: Standard Deviation Analysis (Rolling Variation)

# Calculate the 10-minute rolling standard deviation (window of 12,000 samples)
rolling_window = 2400  # 10 minutes * 60 seconds * 20 Hz = 12,000 samples
rolling_std_df = df.rolling(window=rolling_window).std()

# Number of subplots
num_cols = rolling_std_df.shape[1]

# Create a figure with multiple subplots (one for each column)
fig, axes = plt.subplots(nrows=num_cols, ncols=1, figsize=(10, 2 * num_cols), sharex=True)

# Plot the rolling standard deviation for each snail in its own subplot
for i, col in enumerate(rolling_std_df.columns):
    axes[i].plot(rolling_std_df.index, rolling_std_df[col], label=f'Rolling Std Dev {col}')
    axes[i].set_title(f'{col}')
    axes[i].set_ylabel('Pulse Variation (Std Dev)')
    
    # Set the x-tick locator to 15-minute intervals (0.25 hours)
    axes[i].xaxis.set_major_locator(plt.MultipleLocator(0.25))  # 0.25 hours = 15 minutes

# Set the x-label only on the last subplot
axes[-1].set_xlabel('Time (Hours)')

# Adjust layout to prevent overlap
plt.tight_layout()
plt.show()

# %%: Faceted Plot for Heartbeat Detection and BPM Overlaid

# Set parameters
variation_threshold = 500  # Rough estimate of amplitude range at end of experiment, where it was just noise
min_peak_to_valley_range = 800  # Minimum amplitude difference between peak and valley, again used based on noise at end
bpm_smoothing_window = 200  # Smoothing window for the BPM rolling average (in number of beats)

# Set rolling window parameters
smoothing_window = 100   # Smoothing window size

# Create a dictionary to store detected peaks (heartbeats) for each snail
heartbeats = {}

# Number of snails (columns in the dataframe)
num_cols = df.shape[1]

# Create a figure with multiple subplots (one for each snail)
fig, axes = plt.subplots(nrows=num_cols, ncols=1, figsize=(10, 2 * num_cols), sharex=True)

for i, col in enumerate(df.columns):
    raw_signal = df[col]
    
    # Detect peaks (local maxima)
    peaks, _ = find_peaks(raw_signal, distance=15)
    valleys, _ = find_peaks(-raw_signal, distance=15)
    
    # Filter peaks based on amplitude range
    valid_peaks = []
    for peak in peaks:
        previous_valleys = valleys[valleys < peak]
        if len(previous_valleys) > 0:
            nearest_valley = previous_valleys[-1]
            peak_to_valley_range = raw_signal.iloc[peak] - raw_signal.iloc[nearest_valley]
            if peak_to_valley_range >= min_peak_to_valley_range:
                valid_peaks.append(peak)
    
    heartbeats[col] = valid_peaks
    
    # Calculate the time between valid peaks in seconds
    if len(valid_peaks) > 1:
        time_between_beats = np.diff(df.index[valid_peaks]) * 3600
        no_beat_gap_idx = np.where(time_between_beats >= 60)[0]
        if len(no_beat_gap_idx) > 0:
            first_no_beat_gap_time = df.index[valid_peaks][no_beat_gap_idx[0] + 1]
        else:
            first_no_beat_gap_time = None
    else:
        first_no_beat_gap_time = None
    
    # Select the appropriate axis for this snail
    ax1 = axes[i] if num_cols > 1 else axes

    # Plot raw signal and detected valid heartbeats without legend labels
    ax1.plot(df.index, raw_signal, alpha=0.5, linewidth=1.5)
    ax1.plot(df.index[valid_peaks], raw_signal.iloc[valid_peaks], "x", color='red', alpha=0.5)
    
    # Add vertical line for time of death if it exists, with label for legend
    if first_no_beat_gap_time:
        ax1.axvline(first_no_beat_gap_time, color='blue', linestyle='--', linewidth=3, label=f'Time of Death: {first_no_beat_gap_time:.2f} hours')
    
    ax1.set_ylabel('Pulse Amplitude')
    
    # Create secondary y-axis for BPM
    ax2 = ax1.twinx()
    
    # Calculate BPM if there are enough peaks
    if len(valid_peaks) > 1:
        bpm = 60 / time_between_beats
        rolling_bpm = pd.Series(bpm, index=df.index[valid_peaks][1:]).rolling(window=bpm_smoothing_window).mean()
        
        # Identify gaps and interpolate down to 0 BPM for longer than 1 minute
        gaps = time_between_beats > 60
        bpm_interpolated = rolling_bpm.copy()
        for j, gap in enumerate(gaps):
            if gap:
                bpm_interpolated.iloc[j+1:] = bpm_interpolated.iloc[j+1:].interpolate(limit_direction='both').fillna(0)

        # Plot rolling BPM with lower zorder to place it behind the legend
        ax2.plot(df.index[valid_peaks][1:], bpm_interpolated, color='black', linestyle='--', linewidth=2, zorder=1)
        ax2.set_ylabel('BPM')

        # Find time of maximum BPM and add a vertical line labeled 'Topt'
        if not bpm_interpolated.empty:
            topt_time = bpm_interpolated.idxmax()  # Time of highest BPM
            max_bpm_value = bpm_interpolated.max()
            ax1.axvline(topt_time, color='green', linestyle='-.', linewidth=3, label=f'Topt: {topt_time:.2f} hours')

    # Set x-tick locator to 15-minute intervals (0.25 hours)
    ax1.xaxis.set_major_locator(plt.MultipleLocator(1))

    # Add legend to the plot with a solid white background and higher zorder
    legend = ax1.legend(loc='lower right', fontsize='small', frameon=True)
    legend.get_frame().set_facecolor('white')  # Solid white background
    legend.get_frame().set_edgecolor('black')

# Set the x-label only on the last subplot if there are multiple snails
if num_cols > 1:
    axes[-1].set_xlabel('Time (Hours)')
else:
    axes.set_xlabel('Time (Hours)')

# Adjust layout to prevent overlap and show plot
plt.tight_layout()
plt.show()


# %% create output file containg the death time information

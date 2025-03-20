#R CODES FOR PLIOT OF EXPERIMENT 1

# TO CLEAN AND INTERPOLATE THE DATA

# Load necessary libraries
library(dplyr)
library(zoo)  # For interpolation

# Load the data
 # Replace with your file path
data <- read.csv("RawData.csv")

# Verify the structure of the data
print(head(data))
print(str(data))

# Function to remove outliers beyond ±3 SD
remove_outliers <- function(df) {
  # Apply outlier removal for each numeric column
  df_cleaned <- df %>%
    mutate(across(where(is.numeric), ~ {
      mean_val <- mean(.x, na.rm = TRUE)
      sd_val <- sd(.x, na.rm = TRUE)
      ifelse(.x < (mean_val - 3 * sd_val) | .x > (mean_val + 3 * sd_val), NA, .x)
    }))
  return(df_cleaned)
}

# Clean the data by removing outliers
data_cleaned <- remove_outliers(data)

# Interpolate missing values for numeric columns
data_interpolated <- data_cleaned %>%
  mutate(across(where(is.numeric), ~na.approx(.x, na.rm = FALSE)))

# Save the cleaned and interpolated data to a new CSV file
write.csv(data_interpolated, "Cleaned_Interpolated_Data.csv", row.names = FALSE)

print("Data cleaned, interpolated, and saved to Cleaned_Interpolated_Data.csv.")

# TO EXTRACT MEDIAN AND MEAN VALUE 

# Load the cleaned data
data <- read.csv("Cleaned_Interpolated_Data.csv")

# Verify the structure of the cleaned data
print(head(data))
print(str(data))

# Calculate mean and median for each participant in each condition
result <- data %>%
  group_by(Participant) %>%  # Group by Participant
  summarise(across(where(is.numeric), list(mean = ~mean(.x, na.rm = TRUE), 
                                           median = ~median(.x, na.rm = TRUE)), 
                   .names = "{col}_{fn}"))  # Create new columns for mean and median

# Save the resulting summary to a new CSV file
write.csv(result, "Participant_Mean_Median.csv", row.names = FALSE)

print("Mean and Median for each participant calculated and saved to Participant_Mean_Median.csv.")

# TO DOWNSIZE THE DATA BY KEEPING PARTICIPANT ID INTACT

# Load the cleaned data
data <- read.csv("Cleaned_Interpolated_Data.csv")

# Clean column names
colnames(data) <- trimws(colnames(data))  # Remove spaces
colnames(data) <- make.names(colnames(data))  # Ensure valid R names
colnames(data)[1:2] <- c("Participant", "Timestamp")

# Verify column names
print(colnames(data))

# Define functions
calculate_averages <- function(column) {
  tapply(column, (seq_along(column) - 1) %/% 100, mean, na.rm = TRUE)
}

select_timestamp <- function(timestamps) {
  tapply(timestamps, (seq_along(timestamps) - 1) %/% 100, function(x) x[1])
}

# Initialize an empty data frame for results
result <- data.frame()

# Loop through each participant and process their data
unique_participants <- unique(data$Participant)
for (participant in unique_participants) {
  # Subset data for the current participant
  subset_data <- data %>% filter(Participant == participant)
  
  # Extract representative timestamps
  representative_timestamps <- select_timestamp(subset_data$Timestamp)
  
  # Select numeric columns for averaging
  numeric_cols <- subset_data %>%
    select(-c(Participant, Timestamp)) %>%
    select_if(is.numeric)
  
  # Calculate averages
  averaged_data <- as.data.frame(lapply(numeric_cols, calculate_averages))
  
  # Add back Participant and Timestamp columns
  averaged_data$Participant <- participant
  averaged_data$Timestamp <- representative_timestamps
  
  # Combine results
  result <- bind_rows(result, averaged_data)
}

# Reorder columns to place Timestamp after Participant
result <- result %>% select(Participant, Timestamp, everything())

# Save the resulting data to a new CSV file
write.csv(result, "Downsized_Averages_with_Timestamps.csv", row.names = FALSE)

print("Averages calculated and saved to Downsized_Averages_with_Timestamps.csv.")

# TO GET AVERAGES OF ALL PARTICIPANT'S DATA IN EACH TIMESTAMP FROM DOWNSIZED DATA

# Load the Downsized_Averages_with_Timestamps CSV file
data <- read.csv("Downsized_Averages_with_Timestamps.csv")

# Verify the structure of the data
print(head(data))
print(str(data))

# Calculate the average for each timestamp across all participants
averaged_by_timestamp <- data %>%
  group_by(Timestamp) %>%  # Group by Timestamp
  summarise(across(where(is.numeric), mean, na.rm = TRUE))  # Calculate the mean for each numeric column

# Save the resulting averages to a new CSV file
write.csv(averaged_by_timestamp, "Averages_By_Timestamp.csv", row.names = FALSE)

print("Averages by timestamp calculated and saved to Averages_By_Timestamp.csv.")

#TO SPLIT THE DATA INTO 50ms BINS IN EACH CONDITION FOR TIME-SPLIT ANALYSIS

# Load necessary libraries
library(dplyr)
library(tidyr)

# Load the cleaned data
data <- read.csv("Cleaned_Interpolated_Data.csv", stringsAsFactors = FALSE)

# Define a function to split data into bins
split_into_bins <- function(df, condition, bin_size = 50, start_time = 300, end_time = 1000) {
  # Convert time to milliseconds
  start_time <- start_time / 1000
  end_time <- end_time / 1000
  bin_size <- bin_size / 1000
  
  # Create bins from start_time to end_time with a given bin size
  bins <- seq(from = start_time, to = end_time, by = bin_size)
  
  # Create a new column 'Bin' based on Timestamp
  df <- df %>%
    mutate(Bin = cut(Timestamp, breaks = bins, include.lowest = TRUE, labels = FALSE)) %>%
    filter(!is.na(Bin))  # Remove NA values if any
  
  # Filter only the relevant condition
  df_condition <- df %>%
    select(Participant, Timestamp, Bin, Value = !!sym(condition))
  
  # Reshape data to have bins as columns
  binned_data <- df_condition %>%
    group_by(Participant) %>%
    pivot_wider(names_from = Bin, values_from = Value, names_prefix = "Bin_")
  
  # Create bin labels with the actual time range (e.g., "300-350ms", "350-400ms", etc.)
  bin_labels <- paste0(seq(from = start_time, to = end_time - bin_size, by = bin_size), 
                       "-", 
                       seq(from = start_time + bin_size, to = end_time, by = bin_size), "ms")
  
  # Rename the columns to reflect the time intervals
  colnames(binned_data)[grepl("Bin_", colnames(binned_data))] <- bin_labels
  
  # Remove the Timestamp column
  binned_data <- binned_data %>%
    select(-Timestamp)
  
  # Remove rows where all bin columns are NA
  binned_data <- binned_data %>%
    filter(if_any(starts_with("Bin_"), ~ !is.na(.)))
  
  # Remove NAs and shift the valid data upwards in each bin column
  binned_data <- binned_data %>%
    mutate(across(starts_with("Bin_"), ~ tidyr::replace_na(., NA))) %>%
    mutate(across(starts_with("Bin_"), ~ zoo::na.omit(.) %>% as.vector()))  # Remove NAs and shift valid data
  
  return(binned_data)
}

# Loop over each condition and generate a CSV for each condition
conditions <- c("Happy_Low_M1", "Happy_Low_M2", "Happy_High_M1", "Happy_High_M2",
                "Angry_Low_M1", "Angry_Low_M2", "Angry_High_M1", "Angry_High_M2")

# Generate CSV for each condition
for (condition in conditions) {
  tryCatch({
    binned_data <- split_into_bins(data, condition)
    
    # Write the data to a CSV file for each condition in the working directory
    output_file <- paste0("Binned_", condition, ".csv")
    write.csv(binned_data, output_file, row.names = FALSE)
    print(paste("File created for condition:", condition))
  }, error = function(e) {
    print(paste("Error processing condition:", condition, ":", e$message))
  })
}

#TO PERFORM T-TESTS FOR HAPPY TIME BINS

# Load necessary libraries
library(dplyr)

# Read the two CSV files
file1 <- read.csv("Binned_Happy_Low_M1.csv", stringsAsFactors = FALSE)
file2 <- read.csv("Binned_Happy_High_M1.csv", stringsAsFactors = FALSE)

# Remove the participant ID column (assumed to be the first column in both files)
file1 <- file1[, -1]
file2 <- file2[, -1]

# Generate time bins programmatically
time_bins <- paste(seq(300, 950, by = 50), seq(350, 1000, by = 50), sep = "-")

# Initialize a data frame to store t-test results
t_test_results <- data.frame(
  `Time Bins` = character(),
  t_statistic = numeric(),
  p_value = numeric(),
  cohen_d = numeric(),
  degrees_of_freedom = numeric(),
  stringsAsFactors = FALSE
)

# Function to calculate Cohen's d
cohen_d <- function(x, y) {
  # Calculate Cohen's d for independent samples
  n1 <- length(x)
  n2 <- length(y)
  s1 <- sd(x)
  s2 <- sd(y)
  
  # Pooled standard deviation
  pooled_sd <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
  
  # Cohen's d calculation
  d <- (mean(x) - mean(y)) / pooled_sd
  return(d)
}

# Perform t-test for each corresponding pair of columns
for (i in 1:ncol(file1)) {
  # Get the corresponding columns from both files
  data1 <- file1[[i]]
  data2 <- file2[[i]]
  
  # Remove NA values from both data sets
  valid_data <- na.omit(data.frame(data1, data2))
  
  # Perform the t-test only if both columns have valid data
  if (nrow(valid_data) > 0) {
    t_test <- t.test(valid_data$data1, valid_data$data2)
    
    # Calculate Cohen's d
    cohen_d_value <- cohen_d(valid_data$data1, valid_data$data2)
    
    # Append results to the data frame
    t_test_results <- rbind(
      t_test_results,
      data.frame(
        `Time Bins` = time_bins[i],         # Add the time bin
        t_statistic = round(t_test$statistic, 4),  # Rounded t-statistic
        p_value = round(t_test$p.value, 4),        # Rounded p-value
        cohen_d = round(cohen_d_value, 4),         # Rounded Cohen's d
        degrees_of_freedom = nrow(valid_data) - 2, # Degrees of freedom
        stringsAsFactors = FALSE
      )
    )
  }
}

# Print the results
print(t_test_results)

# Optionally, save the t-test results to a CSV file
write.csv(t_test_results, "t_test_results_Happy_HighLoad*LowLoad.csv", row.names = FALSE)

#TO PERFORM T-TESTS FOR ANGRY TIME BINS

# Load necessary libraries
library(dplyr)

# Read the two CSV files
file1 <- read.csv("Binned_Angry_Low_M2.csv", stringsAsFactors = FALSE)
file2 <- read.csv("Binned_Angry_High_M2.csv", stringsAsFactors = FALSE)

# Remove the participant ID column (assumed to be the first column in both files)
file1 <- file1[, -1]
file2 <- file2[, -1]

# Generate time bins programmatically
time_bins <- paste(seq(300, 950, by = 50), seq(350, 1000, by = 50), sep = "-")

# Initialize a data frame to store t-test results
t_test_results <- data.frame(
  `Time Bins` = character(),
  t_statistic = numeric(),
  p_value = numeric(),
  cohen_d = numeric(),
  degrees_of_freedom = numeric(),
  stringsAsFactors = FALSE
)

# Function to calculate Cohen's d
cohen_d <- function(x, y) {
  # Calculate Cohen's d for independent samples
  n1 <- length(x)
  n2 <- length(y)
  s1 <- sd(x)
  s2 <- sd(y)
  
  # Pooled standard deviation
  pooled_sd <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
  
  # Cohen's d calculation
  d <- (mean(x) - mean(y)) / pooled_sd
  return(d)
}

# Perform t-test for each corresponding pair of columns
for (i in 1:ncol(file1)) {
  # Get the corresponding columns from both files
  data1 <- file1[[i]]
  data2 <- file2[[i]]
  
  # Remove NA values from both data sets
  valid_data <- na.omit(data.frame(data1, data2))
  
  # Perform the t-test only if both columns have valid data
  if (nrow(valid_data) > 0) {
    t_test <- t.test(valid_data$data1, valid_data$data2)
    
    # Calculate Cohen's d
    cohen_d_value <- cohen_d(valid_data$data1, valid_data$data2)
    
    # Append results to the data frame
    t_test_results <- rbind(
      t_test_results,
      data.frame(
        `Time Bins` = time_bins[i],         # Add the time bin
        t_statistic = round(t_test$statistic, 4),  # Rounded t-statistic
        p_value = round(t_test$p.value, 4),        # Rounded p-value
        cohen_d = round(cohen_d_value, 4),         # Rounded Cohen's d
        degrees_of_freedom = nrow(valid_data) - 2, # Degrees of freedom
        stringsAsFactors = FALSE
      )
    )
  }
}

# Print the results
print(t_test_results)

# Optionally, save the t-test results to a CSV file
write.csv(t_test_results, "t_test_results_Angry_HighLoad*LowLoad.csv", row.names = FALSE)

#TO PLOT RAINCLOUD PLOTS FOR HAPPY HIGH LOAD VS LOW LOAD

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(jtools)  # To generate raincloud plots

# Read the two CSV files
file1 <- read.csv("Binned_Happy_Low_M1.csv", stringsAsFactors = FALSE)
file2 <- read.csv("Binned_Happy_High_M1.csv", stringsAsFactors = FALSE)

# Remove the participant ID column (assuming it's the first column in both files)
file1 <- file1[, -1]
file2 <- file2[, -1]

# Combine the data from both files and reshape it to long format for plotting
long_data <- bind_rows(
  lapply(1:ncol(file1), function(i) {
    data_frame(
      Condition = paste0(seq(from = 300, by = 50, length.out = ncol(file1))[i], "-", seq(from = 350, by = 50, length.out = ncol(file1))[i], "ms"),  # Naming based on time range
      Value = c(file1[[i]], file2[[i]]),
      Source = rep(c("Low-load", "High-load"), each = length(file1[[i]]))
    )
  })
)

# Remove NA values
long_data <- long_data %>%
  filter(!is.na(Value))

# Set the factor levels of the Condition variable to ensure correct order
long_data$Condition <- factor(long_data$Condition, levels = paste0(seq(from = 300, by = 50, length.out = ncol(file1)), "-", seq(from = 350, by = 50, length.out = ncol(file1)), "ms"))

# Plot the raincloud plot
raincloud_plot <- ggplot(long_data, aes(x = Condition, y = Value, fill = Source)) +
  geom_violin(position = position_nudge(x = 0.2), adjust = 1.5, alpha = 0.5) +  # Violin plot
  geom_jitter(aes(color = Source), position = position_jitter(width = 0.15), size = 1, alpha = 0.7) +  # Scatter points (jitter)
  geom_boxplot(width = 0.1, alpha = 0.3, outlier.shape = NA) +  # Boxplot for summary stats
  scale_fill_manual(values = c("blue", "orange")) +  # Color for Low-load and High-load
  scale_color_manual(values = c("blue", "orange")) +  # Color for scatter points
  theme_bw() +  # White background
  labs(title = "Raincloud Plot for Happy Low-load vs Happy High-load", y = "Value", x = "Condition") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the raincloud plot
print(raincloud_plot)

# Optionally, save the plot to a file
ggsave("raincloud_plot_happy.png", plot = raincloud_plot, width = 10, height = 6)

#TO PLOT RAINCLOUD PLOTS FOR ANGRY HIGH LOAD VS LOW LOAD

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(jtools)  # To generate raincloud plots

# Read the two CSV files
file1 <- read.csv("Binned_Angry_Low_M2.csv", stringsAsFactors = FALSE)
file2 <- read.csv("Binned_Angry_High_M2.csv", stringsAsFactors = FALSE)

# Remove the participant ID column (assuming it's the first column in both files)
file1 <- file1[, -1]
file2 <- file2[, -1]

# Combine the data from both files and reshape it to long format for plotting
long_data <- bind_rows(
  lapply(1:ncol(file1), function(i) {
    data_frame(
      Condition = paste0(seq(from = 300, by = 50, length.out = ncol(file1))[i], "-", seq(from = 350, by = 50, length.out = ncol(file1))[i], "ms"),  # Naming based on time range
      Value = c(file1[[i]], file2[[i]]),
      Source = rep(c("Low-load", "High-load"), each = length(file1[[i]]))
    )
  })
)

# Remove NA values
long_data <- long_data %>%
  filter(!is.na(Value))

# Set the factor levels of the Condition variable to ensure correct order
long_data$Condition <- factor(long_data$Condition, levels = paste0(seq(from = 300, by = 50, length.out = ncol(file1)), "-", seq(from = 350, by = 50, length.out = ncol(file1)), "ms"))

# Plot the raincloud plot
raincloud_plot <- ggplot(long_data, aes(x = Condition, y = Value, fill = Source)) +
  geom_violin(position = position_nudge(x = 0.2), adjust = 1.5, alpha = 0.5) +  # Violin plot
  geom_jitter(aes(color = Source), position = position_jitter(width = 0.15), size = 1, alpha = 0.7) +  # Scatter points (jitter)
  geom_boxplot(width = 0.1, alpha = 0.3, outlier.shape = NA) +  # Boxplot for summary stats
  scale_fill_manual(values = c("blue", "orange")) +  # Color for Low-load and High-load
  scale_color_manual(values = c("blue", "orange")) +  # Color for scatter points
  theme_bw() +  # White background
  labs(title = "Raincloud Plot for Angry Low-load vs Angry High-load", y = "Value", x = "Condition") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the raincloud plot
print(raincloud_plot)

# Optionally, save the plot to a file
ggsave("raincloud_plot_angry.png", plot = raincloud_plot, width = 10, height = 6)

#TO PLOT LINE PLOTS FOR HAPPY HIGH-LOAD VS LOW LOAD (along with CS muscle data)

# Load necessary library
library(ggplot2)
library(tidyr)
library(dplyr)

# Read the CSV file
data <- read.csv("Averages_By_Timestamp.csv", stringsAsFactors = FALSE)

# Check column names
print(colnames(data))

# Rename the first column to "Timestamp" if needed
colnames(data)[1] <- "Timestamp"

# Select only the Timestamp and the first 4 variables for plotting
selected_data <- data[, c("Timestamp", colnames(data)[2:5])]

# Reshape the selected data into long format for ggplot
long_data <- pivot_longer(selected_data, 
                          cols = -Timestamp,  # All columns except Timestamp
                          names_to = "Variable", 
                          values_to = "Value")

# Rename specific variables
long_data <- long_data %>%
  mutate(Variable = recode(Variable, 
                           "Happy_Low_M1" = "Happy_Low_ZM",
                           "Happy_Low_M2" = "Happy_Low_CS",
                           "Happy_High_M1" = "Happy_High_ZM",
                           "Happy_High_M2" = "Happy_High_CS"))

# Add custom styles for each variable
long_data <- long_data %>%
  mutate(
    LineType = case_when(
      Variable == "Happy_Low_CS" ~ "dashed",
      Variable == "Happy_High_CS" ~ "dashed",
      TRUE ~ "solid"
    ),
    LineSize = case_when(
      Variable == "Happy_Low_ZM" ~ 1.2,
      Variable == "Happy_High_ZM" ~ 1.2,
      TRUE ~ 0.8
    )
  )

# Create the line plot
line_plot <- ggplot(long_data, aes(x = Timestamp, y = Value, color = Variable)) +
  geom_line(aes(linetype = LineType, linewidth = LineSize)) +  # Apply linetype and linewidth
  labs(
    title = "Line Plot for Happy Trials Over Time window (300ms - 1000ms)",
    x = "Timestamp",
    y = "Value",
    color = "Variables"
  ) +
  scale_x_continuous(breaks = seq(0.3, 1, by = 0.05)) +  # Custom x-axis
  scale_linetype_identity() +  # Use custom linetype
  scale_linewidth_identity() +  # Use custom linewidth
  scale_color_manual(values = c(
    "Happy_Low_ZM" = "blue",
    "Happy_High_ZM" = "forestgreen",
    "Happy_Low_CS" = "red",
    "Happy_High_CS" = "orange"
  ), breaks = c("Happy_Low_ZM", "Happy_High_ZM", "Happy_Low_CS", "Happy_High_CS")) +  # Custom color order
  theme_minimal() +  # Clean theme
  theme(
    panel.background = element_rect(fill = "white", color = "black"),  # White background
    panel.grid.major = element_line(color = "gray80"),  # Light gray grid lines
    panel.grid.minor = element_line(color = "gray90"),  # Lighter grid lines
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.background = element_rect(fill = "white", color = NA)  # Ensures white plot background
  )

# Display the plot
print(line_plot)

# Save the plot (optional)
ggsave("line_plot_Happy.png", plot = line_plot, width = 10, height = 6)


#TO PLOT LINE PLOTS FOR ANGRY HIGH-LOAD VS LOW LOAD (along with ZM muscle data)

# Load necessary library
library(ggplot2)
library(tidyr)
library(dplyr)

# Read the CSV file
data <- read.csv("Averages_By_Timestamp.csv", stringsAsFactors = FALSE)

# Check column names
print(colnames(data))

# Rename the first column to "Timestamp" if needed
colnames(data)[1] <- "Timestamp"

# Select only the Timestamp and the first 4 variables for plotting
selected_data <- data[, c("Timestamp", colnames(data)[6:9])]

# Reshape the selected data into long format for ggplot
long_data <- pivot_longer(selected_data, 
                          cols = -Timestamp,  # All columns except Timestamp
                          names_to = "Variable", 
                          values_to = "Value")

# Rename specific variables
long_data <- long_data %>%
  mutate(Variable = recode(Variable, 
                           "Angry_Low_M1" = "Angry_Low_ZM",
                           "Angry_Low_M2" = "Angry_Low_CS",
                           "Angry_High_M1" = "Angry_High_ZM",
                           "Angry_High_M2" = "Angry_High_CS"))

# Add custom styles for each variable
long_data <- long_data %>%
  mutate(
    LineType = case_when(
      Variable == "Angry_Low_ZM" ~ "dashed",
      Variable == "Angry_High_ZM" ~ "dashed",
      TRUE ~ "solid"
    ),
    LineSize = case_when(
      Variable == "Angry_Low_CS" ~ 1.2,
      Variable == "Angry_High_CS" ~ 1.2,
      TRUE ~ 0.8
    )
  )

# Create the line plot
line_plot <- ggplot(long_data, aes(x = Timestamp, y = Value, color = Variable)) +
  geom_line(aes(linetype = LineType, linewidth = LineSize)) +  # Apply linetype and linewidth
  labs(
    title = "Line Plot for Angry Trials Over Time window (300ms - 1000ms)",
    x = "Timestamp",
    y = "Value",
    color = "Variables"
  ) +
  scale_x_continuous(breaks = seq(0.3, 1, by = 0.05)) +  # Custom x-axis
  scale_linetype_identity() +  # Use custom linetype
  scale_linewidth_identity() +  # Use custom linewidth
  scale_color_manual(values = c(
    "Angry_Low_ZM" = "blue",
    "Angry_High_ZM" = "forestgreen",
    "Angry_Low_CS" = "red",
    "Angry_High_CS" = "orange"
  ), breaks = c("Angry_Low_CS", "Angry_High_CS", "Angry_Low_ZM", "Angry_High_ZM")) +  # Custom color order
  theme_minimal() +  # Clean theme
  theme(
    panel.background = element_rect(fill = "white", color = "black"),  # White background
    panel.grid.major = element_line(color = "gray80"),  # Light gray grid lines
    panel.grid.minor = element_line(color = "gray90"),  # Lighter grid lines
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.background = element_rect(fill = "white", color = NA)  # Ensures white plot background
  )

# Display the plot
print(line_plot)

# Save the plot (optional)
ggsave("line_plot_Angry.png", plot = line_plot, width = 10, height = 6)

#TO COMPUTE 2*2 REPEATED MEASURES ANOVA

# Load necessary libraries
library(tidyverse)
library(ez)

# Read the data file
data <- read.csv("Participant_Mean_Median.csv", stringsAsFactors = FALSE)

# Reshape data into long format for ANOVA
long_data <- data %>%
  pivot_longer(cols = c("Happy_High_M1_median", "Happy_Low_M1_median", 
                        "Angry_High_M2_median", "Angry_Low_M2_median"),
               names_to = "Condition", 
               values_to = "Value") %>%
  mutate(Load = ifelse(str_detect(Condition, "High"), "High", "Low"),
         Emotion = ifelse(str_detect(Condition, "Happy"), "Happy", "Angry"))

# Convert necessary columns to factors
long_data$Participant <- as.factor(long_data$Participant)
long_data$Load <- as.factor(long_data$Load)
long_data$Emotion <- as.factor(long_data$Emotion)

# Perform repeated measures ANOVA
anova_result <- ezANOVA(
  data = long_data,
  dv = Value,          # Dependent variable
  wid = Participant,   # Subject identifier
  within = .(Load, Emotion), # Within-subject factors
  detailed = TRUE
)

# Extract the ANOVA table
anova_results_df <- as.data.frame(anova_result$ANOVA)

# Calculate Partial Eta Squared for each effect
anova_results_df <- anova_results_df %>%
  mutate(
    partial_eta_squared = SSn / (SSn + SSd),
    partial_eta_squared = round(partial_eta_squared, 4)  # rounding to 4 decimal places
  )

# Print the results
print(anova_results_df)

# Save the ANOVA result to a CSV file
write.csv(anova_results_df, "ANOVA_Result_with_Partial_Eta_Squared.csv", row.names = FALSE)



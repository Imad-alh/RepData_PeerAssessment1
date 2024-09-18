# Loading the Data
unzip(zipfile="activity.zip")
df <- read.csv("activity.csv")  # Load the activity data from a CSV file
df$date <- as.Date(df$date, format = "%Y-%m-%d")  # Convert the 'date' column to Date format

# Summing the steps per day, ignoring NA values
steps_per_day <- tapply(df$steps, df$date, sum, na.rm = TRUE)

# Create a data frame from the steps per day
steps_per_day_df <- data.frame(date = names(steps_per_day), steps = as.vector(steps_per_day))

# Load the ggplot2 library for plotting
library(ggplot2)

# Calculate the mean and median steps per day
mean_val <- mean(steps_per_day, na.rm = TRUE)
median_val <- median(steps_per_day, na.rm = TRUE)

# Plot a histogram of the steps per day with mean and median lines
ggplot(steps_per_day_df, aes(x = steps)) +
  geom_histogram(bins = 10, fill = "lightblue", color = "black") +
  geom_vline(aes(xintercept = mean_val), color = "red", linetype = "dashed", linewidth = 1) +  # Red dashed line for mean
  geom_vline(aes(xintercept = median_val), color = "blue", linetype = "dotted", linewidth = 1) +  # Blue dotted line for median
  labs(title = "Total Steps per Day", x = "Steps", y = "Frequency") +  # Axis labels and title
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +  # Center-align the title
  # Annotate the histogram with mean and median values
  annotate("text", x = min(steps_per_day_df$steps, na.rm = TRUE), 
           y = Inf, label = paste("Mean:", round(mean_val, 2)), color = "red", hjust = 0, vjust = 1.5) +
  annotate("text", x = min(steps_per_day_df$steps, na.rm = TRUE), 
           y = Inf, label = paste("Median:", round(median_val, 2)), color = "blue", hjust = 0, vjust = 4)

# Calculate the average steps per interval across all days
averages <- aggregate(x=list(steps=df$steps), by=list(interval=df$interval), FUN=mean, na.rm=TRUE)

# Find the maximum average steps and corresponding interval
max_value <- max(averages$steps)
max_interval <- averages$interval[which.max(averages$steps)]

# Plot the average steps per 5-min interval and highlight the max value
ggplot(data=averages, aes(x=interval, y=steps)) +
  geom_line() +
  annotate("point", x = max_interval, y = max_value, color = "red", size = 3) +  # Highlight max value with a red point
  annotate("text", x = max_interval, y = max_value, label = paste("Max:", round(max_value, 2), "\nInterval:", max_interval),
           vjust = 1, hjust = 1.2, color = "red") +  # Annotate the max value and corresponding interval
  labs(x = "5-min Intervals", y = "Average Number of Steps") +  # Axis labels
  theme_minimal()

# Handle missing data (NA values in 'steps' column)
missing <- is.na(df$steps)  # Identify missing values
table(missing)  # Show how many missing values exist

# Fill NA values in 'steps' with the average steps for the corresponding interval
df$complet_steps <- ifelse(is.na(df$steps),
                           averages$steps[match(df$interval, averages$interval)],
                           df$steps)

# Calculate the total steps per day after filling in missing data
steps_per_day_df$complet_steps <- tapply(df$complet_steps, df$date, sum, na.rm = TRUE)

# Recalculate the mean and median with filled data
mean_val2 <- mean(steps_per_day_df$complet_steps, na.rm = TRUE)
median_val2 <- median(steps_per_day_df$complet_steps, na.rm = TRUE)

# Plot the updated histogram with mean and median lines
ggplot(steps_per_day_df, aes(x = complet_steps)) +
  geom_histogram(bins = 10, fill = "lightblue", color = "black") +
  geom_vline(aes(xintercept = mean_val2), color = "red", linetype = "dashed", linewidth = 1) +  # Mean line
  geom_vline(aes(xintercept = median_val2), color = "blue", linetype = "dotted", linewidth = 1) +  # Median line
  labs(title = "Total Steps per Day (with Filled NA Values)", x = "Steps", y = "Frequency") +  # Axis labels and title
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +  # Center-align the title
  # Annotate the histogram with updated mean and median values
  annotate("text", x = min(steps_per_day_df$complet_steps, na.rm = TRUE), 
           y = Inf, label = paste("Mean:", round(mean_val2, 2)), color = "red", hjust = 0, vjust = 1.5) +
  annotate("text", x = min(steps_per_day_df$complet_steps, na.rm = TRUE), 
           y = Inf, label = paste("Median:", round(median_val2, 2)), color = "blue", hjust = 0, vjust = 4)

Sys.setlocale("LC_TIME", "English") # Seting locale to English

# Add a new column "day_type" to indicate whether the date is a weekday or weekend
df$day_type <- ifelse(weekdays(df$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")

# Convert it to a factor for better handling in plots, if necessary
df$day_type <- factor(df$day_type, levels = c("weekday", "weekend"))

# Calculate the average steps per interval separately for weekdays and weekends
averages_day_type <- aggregate(complet_steps ~ interval + day_type, data=df, FUN=mean, na.rm=TRUE)

# Plot the line graph with panels for weekday and weekend
ggplot(data=averages_day_type, aes(x=interval, y=complet_steps)) +
  geom_line() +  # Line plot for average steps per interval
  facet_wrap(~ day_type, ncol = 1) +  # Create separate panels for weekdays and weekends
  labs(x = "5-min Intervals", y = "Average Number of Steps", title = "Average Steps per Interval by Day Type") +  # Axis labels and title
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

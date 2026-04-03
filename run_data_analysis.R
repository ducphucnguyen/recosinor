# ============================================================
# RECOSINOR - Data Preparation & Analysis Script
# ============================================================

# Set library path
.libPaths(c('C:/Users/nguy0936/Rpackages', 'C:/Program Files/R/R-4.5.3/library'))

# ---- CONFIGURATION ----
# Update these paths for your data
DATA_CBT_FILE <- "C:/dev/Rproject/recosinor/recosinor-main/recosinor-main/data/P007/cbt.csv"
DATA_SENSOR_FILE <- "C:/dev/Rproject/recosinor/recosinor-main/recosinor-main/data/P007/df_empatica.csv"
OUTPUT_DIR <- "C:/dev/Rproject/recosinor/recosinor-main/recosinor-main/output/"

# Column name mapping - UPDATE THESE to match your CSV headers
col_mapping <- list(
  cbt_time = "time",           # Column with timestamp in CBT file
  cbt_temp = "meanCBT",       # Column with core body temperature
  sensor_time = "Datetime",   # Column with timestamp in sensor file
  sensor_hr = "PulseRate",    # Column with heart rate
  sensor_acc = "AccelStdG",   # Column with accelerometer (activity)
  sensor_temp = "TemperatureC" # Column with skin temperature
)

# Sleep window (hours when sleep = 1)
sleep_start <- 22  # 10 PM
sleep_end <- 6     # 6 AM

# Data row range to use (optional, set to NULL to use all)
row_start <- 490
row_end <- 11366

# ============================================================

# Load libraries
source("C:/dev/Rproject/recosinor/recosinor-main/recosinor-main/R/recosinor.fit.R")
source("C:/dev/Rproject/recosinor/recosinor-main/recosinor-main/R/cir_funs.R")
source("C:/dev/Rproject/recosinor/recosinor-main/recosinor-main/R/recosinor.plot.R")

library(readr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(lubridate)
library(imputeTS)
library(zoo)
library(mgcv)
library(pracma)

# Load data
cat("Loading data files...\n")
cbt <- read_csv(DATA_CBT_FILE)
sensor <- read_csv(DATA_SENSOR_FILE)

cat("CBT data rows:", nrow(cbt), "\n")
cat("Sensor data rows:", nrow(sensor), "\n")

# Parse timestamps
# Adjust format based on your data (currently set for P007 example format)
cbt[[col_mapping$cbt_time]] <- as.POSIXct(cbt[[col_mapping$cbt_time]], 
                                          format = "%d/%m/%Y %I:%M:%S %p", 
                                          tz = "Australia/Darwin")

# For sensor data - adjust format as needed
sensor[[col_mapping$sensor_time]] <- as.POSIXct(sensor[[col_mapping$sensor_time]], 
                                                format = "%d-%b-%Y %H:%M:%S") + hours(9) + minutes(30)

# Rename first column if needed
colnames(cbt)[1] <- "Datetime"

# Merge datasets
cat("Merging datasets...\n")
df <- left_join(cbt, sensor, by = "Datetime")

# Subset rows if specified
if (!is.null(row_start) && !is.null(row_end)) {
  df <- df[row_start:row_end, ]
  cat("Using rows", row_start, "to", row_end, "\n")
}

cat("Combined data rows:", nrow(df), "\n")

# Interpolate missing values
cat("Preprocessing data...\n")
df[[col_mapping$cbt_temp]] <- na_interpolation(df[[col_mapping$cbt_temp]])
df[[col_mapping$sensor_hr]] <- na_interpolation(df[[col_mapping$sensor_hr]])
df[[col_mapping$sensor_acc]] <- na_interpolation(df[[col_mapping$sensor_acc]])
df[[col_mapping$sensor_temp]] <- na_interpolation(df[[col_mapping$sensor_temp]])

# Apply smoothing
df[[col_mapping$sensor_hr]] <- rollmean(df[[col_mapping$sensor_hr]], k = 25, fill = NA, align = "center")
df[[col_mapping$sensor_acc]] <- rollmean(df[[col_mapping$sensor_acc]], k = 25, fill = NA, align = "center")
df[[col_mapping$sensor_temp]] <- rollmean(df[[col_mapping$sensor_temp]], k = 5, fill = NA, align = "center")

# Create hours from start
df$hrs <- as.numeric(difftime(df$Datetime, df$Datetime[1], units = "hours"))

# Create sleep/wake vector
df$sw <- ifelse(hour(df$Datetime) >= sleep_start | hour(df$Datetime) < sleep_end, 1, 0)

# Prepare final dataset
cbt_data <- df %>%
  select(Datetime, all_of(col_mapping$cbt_temp), hrs, sw, 
         all_of(col_mapping$sensor_hr), all_of(col_mapping$sensor_acc), 
         all_of(col_mapping$sensor_temp)) %>%
  rename(
    datetime = Datetime,
    cbt = col_mapping$cbt_temp,
    MovingAvgHR = col_mapping$sensor_hr,
    acc = col_mapping$sensor_acc,
    tempC = col_mapping$sensor_temp
  )

# Remove rows with NA in cbt
cbt_data <- cbt_data %>% filter(!is.na(cbt))

cat("Final dataset rows:", nrow(cbt_data), "\n")
cat("Sleep points:", sum(cbt_data$sw == 1), "\n")
cat("Wake points:", sum(cbt_data$sw == 0), "\n")

# Fit model
cat("\nFitting recosinor model...\n")
model <- recosinor.fit(
  cbt ~ time(hrs) + sleep(sw) + s(MovingAvgHR) + s(tempC) + s(acc),
  cbt_data,
  tau = c(24)
)

# Get the processed data from global environment
processed_data <- get("data_output", envir = .GlobalEnv)

# Summary
cat("\n===== MODEL SUMMARY =====\n")
summary(model)

# Generate plot - use simple custom plotting
cat("\nGenerating plot...\n")

# Get data from global environment 
fitted_data <- get("data_output", envir = .GlobalEnv)
n_rows <- length(model$fitted.values)

# Build simple data for plotting
plot_data <- data.frame(
  datetime = fitted_data$datetime[1:n_rows],
  cbt = model$y,
  cbt_pred = model$fitted.values,
  ysin = fitted_data$ysin[1:n_rows],
  ycos = fitted_data$ycos[1:n_rows]
)

# Add sleep columns if they exist
sleep_cols <- grep("^y_sleep_", names(fitted_data), value = TRUE)
for (col in sleep_cols) {
  plot_data[[col]] <- fitted_data[[col]][1:n_rows]
}

# Calculate circadian and sleep processes manually
plot_data$c_process <- plot_data$ysin + plot_data$ycos

if (length(sleep_cols) > 0) {
  plot_data$s_process <- rowSums(plot_data[, sleep_cols, drop = FALSE])
} else {
  plot_data$s_process <- 0
}

# Calculate R-squared
r2 <- summary(model)[["r.sq"]]

# Panel 1: Measured vs Fitted
p1 <- ggplot(plot_data) +
  geom_line(aes(x = datetime, y = cbt, color = "Measured"), na.rm = TRUE) +
  geom_line(aes(x = datetime, y = cbt_pred, color = "Fitted Recosinor"), na.rm = TRUE) +
  labs(x = "", y = "CBT, oC") +
  scale_color_manual(values = c("Measured" = "#636363", "Fitted Recosinor" = "#e6550d")) +
  theme(legend.title = element_blank()) +
  annotate("text", x = plot_data$datetime[1], y = max(plot_data$cbt, na.rm = TRUE),
           label = paste("R-squared =", round(r2, 2)),
           color = "black", hjust = 0, vjust = 1, size = 4)

# Panel 2: Components
p2 <- ggplot(plot_data, aes(x = datetime, y = cbt)) +
  geom_path(aes(x = datetime, y = c_process, color = "Endogenous circadian"), na.rm = TRUE) +
  geom_path(aes(x = datetime, y = s_process, color = "Homeostatic sleep"), na.rm = TRUE) +
  scale_color_manual(values = c("Endogenous circadian" = "#756bb1", "Homeostatic sleep" = "#31a354")) +
  labs(x = "Datetime", y = "CBT, oC") +
  theme(legend.title = element_blank())

# Combine
library(ggpubr)
plot <- ggarrange(p1, p2, labels = c("A", "B"), align = "v", heights = c(0.5, 0.5), ncol = 1, nrow = 2)

# Save
ggsave(paste0(OUTPUT_DIR, "recosinor_analysis.png"), plot, width = 14, height = 12, dpi = 150)

cat("Plot saved to:", paste0(OUTPUT_DIR, "recosinor_analysis.png"), "\n")

# Save outputs
dir.create(OUTPUT_DIR, showWarnings = FALSE)
ggsave(paste0(OUTPUT_DIR, "recosinor_analysis.png"), 
       plot, width = 14, height = 12, dpi = 150)

# Save predictions - use fitted values instead of predict()
plot_data$cbt_pred <- model$fitted.values
write_csv(plot_data, paste0(OUTPUT_DIR, "cbt_predictions.csv"))

cat("\n===== OUTPUTS SAVED =====\n")
cat("Plot: ", paste0(OUTPUT_DIR, "recosinor_analysis.png"), "\n")
cat("Data: ", paste0(OUTPUT_DIR, "cbt_predictions.csv"), "\n")

# Metrics
r2 <- summary(model)[["r.sq"]]
rmse <- sqrt(mean(residuals(model)^2, na.rm = TRUE))
cat("\n===== METRICS =====\n")
cat("R-squared:", round(r2, 4), "\n")
cat("RMSE:", round(rmse, 4), "°C\n")

cat("\nDone!\n")
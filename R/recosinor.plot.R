recosinor.plot <- function(model, data) {

  library(pracma)
  library(ggplot2)
  library(ggpubr)

  cbt_data <- data

  # Use fitted values directly
  cbt_data$cbt_pred <- model$fitted.values

  r2 <- summary(model)[["r.sq"]]

  # Panel 1: Measured vs Fitted
  p1 <- ggplot(cbt_data) +
    geom_line(aes(x = datetime, y = cbt, color = "Measured"), na.rm = TRUE) +
    geom_line(aes(x = datetime, y = cbt_pred, color = "Fitted Recosinor"), na.rm = TRUE) +
    labs(x = "", y = "CBT, oC") +
    scale_color_manual(values = c("Measured" = "#636363", "Fitted Recosinor" = "#e6550d")) +
    theme(legend.title = element_blank()) +
    annotate("text", x = cbt_data$datetime[1], y = max(cbt_data$cbt, na.rm = TRUE),
             label = paste("R-squared =", round(r2, 2)),
             color = "black", hjust = 0, vjust = 1, size = 4)

  # Panel 2: Extract terms from model coefficients
  # Get coefficient names and values
  coef_names <- names(model$coefficients)
  coef_vals <- model$coefficients
  
  # Extract circadian components (ysin, ycos)
  if ("ysin" %in% coef_names && "ycos" %in% coef_names) {
    # Use the data columns directly
    if ("ysin" %in% names(cbt_data) && "ycos" %in% names(cbt_data)) {
      cbt_data$c_process <- cbt_data$ysin + cbt_data$ycos
    } else {
      # Calculate from model terms
      cbt_data$c_process <- rep(0, nrow(cbt_data))
    }
  } else {
    cbt_data$c_process <- rep(0, nrow(cbt_data))
  }
  
  # Extract sleep components
  sleep_cols <- grep("^y_sleep_", coef_names, value = TRUE)
  if (length(sleep_cols) > 0 && "y_sleep_1" %in% names(cbt_data)) {
    sleep_cols_data <- grep("^y_sleep_", names(cbt_data), value = TRUE)
    if (length(sleep_cols_data) > 0) {
      cbt_data$s_process <- rowSums(cbt_data[, sleep_cols_data, drop = FALSE])
    } else {
      cbt_data$s_process <- rep(0, nrow(cbt_data))
    }
  } else {
    cbt_data$s_process <- rep(0, nrow(cbt_data))
  }

  # Find circadian troughs
  result <- tryCatch({
    pracma::findpeaks(-as.numeric(cbt_data$c_process))
  }, error = function(e) NULL)
  
  if (!is.null(result) && nrow(result) > 0) {
    pks1 <- result[, 1]
    locs1 <- result[, 2]
    TROUGH <- data.frame(Datetime_loc = cbt_data$datetime[locs1], Peak_val = -pks1)
  } else {
    TROUGH <- NULL
  }

  # Find sleep process troughs
  result2 <- tryCatch({
    pracma::findpeaks(-cbt_data$s_process)
  }, error = function(e) NULL)
  
  if (!is.null(result2) && nrow(result2) > 0) {
    pks2 <- result2[, 1]
    locs2 <- result2[, 2]
    TROUGH3 <- data.frame(Datetime_loc = cbt_data$datetime[locs2], Peak_val = -pks2)
  } else {
    TROUGH3 <- NULL
  }

  # Build p2
  p2 <- ggplot(cbt_data, aes(x = datetime, y = cbt)) +
    geom_path(aes(x = datetime, y = c_process, color = "Endogenous circadian"), na.rm = TRUE) +
    geom_path(aes(x = datetime, y = s_process, color = "Homeostatic sleep"), na.rm = TRUE) +
    scale_color_manual(values = c("Endogenous circadian" = "#756bb1", "Homeostatic sleep" = "#31a354")) +
    labs(x = "Datetime", y = "CBT, oC") +
    theme(legend.title = element_blank())

  # Add trough points if available
  if (!is.null(TROUGH) && nrow(TROUGH) > 0) {
    p2 <- p2 + geom_point(data = TROUGH, aes(x = Datetime_loc, y = Peak_val),
                          colour = "#fdae6b", fill = "#e6550d", size = 2.5, shape = 21)
    p2 <- p2 + geom_text(data = TROUGH, aes(x = Datetime_loc, y = Peak_val,
                           label = paste(round(Peak_val, 2), "oC", format(Datetime_loc, "%H:%M"), sep = " | ")),
                         angle = 90, size = 3, hjust = -0.1)
  }

  if (!is.null(TROUGH3) && nrow(TROUGH3) > 0) {
    p2 <- p2 + geom_text(data = TROUGH3, aes(x = Datetime_loc, y = Peak_val,
                           label = paste(round(Peak_val, 2), "oC", format(Datetime_loc, "%H:%M"), sep = " | ")),
                         angle = 90, size = 3, hjust = -0.1)
  }

  # Combine plots
  pp <- ggarrange(p1, p2,
                 labels = c("A", "B"),
                 align = c("v"),
                 heights = c(0.5, 0.5),
                 ncol = 1, nrow = 2)

  return(pp)
}
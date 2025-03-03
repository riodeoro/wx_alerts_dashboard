# Copyright 2025 Province of British Columbia
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at

# http://www.apache.org/licenses/LICENSE-2.0

# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# r/power_module/power_output.R

#' Render power status output
#' @param input Shiny input object
#' @param all_power_data Reactive expression containing power data
render_power_status_output <- function(input, all_power_data) {
  req(input$power_station_select)
  power_data <- all_power_data()
  
  if (input$power_station_select == "All Stations") {
    cat("Select a station to view power status details\n")
    return(invisible())
  }
  
  station_data <- power_data[[input$power_station_select]]
  
  # Basic validation
  if (is.null(station_data) || is.null(station_data$data) || nrow(station_data$data) == 0) {
    cat("No power data available for this station\n")
    return(invisible())
  }
  
  data <- station_data$data
  
  # Validate required columns exist
  required_columns <- c("Vbat", "Ibat", "Vslr", "Islr")
  missing_columns <- required_columns[!required_columns %in% names(data)]
  
  if (length(missing_columns) > 0) {
    cat(sprintf("Missing required power measurements: %s\n", 
                paste(missing_columns, collapse = ", ")))
    return(invisible())
  }
  
  # Check for valid data in required columns
  all_na <- sapply(required_columns, function(col) all(is.na(data[[col]])))
  if (any(all_na)) {
    cat(sprintf("No valid measurements for: %s\n", 
                paste(names(all_na)[all_na], collapse = ", ")))
    return(invisible())
  }
  
  # Have valid data to process
  cat(sprintf("%s\n", input$power_station_select))

safe_stats <- function(x, fn, non_zero_only = FALSE) {
  if (all(is.na(x))) return(NA)
  
  if (non_zero_only) {
    x <- x[!is.na(x) & x != 0]
    if (length(x) == 0) return(NA)
  }
  
  fn(x, na.rm = TRUE)
}
  
  # Calculate statistics - note the non_zero_only parameter for solar voltage
  stats <- list(
    battery = list(
      mean_voltage = safe_stats(data$Vbat, mean),
      median_voltage = safe_stats(data$Vbat, median),
      min_voltage = safe_stats(data$Vbat, min),
      max_voltage = safe_stats(data$Vbat, max),
      mean_current = safe_stats(data$Ibat, mean),
      median_current = safe_stats(data$Ibat, median),
      min_current = safe_stats(data$Ibat, min),
      max_current = safe_stats(data$Ibat, max)
    ),
    solar = list(
      mean_voltage = safe_stats(data$Vslr, mean, non_zero_only = TRUE),
      median_voltage = safe_stats(data$Vslr, median, non_zero_only = TRUE),
      max_voltage = safe_stats(data$Vslr, max),
      mean_current = safe_stats(data$Islr, mean),
      median_current = safe_stats(data$Islr, median),
      max_current = safe_stats(data$Islr, max)
    )
  )
  
  # Find timestamps for min/max values
  if (!all(is.na(data$Vbat))) {
    min_v_idx <- which.min(data$Vbat)
    max_v_idx <- which.max(data$Vbat)
    stats$battery$min_voltage_time <- data$DateTimeNum[min_v_idx]
    stats$battery$max_voltage_time <- data$DateTimeNum[max_v_idx]
  }
  
  if (!all(is.na(data$Ibat))) {
    min_i_idx <- which.min(data$Ibat)
    max_i_idx <- which.max(data$Ibat)
    stats$battery$min_current_time <- data$DateTimeNum[min_i_idx]
    stats$battery$max_current_time <- data$DateTimeNum[max_i_idx]
  }
  
  # Output formatted statistics
  cat("\nBattery Info:\n")
  cat("-------------\n")
  cat(sprintf("Voltage:\n"))
  cat(sprintf("  Average: %.2f V\n", stats$battery$mean_voltage))
  cat(sprintf("  Median:  %.2f V\n", stats$battery$median_voltage))
  if (!is.na(stats$battery$min_voltage)) {
    cat(sprintf("  Minimum: %.2f V at %s\n", 
                stats$battery$min_voltage, 
                stats$battery$min_voltage_time))
    cat(sprintf("  Maximum: %.2f V at %s\n", 
                stats$battery$max_voltage,
                stats$battery$max_voltage_time))
  }
  
  cat(sprintf("\nCurrent:\n"))
  cat(sprintf("  Average: %.2f A\n", stats$battery$mean_current))
  cat(sprintf("  Median:  %.2f A\n", stats$battery$median_current))
  if (!is.na(stats$battery$min_current)) {
    cat(sprintf("  Minimum: %.2f A at %s\n", 
                stats$battery$min_current,
                stats$battery$min_current_time))
    cat(sprintf("  Maximum: %.2f A at %s\n", 
                stats$battery$max_current,
                stats$battery$max_current_time))
  }
  
  cat("\nSolar Panel Status:\n")
  cat("-----------------\n")
  cat(sprintf("Voltage:\n"))
  cat(sprintf("  Average (non-zero): %.2f V\n", stats$solar$mean_voltage))
  cat(sprintf("  Median (non-zero):  %.2f V\n", stats$solar$median_voltage))
  if (!is.na(stats$solar$max_voltage)) {
    cat(sprintf("  Maximum: %.2f V\n", stats$solar$max_voltage))
  }
  
  cat(sprintf("\nCurrent:\n"))
  cat(sprintf("  Average: %.2f A\n", stats$solar$mean_current))
  cat(sprintf("  Median:  %.2f A\n", stats$solar$median_current))
  if (!is.na(stats$solar$max_current)) {
    cat(sprintf("  Maximum: %.2f A\n", stats$solar$max_current))
  }
  
  return(invisible())
}

#' Render battery voltage alerts
#' @param all_power_data Reactive expression containing power data
render_battery_voltage_alerts <- function(all_power_data) {
  cat("Checks for Vbat readings < 12 V\n")  

  req(all_power_data)
  power_data <- all_power_data
  
  if (length(power_data) == 0) {
    cat("No power data available")
    invisible()  # Return invisibly to prevent NULL from being printed
    return()
  }
  
  cat(monitor_battery_voltage(power_data))
  invisible()  # Return invisibly to prevent NULL from being printed
}

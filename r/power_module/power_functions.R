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

# r/power_module/power_functions.R

# Function to identify decreasing Vbat trends
identify_decreasing_vbat_trends <- function(station_data, min_duration = 3, threshold = -0.5) {
  # Initial data validation
  if (is.null(station_data) || 
      !all(c("DateTimeNum", "Vbat", "Temp") %in% names(station_data)) ||
      nrow(station_data) < min_duration) {
    return(NULL)
  }
  
  # Ensure Vbat is numeric
  station_data$Vbat <- as.numeric(station_data$Vbat)
  
  # Remove NA values - keep original order (newest to oldest)
  valid_data <- !is.na(station_data$Vbat) & !is.na(station_data$Temp)
  if (sum(valid_data) < min_duration) {
    return(NULL)
  }
  
  station_data <- station_data[valid_data, ]
  n <- nrow(station_data)
  
  # Initialize variables for trend detection
  trends <- data.frame(
    start_idx = integer(),
    end_idx = integer(),
    duration = integer(),
    start_voltage = numeric(),
    end_voltage = numeric(),
    voltage_change = numeric(),
    stringsAsFactors = FALSE
  )
  
  current_trend_start <- 1
  
  if (n < min_duration) {
    return(trends)
  }
  
  for (i in 2:n) {
    if (is.na(station_data$Vbat[i]) || is.na(station_data$Vbat[i-1])) {
      current_trend_start <- i
      next
    }
    
    # Convert Vbat values to numeric explicitly before comparison
    current_voltage <- as.numeric(station_data$Vbat[i])
    prev_voltage <- as.numeric(station_data$Vbat[i-1])
    
    if (is.na(current_voltage) || is.na(prev_voltage)) {
      current_trend_start <- i
      next
    }
    
    # Since data is newest to oldest, we reverse the comparison
    voltage_change <- current_voltage - prev_voltage
    
    # If voltage decreases or stays same, end current trend
    if (voltage_change <= 0) {
      duration <- i - current_trend_start
      if (duration >= min_duration) {
        # Switch start and end points since we're going backwards in time
        start_v <- as.numeric(station_data$Vbat[i-1])
        end_v <- as.numeric(station_data$Vbat[current_trend_start])
        total_change <- end_v - start_v
        
        if (!is.na(total_change) && total_change <= threshold) {
          trends <- rbind(trends, data.frame(
            start_idx = i-1,  # This is actually the earlier time
            end_idx = current_trend_start,  # This is the later time
            duration = duration,
            start_voltage = start_v,  # Earlier voltage
            end_voltage = end_v,  # Later voltage
            voltage_change = total_change,
            stringsAsFactors = FALSE
          ))
        }
      }
      current_trend_start <- i
    }
  }
  
  # Check final trend
  duration <- n - current_trend_start + 1
  if (duration >= min_duration) {
    start_v <- as.numeric(station_data$Vbat[n])
    end_v <- as.numeric(station_data$Vbat[current_trend_start])
    total_change <- end_v - start_v
    
    if (!is.na(total_change) && total_change <= threshold) {
      trends <- rbind(trends, data.frame(
        start_idx = n,  # Earlier time
        end_idx = current_trend_start,  # Later time
        start_voltage = start_v,  # Earlier voltage
        end_voltage = end_v,  # Later voltage
        duration = duration,
        voltage_change = total_change,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  return(trends)
}

# Function to format the trend output
format_vbat_trend_output <- function(station_name, station_data, trends) {
  if (is.null(trends) || nrow(trends) == 0) {
    return(NULL)
  }
  
  output_text <- sprintf("\nDecreasing Battery Voltage Trends for %s\n", station_name)
  output_text <- paste0(output_text, 
                       paste(rep("=", nchar(sprintf("Decreasing Battery Voltage Trends for %s", station_name))), 
                             collapse = ""), "\n")
  
  for (i in 1:nrow(trends)) {
    trend <- trends[i,]
    start_time <- station_data$DateTimeNum[trend$start_idx]
    end_time <- station_data$DateTimeNum[trend$end_idx]
    
    # Safely get temperature values with error handling
    start_temp <- tryCatch({
      temp <- station_data$Temp[trend$start_idx]
      if (is.null(temp) || is.na(temp)) NA else temp
    }, error = function(e) NA)
    
    end_temp <- tryCatch({
      temp <- station_data$Temp[trend$end_idx]
      if (is.null(temp) || is.na(temp)) NA else temp
    }, error = function(e) NA)
    
    # Build output text with conditional temperature information
    output_text <- paste0(output_text,
      sprintf("\nTrend Period %d:\n", i),
      sprintf("Start Time: %s\n", start_time),
      sprintf("End Time: %s\n", end_time),
      sprintf("Duration: %d hours\n", trend$duration),
      sprintf("Start Voltage: %.2f V\n", trend$start_voltage),
      sprintf("End Voltage: %.2f V\n", trend$end_voltage),
      sprintf("Total Change: %.2f V\n", trend$voltage_change)
    )
    
    # Add temperature information only if valid temperatures are available
    if (!any(is.na(c(start_temp, end_temp)))) {
      output_text <- paste0(output_text,
        sprintf("Temperature Range: %.1f°C to %.1f°C\n", start_temp, end_temp)
      )
    }
    
    output_text <- paste0(output_text, "----------------------------------------\n")
  }
  
  return(output_text)
}

# Main function to analyze all stations
analyze_vbat_trends <- function(power_data) {
  if (is.null(power_data) || length(power_data) == 0) {
    return("No power data available")
  }
  
  output_text <- "Battery Voltage Analysis\n\n"
  output_text <- paste0(output_text, 
                       "Identifies battery voltage drops that decrease > 0.5V over at least 3 consecutive readings.\n\n")
  output_text <- paste0(output_text, "============================\n")
  
  found_trends <- FALSE
  
  for (station_name in names(power_data)) {
    if (is.null(power_data[[station_name]]) || 
        is.null(power_data[[station_name]]$data)) {
      next
    }
    
    station_data <- power_data[[station_name]]$data
    
    if (!all(c("Vbat", "Temp") %in% names(station_data))) {
      next
    }
    
    trends <- identify_decreasing_vbat_trends(station_data)
    
    if (!is.null(trends) && nrow(trends) > 0) {
      found_trends <- TRUE
      trend_output <- format_vbat_trend_output(station_name, station_data, trends)
      if (!is.null(trend_output)) {
        output_text <- paste0(output_text, trend_output)
      }
    }
  }
  
  if (!found_trends) {
    output_text <- paste0(output_text, "\nNo significant decreasing voltage trends detected in selected stations.")
  }
  
  return(output_text)
}

# Function to identify decreasing trends and prepare data
prepare_trend_data <- function(power_data) {
  if (is.null(power_data) || length(power_data) == 0) {
    return(NULL)
  }
  
  all_trends <- list()
  
  for (station_name in names(power_data)) {
    if (is.null(power_data[[station_name]]) || 
        is.null(power_data[[station_name]]$data)) {
      next
    }
    
    station_data <- power_data[[station_name]]$data
    
    if (!all(c("Vbat", "Temp") %in% names(station_data))) {
      next
    }
    
    trends <- identify_decreasing_vbat_trends(station_data)
    
    if (!is.null(trends) && nrow(trends) > 0) {
      station_trends <- list()
      
      for (i in 1:nrow(trends)) {
        trend <- trends[i,]
        trend_points <- station_data[seq(trend$end_idx, trend$start_idx), ]
        trend_points$trend_id <- i
        trend_points$station <- station_name
        station_trends[[i]] <- trend_points
      }
      
      if (length(station_trends) > 0) {
        all_trends[[station_name]] <- do.call(rbind, station_trends)
      }
    }
  }
  
  return(all_trends)
}


# LOW VBAT FUNCTION
# Function to monitor battery voltage and generate alerts
monitor_battery_voltage <- function(power_data) {
  output_text <- ""
  
  for (station_name in names(power_data)) {
    station_data <- power_data[[station_name]]$data
    
    if (!all(c("Vbat", "Ibat", "Vslr", "Islr", "Temp") %in% names(station_data))) {
      next
    }
    
    # Find instances where Vbat < 12
    low_voltage_instances <- which(station_data$Vbat < 12)
    
    if (length(low_voltage_instances) > 0) {
      # Calculate summary statistics
      vbat_stats <- list(
        min_voltage = min(station_data$Vbat[low_voltage_instances], na.rm = TRUE),
        max_voltage = max(station_data$Vbat[low_voltage_instances], na.rm = TRUE),
        mean_voltage = mean(station_data$Vbat[low_voltage_instances], na.rm = TRUE),
        min_temp = min(station_data$Temp[low_voltage_instances], na.rm = TRUE),
        max_temp = max(station_data$Temp[low_voltage_instances], na.rm = TRUE)
      )
      
      # Get timestamps for first and last occurrence
      first_occurrence <- station_data$DateTimeNum[low_voltage_instances[length(low_voltage_instances)]]
      last_occurrence <- station_data$DateTimeNum[low_voltage_instances[1]]
      
      output_text <- paste0(output_text, 
                           "\nLow Battery Voltage Alert for ", station_name, "\n",
                           "=============================================\n",
                           sprintf("Number of Alerts: %d\n", length(low_voltage_instances)),
                           sprintf("Time Period: %s to %s\n", first_occurrence, last_occurrence),
                           sprintf("Voltage Range: %.2f V to %.2f V (avg: %.2f V)\n", 
                                 vbat_stats$min_voltage, vbat_stats$max_voltage, vbat_stats$mean_voltage),
                           sprintf("Temperature Range: %.1f°C to %.1f°C\n\n", 
                                 vbat_stats$min_temp, vbat_stats$max_temp))
    }
  }
  
  if (output_text == "") {
    return("No low battery voltage alerts (all stations reporting Vbat ≥ 12V)")
  }
  
  return(output_text)
}

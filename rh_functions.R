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

# r/rh_module/rh_functions.R

# Function to find 0% RH
check_low_rh_values <- function(data_list) {
  low_rh_info <- data.frame(
    Table = character(),
    Row = integer(),
    Rh = numeric(),
    DateTime = character(),
    stringsAsFactors = FALSE
  )
  
  for (table_name in names(data_list)) {
    data <- data_list[[table_name]]
    
    if ("Rh" %in% colnames(data)) {
      low_rh_rows <- which(data$Rh == 0)
      
      if (length(low_rh_rows) > 0) {
        low_rh_info <- rbind(low_rh_info, data.frame(
          Table = table_name,
          Row = low_rh_rows,
          Rh = data[low_rh_rows, "Rh"],
          DateTime = as.character(data[low_rh_rows, "DateTimeNum"]),
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  if (nrow(low_rh_info) > 0) low_rh_info else "No Rh values = 0% detected."
}
#Function to find RH 100
check_consecutive_rh <- function(stations_data) {
  # Initialize results dataframe
  results <- data.frame(
    Station = character(),
    Consecutive_Count = numeric(),
    Start_Time = character(),
    End_Time = character(),
    Total_Rn_1 = numeric(),
    Avg_Temp = numeric(),
    Max_Temp = numeric(),
    Min_Temp = numeric(),
    Has_Positive_Temp = logical(),
    stringsAsFactors = FALSE
  )
  
  if (is.null(stations_data) || length(stations_data) == 0) {
    return("No station data provided.")
  }
  
  for (station_name in names(stations_data)) {
    data <- stations_data[[station_name]]
    
    # Skip invalid station data
    if (is.null(data) || nrow(data) == 0 || 
        !all(c("DateTimeNum", "Rh", "Rn_1", "Temp") %in% names(data))) {
      next
    }
    
    tryCatch({
      # Convert DateTimeNum to POSIXct if needed
      if (!inherits(data$DateTimeNum, "POSIXct")) {
        data$DateTimeNum <- as.POSIXct(data$DateTimeNum, 
                                      format = "%Y-%b-%d %H:%M:%S", 
                                      tz = "America/Los_Angeles")
      }
      
      # Sort data by time
      data <- data[order(data$DateTimeNum), ]
      
      # Convert columns to numeric
      data$Rh <- as.numeric(data$Rh)
      data$Rn_1 <- as.numeric(data$Rn_1)
      data$Temp <- as.numeric(data$Temp)
      
      # Initialize tracking variables
      consecutive_count <- 0
      current_start <- NULL
      current_rn_1_sum <- 0
      temp_values <- numeric()
      spans <- list()
      
      # Process each row
      for (i in seq_len(nrow(data))) {
        if (!is.na(data$Rh[i]) && data$Rh[i] == 100) {
          # Continue or start new consecutive span
          consecutive_count <- consecutive_count + 1
          if (consecutive_count == 1) {
            current_start <- data$DateTimeNum[i]
            temp_values <- numeric()
          }
          current_rn_1_sum <- current_rn_1_sum + ifelse(is.na(data$Rn_1[i]), 0, data$Rn_1[i])
          temp_values <- c(temp_values, data$Temp[i])
        } else {
          # Process completed sequence if it meets threshold
          if (consecutive_count >= 12) {
            spans[[length(spans) + 1]] <- list(
              start_time = current_start,
              end_time = data$DateTimeNum[i-1],
              count = consecutive_count,
              total_rn_1 = current_rn_1_sum,
              temp_values = temp_values
            )
          }
          # Reset counters
          consecutive_count <- 0
          current_rn_1_sum <- 0
          temp_values <- numeric()
        }
      }
      
      # Handle case where sequence ends with 100s
      if (consecutive_count >= 12) {
        spans[[length(spans) + 1]] <- list(
          start_time = current_start,
          end_time = tail(data$DateTimeNum, 1),
          count = consecutive_count,
          total_rn_1 = current_rn_1_sum,
          temp_values = temp_values
        )
      }
      
      # Process all spans into results dataframe
      for (span in spans) {
        if (length(span$temp_values) > 0) {
          avg_temp <- mean(span$temp_values, na.rm = TRUE)
          max_temp <- max(span$temp_values, na.rm = TRUE)
          min_temp <- min(span$temp_values, na.rm = TRUE)
          has_positive_temp <- any(span$temp_values > 0, na.rm = TRUE)
          
          results <- rbind(results, data.frame(
            Station = gsub("dbo\\.", "", station_name),
            Consecutive_Count = span$count,
            Start_Time = format(span$start_time, "%Y-%b-%d %H:%M:%S"),
            End_Time = format(span$end_time, "%Y-%b-%d %H:%M:%S"),
            Total_Rn_1 = span$total_rn_1,
            Avg_Temp = avg_temp,
            Max_Temp = max_temp,
            Min_Temp = min_temp,
            Has_Positive_Temp = has_positive_temp,
            stringsAsFactors = FALSE
          ))
        }
      }
    }, error = function(e) {
      warning(sprintf("Error processing station %s: %s", station_name, e$message))
      next
    })
  }
  
  # Return results
  if (nrow(results) > 0) {
    return(results[order(results$Station, results$Start_Time), ])
  } else {
    return("No stations found with 12 or more consecutive hourly RH 100% readings.")
  }
}

# Function to calculate percentage of RH 100% readings
calculate_rh_percentage <- function(data) {
  if (is.null(data) || nrow(data) == 0 || !"Rh" %in% names(data)) {
    return(0)
  }
  
  # Ensure Rh column is numeric and handle any conversion issues
  data$Rh <- suppressWarnings(as.numeric(as.character(data$Rh)))
  
  # Remove any invalid values (NA, NaN, Inf)
  valid_rh <- data$Rh[!is.na(data$Rh) & is.finite(data$Rh)]
  if (length(valid_rh) == 0) return(0)
  
  # Count exact matches to 100 (using near() for floating point comparison)
  rh_100_count <- sum(abs(valid_rh - 100) < 1e-10)
  total_entries <- length(valid_rh)
  
  # Calculate percentage
  percentage <- (rh_100_count / total_entries) * 100
  
  return(percentage)
}

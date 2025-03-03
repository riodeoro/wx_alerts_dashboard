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

#r/wind_module/wind_functions.R

get_most_recent_non_zero_wspd <- function(data) {
  # Early returns if data is invalid
  if (is.null(data) || nrow(data) == 0 || !"Wspd" %in% names(data)) {
    return(NULL)
  }
  
  # Handle date conversion
  data$DateTimeNum <- if(is.character(data$DateTimeNum)) {
    as.POSIXct(data$DateTimeNum, format = "%Y-%b-%d %H:%M:%S", tz = "UTC")
  } else {
    data$DateTimeNum
  }
  
  # Find first non-zero, non-NA wind speed
  valid_data <- data[!is.na(data$Wspd) & data$Wspd > 0, ]
  
  # Sort by date in descending order
  valid_data <- valid_data[order(valid_data$DateTimeNum, decreasing = TRUE), ]
  
  # Return NULL if no valid entries found
  if (nrow(valid_data) == 0) {
    return(NULL)
  }
  
  # Get the most recent entry
  most_recent <- valid_data[1, ]
  
  return(list(
    wind_speed = most_recent$Wspd,
    date_time = most_recent$DateTimeNum
  ))
}
check_high_wspd <- function(data, threshold = 60) {
  high_wspd_info <- data.frame(
    Table = character(),
    DateTime = character(),
    Type = character(),
    Value = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (table_name in names(data)) {
    df <- data[[table_name]]
    
    if ("Wspd" %in% colnames(df)) {
      high_wspd_rows <- df[df$Wspd > threshold, ]
      
      if (nrow(high_wspd_rows) > 0) {
        high_wspd_info <- rbind(high_wspd_info, data.frame(
          Table = table_name,
          DateTime = high_wspd_rows$DateTimeNum,
          Type = "Wspd",
          Value = high_wspd_rows$Wspd,
          stringsAsFactors = FALSE
        ))
      }
    }
    
    if ("Mx_Spd" %in% colnames(df)) {
      high_mx_spd_rows <- df[df$Mx_Spd > threshold, ]
      
      if (nrow(high_mx_spd_rows) > 0) {
        high_wspd_info <- rbind(high_wspd_info, data.frame(
          Table = table_name,
          DateTime = high_mx_spd_rows$DateTimeNum,
          Type = "Mx_Spd",
          Value = high_mx_spd_rows$Mx_Spd,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  if (nrow(high_wspd_info) > 0) high_wspd_info else "No Wspd or Mx_Spd values greater than 30 detected."
}

#HIGH WSPD CHECK FUNCTION
check_high_wspd <- function(data, threshold = 60) {
  high_wspd_info <- data.frame(
    Table = character(),
    DateTime = character(),
    Type = character(),  
    Value = numeric(),   
    stringsAsFactors = FALSE
  )
  
  for (table_name in names(data)) {
    df <- data[[table_name]]
    
    # Check Wspd
    if ("Wspd" %in% colnames(df)) {
      high_wspd_rows <- df[df$Wspd > threshold, ]
      
      if (nrow(high_wspd_rows) > 0) {
        high_wspd_info <- rbind(high_wspd_info, data.frame(
          Table = table_name,
          DateTime = high_wspd_rows$DateTimeNum,
          Type = "Wspd",
          Value = high_wspd_rows$Wspd,
          stringsAsFactors = FALSE
        ))
      }
    }
    
    # Check Mx_Spd
    if ("Mx_Spd" %in% colnames(df)) {
      high_mx_spd_rows <- df[df$Mx_Spd > threshold, ]
      
      if (nrow(high_mx_spd_rows) > 0) {
        high_wspd_info <- rbind(high_wspd_info, data.frame(
          Table = table_name,
          DateTime = high_mx_spd_rows$DateTimeNum,
          Type = "Mx_Spd",
          Value = high_mx_spd_rows$Mx_Spd,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  if (nrow(high_wspd_info) > 0) high_wspd_info else "No Wspd or Mx_Spd values greater than 30 detected."
}

# Function to find zero wind speed periods with Mx_Spd range
find_zero_wspd_periods <- function(data, threshold = 12) {
  if (is.null(data) || nrow(data) == 0) {
    return(data.frame(
      period_id = numeric(0),
      period_start = as.POSIXct(character(0)),
      period_end = as.POSIXct(character(0)),
      count_consecutive = numeric(0),
      dir_changes = numeric(0),
      max_dir_change = numeric(0),
      min_mx_spd = numeric(0),
      max_mx_spd = numeric(0),
      anomalous = logical(0)
    ))
  }
  
  data$DateTimeNum <- if(is.character(data$DateTimeNum)) {
    as.POSIXct(data$DateTimeNum, format = "%Y-%b-%d %H:%M:%S", tz = "UTC")
  } else {
    data$DateTimeNum
  }
  
  # Check if Mx_Spd column exists and has any non-NA values
  has_mx_spd <- "Mx_Spd" %in% names(data) && any(!is.na(data$Mx_Spd))
  
  # First, ensure Dir column exists and handle missing values
  if (!"Dir" %in% names(data)) {
    data$Dir <- NA_real_
  }
  
  result <- data %>%
    arrange(DateTimeNum) %>%
    mutate(
      zero_wspd = ifelse(is.na(Wspd), FALSE, Wspd == 0),
      shift_zero_wspd = lag(zero_wspd, default = FALSE),
      change = zero_wspd != shift_zero_wspd,
      period_id = cumsum(change),
      # Calculate direction change only when both current and previous directions are valid
      dir_change = case_when(
        is.na(Dir) | is.na(lag(Dir)) ~ 0,
        TRUE ~ abs(Dir - lag(Dir))
      )
    ) %>%
    group_by(period_id) %>%
    summarize(
      period_start = min(DateTimeNum, na.rm = TRUE),
      period_end = max(DateTimeNum, na.rm = TRUE),
      count_consecutive = sum(zero_wspd, na.rm = TRUE),
      # Only count significant direction changes (> 10 degrees)
      dir_changes = sum(dir_change > 10, na.rm = TRUE),
      # Handle case where all direction changes are NA
      max_dir_change = ifelse(all(is.na(dir_change)), 0, max(dir_change, na.rm = TRUE)),
      # Only calculate Mx_Spd statistics if the column exists and has valid data
      min_mx_spd = if(has_mx_spd) {
        mx_spd_values <- Mx_Spd[!is.na(Mx_Spd)]
        if(length(mx_spd_values) > 0) min(mx_spd_values) else NA_real_
      } else NA_real_,
      max_mx_spd = if(has_mx_spd) {
        mx_spd_values <- Mx_Spd[!is.na(Mx_Spd)]
        if(length(mx_spd_values) > 0) max(mx_spd_values) else NA_real_
      } else NA_real_,
      .groups = 'drop'
    ) %>%
    mutate(
      anomalous = count_consecutive >= threshold & dir_changes > 2
    ) %>%
    filter(count_consecutive >= threshold)
  
  return(result)
}
# Function to check current zero wind speed block
check_current_zero_wspd_block <- function(data, zero_wspd_periods) {
  data$DateTimeNum <- as.POSIXct(data$DateTimeNum, format = "%Y-%b-%d %H:%M:%S", tz = "UTC")
  most_recent_entry <- data %>%
    arrange(desc(DateTimeNum)) %>%
    slice_head(n = 1)
  
  if (nrow(most_recent_entry) > 0) {
    latest_time <- most_recent_entry$DateTimeNum
    latest_wspd <- most_recent_entry$Wspd
    
    still_in_block <- any(zero_wspd_periods$period_start <= latest_time & 
                         zero_wspd_periods$period_end >= latest_time)
    
    return(list(
      still_in_block = still_in_block,
      most_recent_time = latest_time,
      most_recent_wspd = latest_wspd
    ))
  }
  
  return(list(
    still_in_block = FALSE,
    most_recent_time = NULL,
    most_recent_wspd = NULL
  ))
}

# Function to find periods of constant wind direction with detailed wind analysis
find_constant_dir_periods <- function(data, min_entries = 4) {
  # Return empty dataframe if data is invalid
  if (is.null(data) || nrow(data) == 0 || !"Dir" %in% names(data)) {
    return(data.frame(
      period_id = integer(0),
      period_start = character(0),
      period_end = character(0),
      duration_entries = integer(0),
      dir = numeric(0),
      before_dir = numeric(0),
      before_time = character(0),
      after_dir = numeric(0),
      after_time = character(0),
      avg_wspd = numeric(0),
      max_wspd = numeric(0),
      max_gust = numeric(0),
      sustained_wind_count = numeric(0),
      total_entries = numeric(0),
      suspicious = logical(0)
    ))
  }

  # Ensure datetime is in correct format
  data$DateTimeNum <- if(is.character(data$DateTimeNum)) {
    as.POSIXct(data$DateTimeNum, format = "%Y-%b-%d %H:%M:%S", tz = "UTC")
  } else {
    data$DateTimeNum
  }

  # Sort data chronologically
  data <- data %>% arrange(DateTimeNum)

  # Find periods of constant direction
  result <- data %>%
    mutate(
      dir_change = Dir != lag(Dir, default = first(Dir)),
      period_id = cumsum(dir_change),
      wind_over_threshold = Wspd >= 5  # Track readings above threshold
    ) %>%
    group_by(period_id) %>%
    summarize(
      period_start = first(DateTimeNum),
      period_end = last(DateTimeNum),
      duration_entries = n(),
      dir = first(Dir),
      avg_wspd = mean(Wspd, na.rm = TRUE),
      max_wspd = max(Wspd, na.rm = TRUE),
      max_gust = if("Mx_Spd" %in% names(data)) max(Mx_Spd, na.rm = TRUE) else NA,
      sustained_wind_count = sum(wind_over_threshold, na.rm = TRUE),
      total_entries = sum(!is.na(Wspd)),
      .groups = 'drop'
    ) %>%
    filter(duration_entries >= min_entries)

  # Add before/after information
  result$before_dir <- NA_real_
  result$before_time <- NA_character_
  result$after_dir <- NA_real_
  result$after_time <- NA_character_

  # Process each row to add before/after information
  for(i in seq_len(nrow(result))) {
    period_start_time <- result$period_start[i]
    period_end_time <- result$period_end[i]
    
    start_idx <- which(data$DateTimeNum == period_start_time)
    end_idx <- which(data$DateTimeNum == period_end_time)
    
    if(start_idx > 1) {
      result$before_dir[i] <- data$Dir[start_idx - 1]
      result$before_time[i] <- format(data$DateTimeNum[start_idx - 1], "%Y-%b-%d %H:%M:%S")
    }
    
    if(end_idx < nrow(data)) {
      result$after_dir[i] <- data$Dir[end_idx + 1]
      result$after_time[i] <- format(data$DateTimeNum[end_idx + 1], "%Y-%b-%d %H:%M:%S")
    }
  }

  # Flag periods based on wind conditions
  result <- result %>%
    mutate(
      suspicious = !is.na(avg_wspd) & !is.na(max_wspd) & (
        # Flag if majority of readings show wind >= 5 km/h
        (sustained_wind_count >= total_entries * 0.75 & duration_entries >= 6) |
        # Flag if max wind speed or gust is high during constant direction
        (max_wspd >= 10) |
        (!is.na(max_gust) & max_gust >= 15)
      )
    )

  return(result)
}

# Helper function to display period information with wind details
display_period <- function(row, flagged = FALSE) {
  cat("\nConstant Direction Period", if(flagged) " *** FLAGGED ***" else "", "\n")
  cat(sprintf("Start Time:    %s\n", format(row$period_start, "%Y-%b-%d %H:%M:%S")))
  cat(sprintf("End Time:      %s\n", format(row$period_end, "%Y-%b-%d %H:%M:%S")))
  cat(sprintf("Entries:       %d\n", row$duration_entries))
  cat(sprintf("Direction:     %.2f°\n", row$dir))
  cat(sprintf("Avg Wspd:      %.1f km/h\n", row$avg_wspd))
  cat(sprintf("Max Wspd:      %.1f km/h\n", row$max_wspd))
  
  # Add wind persistence information
  if (!is.na(row$sustained_wind_count) && !is.na(row$total_entries)) {
    wind_persistence <- (row$sustained_wind_count / row$total_entries) * 100
    cat(sprintf("Wind ≥ 5 km/h: %d of %d readings (%.1f%%)\n", 
                row$sustained_wind_count, row$total_entries, wind_persistence))
  }
  
  if (!is.na(row$max_gust)) {
    cat(sprintf("Mx_Spd:        %.1f km/h\n", row$max_gust))
  }
  
  if (!is.na(row$before_dir)) {
    cat(sprintf("Dir Before:    %.2f° at %s\n", row$before_dir, row$before_time))
  } else {
    cat("Dir Before:    No prior direction available\n")
  }
  
  if (!is.na(row$after_dir)) {
    cat(sprintf("Dir After:     %.2f° at %s\n", row$after_dir, row$after_time))
  } else {
    cat("Dir After:     No subsequent direction available\n")
  }
  
  if (flagged) {
    cat("\nFlagged due to:\n")
    if (!is.na(row$sustained_wind_count) && !is.na(row$total_entries) &&
        (row$sustained_wind_count >= row$total_entries * 0.75) && row$duration_entries >= 6) {
      cat(sprintf("- Sustained winds ≥ 5 km/h for %.1f%% of period\n", 
                 (row$sustained_wind_count / row$total_entries) * 100))
    }
    if (!is.na(row$max_wspd) && row$max_wspd >= 10) {
      cat("- High maximum wind speed (≥ 10 km/h)\n")
    }
    if (!is.na(row$max_gust) && row$max_gust >= 15) {
      cat("- Strong gusts (≥ 15 km/h)\n")
    }
  }
  cat("\n")
}

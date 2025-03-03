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

# r/temperature_module/temperature_functions.R

detect_extreme_temps <- function(wx_data, high_threshold = 40, low_threshold = -40) {
  if (is.null(wx_data) || length(wx_data) == 0) {
    return(NULL)
  }
  
  extreme_temps <- list()
  
  for (station_name in names(wx_data)) {
    station_data <- wx_data[[station_name]]
    
    if (!is.null(station_data) && "Temp" %in% names(station_data) && nrow(station_data) > 0) {
      station_data$Temp <- as.numeric(station_data$Temp)
      
      high_temps <- station_data[station_data$Temp >= high_threshold, ]
      low_temps <- station_data[station_data$Temp <= low_threshold, ]
      
      if (nrow(high_temps) > 0 || nrow(low_temps) > 0) {
        clean_station_name <- sub("^dbo\\.", "", station_name)
        extreme_temps[[clean_station_name]] <- list(
          high_temps = if(nrow(high_temps) > 0) {
            data.frame(
              time = high_temps$DateTimeNum,
              temp = high_temps$Temp,
              stringsAsFactors = FALSE
            )
          } else NULL,
          low_temps = if(nrow(low_temps) > 0) {
            data.frame(
              time = low_temps$DateTimeNum,
              temp = low_temps$Temp,
              stringsAsFactors = FALSE
            )
          } else NULL
        )
      }
    }
  }
  
  return(extreme_temps)
}


analyze_temp_patterns <- function(temp_changes, change_threshold = 5, variability_window = 3, sd_threshold = 4) {
  if (is.null(temp_changes) || nrow(temp_changes) == 0) {
    return(NULL)
  }
  
  stations <- unique(temp_changes$Station)
  results <- list()
  
  for (station in stations) {
    station_data <- temp_changes %>%
      filter(Station == station) %>%
      arrange(DateTimeNum)
    
    if (nrow(station_data) < variability_window) {
      next
    }
    
    # Identify rapid changes
    rapid_changes <- station_data %>%
      filter(abs(temp_change) >= change_threshold) %>%
      mutate(
        change_type = ifelse(temp_change > 0, "Warming", "Cooling")
      ) %>%
      arrange(desc(abs(temp_change)))
    
    # Calculate rolling standard deviation for variability
    station_data <- station_data %>%
      mutate(
        rolling_sd = rollapply(temp_change, width = variability_window, 
                             FUN = sd, fill = NA, align = "right")
      )
    
    variable_periods <- station_data %>%
      filter(rolling_sd >= sd_threshold) %>%
      arrange(desc(rolling_sd))
    
    results[[station]] <- list(
      rapid_changes = rapid_changes,
      variable_periods = variable_periods
    )
    
    # Add summary statistics
    if (nrow(rapid_changes) > 0 || nrow(variable_periods) > 0) {
      attr(results[[station]], "summary") <- list(
        total_rapid_changes = nrow(rapid_changes),
        max_change = if(nrow(rapid_changes) > 0) max(abs(rapid_changes$temp_change)) else 0,
        mean_change = if(nrow(rapid_changes) > 0) mean(abs(rapid_changes$temp_change)) else 0,
        warming_count = if(nrow(rapid_changes) > 0) sum(rapid_changes$temp_change > 0) else 0,
        cooling_count = if(nrow(rapid_changes) > 0) sum(rapid_changes$temp_change < 0) else 0,
        variable_periods = nrow(variable_periods),
        max_variability = if(nrow(variable_periods) > 0) max(variable_periods$rolling_sd, na.rm = TRUE) else 0
      )
    }
  }
  
  return(results)
}
calculate_hourly_temp_changes <- function(wx_data) {
  if (is.null(wx_data) || length(wx_data) == 0) {
    return(NULL)
  }
  
  # Initialize empty data frame for results
  all_changes <- data.frame()
  
  # Process each station
  for (station_name in names(wx_data)) {
    station_data <- wx_data[[station_name]]
    
    if (!is.null(station_data) && "Temp" %in% names(station_data) && nrow(station_data) > 0) {
      # Convert DateTimeNum to POSIXct if it's character
      if (is.character(station_data$DateTimeNum)) {
        station_data$DateTimeNum <- as.POSIXct(station_data$DateTimeNum, 
                                              format = "%Y-%b-%d %H:%M:%S", 
                                              tz = "UTC")
      }
      
      # Calculate temperature changes
      changes <- station_data %>%
        arrange(DateTimeNum) %>%
        mutate(
          Temp = as.numeric(Temp),
          temp_change = lead(Temp) - Temp,
          Station = sub("^dbo\\.", "", station_name)
        ) %>%
        select(Station, DateTimeNum, temp_change) %>%
        filter(!is.na(temp_change))
      
      all_changes <- rbind(all_changes, changes)
    }
  }
  
  return(all_changes)
}

#Function to find contant temp periods
find_constant_temp_periods <- function(data, min_entries = 4) {
  # Check if data is empty or NULL
  if (is.null(data) || nrow(data) == 0) {
    return(data.frame(
      period_id = integer(0),
      period_start = as.POSIXct(character(0)),
      period_end = as.POSIXct(character(0)),
      duration_entries = integer(0),
      temp = numeric(0),
      percentage_constant = numeric(0)
    ))
  }
  
  # Convert DateTimeNum to POSIXct if it's character
  data$DateTimeNum <- if(is.character(data$DateTimeNum)) {
    as.POSIXct(data$DateTimeNum, format = "%Y-%b-%d %H:%M:%S", tz = "UTC")
  } else {
    data$DateTimeNum
  }
  
  # Handle NA values and create groups
  result <- data %>%
    arrange(DateTimeNum) %>%
    mutate(
      temp_change = Temp != lag(Temp, default = first(Temp)),
      period_id = cumsum(temp_change)
    ) %>%
    group_by(period_id) %>%
    summarize(
      period_start = first(DateTimeNum),
      period_end = last(DateTimeNum),
      duration_entries = n(),
      temp = first(Temp),
      .groups = 'drop'  # Explicitly drop grouping
    ) %>%
    filter(duration_entries >= min_entries)
  
  # Calculate percentage of entries that are constant
  if (nrow(result) > 0) {
    total_entries <- nrow(data)
    total_constant_entries <- sum(result$duration_entries)
    result$percentage_constant <- (result$duration_entries / total_entries) * 100
    
    # Add overall percentage as an attribute
    attr(result, "overall_percentage") <- (total_constant_entries / total_entries) * 100
  } else {
    result$percentage_constant <- numeric(0)
    attr(result, "overall_percentage") <- 0
  }
  
  return(result)
}
#Function to find rapid temp changes
find_rapid_temp_changes <- function(data, threshold = 8) {
        data %>%
          arrange(DateTimeNum) %>%
          mutate(
            Temp = as.numeric(as.character(Temp)),
            temp_diff = abs(lead(Temp) - Temp),
            rapid_change = temp_diff >= threshold,
            change_type = ifelse(lead(Temp) > Temp, "Warming", "Cooling")
          ) %>%
          filter(rapid_change) %>%
          select(DateTimeNum, Temp, temp_diff, change_type) %>%
          arrange(DateTimeNum)
      }

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

# r/precipitation_module/precipitation_functions.R

#RN_1 OUTLIERS FUNCTION
identify_outliers <- function(data, k = 6, threshold_multiplier = 4) {
  if (!"Rn_1" %in% colnames(data)) {
    return(NULL)
  }
  
  # Ensure data is properly formatted
  data <- tryCatch({
    data %>%
      arrange(DateTimeNum) %>%
      mutate(
        Rn_1 = as.numeric(Rn_1),
        Rh = as.numeric(Rh),
        Temp = as.numeric(Temp)
      )
  }, error = function(e) {
    warning("Error converting data types: ", e$message)
    return(NULL)
  })
  
  if (is.null(data)) return(NULL)
  
  # Calculate running average and identify outliers
  result <- tryCatch({
    data %>%
      mutate(
        RunningAvg = zoo::rollmean(Rn_1, k = k, fill = NA),
        Outlier = abs(Rn_1 - RunningAvg) > (threshold_multiplier * sd(Rn_1, na.rm = TRUE))
      ) %>%
      filter(Outlier == TRUE) %>%
      select(DateTimeNum, Rn_1, RunningAvg, Rh, Temp) %>%
      arrange(desc(DateTimeNum))
  }, error = function(e) {
    warning("Error calculating outliers: ", e$message)
    return(NULL)
  })
  
  return(result)
}
# Temperature trends analysis
identify_temp_trends <- function(data, 
                               min_rh = 99,         
                               min_hours = 3,       
                               min_total_drop = 3.0) { 
    
  if(nrow(data) < 2) return(list())
  
  # Ensure data is chronologically ordered and filter for high RH
  processed_data <- data %>%
    arrange(DateTimeNum) %>%
    mutate(DateTimeNum = as.POSIXct(DateTimeNum, format = "%Y-%b-%d %H:%M:%S", tz = "UTC")) %>%
    filter(Rh >= min_rh)
  
  if(nrow(processed_data) < min_hours) return(list())

  trends <- list()
  current_trend <- NULL
  
  for(i in 2:nrow(processed_data)) {
    # Check if this is a consecutive hour and cooling
    time_diff <- as.numeric(difftime(processed_data$DateTimeNum[i], 
                                   processed_data$DateTimeNum[i-1], 
                                   units="hours"))
    is_consecutive_hour <- abs(time_diff - 1) < 0.1  # Allow slight deviation from exactly 1 hour
    is_cooling <- processed_data$Temp[i] < processed_data$Temp[i-1]
    
    if(is_consecutive_hour && is_cooling) {
      if(is.null(current_trend)) {
        # Start new trend with both points
        current_trend <- processed_data[(i-1):i,]
      } else {
        # Add point to existing trend
        current_trend <- rbind(current_trend, processed_data[i,])
      }
    } else {
      # End current trend if we have one and evaluate it
      if(!is.null(current_trend) && nrow(current_trend) >= min_hours) {
        total_temp_drop <- max(current_trend$Temp) - min(current_trend$Temp)
        
        # Only keep trends with total temperature drop >= min_total_drop
        if(total_temp_drop >= min_total_drop) {
          # Check data coverage after trend
          trend_end_time <- max(current_trend$DateTimeNum)
          data_coverage <- check_data_coverage(data, trend_end_time)
          
          trend_stats <- list(
            start_time = min(current_trend$DateTimeNum),
            end_time = max(current_trend$DateTimeNum),
            duration = nrow(current_trend) - 1,  # Number of hourly decreases
            total_temp_change = total_temp_drop,
            avg_rate = total_temp_drop / (nrow(current_trend) - 1),
            min_temp = min(current_trend$Temp),
            max_temp = max(current_trend$Temp),
            avg_rh = mean(current_trend$Rh),
            data_coverage = data_coverage  # Add data coverage information
          )
          
          trends[[length(trends) + 1]] <- list(
            data = current_trend,
            stats = trend_stats
          )
        }
      }
      current_trend <- NULL
    }
  }
  
  # Check final trend
  if(!is.null(current_trend) && nrow(current_trend) >= min_hours) {
    total_temp_drop <- max(current_trend$Temp) - min(current_trend$Temp)
    
    if(total_temp_drop >= min_total_drop) {
      trend_end_time <- max(current_trend$DateTimeNum)
      data_coverage <- check_data_coverage(data, trend_end_time)
      
      trend_stats <- list(
        start_time = min(current_trend$DateTimeNum),
        end_time = max(current_trend$DateTimeNum),
        duration = nrow(current_trend) - 1,
        total_temp_change = total_temp_drop,
        avg_rate = total_temp_drop / (nrow(current_trend) - 1),
        min_temp = min(current_trend$Temp),
        max_temp = max(current_trend$Temp),
        avg_rh = mean(current_trend$Rh),
        data_coverage = data_coverage  # Add data coverage information
      )
      
      trends[[length(trends) + 1]] <- list(
        data = current_trend,
        stats = trend_stats
      )
    }
  }
  
  return(trends)
}

# New helper function to check data coverage
check_data_coverage <- function(data, trend_end_time) {
  # Convert trend_end_time to POSIXct if it isn't already
  if(!inherits(trend_end_time, "POSIXct")) {
    trend_end_time <- as.POSIXct(trend_end_time, format="%Y-%b-%d %H:%M:%S", tz="UTC")
  }
  
  # Get the last timestamp in the dataset
  last_timestamp <- max(as.POSIXct(data$DateTimeNum, format="%Y-%b-%d %H:%M:%S", tz="UTC"))
  
  # Calculate time difference in hours
  hours_after_trend <- as.numeric(difftime(last_timestamp, trend_end_time, units="hours"))
  
  # Return coverage information
  list(
    hours_after_trend = hours_after_trend,
    last_timestamp = last_timestamp,
    has_full_coverage = hours_after_trend >= 24  # Assuming we want at least 24 hours of data after the trend
  )
}
#RECENT RN_1 FUNCTION
get_most_recent_rn1 <- function(data) {
  recent_entry <- data %>%
    filter(Rn_1 > 0) %>%
    arrange(desc(DateTimeNum)) %>%
    slice(1)
  
  if (nrow(recent_entry) == 0) {
    return(data.frame(Message = "No precip recorded in fetched data"))
  } else {
    return(recent_entry)
  }
}

#RECENT RN_1 FUNCTION
get_most_recent_rn1 <- function(data) {
  recent_entry <- data %>%
    filter(Rn_1 > 0) %>%
    arrange(desc(DateTimeNum)) %>%
    slice(1)
  
  if (nrow(recent_entry) == 0) {
    return(data.frame(Message = "No precip recorded in fetched data"))
  } else {
    return(recent_entry)
  }
}

calculate_station_rainfall_totals <- function(stations_data) {
  results <- data.frame(
    Fire_Zone = character(),
    Station = character(),
    Total_Rn_1 = numeric(),
    First_Reading = character(),
    Last_Reading = character(),
    Num_Readings = integer(),
    Mean_Rn_1 = numeric(),
    stringsAsFactors = FALSE
  )
  
  if (is.null(stations_data) || length(stations_data) == 0) {
    return(results)
  }

  # Create a lookup table for station to zone mapping with underscores
  station_zone_lookup <- list()
  for (zone_name in names(fire_zone_mapping)) {
    for (station in fire_zone_mapping[[zone_name]]) {
      # Convert space-separated names to underscore format
      lookup_name <- gsub(" ", "_", station)
      station_zone_lookup[[lookup_name]] <- zone_name
    }
  }

  for (station_name in names(stations_data)) {
    data <- stations_data[[station_name]]
    
    if (is.null(data) || 
        !is.data.frame(data) || 
        nrow(data) == 0 || 
        !all(c("Rn_1", "DateTimeNum") %in% colnames(data))) {
      next
    }
    
    data$Rn_1 <- as.numeric(data$Rn_1)
    valid_rows <- !is.na(data$DateTimeNum) & !is.na(data$Rn_1)
    
    if (sum(valid_rows) > 0) {
      total_rn_1 <- sum(data$Rn_1[valid_rows], na.rm = TRUE)
      first_reading <- min(data$DateTimeNum[valid_rows])
      last_reading <- max(data$DateTimeNum[valid_rows])
      mean_rn_1 <- mean(data$Rn_1[valid_rows], na.rm = TRUE)
      
      clean_station_name <- gsub("dbo.", "", station_name)
      lookup_name <- toupper(clean_station_name)
      
      # Find the zone using the lookup table
      fire_zone <- station_zone_lookup[[lookup_name]]
      if (is.null(fire_zone)) {
        fire_zone <- "Unassigned Zone"
      }
      
      results <- rbind(results, data.frame(
        Fire_Zone = fire_zone,
        Station = clean_station_name,
        Total_Rn_1 = round(total_rn_1, 1),
        First_Reading = first_reading,
        Last_Reading = last_reading,
        Num_Readings = sum(valid_rows),
        Mean_Rn_1 = round(mean_rn_1, 3),
        stringsAsFactors = FALSE
      ))
    }
  }
  
  if (nrow(results) > 0) {
    results <- results[order(results$Fire_Zone, -results$Total_Rn_1), ]
  }
  
  return(results)
}

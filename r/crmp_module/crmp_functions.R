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

# r/crmp_module/crmp_functions.R

identify_sdepth_concerns <- function(data, change_threshold = 10, negative_threshold = -2.0) {
  has_sdepth <- "SDepth" %in% colnames(data)
  has_sd <- "SD" %in% colnames(data)
  
  if (!has_sdepth && !has_sd) {
    warning("Neither 'SDepth' nor 'SD' columns are present in the data. Returning empty results.")
    return(list(
      rapid_changes = data.frame(),
      negative_summary = data.frame()
    ))
  }
  
  # Process each sensor type separately and combine results
  results_list <- list()
  
  if (has_sdepth) {
    # Calculate changes for SDepth
    sdepth_data <- data %>%
      arrange(DateTimeNum) %>%
      mutate(
        sensor_type = "SDepth",
        depth_value = SDepth,
        depth_change = SDepth - lag(SDepth),
        is_negative = SDepth < negative_threshold
      )
    results_list$SDepth <- process_depth_data(sdepth_data, change_threshold)
  }
  
  if (has_sd) {
    # Calculate changes for SD
    sd_data <- data %>%
      arrange(DateTimeNum) %>%
      mutate(
        sensor_type = "SD",
        depth_value = SD,
        depth_change = SD - lag(SD),
        is_negative = SD < negative_threshold
      )
    results_list$SD <- process_depth_data(sd_data, change_threshold)
  }
  
  # Combine results from both sensors
  combined_results <- list(
    rapid_changes = bind_rows(
      if(has_sdepth) results_list$SDepth$rapid_changes else NULL,
      if(has_sd) results_list$SD$rapid_changes else NULL
    ),
    negative_summary = bind_rows(
      if(has_sdepth) results_list$SDepth$negative_summary else NULL,
      if(has_sd) results_list$SD$negative_summary else NULL
    )
  )
  
  return(combined_results)
}

# Helper function to process depth data
process_depth_data <- function(data, change_threshold) {
  # Identify rapid changes
  rapid_changes <- data %>%
    filter(abs(depth_change) > change_threshold) %>%
    mutate(
      change_type = ifelse(depth_change > 0, "increase", "decrease")
    ) %>%
    select(DateTimeNum, sensor_type, depth_value, depth_change, change_type) %>%
    arrange(desc(DateTimeNum))
  
  # Summarize negative values
  negative_summary <- data %>%
    filter(is_negative) %>%
    summarize(
      sensor_type = first(sensor_type),
      total_negative_count = n(),
      first_occurrence = if (n() > 0) min(DateTimeNum, na.rm = TRUE) else NA,
      last_occurrence = if (n() > 0) max(DateTimeNum, na.rm = TRUE) else NA,
      min_value = if (n() > 0) min(depth_value, na.rm = TRUE) else NA,
      median_value = if (n() > 0) median(depth_value, na.rm = TRUE) else NA
    )
  
  list(
    rapid_changes = rapid_changes,
    negative_summary = negative_summary
  )
}

render_sdepth_concerns_output <- function(WX_stations, active_crmp_sensors) {
  cat("\nChecks for rapid hourly changes (>10cm) or negative values\n\n")
  
  # Get stations with either SDepth or SD sensors
  depth_stations <- names(active_crmp_sensors)[sapply(active_crmp_sensors, function(x) 
    "SDepth" %in% x || "SD" %in% x)]
  concerns_found <- FALSE
  
  for (station in depth_stations) {
    station_data <- WX_stations[[station]]
    if (is.null(station_data)) next
    concerns <- identify_sdepth_concerns(station_data)
    if (is.null(concerns)) next
    
    # Only proceed if there are actual concerns to report
    if (nrow(concerns$rapid_changes) > 0 || 
        (nrow(concerns$negative_summary) > 0 && any(concerns$negative_summary$total_negative_count > 0))) {
      concerns_found <- TRUE
      cat(sprintf("Station: %s\n", station))
      cat(paste(rep("-", nchar(station) + 9), collapse = ""), "\n")
      
      # Report rapid changes
      if (nrow(concerns$rapid_changes) > 0) {
        cat("\nRapid Changes (>10cm):\n")
        changes_to_print <- concerns$rapid_changes %>%
          mutate(
            Change = sprintf("%+.1f cm", depth_change)
          ) %>%
          select(DateTimeNum, sensor_type, change_type, Change, depth_value)
        print(changes_to_print, row.names = FALSE)
      }
      
      # Report negative value summary by sensor type
      if (nrow(concerns$negative_summary) > 0) {
        for (sensor in unique(concerns$negative_summary$sensor_type)) {
          sensor_summary <- concerns$negative_summary %>% 
            filter(sensor_type == sensor, total_negative_count > 0)
          
          if (nrow(sensor_summary) > 0) {
            cat(sprintf("\nNegative Values Summary (%s):\n", sensor))
            cat(sprintf("Total occurrences: %d\n", sensor_summary$total_negative_count))
            cat(sprintf("Period: %s to %s\n", sensor_summary$first_occurrence, 
                       sensor_summary$last_occurrence))
            cat(sprintf("Min value: %.1f cm, Median: %.1f cm\n", 
                       sensor_summary$min_value, 
                       sensor_summary$median_value))
          }
        }
      }
      cat("\n")
    }
  }
  if (!concerns_found) {
    cat("No snow depth alerts detected.\n")
  }
}

# PRECIPOP2 and PRECIPP2 alert function
identify_precip_changes <- function(data, column, increase_threshold = 10, decrease_threshold = -2.0) {
  if (!column %in% colnames(data)) {
    return(NULL)
  }

  data <- data %>%
    mutate(
      # Calculate change looking at the next reading in descending order
      PrecipChange = !!sym(column) - lead(!!sym(column)),
      # Flag concerning changes
      ChangeType = case_when(
        PrecipChange > increase_threshold ~ "Large Increase",
        PrecipChange < decrease_threshold ~ "Decrease",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(ChangeType)) %>%  # Keep only rows with flagged changes
    select(DateTimeNum, !!sym(column), PrecipChange, ChangeType)

  # Return NULL if no concerning changes are detected
  if (nrow(data) == 0) return(NULL)
  
  data
}

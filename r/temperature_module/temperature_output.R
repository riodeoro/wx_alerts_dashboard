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

# r/temperature_module/temperature_output.R

render_extreme_temp_output <- function(wx_data) {
  if (is.null(wx_data) || length(wx_data) == 0) {
    cat("No data available for analysis.\n")
    return(invisible())
  }
  
  extreme_temps <- detect_extreme_temps(wx_data)
  
  cat("Identifies temperatures >= 40°C or <= -40°C\n")
  cat("----------------------------------------\n\n")
  
  if (is.null(extreme_temps) || length(extreme_temps) == 0) {
    cat("No extreme temperatures detected in any station.\n")
    return(invisible())
  }
  
  # Process high temperatures first
  has_high_temps <- FALSE
  for (station_name in names(extreme_temps)) {
    if (!is.null(extreme_temps[[station_name]]$high_temps) && 
        nrow(extreme_temps[[station_name]]$high_temps) > 0) {
      
      # Filter out NA values
      high_temps <- extreme_temps[[station_name]]$high_temps
      high_temps <- high_temps[!is.na(high_temps$temp), ]
      
      if (nrow(high_temps) > 0) {
        if (!has_high_temps) {
          cat("High Temperature Events (>= 40°C):\n")
          has_high_temps <- TRUE
        }
        cat(sprintf("\n%s:\n", station_name))
        for (i in 1:nrow(high_temps)) {
          cat(sprintf("  %s: %.1f°C\n",
                     high_temps$time[i],
                     high_temps$temp[i]))
        }
      }
    }
  }
  
  # Process low temperatures
  has_low_temps <- FALSE
  for (station_name in names(extreme_temps)) {
    if (!is.null(extreme_temps[[station_name]]$low_temps) && 
        nrow(extreme_temps[[station_name]]$low_temps) > 0) {
      
      # Filter out NA values
      low_temps <- extreme_temps[[station_name]]$low_temps
      low_temps <- low_temps[!is.na(low_temps$temp), ]
      
      if (nrow(low_temps) > 0) {
        if (!has_low_temps) {
          if (has_high_temps) cat("\n") # Add spacing between sections
          cat("Low Temperature Events (<= -40°C):\n")
          has_low_temps <- TRUE
        }
        cat(sprintf("\n%s:\n", station_name))
        for (i in 1:nrow(low_temps)) {
          cat(sprintf("  %s: %.1f°C\n",
                     low_temps$time[i],
                     low_temps$temp[i]))
        }
      }
    }
  }
  
  if (!has_high_temps && !has_low_temps) {
    cat("No extreme temperatures found.\n")
  }
  
  invisible()
}
#' Render constant temperature output
#' @param input Shiny input object
#' @param constant_temp_data Reactive expression containing constant temperature data
render_temp_check <- function(input, constant_temp_data) {

cat("Detects constant temperature spans > 3 consecutive hours.\n")

  if (is.null(constant_temp_data) || length(constant_temp_data) == 0) {
    cat("No temperature data available.")
    return()
  }
  
  constant_temp <- constant_temp_data
  selected_station <- input$constant_temp_station_select
  
  if (!is.null(selected_station) && selected_station != "All Stations") {
    if (!selected_station %in% names(constant_temp)) {
      cat("No data available for selected station.")
      return()
    }
    constant_temp <- constant_temp[selected_station]
  }  
  for (table_name in names(constant_temp)) {
    results <- constant_temp[[table_name]]
    if (nrow(results) > 0) {
      clean_table_name <- sub("^dbo\\.", "", table_name)
      overall_percentage <- attr(results, "overall_percentage")
      
      cat("\n", clean_table_name, "\n", sep = "")
      cat(sprintf("Overall %.1f%% of hourly readings contribute to constant temperature spans > 4 consecutive hours\n", overall_percentage))
      
      for (i in seq_len(nrow(results))) {
        row <- results[i, ]
        cat("\nConstant Temperature Period\n")
        cat(sprintf("Start Time: %s\n", format(row$period_start, "%Y-%m-%d %H:%M:%S")))
        cat(sprintf("End Time: %s\n", format(row$period_end, "%Y-%m-%d %H:%M:%S")))
        cat(sprintf("Entries: %d (%.1f%% of total readings)\n", 
                   row$duration_entries, row$percentage_constant))
        cat(sprintf("Temperature: %.2f°C\n", row$temp))
      }
      cat(paste(rep("=", 80), collapse = ""), "\n")
    }
  }
}
#' Render rapid temperature changes output
#' @param WX_stations List of weather station data
render_rapid_temp_changes_output <- function(WX_stations) {
  results_list <- list()
  
  for (table_name in names(WX_stations)) {
    data <- WX_stations[[table_name]]
    
    rapid_changes <- find_rapid_temp_changes(data)
    
    if (nrow(rapid_changes) > 0) {
      results_list[[table_name]] <- rapid_changes
    }
  }
  
  if (length(results_list) > 0) {
    cat("\nRapid Temperature Changes Detected, 8°C threshold:\n")
    cat("--------------------------\n")
    
    for (table_name in names(results_list)) {
      clean_table_name <- sub("^dbo\\.", "", table_name)
      
      cat("\n", clean_table_name, "\n", sep = "")
      
      rapid_changes <- results_list[[table_name]]
      
      apply(rapid_changes, 1, function(row) {
        cat(sprintf("%s | Temp: %.2f°C | Change: %.2f°C | %s\n",
                    row["DateTimeNum"], 
                    as.numeric(row["Temp"]), 
                    as.numeric(row["temp_diff"]),
                    row["change_type"]))
      })
    }
  } else {
    cat("No rapid temperature changes (≥8°C/hour) found\n")
  }
}

render_erratic_temp_output <- function(wx_data, selected_station = NULL) {
  # Initial data validation
  if (is.null(wx_data) || length(wx_data) == 0) {
    cat("No data available for analysis.\n")
    return(invisible(NULL))
  }
  
  # Filter data based on selected station
  if (!is.null(selected_station) && selected_station != "All Stations") {
    wx_data <- wx_data[selected_station]
  }
  
  temp_changes <- calculate_hourly_temp_changes(wx_data)
  
  if (is.null(temp_changes) || nrow(temp_changes) == 0) {
    cat("No temperature change data available for analysis.\n")
    return(invisible(NULL))
  }
  
  results <- analyze_temp_patterns(temp_changes, 
                                 change_threshold = 5, 
                                 variability_window = 3, 
                                 sd_threshold = 4)
  
  if (is.null(results) || length(results) == 0) {
    # Print analysis criteria even when no results are found
    cat("Temperature Change Analysis\n")
    cat("Identifies:\n")
    cat("1. Temperature changes of 5°C or more between consecutive hourly readings\n")
    cat("2. Periods of high variability (SD > 4°C over 3 consecutive hours)\n\n")
    cat("No significant temperature patterns detected in any stations.\n")
    return(invisible(NULL))
  }
  
  # Check if any station has actual results
  has_results <- FALSE
  for (station in names(results)) {
    station_results <- results[[station]]
    if (!is.null(station_results)) {
      summary_stats <- attr(station_results, "summary")
      if (!is.null(summary_stats) && 
          ((!is.null(summary_stats$total_rapid_changes) && summary_stats$total_rapid_changes > 0) ||
           (!is.null(summary_stats$variable_periods) && summary_stats$variable_periods > 0))) {
        has_results <- TRUE
        break
      }
    }
  }
  
  if (!has_results) {
    cat("Temperature Change Analysis\n")
    cat("Identifies:\n")
    cat("1. Temperature changes of 5°C or more between consecutive hourly readings\n")
    cat("2. Periods of high variability (SD > 4°C over 3 consecutive hours)\n\n")
    cat("No significant temperature patterns detected in any stations.\n")
    return(invisible(NULL))
  }
  
  # Print analysis header
  cat("Temperature Change Analysis\n")
  cat("Identifies:\n")
  cat("1. Temperature changes of 5°C or more between consecutive hourly readings\n")
  cat("2. Periods of high variability (SD > 4°C over 3 consecutive hours)\n\n")
  
  # Process each station
  for (station in names(results)) {
    station_results <- results[[station]]
    if (is.null(station_results)) next
    
    summary_stats <- attr(station_results, "summary")
    if (is.null(summary_stats)) next
    
    # Skip stations with no significant patterns
    if ((!is.null(summary_stats$total_rapid_changes) && summary_stats$total_rapid_changes == 0) &&
        (!is.null(summary_stats$variable_periods) && summary_stats$variable_periods == 0)) {
      next
    }
    
    cat(sprintf("=== %s ===\n", station))
    
    # Rapid changes section
    if (!is.null(summary_stats$total_rapid_changes) && summary_stats$total_rapid_changes > 0) {
      cat("\nRapid Changes:\n")
      cat(sprintf("Total rapid changes: %d (%d warming, %d cooling)\n", 
                 summary_stats$total_rapid_changes,
                 summary_stats$warming_count,
                 summary_stats$cooling_count))
      cat(sprintf("Maximum change: %.1f°C\n", summary_stats$max_change))
      
      if (!is.null(station_results$rapid_changes) && nrow(station_results$rapid_changes) > 0) {
        cat("\nLargest temperature changes:\n")
        top_changes <- head(station_results$rapid_changes, 3)
        for (i in seq_len(nrow(top_changes))) {
          cat(sprintf(
            "%s - %s of %.1f°C\n",
            format(top_changes$DateTimeNum[i], "%Y-%b-%d %H:%M:%S"),
            top_changes$change_type[i],
            abs(top_changes$temp_change[i])
          ))
        }
      }
    }
    
    # Variable periods section
    if (!is.null(summary_stats$variable_periods) && summary_stats$variable_periods > 0) {
      cat("\nPeriods of High Variability:\n")
      cat(sprintf("Number of variable periods: %d\n", summary_stats$variable_periods))
      cat(sprintf("Maximum variability (SD): %.1f°C\n", summary_stats$max_variability))
      
      if (!is.null(station_results$variable_periods) && nrow(station_results$variable_periods) > 0) {
        cat("\nMost variable periods:\n")
        top_variable <- head(station_results$variable_periods, 3)
        for (i in seq_len(nrow(top_variable))) {
          cat(sprintf(
            "%s - SD: %.1f°C\n",
            format(top_variable$DateTimeNum[i], "%Y-%b-%d %H:%M:%S"),
            top_variable$rolling_sd[i]
          ))
        }
      }
    }
    
    if (station != tail(names(results), 1)) {
      cat("\n", paste(rep("-", 50), collapse = ""), "\n")
    }
  }
}

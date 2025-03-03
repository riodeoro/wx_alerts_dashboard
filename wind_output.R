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

#r/wind_module/wind_output.R

render_high_wspd_output <- function(WX_stations) {
  high_wspd_results <- check_high_wspd(WX_stations)
  
  cat("Checks for Wspd or Mx_Spd over 50 km/h.\n")
  
  if (is.data.frame(high_wspd_results) && nrow(high_wspd_results) > 0) {
    print(high_wspd_results)
  } else {
    cat("No Wspd or Mx_Spd values > 50 km/h found.\n")
  }
}

render_dir_check_output <- function(WX_stations) {
  any_constant_periods <- FALSE
  all_station_results <- list()
  flagged_periods <- list()

  for (table_name in names(WX_stations)) {
    data <- WX_stations[[table_name]]
    results <- find_constant_dir_periods(data)
    
    if (!is.null(results) && nrow(results) > 0) {
      any_constant_periods <- TRUE
      station_name <- sub("^dbo\\.", "", table_name)
      
      # Separate flagged periods
      if (any(results$suspicious)) {
        flagged_results <- results[results$suspicious, ]
        flagged_periods[[station_name]] <- flagged_results
      }
      
      all_station_results[[table_name]] <- list(
        data = data,
        results = results,
        max_duration = max(results$duration_entries),
        flagged_count = sum(results$suspicious, na.rm = TRUE)
      )
    }
  }

  cat("Analysis of constant wind direction periods (> 3 hours)\n")
  
  if (any_constant_periods) {
    # First display flagged periods if any exist
    if (length(flagged_periods) > 0) {
      cat("\n=== FLAGGED PERIODS ===\n")
      for (station_name in names(flagged_periods)) {
        cat("\n", station_name, "\n", sep = "")
        flagged_results <- flagged_periods[[station_name]]
        
        for (i in seq_len(nrow(flagged_results))) {
          row <- flagged_results[i, ]
          display_period(row, flagged = TRUE)
        }
        cat(paste(rep("=", 30), collapse = ""), "\n")
      }
      cat("\n=== ALL CONSTANT DIRECTION PERIODS ===\n")
    }
    
    # Sort stations by number of flagged periods first, then by max duration
    sorted_stations <- names(all_station_results)[order(
      sapply(all_station_results, function(x) x$flagged_count),
      sapply(all_station_results, function(x) x$max_duration),
      decreasing = TRUE
    )]
    
    for (table_name in sorted_stations) {
      station_info <- all_station_results[[table_name]]
      results <- station_info$results
      
      clean_table_name <- sub("^dbo\\.", "", table_name)
      cat("\n", clean_table_name, "\n", sep = "")
      
      results <- results[order(results$suspicious, results$duration_entries, decreasing = TRUE), ]
      
      for (i in seq_len(nrow(results))) {
        row <- results[i, ]
        display_period(row, flagged = row$suspicious)
      }
      cat(paste(rep("=", 30), collapse = ""), "\n")
    }
  } else {
    cat("No constant direction periods detected.\n")
  }
}
render_zero_wspd_output <- function(WX_stations, selected_station) {
  any_results <- FALSE

  cat("Flags hourly zero wspd blocks of at least 12 hours with concurrent dir changes.\n\n")
  
  
  stations_to_display <- if (!is.null(selected_station) && selected_station != "All Stations") {
    c(selected_station)
  } else {
    names(WX_stations)
  }
  
  # Create a list to store all results for sorting
  all_results <- list()
  
  for (table_name in stations_to_display) {
    data <- WX_stations[[table_name]]
    
    diagnostic_results <- find_zero_wspd_periods(data)
    recent_non_zero <- get_most_recent_non_zero_wspd(data)
    
    if (nrow(diagnostic_results) > 0) {
      any_results <- TRUE
      max_consecutive <- max(diagnostic_results$count_consecutive)
      all_results[[table_name]] <- list(
        table_name = table_name,
        diagnostic_results = diagnostic_results,
        recent_non_zero = recent_non_zero,
        max_consecutive = max_consecutive
      )
    }
  }
  
  # Sort stations by maximum consecutive hours
  if (length(all_results) > 0) {
    sorted_stations <- names(all_results)[order(sapply(all_results, function(x) x$max_consecutive), decreasing = TRUE)]
    
    for (table_name in sorted_stations) {
      result <- all_results[[table_name]]
      diagnostic_results <- result$diagnostic_results
      recent_non_zero <- result$recent_non_zero
      
      clean_table_name <- sub("dbo\\.", "", table_name)
      cat("\nStation:", clean_table_name, "\n")
      cat(rep("-", 50), "\n")
      
      anomaly_count <- sum(diagnostic_results$anomalous)
      if (anomaly_count > 0) {
        cat(sprintf("Found %d block(s) with wspd/dir inconsistencies\n", anomaly_count))
      }
      
      for (i in seq_len(nrow(diagnostic_results))) {
        period <- diagnostic_results[i, ]
        
        cat(sprintf("\nBlock %d %s\n", i, 
            if(period$anomalous) "*** Hourly Wspd/Dir Flagged ***" else ""))
        cat(sprintf("Start Time:           %s\n", format(period$period_start, "%Y-%m-%d %H:%M:%S")))
        cat(sprintf("End Time:             %s\n", format(period$period_end, "%Y-%m-%d %H:%M:%S")))
        cat(sprintf("Consecutive Hourly Zero Wspd:    %d\n", period$count_consecutive))
        cat(sprintf("Direction Changes:    %d\n", period$dir_changes))
        cat(sprintf("Max Direction Change: %.1f degrees\n", period$max_dir_change))
        
        if (!is.na(period$min_mx_spd) && !is.na(period$max_mx_spd)) {
          cat(sprintf("Mx_Spd Range:         %.1f to %.1f\n", 
                     period$min_mx_spd, period$max_mx_spd))
        }
        
        if (period$anomalous) {
          cat("\nZero hourly wspd detected with concurrent direction changes\n")
        }
      }
      
      if (!is.null(recent_non_zero)) {
        cat(sprintf("\nMost recent non-zero hourly wspd in fetched data: %.1f at %s\n",
                   recent_non_zero$wind_speed,
                   format(recent_non_zero$date_time, "%Y-%m-%d %H:%M:%S")))
      } else {
        cat("\nNo non-zero hourly wspd values found in the fetched data\n")
      }
      
      cat(strrep("=", 50), "\n")
    }
  }
  
  if (!any_results) {
    cat("No hourly zero wind speed blocks detected.\n")
  }
}

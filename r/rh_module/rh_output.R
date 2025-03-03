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

# r/rh_module/rh_output.R

#' Render the low RH values output
#' @param WX_stations List of weather station data
render_low_rh_output <- function(WX_stations) {
  low_rh_results <- check_low_rh_values(WX_stations)
  if (is.data.frame(low_rh_results)) {
    cat("Low Rh values detected in the following details:\n")
    print(low_rh_results)
  } else {
    cat(low_rh_results, "\n")
  }
}

#' Render the consecutive RH 100% output
#' @param WX_stations List of weather station data
#' @param selected_station Selected station name or "All Stations"
# Function to print section header with consistent formatting
print_section_header <- function(text, is_subsection = FALSE) {
  if (!is_subsection) {
    cat("\n", paste0(rep("=", 80), collapse=""), "\n")
    cat(sprintf("%s\n", toupper(text)))
    cat(paste0(rep("=", 80), collapse=""), "\n\n")
  } else {
    cat("\n")
    cat(sprintf("%s\n", text))
    cat(paste0(rep("-", 40), collapse=""), "\n\n")
  }
}

#' Render the consecutive RH 100% output with percentages
#' @param stations_data List of weather station data
#' @param selected_station Selected station name or "All Stations"
render_consecutive_rh_output <- function(stations_data, selected_station = "All Stations") {
  consecutive_rh_results <- check_consecutive_rh(stations_data)
  
  cat("Identifies occurrences where RH remains at 100% for 12 or more consecutive hours and flags spans with minimum temperature > 0°C and Rn_1 = 0.\n\n")
  
  if (!is.data.frame(consecutive_rh_results)) {
    cat(consecutive_rh_results, "\n")
    return()
  }
  
  if (selected_station != "All Stations") {
    consecutive_rh_results <- consecutive_rh_results[consecutive_rh_results$Station == selected_station, ]
    if (nrow(consecutive_rh_results) == 0) {
      cat("No consecutive RH 100% readings found for the selected station.\n")
      return()
    }
  }
  
  # Calculate summary statistics with percentages for all stations
  station_summary <- data.frame(
    station = names(stations_data),
    stringsAsFactors = FALSE
  )
  
  # Add percentage calculations for all stations
  station_summary$rh_percentage <- sapply(station_summary$station, function(station) {
    station_data <- stations_data[[station]]
    calculate_rh_percentage(station_data)
  })
  
  # Add counts and flagged counts
  station_counts <- table(consecutive_rh_results$Station)
  station_summary$count <- 0
  station_summary$count[match(names(station_counts), gsub("dbo\\.", "", station_summary$station))] <- as.numeric(station_counts)
  
  station_summary$flagged_count <- sapply(gsub("dbo\\.", "", station_summary$station), function(station) {
    station_data <- consecutive_rh_results[consecutive_rh_results$Station == station, ]
    sum(station_data$Min_Temp > 0 & station_data$Total_Rn_1 == 0)
  })
  
  # Clean up station names and filter
  station_summary$station <- gsub("dbo\\.", "", station_summary$station)
  station_summary <- station_summary[station_summary$count > 0, ]
  
  # Sort stations
  flagged_stations <- station_summary[station_summary$flagged_count > 0, ]
  non_flagged_stations <- station_summary[station_summary$flagged_count == 0, ]
  flagged_stations <- flagged_stations[order(-flagged_stations$flagged_count, -flagged_stations$count), ]
  non_flagged_stations <- non_flagged_stations[order(-non_flagged_stations$rh_percentage), ]
  station_summary <- rbind(flagged_stations, non_flagged_stations)
  
  # Store the order for later use
  station_order <- station_summary$station
  
  # Print Overview section
  print_section_header("Overview")
  cat(sprintf("Total stations with extended RH 100%% periods: %d\n\n", nrow(station_summary)))
  
  cat("Breakdown by station:\n")
  
  # Only add line break if there are both flagged and non-flagged stations
  has_flagged <- any(station_summary$flagged_count > 0)
  last_flagged <- if(has_flagged) max(which(station_summary$flagged_count > 0)) else 0
  
  for (i in 1:nrow(station_summary)) {
    if (has_flagged && i == last_flagged + 1) cat("\n")
    
    station_name <- paste0(station_summary$station[i], ":")
    spans_info <- if (station_summary$flagged_count[i] > 0) {
      sprintf("%d (%d flagged)", 
              station_summary$count[i],
              station_summary$flagged_count[i])
    } else {
      sprintf("%d", 
              station_summary$count[i])
    }
    
    cat(sprintf("%-20s %-25s • RH=100%% for %.1f%% of available data\n",
               station_name,
               spans_info,
               station_summary$rh_percentage[i]))
  }
  
  # Print Flagged Events section
  flagged_results <- consecutive_rh_results[consecutive_rh_results$Min_Temp > 0 & consecutive_rh_results$Total_Rn_1 == 0, ]
  if (nrow(flagged_results) > 0) {
    print_section_header("Flagged Spans")
    
    # Sort flagged results first by station order, then by start time
    flagged_results <- flagged_results[order(
      match(flagged_results$Station, station_order),
      as.POSIXct(flagged_results$Start_Time, format="%Y-%b-%d %H:%M:%S")
    ), ]
    
    current_station <- ""
    for (i in 1:nrow(flagged_results)) {
      if (current_station != flagged_results$Station[i]) {
        if (current_station != "") cat("\n")
        current_station <- flagged_results$Station[i]
        print_section_header(current_station, TRUE)
      } else {
        cat("\n\nSpan ", i, "\n", sep="")
      }
      
      cat(sprintf("Start Time: %s\nEnd Time: %s\nDuration: %d hours\nTotal Rain: %.1f mm\nAvg Temp: %.1f°C\nMax Temp: %.1f°C\nMin Temp: %.1f°C\n",
                flagged_results$Start_Time[i],
                flagged_results$End_Time[i],
                flagged_results$Consecutive_Count[i],
                flagged_results$Total_Rn_1[i],
                flagged_results$Avg_Temp[i],
                flagged_results$Max_Temp[i],
                flagged_results$Min_Temp[i]))
    }
  }
  
  # Print Detailed Records section
  non_flagged <- consecutive_rh_results[!(consecutive_rh_results$Min_Temp > 0 & consecutive_rh_results$Total_Rn_1 == 0), ]
  if (nrow(non_flagged) > 0) {
    print_section_header("ALL EXTENDED RH 100% PERIODS")
    
    # Sort non-flagged results by station order and start time
    non_flagged <- non_flagged[order(
      match(non_flagged$Station, station_order),
      as.POSIXct(non_flagged$Start_Time, format="%Y-%b-%d %H:%M:%S")
    ), ]
    
    # Add span counter per station
    station_span_counter <- 1
    current_station <- ""
    for (i in 1:nrow(non_flagged)) {
      if (current_station != non_flagged$Station[i]) {
        if (current_station != "") cat("\n")
        current_station <- non_flagged$Station[i]
        print_section_header(current_station, TRUE)
        station_span_counter <- 1
      } else {
        station_span_counter <- station_span_counter + 1
      }
      
      cat(if(station_span_counter == 1) "" else "\n\n", "Span ", station_span_counter, "\n", sep="")
      
      cat(sprintf("Start Time: %s\nEnd Time: %s\nDuration: %d hours\nTotal Rain: %.1f mm\nAvg Temp: %.1f°C\nMax Temp: %.1f°C\nMin Temp: %.1f°C\n",
                non_flagged$Start_Time[i],
                non_flagged$End_Time[i],
                non_flagged$Consecutive_Count[i],
                non_flagged$Total_Rn_1[i],
                non_flagged$Avg_Temp[i],
                non_flagged$Max_Temp[i],
                non_flagged$Min_Temp[i]))
    }
  }
}

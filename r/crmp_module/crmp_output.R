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

# r/crmp_module/crmp_output.R

#' Render SDEPTH and SD concerns output
#' @param WX_stations List of weather station data
#' @param active_crmp_sensors List of active CRMP sensors
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
      
      # Report negative value summary by sensor type only if negative values exist
      if (nrow(concerns$negative_summary) > 0) {
        negative_sensors <- concerns$negative_summary %>%
          filter(total_negative_count > 0)
        
        if (nrow(negative_sensors) > 0) {
          for (sensor in unique(negative_sensors$sensor_type)) {
            sensor_summary <- negative_sensors %>% 
              filter(sensor_type == sensor)
            
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
#' Render PRECIPOP2 and CP2 issues output
#' @param precip_changes List of precipitation changes
render_crmp_precip_changes <- function(precip_changes) {
  cat("Flags notable hourly changes in PrecipOP2 and PrecipPC2:\n")
  cat("- Large Increases: > 10mm between readings\n")
  cat("- Decreases: < -3.0mm\n\n")
  
  # More comprehensive check for "no changes"
  has_changes <- FALSE
  for (station in names(precip_changes)) {
    if (!is.null(precip_changes[[station]]) && 
        inherits(precip_changes[[station]], "data.frame") && 
        nrow(precip_changes[[station]]) > 0) {
      has_changes <- TRUE
      break
    }
  }
  
  if (!has_changes) {
    cat("No notable changes detected.")
  } else {
    for (station in names(precip_changes)) {
      if (!is.null(precip_changes[[station]]) && 
          inherits(precip_changes[[station]], "data.frame") && 
          nrow(precip_changes[[station]]) > 0) {
        # Extract just the station name without dbo. and without the _OP2/_PC2 suffix
        clean_station <- sub("^dbo\\.", "", strsplit(station, "_[OP|PC]2")[[1]][1])
        precip_type <- ifelse(grepl("_OP2$", station), "PrecipOP2", "PrecipPC2")
        cat("\n", clean_station, " (", precip_type, ")\n", sep = "")
        print(precip_changes[[station]], row.names = FALSE)
      }
    }
  }
}

#' Render CRMP entries output
#' @param WX_stations List of weather station data
#' @param active_crmp_sensors List of active CRMP sensors
render_crmp_entries <- function(WX_stations, active_crmp_sensors) {
  req(WX_stations)
  
  for (station in names(active_crmp_sensors)) {
    data <- WX_stations[[station]]
    if (is.null(data)) next
    
    clean_station_name <- sub("^dbo\\.", "", station)
    cat("\n", clean_station_name, "\n", sep = "")
    
    # Get active sensors for this station
    active_sensors <- active_crmp_sensors[[station]]
    
    # Get columns that exist in the data
    cols_to_show <- c("DateTimeNum", intersect(active_sensors, names(data)))
    if (length(cols_to_show) > 1) {  # Only proceed if we have CRMP columns
      print(data[cols_to_show])
    }
  }
}

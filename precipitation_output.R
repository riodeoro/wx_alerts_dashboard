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

# r/precipitation_module/precipitation_output.R

#' Render decreasing temperature trends output
#' @param WX_stations List of weather station data frames
#' @return Rendered output of decreasing temperature trends with high RH
render_decreasing_trends_output <- function(WX_stations, selected_station = "All Stations") {
  cat("Finds consecutive hourly temperature drops of 3.0°C or more over at least 3 hours when relative humidity is at least 99%.\n")
  cat("Flags trend with all positive temperatures and no precipitation.\n\n")
  
  flagged_stations <- list()
  unflagged_stations <- list()
  
  for (table_name in names(WX_stations)) {
    if(selected_station != "All Stations" && 
       gsub("^dbo\\.", "", table_name) != selected_station) next
    
    data <- WX_stations[[table_name]]
    
    if (!is.null(data) && nrow(data) > 0) {
      data$DateTimeNum <- as.POSIXct(data$DateTimeNum, format = "%Y-%b-%d %H:%M:%S", tz = "UTC")
      trends <- identify_temp_trends(data)
      
      if (length(trends) > 0) {
        station_has_flag <- FALSE
        station_trends <- list(
          name = gsub("^dbo\\.", "", table_name),
          data = data,
          trends = trends
        )
        
        for (trend in trends) {
          all_positive_temps <- all(trend$data$Temp > 0)
          trend_precip <- data %>%
            filter(DateTimeNum >= trend$stats$start_time,
                   DateTimeNum <= trend$stats$end_time,
                   Rn_1 > 0) %>%
            summarize(total_rn = sum(Rn_1, na.rm = TRUE)) %>%
            pull(total_rn)
          
          if (all_positive_temps && trend_precip == 0) {
            station_has_flag <- TRUE
            break
          }
        }
        
        if (station_has_flag) {
          flagged_stations[[table_name]] <- station_trends
        } else {
          unflagged_stations[[table_name]] <- station_trends
        }
      }
    }
  }
  
  print_station_trends <- function(station_info) {
    cat("\n",station_info$name, "\n")
    cat("----------------------------------------\n\n")
    
    for (i in seq_along(station_info$trends)) {
      stats <- station_info$trends[[i]]$stats
      trend_data <- station_info$trends[[i]]$data
      
      all_positive_temps <- all(trend_data$Temp > 0)
      trend_precip <- station_info$data %>%
        filter(DateTimeNum >= stats$start_time,
               DateTimeNum <= stats$end_time,
               Rn_1 > 0) %>%
        summarize(total_rn = sum(Rn_1, na.rm = TRUE)) %>%
        pull(total_rn)
      
      if (all_positive_temps && trend_precip == 0) {
        cat("FLAGGED: All positive temperatures with no precipitation\n")
      }
      
      cat(sprintf("Trend %d: %d consecutive cooling hours\n", i, stats$duration))
      cat(sprintf("Time: %s to %s\n", 
          format(stats$start_time, "%Y-%m-%d %H:%M"),
          format(stats$end_time, "%Y-%m-%d %H:%M")))
      cat(sprintf("Total Temperature Drop: %.1f°C (%.2f°C/hr average)\n", 
          abs(stats$total_temp_change), abs(stats$avg_rate)))
      
      has_vslr <- "Vslr" %in% names(station_info$data)
      if (has_vslr) {
        valid_vslr <- station_info$data %>%
          filter(DateTimeNum >= stats$start_time,
                 DateTimeNum <= stats$end_time) %>%
          pull(Vslr) %>%
          as.numeric()
        
        if (length(valid_vslr) > 0 && !all(is.na(valid_vslr))) {
          min_vslr <- min(valid_vslr, na.rm = TRUE)
          max_vslr <- max(valid_vslr, na.rm = TRUE)
          
          if (is.finite(min_vslr) && is.finite(max_vslr)) {
            cat(sprintf("Solar Voltage Range: %.1fV to %.1fV\n",
                min_vslr, max_vslr))
          } else {
            cat("Solar Voltage Range: Not available\n")
          }
        } else {
          cat("Solar Voltage Range: Not available\n")
        }
      } else {
        cat("Solar Voltage Range: No solar voltage data available for this station\n")
      }
      
      cat(sprintf("Temperature Range: %.1f°C to %.1f°C\n", 
          stats$max_temp, stats$min_temp))
      cat(sprintf("Average RH: %.1f%%\n", stats$avg_rh))
      
      if (trend_precip > 0) {
        cat(sprintf("Precipitation during trend: %.2f mm\n", trend_precip))
      } else {
        cat("No precipitation recorded during trend.\n")
      }
      
      trend_end_time <- max(trend_data$DateTimeNum)
      next_precip <- station_info$data %>%
        filter(DateTimeNum > trend_end_time, Rn_1 > 0) %>%
        arrange(DateTimeNum) %>%
        slice(1)
      
      if(nrow(next_precip) > 0) {
        cat(sprintf("\nMost recent precipitation after trend: %.2f mm at %s\n",
            next_precip$Rn_1, 
            format(next_precip$DateTimeNum, "%Y-%m-%d %H:%M")))
      } else {
        cat("\nNo precipitation recorded after this trend in the fetched data.\n")
      }
      
      cat("\n")
    }
  }
  
  if (length(flagged_stations) > 0) {
    if (length(flagged_stations) == 1) {
      cat("Found 1 station with flagged trends:\n")
    } else {
      cat(sprintf("Found %d stations with flagged trends:\n", length(flagged_stations)))
    }
    for (station in flagged_stations) {
      print_station_trends(station)
    }
  }
  
  if (length(unflagged_stations) > 0) {
    if (length(flagged_stations) > 0) {
      cat("\nOther stations with cooling trends:\n")
    }
    for (station in unflagged_stations) {
      print_station_trends(station)
    }
  }
  
  if (length(flagged_stations) == 0 && length(unflagged_stations) == 0) {
    cat("No consecutive hourly cooling trends found for the selected station(s).\n")
  }
}
#' Render Rn_1 outliers output
#' @param input Shiny input object
#' @param all_rn1_outliers Reactive expression containing Rn_1 outlier data
render_rn1_outliers_output <- function(input, all_rn1_outliers) {
  rn1_outliers <- all_rn1_outliers
  
  cat("Flags Rn_1 values that exceed 4x standard deviation from their 6-hour running average.\n\n")
  
  selected_station <- input$rn1_outliers_station_select
  
  if (!is.null(selected_station) && selected_station != "All Stations") {
    rn1_outliers <- rn1_outliers[selected_station]
  }
  
  if (length(rn1_outliers) == 0 || all(sapply(rn1_outliers, is.null))) {
    cat("No outliers detected.")
  } else {
    for (station in names(rn1_outliers)) {
      if (!is.null(rn1_outliers[[station]]) && nrow(rn1_outliers[[station]]) > 0) {
        station_name <- sub("^dbo\\.", "", station)
        cat("\n", station_name, "\n", sep = "")
        print(rn1_outliers[[station]], row.names = FALSE)
      }
    }
  }
}


render_station_rainfall_output <- function(wx_data, station_select = NULL) {
  if (is.null(wx_data) || length(wx_data) == 0) {
    cat("No data available for analysis.")
    return(invisible(NULL))
  }

  results <- data.frame(
    Fire_Zone = character(),
    Station = character(),
    Total_Rn_1 = numeric(),
    stringsAsFactors = FALSE
  )

  # Create lookup table handling multi-word station names
  station_zone_lookup <- list()
  for (zone_name in names(fire_zone_mapping)) {
    for (station in fire_zone_mapping[[zone_name]]) {
      # Handle both space-separated and underscore-separated formats
      space_name <- toupper(station)
      underscore_name <- toupper(gsub(" ", "_", station))
      station_zone_lookup[[space_name]] <- zone_name
      station_zone_lookup[[underscore_name]] <- zone_name
    }
  }

  for (table_name in names(wx_data)) {
    station_data <- wx_data[[table_name]]
    if (!is.null(station_data) && "Rn_1" %in% names(station_data)) {
      # Convert Rn_1 to numeric before summing
      station_data$Rn_1 <- as.numeric(as.character(station_data$Rn_1))
      
      # Clean station name and try both formats
      clean_station_name <- gsub("^dbo\\.", "", table_name)
      lookup_name_underscore <- toupper(clean_station_name)
      lookup_name_space <- toupper(gsub("_", " ", clean_station_name))
      
      # Find the zone using the lookup table, trying both formats
      station_fire_zone <- station_zone_lookup[[lookup_name_underscore]]
      if (is.null(station_fire_zone)) {
          station_fire_zone <- station_zone_lookup[[lookup_name_space]]
      }
      if (is.null(station_fire_zone)) {
          station_fire_zone <- "Unassigned Zone"
      }
      
      # Sum only if conversion was successful
      if (!all(is.na(station_data$Rn_1))) {
        total_rn1 <- sum(station_data$Rn_1, na.rm = TRUE)
        
        results <- rbind(results, data.frame(
          Fire_Zone = station_fire_zone,
          Station = clean_station_name,
          Total_Rn_1 = round(total_rn1, 1),
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  if (!is.null(station_select) && station_select != "All Stations") {
    results <- results[results$Station == station_select, ]
  }

  results <- results[order(results$Fire_Zone, -results$Total_Rn_1), ]

  if (nrow(results) == 0) {
    cat("No rainfall data available.")
    return(invisible(NULL))
  }

  current_zone <- ""
  max_station_width <- max(nchar(results$Station))
  line_width <- max_station_width + 20

  for (i in 1:nrow(results)) {
    if (results$Fire_Zone[i] != current_zone) {
      if (current_zone != "") cat("\n")
      current_zone <- results$Fire_Zone[i]
      cat(sprintf("\n%s\n%s\n", current_zone, strrep("═", nchar(current_zone))))
    }

    cat(sprintf(" %-*s │ %6.1f mm\n", 
                max_station_width,
                results$Station[i],
                results$Total_Rn_1[i]))

    if (i == nrow(results) || results$Fire_Zone[i] != results$Fire_Zone[i + 1]) {
      zone_total <- sum(results$Total_Rn_1[results$Fire_Zone == current_zone])
      cat(sprintf("%s\n", strrep("─", line_width)))
      cat(sprintf(" %-*s │ %6.1f mm\n", 
                  max_station_width,
                  "Zone Total",
                  zone_total))
    }
  }

  invisible(results)
}

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

# r/db_module/db_output.R

render_missing_entries_output <- function(missing_entries_data, selected_station) {
  missing_entries <- missing_entries_data()
  
  if (!is.data.frame(missing_entries) || nrow(missing_entries) == 0) {
    cat("No missing entries detected.\n")
    return(invisible())
  }
  
  missing_entries <- missing_entries %>% 
    filter(!is.na(MissingDateTime)) %>%
    arrange(Table, desc(MissingDateTime))
  
  if (!is.null(selected_station) && selected_station != "All Stations") {
    missing_entries <- missing_entries %>% filter(Table == selected_station)
  }
  
  if (nrow(missing_entries) == 0) {
    cat("No missing entries detected for the selected station(s).\n")
    return(invisible())
  }
  
  # Calculate summary statistics
  station_summary <- missing_entries %>%
    group_by(Table) %>%
    summarise(
      total_missing = n(),
      earliest_missing = min(MissingDateTime),
      latest_missing = max(MissingDateTime)
    )
  
  for (station in unique(missing_entries$Table)) {
    station_stats <- station_summary %>% filter(Table == station)
    cleaned_station_name <- gsub("^dbo\\.", "", station)
    
    cat(cleaned_station_name, "\n", rep("-", nchar(cleaned_station_name)), "\n", sep = "")
    cat("Total missing hourly entries:", station_stats$total_missing, "\n")
    cat("Time span:", format(station_stats$earliest_missing, "%Y-%m-%d %H:00"),
        "to", format(station_stats$latest_missing, "%Y-%m-%d %H:00"), "\n\n")
  }
  
  return(invisible())
}

#' Render the report status output table
#' @param last_entry_time_check Reactive data frame with station status information
render_report_status_output <- function(last_entry_time_check) {
  datatable(
    last_entry_time_check,
    options = list(
      pageLength = 10,  
      order = list(list(1, 'asc')),
      columnDefs = list(
        list(width = "20%", targets = c(0, 1, 2, 3))
      ),
      dom = '<"top"f>rt<"bottom"ip>',  # Customize the DOM positioning
      language = list(
        search = "Filter stations:"  # Customize search placeholder
      )
    ),
    rownames = FALSE,
    class = 'cell-border stripe hover'  # Add additional classes for better styling
  ) %>%
    formatStyle(
      'Status',
      backgroundColor = styleEqual(
        c("Up to date", "Slightly delayed", "Delayed", "Significantly delayed", "No data", "Historical"),
        c('rgba(67, 160, 71, 0.15)',    # Soft green
          'rgba(251, 192, 45, 0.15)',    # Warm yellow
          'rgba(251, 140, 0, 0.15)',     # Soft orange
          'rgba(229, 57, 53, 0.15)',     # Muted red
          'rgba(117, 117, 117, 0.12)',   # Neutral grey
          'rgba(66, 165, 245, 0.15)')    # Pleasant blue
      ),
      color = styleEqual(
        c("Up to date", "Slightly delayed", "Delayed", "Significantly delayed", "No data", "Historical"),
        c('rgb(46, 125, 50)',     # Darker green text
          'rgb(198, 140, 0)',     # Darker yellow text
          'rgb(198, 85, 0)',      # Darker orange text
          'rgb(183, 28, 28)',     # Darker red text
          'rgb(66, 66, 66)',      # Darker grey text
          'rgb(25, 118, 210)')    # Darker blue text
      ),
      fontWeight = styleEqual(
        c("Up to date", "Slightly delayed", "Delayed", "Significantly delayed", "No data", "Historical"),
        rep('500', 6)
      )
    )
}

render_space_weather_table <- function(space_weather_data, input) {
  req(space_weather_data)
  
  # Get the earliest available alert date
  earliest_alert_date <- min(as.Date(space_weather_data$issue_datetime))
  
  # Check if requested dates are before the earliest available alert
  is_data_available <- if (input$fetch_type == "last_n") {
    TRUE  # Always available for last n hours
  } else if (input$fetch_type == "by_date") {
    as.Date(input$select_date) >= earliest_alert_date
  } else if (input$fetch_type == "date_range") {
    as.Date(input$end_date) >= earliest_alert_date
  } else {
    TRUE
  }
  
  # If data isn't available for the time period, return appropriate message
  if (!is_data_available) {
    return(datatable(
      data.frame(
        Message = sprintf(
          "Space weather alerts are only available from %s onwards. Historical data is not available through the NOAA SWPC API.", 
          format(earliest_alert_date, "%B %d, %Y")
        ),
        DateTime = ""
      ),
      options = list(
        dom = 't',  # Only show table, no pagination or search
        ordering = FALSE
      ),
      rownames = FALSE,
      colnames = c("Alert Details", "Issue Time")
    ))
  }
  
  # Filter data based on user's fetch type
  filtered_data <- if (input$fetch_type == "last_n") {
    cutoff_time <- Sys.time() - hours(input$num_entries)
    space_weather_data[space_weather_data$issue_datetime >= cutoff_time, ]
    
  } else if (input$fetch_type == "by_date") {
    selected_date <- as.Date(input$select_date)
    space_weather_data[as.Date(space_weather_data$issue_datetime) == selected_date, ]
    
  } else if (input$fetch_type == "date_range") {
    start_date <- as.Date(input$start_date)
    end_date <- as.Date(input$end_date)
    space_weather_data[as.Date(space_weather_data$issue_datetime) >= start_date & 
                      as.Date(space_weather_data$issue_datetime) <= end_date, ]
  } else {
    space_weather_data
  }
  
  # If no alerts in the time period (but data is available), return no alerts message
  if (nrow(filtered_data) == 0) {
    return(datatable(
      data.frame(
        Message = "No space weather alerts during the selected time period",
        DateTime = ""
      ),
      options = list(
        dom = 't',  # Only show table, no pagination or search
        ordering = FALSE
      ),
      rownames = FALSE,
      colnames = c("Alert Details", "Issue Time")
    ))
  }
  
  # Function to format the message in HTML
  format_message <- function(message) {
    lines <- strsplit(message, "\n")[[1]]
    html_output <- '<div style="font-family: monospace; white-space: pre-wrap; padding: 1rem; background-color: white; border: 1px solid #e1e4e8; border-radius: 4px; margin-bottom: 1rem;">'
    
    for (line in lines) {
      if (grepl(":", line) && !grepl("www\\.", line)) {
        parts <- strsplit(line, ":", 2)[[1]]
        if (length(parts) == 2) {
          header <- trimws(parts[1])
          content <- trimws(parts[2])
          line <- sprintf('<div><strong>%s:</strong> %s</div>', header, content)
        }
      } else {
        line <- sprintf('<div>%s</div>', line)
      }
      html_output <- paste0(html_output, line)
    }
    
    html_output <- paste0(html_output, '</div>')
    return(html_output)
  }
  
  # Create display data frame with formatted messages
  display_data <- data.frame(
    Message = sapply(filtered_data$message, format_message),
    DateTime = format(filtered_data$issue_datetime, "%Y-%m-%d %H:%M:%S %Z")
  )
  
  # Create DataTable with formatted HTML
  datatable(
    display_data,
    options = list(
      pageLength = 2,
      scrollX = TRUE,
      order = list(list(1, 'desc')),  # Order by datetime descending
      language = list(
        zeroRecords = "No space weather alerts during the selected time period"
      )
    ),
    escape = FALSE,  # Allow HTML rendering
    rownames = FALSE,
    colnames = c("Alert Details", "Issue Time")
  )
}

#' Render recent entries output
#' @param WX_stations List of weather station data
#' Render recent entries output
#' @param WX_stations List of weather station data
render_recent_entries <- function(WX_stations) {
  # If no stations data is provided, return a message
  if (length(WX_stations) == 0) {
    return(datatable(
      data.frame(Message = "No data available"),
      options = list(dom = 't'),
      rownames = FALSE
    ))
  }
  
  # Initialize empty list to store processed data frames
  all_data <- list()
  
  # Process each station's data
  for (table in names(WX_stations)) {
    data <- WX_stations[[table]]
    
    # Skip if data is NULL or has 0 rows
    if (is.null(data) || nrow(data) == 0) {
      next
    }
    
    # Select only the relevant columns that exist in the data
    columns_to_include <- c("DateTimeNum", "Temp", "Rh", "Rn_1", "Dir", "Wspd")
    columns_existing <- intersect(columns_to_include, names(data))
    
    if (length(columns_existing) > 0) {
      # Create a subset of the data with only existing columns
      station_data <- data[, columns_existing, drop = FALSE]
      
      # Add station name column
      station_data$Station <- sub("^dbo\\.", "", table)
      
      # Ensure all numeric columns are properly converted
      numeric_cols <- c("Temp", "Rh", "Rn_1", "Dir", "Wspd")
      for (col in intersect(numeric_cols, names(station_data))) {
        station_data[[col]] <- as.numeric(station_data[[col]])
      }
      
      # Reorder columns to put Station first
      col_order <- c("Station", columns_existing)
      station_data <- station_data[, col_order]
      
      # Add to list only if we have data
      if (nrow(station_data) > 0) {
        all_data[[table]] <- station_data
      }
    }
  }
  
  # If no valid data was collected, return message
  if (length(all_data) == 0) {
    return(datatable(
      data.frame(Message = "No valid data available for display"),
      options = list(dom = 't'),
      rownames = FALSE
    ))
  }
  
  # Combine all data frames
  combined_data <- do.call(rbind, all_data)
  
  # Format the datetime column
  combined_data$DateTimeNum <- format(
    as.POSIXct(combined_data$DateTimeNum, format = "%Y-%b-%d %H:%M:%S", tz = "UTC"),
    "%Y-%m-%d %H:%M"
  )
  
  # Create the DataTable
  datatable(
    combined_data,
    options = list(
      pageLength = 20,
      order = list(list(0, 'asc'), list(1, 'desc')),
      scrollX = TRUE,
      dom = '<"top"f>rt<"bottom"ip>',
      columnDefs = list(
        list(
          targets = c("Temp", "Rh", "Rn_1", "Dir", "Wspd"),
          render = JS("function(data, type, row) {
            if (type === 'display') {
              return data === null ? '-' : parseFloat(data).toFixed(1);
            }
            return data;
          }")
        ),
        list(
          targets = '_all',
          className = 'dt-nowrap'
        )
      )
    ),
    rownames = FALSE,
    colnames = c(
      "Station", "DateTimeNum", "Temp", "Rh", "Rn_1",
      "Dir", "Wspd"
    ),
    class = 'cell-border stripe compact'
  ) %>%
    formatStyle(
      'Station',
      backgroundColor = styleEqual(
        unique(combined_data$Station),
        colorRampPalette(c("#f7fbff", "#deebf7"))(length(unique(combined_data$Station)))
      )
    ) %>%
    formatRound(
      columns = c("Temp", "Rh", "Rn_1", "Dir", "Wspd"),
      digits = 1
    )
}

#' Render check for blanks output
#' @param WX_stations List of weather station data
# Function to render blank check output
#' Render check for blanks output
#' @param WX_stations List of weather station data
render_check_for_blanks <- function(WX_stations) {
  # Get the blank check results
  blank_results <- check_for_blanks(WX_stations)
  
  if (is.data.frame(blank_results)) {
    # Calculate summaries
    total_issues <- nrow(blank_results)
    issues_by_station <- table(blank_results$Table)
    issues_by_type <- table(blank_results$Issue)
    issues_by_column <- table(blank_results$Column)
    
    # Print overall summary
    cat("BLANK VALUE SUMMARY\n")
    cat(paste(rep("-", 50), collapse = ""), "\n")
    cat("Total issues found:", total_issues, "\n\n")
    
    # Print issues by station
    cat("Issues by Station:\n")
    cat(paste(rep("-", 20), collapse = ""), "\n")
    for(station in names(issues_by_station)) {
      clean_station <- gsub("^dbo\\.", "", station)
      cat(sprintf("%-30s: %d\n", clean_station, issues_by_station[station]))
    }
    cat("\n")
    
    # Print issues by type
    cat("Issues by Type:\n")
    cat(paste(rep("-", 20), collapse = ""), "\n")
    for(issue in names(issues_by_type)) {
      cat(sprintf("%-30s: %d\n", issue, issues_by_type[issue]))
    }
    cat("\n")
    
    # Print issues by column
    cat("Issues by Column:\n")
    cat(paste(rep("-", 20), collapse = ""), "\n")
    for(column in names(issues_by_column)) {
      cat(sprintf("%-30s: %d\n", column, issues_by_column[column]))
    }
    cat("\n")
    
    # Print detailed results
    cat("DETAILED RESULTS\n")
    cat(paste(rep("-", 50), collapse = ""), "\n")
    
    # Clean up station names
    blank_results$Table <- gsub("^dbo\\.", "", blank_results$Table)
    
    # Process the DateTime field more carefully
    blank_results$parsed_date <- as.POSIXct(blank_results$DateTime, 
                                          format="%Y-%b-%d %H:%M:%S", 
                                          tz="UTC")
    
    # Sort by Table, DateTime, Column
    blank_results <- blank_results[order(blank_results$Table, 
                                       blank_results$parsed_date, 
                                       blank_results$Column), ]
    
    # Process each station
    current_station <- NULL
    for(station in unique(blank_results$Table)) {
      station_data <- blank_results[blank_results$Table == station, ]
      
      cat("\n\n", station, "\n")
      cat(paste(rep("-", nchar(station)), collapse = ""), "\n")
      
      # Group by Column and Issue
      for(col in unique(station_data$Column)) {
        for(issue in unique(station_data[station_data$Column == col, "Issue"])) {
          subset_data <- station_data[station_data$Column == col & 
                                    station_data$Issue == issue, ]
          
          if(nrow(subset_data) > 24) {
            # Print condensed format for large number of entries
            cat(sprintf("From %s to %s: %d %s entries for %s\n",
                       format(min(subset_data$parsed_date), "%Y-%b-%d"),
                       format(max(subset_data$parsed_date), "%Y-%b-%d"),
                       nrow(subset_data),
                       issue,
                       col))
            
            # Print first and last entries without label
            cat(sprintf("%-20s | %-15s | %-20s | %s\n",
                       subset_data$DateTime[1],
                       col,
                       issue,
                       ifelse(is.na(subset_data$Value[1]), "NA", 
                             as.character(subset_data$Value[1]))))
            cat(sprintf("%-20s | %-15s | %-20s | %s\n\n",
                       subset_data$DateTime[nrow(subset_data)],
                       col,
                       issue,
                       ifelse(is.na(subset_data$Value[nrow(subset_data)]), "NA", 
                             as.character(subset_data$Value[nrow(subset_data)]))))
          } else {
            # Print all entries for small number of issues
            for(i in 1:nrow(subset_data)) {
              cat(sprintf("%-20s | %-15s | %-20s | %s\n",
                         subset_data$DateTime[i],
                         col,
                         issue,
                         ifelse(is.na(subset_data$Value[i]), "NA", 
                               as.character(subset_data$Value[i]))))
            }
            cat("\n")
          }
        }
      }
    }
  } else {
    # If no issues found, print the message
    cat("\n", blank_results, "\n")
  }
}

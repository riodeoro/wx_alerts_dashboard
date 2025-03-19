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

# app.R

# ---------- Library Imports ----------
library(shiny)
library(dplyr)
library(lubridate)
library(plotly)
library(bslib)
library(jsonlite)
library(DT)
library(zoo)
library(viridis)
library(ggplot2)
library(httr)
library(xml2)
library(future)
library(furrr)
library(purrr)
library(tidyr)
library(stringdist) # Added for metadata update functions
library(readr)      # For read_csv

# ---------- API Configuration ----------
BASE_URL <- "el-086-api.elements360.aem.eco/aem/DataAPI"
SYSTEM_KEY <- "6af158e4-53d9-4747-ad67-71197f689e1f"

# ---------- Generic Caching Helper ----------
# Function to manage cached data with expiration
is_cache_valid <- function(cache_timestamp, max_age_minutes = 30) {
  if (is.null(cache_timestamp)) return(FALSE)
  
  current_time <- Sys.time()
  if (difftime(current_time, cache_timestamp, units = "mins") < max_age_minutes) {
    return(TRUE)
  }
  return(FALSE)
}

# ---------- Update metadata from API (in-memory version) ----------
update_metadata_from_api <- function(api_url,
                                   original_metadata = NULL,
                                   coord_tolerance = 1,
                                   name_similarity_threshold = 0.8) {
  
  tryCatch({
    # Fetch XML from API
    message("Fetching data from API...")
    response <- GET(api_url)
    
    if (status_code(response) != 200) {
      stop(sprintf("API request failed with status code: %d", status_code(response)))
    }
    
    xml_string <- rawToChar(response$content)
    
    # Parse XML string directly into a dataframe
    xml_doc <- read_xml(xml_string)
    rows <- xml_find_all(xml_doc, "//row")
    
    message(sprintf("Retrieved %d records from API", length(rows)))
    
    # Convert XML to dataframe directly
    site_data <- map_dfr(rows, function(row) {
      elements <- xml_children(row)
      setNames(
        as.list(xml_text(elements)),
        xml_name(elements)
      )
    }) %>%
      # Select only the columns we need for matching and updating
      select(location, latitude_dec, longitude_dec, or_site_id, site_id, client_id, system_id)
    
    # Use provided metadata or create minimal structure if NULL
    metadata <- original_metadata
    if (is.null(metadata)) {
      metadata <- data.frame(
        STATION = character(),
        LATITUDE = numeric(),
        LONGITUDE = numeric(),
        FIRE_CENTRE = character(),
        FIRE_ZONE = character(),
        ELEVATION = numeric(),
        stringsAsFactors = FALSE
      )
    }
    
    # Standardize station names for comparison
    standardize_name <- function(name) {
      name %>%
        toupper() %>%
        gsub("[[:punct:]]", "", .) %>%
        gsub("\\s+", "", .) %>%
        gsub("STATION", "", .) %>%
        gsub("STN", "", .)
    }
    
    # Create temporary working copies with standardized names
    site_data_temp <- site_data %>%
      mutate(
        latitude_dec = as.numeric(latitude_dec),
        longitude_dec = as.numeric(longitude_dec),
        location_std = standardize_name(location)
      )
    
    metadata_temp <- metadata %>%
      mutate(
        LATITUDE = as.numeric(LATITUDE),
        LONGITUDE = as.numeric(LONGITUDE),
        STATION_std = standardize_name(STATION)
      )
    
    # Calculate name similarity
    calculate_name_similarity <- function(name1, name2) {
      if (is.na(name1) || is.na(name2)) return(0)
      exact_match <- as.numeric(name1 == name2)
      levenshtein_sim <- 1 - stringdist(name1, name2, method = "lv") / max(nchar(name1), nchar(name2))
      return(max(exact_match, levenshtein_sim))
    }
    
    # Find matches using both coordinates and names
    find_matches <- function(lat1, lon1, station_name, site_data_temp, coord_tolerance, name_threshold) {
      coord_matches <- site_data_temp[abs(site_data_temp$latitude_dec - lat1) <= coord_tolerance & 
                                    abs(site_data_temp$longitude_dec - lon1) <= coord_tolerance, ]
      
      if (nrow(coord_matches) == 0) return(NULL)
      
      coord_matches$name_similarity <- sapply(coord_matches$location_std, 
                                            function(x) calculate_name_similarity(x, station_name))
      
      # Find best match regardless of threshold for reporting
      best_match <- coord_matches[which.max(coord_matches$name_similarity), ]
      best_match$meets_threshold <- best_match$name_similarity >= name_threshold
      
      return(best_match)
    }
    
    # Track matching details for reporting
    match_details <- data.frame(
      row = integer(),
      coord_match = logical(),
      name_match = logical(),
      original_station = character(),
      matched_location = character(),
      similarity_score = numeric(),
      was_updated = logical()
    )
    
    # Add new columns to metadata if they don't exist
    if (!"or_site_id" %in% names(metadata)) metadata$or_site_id <- NA
    if (!"site_id" %in% names(metadata)) metadata$site_id <- NA
    if (!"client_id" %in% names(metadata)) metadata$client_id <- NA
    if (!"system_id" %in% names(metadata)) metadata$system_id <- NA
    
    # Apply matching
    for (i in 1:nrow(metadata_temp)) {
      match <- find_matches(metadata_temp$LATITUDE[i], 
                          metadata_temp$LONGITUDE[i],
                          metadata_temp$STATION_std[i],
                          site_data_temp, 
                          coord_tolerance,
                          name_similarity_threshold)
      
      if (!is.null(match)) {
        was_updated <- FALSE
        if (match$meets_threshold) {
          metadata$or_site_id[i] <- match$or_site_id
          metadata$site_id[i] <- match$site_id
          metadata$client_id[i] <- match$client_id
          metadata$system_id[i] <- match$system_id
          was_updated <- TRUE
        }
        
        match_details <- rbind(match_details, data.frame(
          row = i,
          coord_match = TRUE,
          name_match = match$name_similarity >= name_similarity_threshold,
          original_station = metadata$STATION[i],
          matched_location = match$location,
          similarity_score = match$name_similarity,
          was_updated = was_updated
        ))
      }
    }
    
    # Generate report summary info
    total_records <- nrow(metadata)
    total_matches <- sum(match_details$was_updated)
    low_similarity_matches <- sum(match_details$coord_match & !match_details$name_match)
    
    # Create a report attribute to attach to the result
    attr(metadata, "update_report") <- list(
      total_records = total_records,
      total_matches = total_matches,
      low_similarity_matches = low_similarity_matches,
      match_details = match_details,
      coord_tolerance = coord_tolerance,
      name_similarity_threshold = name_similarity_threshold
    )
    
    message(sprintf("Update complete:
    Total records processed: %d
    Records updated: %d
    Coordinate matches found: %d
    - Strong matches (updated): %d
    - Low similarity matches (not updated): %d
    - Coordinate tolerance: %f degrees
    - Name similarity threshold: %f", 
    total_records, total_matches, nrow(match_details), total_matches, 
    low_similarity_matches, coord_tolerance, name_similarity_threshold))
    
    if (low_similarity_matches > 0) {
      message("\nCoordinate matches with low name similarity (these were NOT updated):")
      low_similarity_matches <- match_details[!match_details$name_match, ]
      print(low_similarity_matches[, c("original_station", "matched_location", "similarity_score")])
    }
    
    # Return the updated metadata
    return(metadata)
    
  }, error = function(e) {
    message(sprintf("Error during processing: %s", e$message))
    return(NULL)
  })
}

# ---------- Function to fetch data with caching ----------
fetch_with_cache <- function(fetch_function, cache_container, cache_key, timestamp_key, force_refresh = FALSE, max_age_minutes = 30, ...) {
  # Check if we have valid cached data
  if (!force_refresh && 
      !is.null(cache_container[[cache_key]]) && 
      is_cache_valid(cache_container[[timestamp_key]], max_age_minutes)) {
    
    message(sprintf("Using cached %s (last updated %s)", 
                   cache_key, 
                   format(cache_container[[timestamp_key]], "%H:%M:%S")))
    
    return(cache_container[[cache_key]])
  }
  
  # No valid cache, fetch fresh data
  result <- fetch_function(...)
  
  if (!is.null(result)) {
    # Update cache
    cache_container[[cache_key]] <- result
    cache_container[[timestamp_key]] <- Sys.time()
  }
  
  return(result)
}

# ---------- Function to fetch raw metadata from API ----------
fetch_metadata_from_api <- function() {
  # Define the API endpoint
  api_url <- "el-086-api.elements360.aem.eco/aem/DataAPI?method=GetSiteMetaData&system_key=6af158e4-53d9-4747-ad67-71197f689e1f&format=xml"
  
  tryCatch({
    # Fetch data directly from API
    message("Fetching station metadata from Elements 360 API...")
    response <- GET(api_url)
    
    if (status_code(response) != 200) {
      stop(sprintf("API request failed with status code: %d", status_code(response)))
    }
    
    xml_string <- rawToChar(response$content)
    
    # Parse XML string directly into a dataframe
    xml_doc <- read_xml(xml_string)
    rows <- xml_find_all(xml_doc, "//row")
    
    message(sprintf("Retrieved %d records from API", length(rows)))
    
    # Convert XML to dataframe
    site_data <- map_dfr(rows, function(row) {
      elements <- xml_children(row)
      setNames(
        as.list(xml_text(elements)),
        xml_name(elements)
      )
    })
    
    # Create a properly structured metadata dataframe
    metadata <- site_data %>%
      select(
        site_id,
        location,
        latitude_dec,
        longitude_dec,
        or_site_id,
        client_id,
        system_id
      ) %>%
      mutate(
        STATION = location,
        LATITUDE = as.numeric(latitude_dec),
        LONGITUDE = as.numeric(longitude_dec),
        # Assign default values for required fields
        FIRE_CENTRE = "Unknown",
        FIRE_ZONE = "Unknown",
        ELEVATION = as.numeric(NA)
      )
    
    # Return complete metadata
    return(metadata)
    
  }, error = function(e) {
    message(sprintf("Error fetching metadata: %s", e$message))
    return(NULL)
  })
}

# ---------- Load Static STATION_METADATA from included file ----------
# This function loads the embedded metadata file that ships with the app
load_static_metadata <- function() {
  tryCatch({
    # Try to read from a static file included with the app
    static_metadata_path <- "static_data/STATION_METADATA.csv"
    if (file.exists(static_metadata_path)) {
      metadata <- readr::read_csv(static_metadata_path, show_col_types = FALSE)
      message("Loaded static metadata from embedded file")
      return(metadata)
    } else {
      message("Static metadata file not found, will use API data only")
      return(NULL)
    }
  }, error = function(e) {
    message(sprintf("Error loading static metadata: %s", e$message))
    return(NULL)
  })
}

# ---------- Merge metadata sources with sophisticated matching ----------
merge_metadata_sources <- function(static_metadata, api_metadata) {
  if (is.null(static_metadata) && is.null(api_metadata)) {
    message("No metadata sources available!")
    return(NULL)
  }
  
  if (is.null(static_metadata)) {
    message("Using API metadata only")
    return(api_metadata)
  }
  
  if (is.null(api_metadata)) {
    message("Using static metadata only")
    return(static_metadata)
  }
  
  # We have both sources - use the sophisticated matching algorithm
  # CSV is the "gold standard" - we only want to update site IDs
  message("Merging metadata sources using coordinate and name matching...")
  
  # Use update_metadata_from_api with the actual API URL
  # This preserves the station names and metadata from static_metadata
  # but adds the API site IDs where coordinates match
  api_url <- "el-086-api.elements360.aem.eco/aem/DataAPI?method=GetSiteMetaData&system_key=6af158e4-53d9-4747-ad67-71197f689e1f&format=xml"
  
  merged_metadata <- update_metadata_from_api(
    api_url = api_url,
    original_metadata = static_metadata,
    coord_tolerance = 1,
    name_similarity_threshold = 0.8
  )
  
  if (is.null(merged_metadata)) {
    message("Metadata merge failed, falling back to static metadata")
    return(static_metadata)
  }
  
  # Important: We're considering CSV as the gold standard
  # Only using API to add site_id, not adding new stations from API
  # This ensures dropdown menus only show stations from the CSV
  message("Using CSV as the gold standard - not adding API-only stations")
  
  return(merged_metadata)
}

observe_fire_centers <- function(session, station_metadata) {
  # Extract unique fire centers from the station metadata
  req(station_metadata())
  
  fire_centers <- unique(station_metadata()$FIRE_CENTRE)
  fire_centers <- sort(fire_centers[!is.na(fire_centers)])
  
  # Update the fire centre select input
  updateSelectInput(session, "fire_centre", 
                   choices = fire_centers,
                   selected = fire_centers[1])
}

# ---------- Source Files ----------
# Core files
source("ui_definition2.R")

addResourcePath("www", "www")

# Module function files
source("r/temperature_module/temperature_functions.R")
source("r/rh_module/rh_functions.R")
source("r/wind_module/wind_functions.R")
source("r/power_module/power_functions.R")
source("r/crmp_module/crmp_functions.R")
source("r/db_module/db_functions.R")
source("r/precipitation_module/precipitation_functions.R")

# Module chart files
source("r/wind_module/wind_charts.R")
source("r/temperature_module/temperature_charts.R")
source("r/rh_module/rh_charts.R")
source("r/crmp_module/crmp_charts.R")
source("r/precipitation_module/precipitation_charts.R")
source("r/db_module/db_charts.R")
source("r/power_module/power_charts.R")

# Module output files
source("r/db_module/db_output.R")
source("r/wind_module/wind_output.R")
source("r/precipitation_module/precipitation_output.R")
source("r/temperature_module/temperature_output.R")
source("r/crmp_module/crmp_output.R")
source("r/rh_module/rh_output.R")
source("r/power_module/power_output.R")

# ---------- API Data Fetching Functions ----------

#' Fetch sensor metadata from the API with caching
#' 
#' @param force_refresh Boolean indicating whether to force a refresh
#' @return data.frame containing sensor metadata or NULL if error
fetch_sensor_metadata <- function(force_refresh = FALSE) {
  url <- paste0(
    BASE_URL,
    "?method=GetSensorMetaData",
    "&system_key=", SYSTEM_KEY,
    "&format=xml"
  )
  
  tryCatch({
    response <- GET(url)
    if (http_status(response)$category == "Success") {
      parsed_xml <- read_xml(rawToChar(response$content))
      rows <- xml_find_all(parsed_xml, "//row")
      
      # Pre-allocate vectors for better performance
      n_rows <- length(rows)
      site_ids <- character(n_rows)
      sensor_ids <- character(n_rows)
      descriptions <- character(n_rows)
      units <- character(n_rows)
      active_flags <- logical(n_rows)
      
      # Fill vectors in a single pass
      for (i in seq_len(n_rows)) {
        row <- rows[[i]]
        site_ids[i] <- xml_text(xml_find_first(row, ".//site_id"))
        sensor_ids[i] <- xml_text(xml_find_first(row, ".//sensor_id"))
        descriptions[i] <- xml_text(xml_find_first(row, ".//description"))
        units[i] <- xml_text(xml_find_first(row, ".//units"))
        active_flags[i] <- as.logical(as.numeric(xml_text(xml_find_first(row, ".//active"))))
      }
      
      # Create data frame in one operation
      sensor_data <- data.frame(
        site_id = site_ids,
        sensor_id = sensor_ids,
        description = descriptions,
        units = units,
        active = active_flags,
        stringsAsFactors = FALSE
      )
      
      return(sensor_data)
    }
    return(NULL)
  }, error = function(e) {
    warning(paste("Error fetching sensor metadata:", e$message))
    return(NULL)
  })
}

#' Construct batch API URL for data retrieval
#' 
#' @param start_time Start time in ISO format
#' @param end_time End time in ISO format
#' @return Character string containing the API URL
construct_batch_api_url <- function(start_time, end_time) {
  paste0(
    BASE_URL,
    "?method=GetSensorData",
    "&system_key=", SYSTEM_KEY,
    "&data_start=", start_time,
    "&data_end=", end_time,
    "&timezone=PST"
  )
}

#' Generate time chunks for parallel processing
#' 
#' @param start_time Start time as datetime
#' @param end_time End time as datetime
#' @return List of time chunks
generate_time_chunks <- function(start_time, end_time) {
  # Convert inputs to POSIXct if they aren't already
  start_time <- as.POSIXct(start_time)
  end_time <- as.POSIXct(end_time)
  
  # Generate sequence of dates at once
  dates <- seq(start_time, end_time, by = "24 hours")
  
  # Add end time if it's not included
  if (end_time > dates[length(dates)]) {
    dates <- c(dates, end_time)
  }
  
  # Format all dates at once using vectorized operations
  dates_formatted <- format(dates, "%Y-%m-%d%%20%H:%M")
  
  # Create chunks with proper formatting - pre-allocate list
  n_chunks <- length(dates) - 1
  chunks <- vector("list", n_chunks)
  
  for (i in seq_len(n_chunks)) {
    chunks[[i]] <- list(
      start = dates_formatted[i],
      end = dates_formatted[i + 1]
    )
  }
  
  return(chunks)
}

#' Parse weather data from XML response with optimized type conversions
#' 
#' @param xml_content XML content from API response
#' @return data.frame containing parsed weather data or NULL if error
parse_weather_data <- function(xml_content) {
  if (is.null(xml_content) || nchar(xml_content) == 0) {
    warning("Empty or null XML content")
    return(NULL)
  }
  
  tryCatch({
    parsed_xml <- read_xml(xml_content)
    rows <- xml_find_all(parsed_xml, "//row")
    
    if (length(rows) == 0) {
      warning("No rows found in XML")
      return(NULL)
    }
    
    # Extract all values at once using xpath and vectorized operations
    site_ids <- xml_text(xml_find_all(parsed_xml, "//row/site_id"))
    sensor_ids <- xml_text(xml_find_all(parsed_xml, "//row/sensor_id"))
    receive_times <- xml_text(xml_find_all(parsed_xml, "//row/receive_time"))
    data_times <- xml_text(xml_find_all(parsed_xml, "//row/data_time"))
    data_values_str <- xml_text(xml_find_all(parsed_xml, "//row/data_value"))
    data_qualities <- xml_text(xml_find_all(parsed_xml, "//row/data_quality"))
    units <- xml_text(xml_find_all(parsed_xml, "//row/units"))
    
    # Convert data values to numeric in one operation
    data_values <- suppressWarnings(as.numeric(data_values_str))
    
    # Find valid rows (where data_value is a number)
    valid_rows <- !is.na(data_values)
    
    if (!any(valid_rows)) {
      warning("No valid numeric values found")
      return(NULL)
    }
    
    # Create data frame with only valid rows
    weather_data <- data.frame(
      site_id = site_ids[valid_rows],
      sensor_id = sensor_ids[valid_rows],
      receive_time = receive_times[valid_rows],
      data_time = data_times[valid_rows],
      data_value = data_values[valid_rows],
      data_quality = data_qualities[valid_rows],
      units = units[valid_rows],
      stringsAsFactors = FALSE
    )
    
    # Convert datetime columns only once
    # Use lubridate for faster parsing
    weather_data$receive_time <- ymd_hms(weather_data$receive_time)
    weather_data$data_time <- ymd_hms(weather_data$data_time)
    
    return(weather_data)
  }, error = function(e) {
    warning(sprintf("Error in parse_weather_data: %s", e$message))
    return(NULL)
  })
}

#' Fetch data for multiple stations in parallel with optimized processing
#' 
#' @param time_chunks List of time chunk pairs (start and end times)
#' @return data.frame containing combined weather data or NULL if error
fetch_batch_chunk_data_parallel <- function(time_chunks) {
  # Set up parallel processing
  plan(multisession, workers = min(length(time_chunks), 12))
  
  # Define safe function for fetching
  safe_fetch <- safely(function(chunk) {
    tryCatch({
      url <- construct_batch_api_url(chunk$start, chunk$end)
      response <- GET(url)
      
      if (http_status(response)$category == "Success") {
        content <- rawToChar(response$content)
        if (nchar(content) == 0) {
          warning("Empty response content")
          return(NULL)
        }
        
        parsed_data <- parse_weather_data(content)
        return(parsed_data)
      } else {
        warning(sprintf("HTTP request failed: %s", http_status(response)$message))
        return(NULL)
      }
    }, error = function(e) {
      warning(sprintf("Error in chunk processing: %s", e$message))
      return(NULL)
    })
  })
  
  # Process chunks in parallel
  results <- future_map(time_chunks, safe_fetch, .progress = FALSE)
  
  # Extract valid results
  valid_results <- list()
  for (i in seq_along(results)) {
    result <- results[[i]]
    if (!is.null(result$result) && is.data.frame(result$result) && nrow(result$result) > 0) {
      valid_results[[length(valid_results) + 1]] <- result$result
    }
  }
  
  if (length(valid_results) == 0) {
    warning("No valid results found")
    return(NULL)
  }
  
  # Use dplyr::bind_rows for more efficient binding
  tryCatch({
    combined_data <- bind_rows(valid_results)
    return(combined_data)
  }, error = function(e) {
    warning(sprintf("Error combining results: %s", e$message))
    return(NULL)
  })
}

#' Fetch weather data using API with optimized data conversion
#'
#' @param fire_centre Fire centre to fetch data for
#' @param stations List of stations to fetch
#' @param time_range List containing start and end times
#' @return List of data frames with station data
fetch_weather_data_api <- function(fire_centre, stations, time_range) {
  # Generate time chunks for API calls
  time_chunks <- generate_time_chunks(time_range$start, time_range$end)
  
  # Fetch data in parallel
  all_data <- fetch_batch_chunk_data_parallel(time_chunks)
  
  if (is.null(all_data) || nrow(all_data) == 0) {
    warning("No data retrieved from API")
    return(list())
  }
  
  # Filter for requested stations - use semi_join for efficiency
  stations_subset <- stations %>% select(site_id)
  data_filtered <- all_data %>% 
    semi_join(stations_subset, by = "site_id")
  
  if (nrow(data_filtered) == 0) {
    warning("No data available for the selected stations")
    return(list())
  }
  
  # Join with station metadata - use left_join once
  relevant_station_cols <- stations %>%
    select(site_id, STATION, FIRE_CENTRE, FIRE_ZONE, LATITUDE, LONGITUDE, ELEVATION)
  
  data_with_station <- data_filtered %>%
    left_join(relevant_station_cols, by = "site_id")
  
  # Add DateTimeNum column as formatted string (required format for app compatibility)
  data_with_datetime <- data_with_station %>%
    mutate(DateTimeNum = format(data_time, "%Y-%b-%d %H:%M:%S"))
  
  # Handle duplicates more efficiently by aggregating before pivot
  data_aggregated <- data_with_datetime %>%
    group_by(site_id, STATION, FIRE_CENTRE, FIRE_ZONE, LATITUDE, LONGITUDE, 
            ELEVATION, DateTimeNum, sensor_id) %>%
    summarize(data_value = mean(data_value, na.rm = TRUE), .groups = "drop")
  
  # Now pivot without needing the aggregation function
  transformed_data <- data_aggregated %>%
    pivot_wider(
      id_cols = c(site_id, STATION, FIRE_CENTRE, FIRE_ZONE, LATITUDE, LONGITUDE, 
                 ELEVATION, DateTimeNum),
      names_from = sensor_id,
      values_from = data_value
    ) %>%
    arrange(desc(DateTimeNum)) %>%
    group_by(STATION) %>%
    group_split()
  
  # Convert list to named list and ensure numeric columns
  if (length(transformed_data) > 0) {
    # Get column types once
    numeric_cols <- setdiff(names(transformed_data[[1]]), 
                           c("site_id", "STATION", "FIRE_CENTRE", "FIRE_ZONE", "DateTimeNum"))
    
    # Convert each dataset with vectorized operations
    transformed_data <- lapply(transformed_data, function(df) {
      # Convert all numeric columns at once using across()
      df <- df %>%
        mutate(across(all_of(numeric_cols), as.numeric))
      
      # Convert to data.frame for compatibility
      as.data.frame(df, stringsAsFactors = FALSE)
    })
    
    # Name the list entries without the dbo. prefix
    station_names <- map_chr(transformed_data, ~unique(.x$STATION))
    names(transformed_data) <- station_names
    
    return(transformed_data)
  }
  
  return(list())
}

# ---------- CRMP Module Helper Functions ----------
#' Scan for active CRMP sensors in the station data
#'
#' @param WX_stations List of weather station data
#' @return List of active CRMP sensors by station
scan_active_crmp_sensors <- function(WX_stations) {
  crmp_sensors <- c("SDepth", "SD", "PrecipOP2", "PrecipOP1", "PrecipPC2", "PrecipRIT", "PC", "Pcp_raw")
  
  # Initialize results list
  active_sensors <- list()
  
  # Check each station
  for (station_name in names(WX_stations)) {
    station_data <- WX_stations[[station_name]]
    
    # Skip invalid data
    if (is.null(station_data) || nrow(station_data) == 0) next
    
    # Find which CRMP sensors have data in this station
    active <- character(0)
    for (sensor in crmp_sensors) {
      if (sensor %in% names(station_data) && any(!is.na(station_data[[sensor]]))) {
        active <- c(active, sensor)
      }
    }
    
    # Only add stations with at least one active CRMP sensor
    if (length(active) > 0) {
      active_sensors[[station_name]] <- active
    }
  }
  
  return(active_sensors)
}

#' Fetch space weather alerts from NOAA
#'
#' @return Data frame of alerts or NULL if error
fetch_space_weather_alerts <- function() {
  tryCatch({
    # Simulated response since this is meant to be a placeholder
    alerts <- data.frame(
      alert_time = format(Sys.time() - sample(1:72, 5) * 3600, "%Y-%m-%d %H:%M:%S"),
      category = sample(c("Radio Blackout", "Solar Radiation Storm", "Geomagnetic Storm"), 5, replace = TRUE),
      level = sample(c("Minor", "Moderate", "Strong", "Severe", "Extreme"), 5, replace = TRUE),
      description = c(
        "R1 (Minor) Radio Blackout",
        "S1 (Minor) Solar Radiation Storm",
        "G2 (Moderate) Geomagnetic Storm",
        "S2 (Moderate) Solar Radiation Storm",
        "G1 (Minor) Geomagnetic Storm"
      ),
      expected_impact = c(
        "Minor degradation of HF radio communication",
        "Minor radiation hazard to astronauts",
        "Power grid fluctuations, aurora visible at high latitudes",
        "Increased radiation exposure to astronauts, minor impact on satellites",
        "Minor impact on power systems, aurora visible at high latitudes"
      ),
      stringsAsFactors = FALSE
    )
    return(alerts)
  }, error = function(e) {
    warning("Error fetching space weather alerts:", e$message)
    return(NULL)
  })
}

# ---------- Server Function Definition ----------
server <- function(input, output, session) {
  # Initialize reactive values
  wx_data <- reactiveVal(NULL)
  WX_stations <- reactiveVal(NULL)
  station_metadata <- reactiveVal(NULL)
  sensor_metadata <- reactiveVal(NULL)
  active_crmp_sensors <- reactiveVal(NULL)
  available_stations <- reactiveVal(NULL)
  
  # Create a cache container
  api_cache <- reactiveValues(
    metadata = NULL,
    sensor_metadata = NULL,
    last_metadata_fetch = NULL,
    last_sensor_fetch = NULL
  )
  
  # Initialize caches 
  cached_power_data <- reactiveVal(NULL)
  cached_temp_extremes <- reactiveVal(NULL)
  cached_missing_entries <- reactiveVal(NULL)
  cached_rn1_outliers <- reactiveVal(NULL)
  cached_constant_temp <- reactiveVal(NULL)
  cached_zero_wspd <- reactiveVal(NULL)
  cached_precip_changes <- reactiveVal(NULL)
  cached_consecutive_rh <- reactiveVal(NULL)
  cached_temp_trends <- reactiveVal(NULL)
  cached_vbat_trends <- reactiveVal(NULL)
  
  # Load metadata on startup (in-memory approach)
 # Load metadata on startup (in-memory approach) with optimized timing
observe({
  # Wait until the UI is fully rendered before fetching metadata
  session$onFlushed(function() {
    # Use a separate tryCatch block to avoid reactive context issues
    tryCatch({
      # First try to load static metadata included with the app
      static_metadata <- load_static_metadata()
      
      # Then fetch metadata from API directly without using the reactive cache
      api_metadata <- fetch_metadata_from_api()
      
      # Merge the two sources using sophisticated matching
      final_metadata <- merge_metadata_sources(static_metadata, api_metadata)
      
      # Only after processing is complete, update the reactive cache
      if (!is.null(final_metadata)) {
        # Update cache manually
        api_cache$metadata <- final_metadata
        api_cache$last_metadata_fetch <- Sys.time()
        
        # Update the reactive value
        station_metadata(final_metadata)
        
        # Report success to server log
        message(sprintf("Metadata loaded with %d stations", nrow(final_metadata)))
      }
    }, error = function(e) {
      message(sprintf("Error in metadata loading: %s", e$message))
      # Try to use static metadata if API fails
      static_metadata <- load_static_metadata()
      if (!is.null(static_metadata)) {
        station_metadata(static_metadata)
        message(sprintf("Falling back to static metadata with %d stations", nrow(static_metadata)))
      } else {
        # Create minimal metadata as a last resort
        message("WARNING: Failed to create metadata from any source!")
        empty_metadata <- data.frame(
          STATION = character(0),
          site_id = character(0),
          LATITUDE = numeric(0),
          LONGITUDE = numeric(0),
          FIRE_CENTRE = character(0),
          FIRE_ZONE = character(0),
          ELEVATION = numeric(0),
          stringsAsFactors = FALSE
        )
        station_metadata(empty_metadata)
      }
    })
  })
})  
   
  # Load sensor metadata from API with caching
  observe({
    # Use the fetch_with_cache helper
    sensor_meta <- fetch_with_cache(
      fetch_function = fetch_sensor_metadata,
      cache_container = api_cache,
      cache_key = "sensor_metadata", 
      timestamp_key = "last_sensor_fetch"
    )
    
    if (!is.null(sensor_meta)) {
      sensor_metadata(sensor_meta)
    }
  })
  
  # Update fetch based on time selection
  observe({
    req(input$time_preset)
    if (input$time_preset != "custom") {
      # Convert the preset value to numeric and update num_entries
      updateNumericInput(session, "num_entries",
                        value = as.numeric(input$time_preset))
    }
  })
  
  # Update available stations when fire centre changes
  observe({
    req(input$fire_centre, station_metadata())
    
    # Get stations for the selected fire centre
    available_stations <- station_metadata() %>%
      filter(FIRE_CENTRE == input$fire_centre, !is.na(site_id)) %>%
      select(STATION, site_id)
    
    if (input$station_selection_type == "specific") {
      # Update station selection choices only when in specific selection mode
      updateSelectizeInput(session, "selected_stations",
                         choices = setNames(available_stations$site_id, available_stations$STATION))
    }
    
    # Store available stations for later use
    available_stations(available_stations)
  })
  
  # Add the observer that was causing the error, but inside the server function
 observe({
  req(station_metadata())
  
  # Get current selection
  current_selection <- input$fire_centre
  
  # Get and sort fire centers from metadata
  fire_centers <- unique(station_metadata()$FIRE_CENTRE)
  fire_centers <- sort(fire_centers[!is.na(fire_centers)])
  
  # If no fire centers found, show a warning
  if (length(fire_centers) == 0) {
    showNotification("No fire centers found in metadata", type = "warning")
    return()
  }
  
  # Determine selection to use
  selected_center <- if(current_selection %in% fire_centers) {
    current_selection  # Keep current selection if it exists
  } else {
    fire_centers[1]  # Otherwise use the first one
  }
  
  # Update UI
  updateSelectInput(session, "fire_centre", 
                  choices = fire_centers,
                  selected = selected_center)
})
  
  # Modify the data fetching section
  observeEvent(input$fetch_data, {
    req(input$fire_centre, station_metadata())
    
    withProgress(message = "Fetching data...", {
      # Determine time range based on user selection
      time_range <- if (input$fetch_type == "last_n") {
        end_time <- Sys.time()
        hours <- as.numeric(input$num_entries)
        list(
          start = end_time - hours * 3600,
          end = end_time
        )
      } else if (input$fetch_type == "by_date") {
        list(
          start = as.POSIXct(paste(input$select_date, "00:00:00")),
          end = as.POSIXct(paste(input$select_date, "23:59:59"))
        )
      } else if (input$fetch_type == "date_range") {
        list(
          start = as.POSIXct(paste(input$start_date, "00:00:00")),
          end = as.POSIXct(paste(input$end_date, "23:59:59"))
        )
      }
      
      # Get stations for the selected fire centre
      fire_centre_stations <- station_metadata() %>%
        filter(FIRE_CENTRE == input$fire_centre, !is.na(site_id))
      
      # Filter stations based on user selection
      selected_stations <- if (input$station_selection_type == "specific" && 
                             length(input$selected_stations) > 0) {
        fire_centre_stations %>%
          filter(site_id %in% input$selected_stations)
      } else {
        fire_centre_stations
      }
      
      # Fetch weather data using API
      stations_data <- fetch_weather_data_api(
        input$fire_centre, 
        selected_stations, 
        time_range
      )
      
      # Update reactive values
      WX_stations(stations_data)
      wx_data(stations_data)
      
      # Process and cache active CRMP sensors
      active_sensors <- tryCatch({
        scan_active_crmp_sensors(stations_data)
      }, error = function(e) {
        warning("Error scanning CRMP sensors:", e$message)
        return(list())
      })
      active_crmp_sensors(active_sensors)
      
      # Cache expensive calculations within isolate to prevent reactivity
      isolate({
        # Cache power data
        power_data <- lapply(stations_data, function(station_data) {
          if (is.null(station_data) || 
              !all(c("Vbat", "Ibat", "Vslr", "Islr") %in% names(station_data)) ||
              nrow(station_data) == 0) {
            return(NULL)
          }
          
          # Safely calculate statistics to avoid warnings with empty vectors
          calculate_safe_stat <- function(data, fn, default = NA) {
            if (length(data) > 0 && any(!is.na(data))) {
              fn(data, na.rm = TRUE)
            } else {
              default
            }
          }
          
          # Safe which.min/max that handles empty vectors
          safe_which_min <- function(data) {
            if (length(data) > 0 && any(!is.na(data))) {
              which.min(replace(data, is.na(data), Inf))
            } else {
              NA_integer_
            }
          }
          
          safe_which_max <- function(data) {
            if (length(data) > 0 && any(!is.na(data))) {
              which.max(replace(data, is.na(data), -Inf))
            } else {
              NA_integer_
            }
          }
          
          # Ensure all columns are numeric
          for (col in c("Vbat", "Ibat", "Vslr", "Islr")) {
            station_data[[col]] <- as.numeric(station_data[[col]])
          }
          if ("Temp" %in% names(station_data)) {
            station_data$Temp <- as.numeric(station_data$Temp)
          }
          
          stats <- list(
            mean_battery_voltage = calculate_safe_stat(station_data$Vbat, mean),
            min_battery_voltage = calculate_safe_stat(station_data$Vbat, min),
            max_battery_voltage = calculate_safe_stat(station_data$Vbat, max),
            min_battery_voltage_time = {
              idx <- safe_which_min(station_data$Vbat)
              if (!is.na(idx) && idx <= length(station_data$DateTimeNum)) {
                station_data$DateTimeNum[idx]
              } else {
                NA
              }
            },
            max_battery_voltage_time = {
              idx <- safe_which_max(station_data$Vbat)
              if (!is.na(idx) && idx <= length(station_data$DateTimeNum)) {
                station_data$DateTimeNum[idx]
              } else {
                NA
              }
            },
            mean_battery_current = calculate_safe_stat(station_data$Ibat, mean),
            mean_solar_voltage = calculate_safe_stat(station_data$Vslr, mean),
            max_solar_voltage = calculate_safe_stat(station_data$Vslr, max),
            max_solar_voltage_time = {
              idx <- safe_which_max(station_data$Vslr)
              if (!is.na(idx) && idx <= length(station_data$DateTimeNum)) {
                station_data$DateTimeNum[idx]
              } else {
                NA
              }
            },
            mean_solar_current = calculate_safe_stat(station_data$Islr, mean),
            max_solar_current = calculate_safe_stat(station_data$Islr, max),
            max_solar_current_time = {
              idx <- safe_which_max(station_data$Islr)
              if (!is.na(idx) && idx <= length(station_data$DateTimeNum)) {
                station_data$DateTimeNum[idx]
              } else {
                NA
              }
            }
          )
          
          list(data = station_data, stats = stats)
        })
        cached_power_data(power_data[!sapply(power_data, is.null)])
        
        # Cache other calculations - wrap in try to prevent errors
        tryCatch({
          cached_missing_entries(check_for_missing_entries(stations_data))
        }, error = function(e) {
          warning("Error caching missing entries:", e$message)
        })
        
        tryCatch({
          cached_rn1_outliers(lapply(stations_data, identify_outliers, k = 6))
        }, error = function(e) {
          warning("Error caching RN1 outliers:", e$message)
        })
        
        tryCatch({
          cached_constant_temp(lapply(stations_data, find_constant_temp_periods))
        }, error = function(e) {
          warning("Error caching constant temp data:", e$message)
        })
        
        tryCatch({
          cached_zero_wspd(lapply(stations_data, find_zero_wspd_periods))
        }, error = function(e) {
          warning("Error caching zero wind speed data:", e$message)
        })
        
        tryCatch({
          cached_consecutive_rh(check_consecutive_rh(stations_data))
        }, error = function(e) {
          warning("Error caching consecutive RH data:", e$message)
        })
        
        # Cache temperature extremes
        tryCatch({
          temp_extremes <- data.frame(
            Station = character(),
            Temp = numeric(),
            DateTimeNum = character(),
            stringsAsFactors = FALSE
          )
          for(station_name in names(stations_data)) {
            station_data <- stations_data[[station_name]]
            if (!is.null(station_data) && "Temp" %in% names(station_data) && nrow(station_data) > 0) {
              station_trends <- data.frame(
                Station = station_name,
                Temp = as.numeric(station_data$Temp),
                DateTimeNum = station_data$DateTimeNum,
                stringsAsFactors = FALSE
              )
              temp_extremes <- rbind(temp_extremes, station_trends)
            }
          }
          cached_temp_extremes(temp_extremes)
        }, error = function(e) {
          warning("Error caching temperature extremes:", e$message)
        })
        
        # Cache temperature trends
        tryCatch({
          temp_trends <- list()
          for(station_name in names(stations_data)) {
            trends <- identify_temp_trends(stations_data[[station_name]])
            if(length(trends) > 0) {
              temp_trends[[station_name]] <- trends
            }
          }
          cached_temp_trends(temp_trends)
        }, error = function(e) {
          warning("Error caching temperature trends:", e$message)
        })
        
        # Cache precipitation changes
        tryCatch({
          precip_changes <- list()
          for (station in names(active_sensors)) {
            station_data <- stations_data[[station]]
            if (!is.null(station_data)) {
              if ("PrecipOP2" %in% names(station_data)) {
                precip_changes[[paste0(station, "_OP2")]] <- identify_precip_changes(station_data, "PrecipOP2")
              }
              if ("PrecipOP1" %in% names(station_data)) {
                precip_changes[[paste0(station, "_OP1")]] <- identify_precip_changes(station_data, "PrecipOP1")
              }
              if ("PrecipPC2" %in% names(station_data)) {
                precip_changes[[paste0(station, "_PC2")]] <- identify_precip_changes(station_data, "PrecipPC2")
              }
              if ("Pcp_raw" %in% names(station_data)) {
                precip_changes[[paste0(station, "_Pcp_raw")]] <- identify_precip_changes(station_data, "Pcp_raw")
              }
              if ("PrecipRIT" %in% names(station_data)) {
                precip_changes[[paste0(station, "_RIT")]] <- identify_precip_changes(station_data, "PrecipRIT")
              }
              if ("PC" %in% names(station_data)) {
                precip_changes[[paste0(station, "_PC")]] <- identify_precip_changes(station_data, "PC")
              }
            }
          }
          cached_precip_changes(precip_changes)
        }, error = function(e) {
          warning("Error caching precipitation changes:", e$message)
        })
        
        # Cache Vbat trends
        tryCatch({
          cached_vbat_trends(prepare_trend_data(cached_power_data()))
        }, error = function(e) {
          warning("Error caching Vbat trends:", e$message)
        })
      })

      
      # Update UI station selectors
      updateSelectizeInput(session, "db_station_select",
                         choices = c("All Stations", names(stations_data)))
      
      stations_with_power <- names(cached_power_data())
      updateSelectizeInput(session, "power_station_select",
                         choices = c("All Stations", stations_with_power))
      
      updateSelectInput(session, "temp_analysis_station_select",
                       choices = c("All Stations", names(wx_data())))
      
      observe({
        trend_data <- cached_vbat_trends()
        if (!is.null(trend_data) && length(trend_data) > 0) {
          updateSelectInput(session, "vbat_trends_station_select",
                          choices = c("All Stations", names(trend_data)),
                          selected = "All Stations")
        }
      })
      
      zero_wspd <- cached_zero_wspd()
      zero_wspd_stations <- names(zero_wspd)[sapply(zero_wspd, function(x) !is.null(x) && is.data.frame(x) && nrow(x) > 0)]
      updateSelectizeInput(session, "zero_wspd_station_select",
                         choices = c("All Stations", zero_wspd_stations))
      
      constant_temp <- cached_constant_temp()
      constant_temp_stations <- names(constant_temp)[sapply(constant_temp, function(x) !is.null(x) && is.data.frame(x) && nrow(x) > 0)]
      updateSelectizeInput(session, "constant_temp_station_select",
                         choices = c("All Stations", constant_temp_stations))
      
      consecutive_rh <- cached_consecutive_rh()
      if (is.data.frame(consecutive_rh)) {
        rh_stations <- unique(consecutive_rh$Station)
        updateSelectizeInput(session, "rh_station_select",
                           choices = c("All Stations", rh_stations))
      }
      
      rn1_outliers <- cached_rn1_outliers()
      rn1_outliers_stations <- names(rn1_outliers)[sapply(rn1_outliers, function(x) !is.null(x) && is.data.frame(x) && nrow(x) > 0)]
      updateSelectizeInput(session, "rn1_outliers_station_select",
                         choices = c("All Stations", rn1_outliers_stations))
      
      temp_trends <- cached_temp_trends()
      trend_stations <- names(temp_trends)[sapply(temp_trends, function(x) length(x) > 0)]
      updateSelectizeInput(session, "temp_trends_station_select",
                         choices = c("All Stations", trend_stations))
      
      if (!is.null(active_crmp_sensors()) && length(active_crmp_sensors()) > 0) {
        updateSelectizeInput(session, "crmp_station_select",
                           choices = c("All Stations", names(active_crmp_sensors())))
      }
    })
  })
  
  # ---------- Last Entry Time Check ----------
  last_entry_time_check <- eventReactive(input$fetch_data, {
    req(WX_stations())
    
    # Get current time in Pacific Time
    current_time <- with_tz(Sys.time(), "America/Los_Angeles")
    
    # Flag to determine if we're fetching historical data
    is_historical <- input$fetch_type %in% c("by_date", "date_range")
    
    results <- lapply(names(WX_stations()), function(station_name) {
      station_data <- WX_stations()[[station_name]]
      
      if (is.null(station_data) || nrow(station_data) == 0) {
        return(data.frame(
          Station = station_name,
          LastEntry = as.character(NA),
          TimeDifference = as.numeric(NA),
          Status = "No data"
        ))
      }
      
      # Parse timestamp directly as Pacific Time since that's what it is
      last_entry_time <- try({
        as.POSIXct(station_data$DateTimeNum[1], format = "%Y-%b-%d %H:%M:%S", tz = "America/Los_Angeles")
      }, silent = TRUE)
      
      if (inherits(last_entry_time, "try-error") || is.na(last_entry_time)) {
        return(data.frame(
          Station = clean_station_name,
          LastEntry = as.character(NA),
          TimeDifference = as.numeric(NA),
          Status = "Invalid date"
        ))
      }
      
      # Calculate time difference (both times in Pacific Time)
      time_diff <- as.numeric(difftime(current_time, last_entry_time, units = "hours"))
      
      # Determine status based on whether we're looking at historical data
      status <- if (is_historical) {
        "Historical"
      } else {
        dplyr::case_when(
          is.na(time_diff) ~ "Invalid date",
          time_diff <= 1 ~ "Up to date",
          time_diff <= 3 ~ "Slightly delayed",
          time_diff <= 6 ~ "Delayed",
          TRUE ~ "Significantly delayed"
        )
      }
      
      data.frame(
        Station = station_name,
        LastEntry = format(last_entry_time, "%Y-%m-%d %H:%M"),
        TimeDifference = round(time_diff, 2),
        Status = status,
        stringsAsFactors = FALSE
      )
    })
    
    do.call(rbind, results)
  })
  
  # ---------- Reactive Expressions ----------
  # Use cached values in reactive expressions
  all_power_data <- reactive({
    cached_power_data()
  })
  
  temp_extremes_data <- reactive({
    cached_temp_extremes()
  })
  
  missing_entries_data <- reactive({
    cached_missing_entries()
  })
  
  all_rn1_outliers <- reactive({
    cached_rn1_outliers()
  })
  
  constant_temp_data <- reactive({
    cached_constant_temp()
  })
  
  zero_wspd_data <- reactive({
    cached_zero_wspd()
  })
  
  vbat_trend_data <- reactive({
    cached_vbat_trends()
  })
  
  all_precip_changes <- reactive({
    cached_precip_changes()
  })
  
  # Space weather data
  space_weather_data <- reactive({
    fetch_space_weather_alerts()
  })
  
  # ---------- Output Renderings ----------
  output$data_status <- renderText({
    req(input$fetch_data)
    
    if (input$fetch_type == "last_n") {
      sprintf("Last %dh • %s", input$num_entries, input$fire_centre)
    } else if (input$fetch_type == "by_date") {
      sprintf("%s • %s", 
              format(input$select_date, "%b %d, %Y"),
              input$fire_centre)
    } else {
      sprintf("%s - %s • %s", 
              format(input$start_date, "%b %d, %Y"),
              format(input$end_date, "%b %d, %Y"),
              input$fire_centre)
    }
  })
  
  # DB Module Outputs
  output$last_entry_time_check <- renderDT({
    status_data <- last_entry_time_check()
    
    if (!is.null(input$db_station_select) && input$db_station_select != "All Stations") {
      status_data <- status_data %>%
        filter(Station == input$db_station_select)
    }
    
    # Rename columns for display
    names(status_data) <- c("Station", "Last Entry", "Hours Since Update", "Status")
    
    render_report_status_output(status_data)
  })
  
  output$space_weather_table <- renderDT({
    render_space_weather_table(space_weather_data(), input)
  })
  
  output$missing_entries_output <- renderPrint({
    render_missing_entries_output(missing_entries_data, input$db_station_select)
  })
  
  output$missing_entries_plot <- renderPlotly({
    create_missing_entries_plot(missing_entries_data(), input$db_station_select)
  })
  
  output$recent_entries <- renderDT({
    req(wx_data(), input$db_station_select)
    
    selected_data <- if (input$db_station_select == "All Stations") {
      wx_data()
    } else {
      # Create a new list with only the selected station
      selected_station_data <- list()
      selected_station_data[[input$db_station_select]] <- wx_data()[[input$db_station_select]]
      selected_station_data
    }
    
    render_recent_entries(selected_data)
  }, server = FALSE)  
  
  output$original_output <- renderPrint({
    render_check_for_blanks(wx_data())
  })
  
  # Power Module Outputs
  output$vbat_waterfall_plot <- renderPlotly({
    create_vbat_waterfall_plot(all_power_data(), input$power_station_select)
  })
  
  output$vbat_trends_plot <- renderPlotly({
    trend_data <- vbat_trend_data()
    req(trend_data)
    create_vbat_trends_plot(trend_data, input$vbat_trends_station_select)
  })
  
  output$power_status_output <- renderPrint({
    render_power_status_output(input, all_power_data)
  })
  
  output$battery_voltage_alerts <- renderPrint({
    render_battery_voltage_alerts(all_power_data())
  })
  
  output$vbat_trends_output <- renderPrint({
    req(all_power_data(), input$vbat_trends_station_select)
    selected_data <- all_power_data()
    
    # Filter the data based on station selection
    if (!is.null(input$vbat_trends_station_select) && 
        !"All Stations" %in% input$vbat_trends_station_select) {
      selected_data <- selected_data[input$vbat_trends_station_select]
    }
    
    cat(analyze_vbat_trends(selected_data))
  })
  
  # CRMP Module Outputs
  output$sdepth_concerns_output <- renderPrint({
    render_sdepth_concerns_output(wx_data(), active_crmp_sensors())
  })
  
  output$crmp_precip_changes <- renderPrint({
    render_crmp_precip_changes(all_precip_changes())
  })
  
  output$crmp_entries <- renderPrint({
    req(input$crmp_station_select)
    
    if (input$crmp_station_select == "All Stations") {
      render_crmp_entries(wx_data(), active_crmp_sensors())
    } else {
      selected_station_data <- list()
      selected_station_data[[input$crmp_station_select]] <- wx_data()[[input$crmp_station_select]]
      
      selected_checks <- list()
      selected_checks[[input$crmp_station_select]] <- active_crmp_sensors()[[input$crmp_station_select]]
      
      render_crmp_entries(selected_station_data, selected_checks)
    }
  })
  
  output$precip_plot <- renderPlotly({
    create_precip_plot(wx_data(), input$crmp_station_select)
  })
  
  output$sdepth_plot <- renderPlotly({
    create_sdepth_plot(wx_data(), input$crmp_station_select)
  })
  
  # Wind Module Outputs
  output$high_wspd_output <- renderPrint({
    render_high_wspd_output(wx_data())
  })
  
  output$dir_check <- renderPrint({
    render_dir_check_output(wx_data())
  })
  
  output$zero_wspd_output <- renderPrint({
    render_zero_wspd_output(wx_data(), input$zero_wspd_station_select)
  })
  
  output$zero_wspd_plot <- renderPlotly({
    create_zero_wspd_plot(zero_wspd_data(), input$zero_wspd_station_select)
  })
  
  # Temperature Module Outputs
  output$extreme_temp_output <- renderPrint({
    render_extreme_temp_output(wx_data())
  })
  
  output$temp_change_heatmap <- renderPlotly({
    req(wx_data())
    create_temp_change_heatmap(wx_data(), input$temp_analysis_station_select)
  })
  
  output$temp_extremes_output <- renderPrint({
    render_temp_extremes_output(wx_data())
  })
  
  output$temp_check <- renderPrint({
    req(constant_temp_data(), input$constant_temp_station_select)
    render_temp_check(input, constant_temp_data())
  })
  
  output$erratic_temp_output <- renderPrint({
    req(wx_data())
    render_erratic_temp_output(wx_data(), input$temp_analysis_station_select)
  })  
  
  output$rapid_temp_changes_output <- renderPrint({
    render_rapid_temp_changes_output(wx_data())
  })
  
  output$temp_range_plot <- renderPlotly({
    create_temp_range_plot(temp_extremes_data())
  })
  
  output$constant_temp_summary_plot <- renderPlotly({
    req(constant_temp_data(), input$constant_temp_station_select)
    create_constant_temp_summary(constant_temp_data(), input$constant_temp_station_select)
  })
  
  output$constant_temp_plot <- renderPlotly({
    req(constant_temp_data())
    create_constant_temp_plot(constant_temp_data(), input$constant_temp_station_select)
  })
  
  # RH Module Outputs
  output$low_rh_output <- renderPrint({
    render_low_rh_output(wx_data())
  })
  
  output$consecutive_rh_output <- renderPrint({
    render_consecutive_rh_output(wx_data(), input$rh_station_select)
  })
  
  output$consecutive_rh_plot <- renderPlotly({
    create_consecutive_rh_plot(wx_data(), input$rh_station_select)
  })
  
  # Precipitation Module Outputs
  output$decreasing_trends_output <- renderPrint({
    req(input$fetch_data, input$temp_trends_station_select)
    render_decreasing_trends_output(wx_data(), input$temp_trends_station_select)
  })
  
  output$station_rainfall_output <- renderPrint({
    req(wx_data())  # Ensure wx_data is available
    render_station_rainfall_output(wx_data(), input$station_select)
  })
  
  output$rn1_outliers_output <- renderPrint({
    render_rn1_outliers_output(input, all_rn1_outliers())
  })
  
  output$rn1_outliers_plot <- renderPlotly({
    create_rn1_outliers_plot(wx_data(), all_rn1_outliers(), input$rn1_outliers_station_select)
  })
  
  output$df_structure <- renderPrint({
    req(wx_data(), input$db_station_select)
    
    if (input$db_station_select == "All Stations") {
      # Show structure for all stations
      for (station_name in names(wx_data())) {
        station_data <- wx_data()[[station_name]]
        if (!is.null(station_data) && nrow(station_data) > 0) {
          cat("\nStructure for station:", station_name, "\n")
          str(station_data)
          cat("\n")
        }
      }
    } else {
      # Show structure for selected station only
      selected_data <- wx_data()[[input$db_station_select]]
      if (!is.null(selected_data) && nrow(selected_data) > 0) {
        str(selected_data)
      } else {
        cat("No data available for selected station")
      }
    }
  })
}

# ---------- Run Application ----------
shinyApp(ui = ui, server = server)
# global_functions.R


# Function to get stations for a specific fire center
get_fire_center_stations <- function(fire_centre) {
  # Get all zones for the selected fire center
  zones <- fire_center_mapping[[fire_centre]]
  
  # Get all stations for these zones
  stations <- unique(unlist(sapply(zones, function(zone) {
    fire_zone_mapping[[zone]]
  })))
  
  # Convert station names to database table format
  db_stations <- paste0("dbo.", gsub(" ", "_", stations))
  
  return(db_stations)
}

# Function to scan for active CRMP sensors
scan_active_crmp_sensors <- function(WX_stations) {
  active_crmp_sensors <- list()
  
  for (station_name in names(WX_stations)) {
    station_data <- WX_stations[[station_name]]
    if (is.null(station_data) || nrow(station_data) == 0) next
    
    # Initialize empty vector for active sensors
    active_sensors <- c()
    
    # Check for SDepth
    if ("SDepth" %in% names(station_data) && 
        any(!is.na(station_data$SDepth)) && 
        any(station_data$SDepth != 0, na.rm = TRUE)) {
      active_sensors <- c(active_sensors, "SDepth")
    }
    
    # Check for PrecipPC2
    if ("PrecipPC2" %in% names(station_data) && 
        any(!is.na(station_data$PrecipPC2)) && 
        any(station_data$PrecipPC2 != 0, na.rm = TRUE)) {
      active_sensors <- c(active_sensors, "PrecipPC2")
    }
    
    # Check for PrecipOP2
    if ("PrecipOP2" %in% names(station_data) && 
        any(!is.na(station_data$PrecipOP2)) && 
        any(station_data$PrecipOP2 != 0, na.rm = TRUE)) {
      active_sensors <- c(active_sensors, "PrecipOP2")
    }
    
    # Check for Pcp_raw
    if ("Pcp_raw" %in% names(station_data) && 
        any(!is.na(station_data$Pcp_raw)) && 
        any(station_data$Pcp_raw != 0, na.rm = TRUE)) {
      active_sensors <- c(active_sensors, "Pcp_raw")
    }
	 # Check for PrecipOP1
    if ("PrecipOP1" %in% names(station_data) && 
        any(!is.na(station_data$PrecipOP1)) && 
        any(station_data$PrecipOP1 != 0, na.rm = TRUE)) {
      active_sensors <- c(active_sensors, "PrecipOP1")
    }

 # Check for PrecipRIT
    if ("PrecipRIT" %in% names(station_data) && 
        any(!is.na(station_data$PrecipRIT)) && 
        any(station_data$PrecipRIT != 0, na.rm = TRUE)) {
      active_sensors <- c(active_sensors, "PrecipRIT")
    }

 # Check for PC
    if ("PC" %in% names(station_data) && 
        any(!is.na(station_data$PC)) && 
        any(station_data$PC != 0, na.rm = TRUE)) {
      active_sensors <- c(active_sensors, "PC")
    }

 # Check for SD
    if ("SD" %in% names(station_data) && 
        any(!is.na(station_data$SD)) && 
        any(station_data$SD != 0, na.rm = TRUE)) {
      active_sensors <- c(active_sensors, "SD")
    }
    
    # Add to list if any sensors are active
    if (length(active_sensors) > 0) {
      # Clean station name by removing 'dbo.' prefix if present
      clean_station_name <- gsub("dbo\\.", "", station_name)
      active_crmp_sensors[[clean_station_name]] <- active_sensors
    }
  }
  
  return(active_crmp_sensors)
}
# NOAA data fetch
fetch_space_weather_alerts <- function() {
  tryCatch({
    json_data <- fromJSON("https://services.swpc.noaa.gov/products/alerts.json")
    json_data$issue_datetime <- as.POSIXct(json_data$issue_datetime, format="%Y-%m-%d %H:%M:%S", tz="UTC")
    json_data$issue_datetime <- with_tz(json_data$issue_datetime, tzone = "America/Los_Angeles")
    json_data 
  }, error = function(e) {
    message("Error fetching space weather alerts: ", e$message)
    NULL
  })
}

# Function to convert DateTimeNum to POSIXct
convert_to_datetime <- function(serial) {
  if(is.numeric(serial)) {
    origin <- as.POSIXct("1899-12-30", tz = "UTC")
    result <- as.POSIXct(origin + (serial * 86400), tz = "UTC")
    return(result)
  } else {
    # If serial is not numeric, try to parse it as is
    tryCatch({
      as.POSIXct(serial, tz = "UTC")
    }, error = function(e) {
      message("Error converting date: ", e$message)
      NA
    })
  }
}
# Function to fetch data from the database
fetch_data_from_db <- function(conn, table, num_entries = NULL, selected_date = NULL, date_range = NULL, fire_centre) {
  # Check if station belongs to selected fire centre
  clean_table_name <- gsub("_", " ", gsub("dbo\\.", "", table))
  fire_centre_stations <- unlist(sapply(fire_center_mapping[[fire_centre]], function(zone) {
    toupper(fire_zone_mapping[[zone]])
  }))
  
  if (!toupper(clean_table_name) %in% fire_centre_stations) {
    return(NULL)
  }
  
  # Base columns that should always be present
  base_columns <- c("DateTimeNum", "Rn_1", "Temp", "Rh", "Dir", "Wspd")
  
  # Additional columns to check
  additional_columns <- c(
    "Vbat", "Islr", "Vslr", "Ibat", "Mx_Spd", 
    "PrecipPC2", "SDepth", "PrecipOP2", "Pcp_raw",
    "PrecipOP1", "PrecipRIT", "PC", "SD"
  )
  
  # Check which columns exist using DBI
  check_columns_query <- sprintf("
    SELECT COLUMN_NAME 
    FROM INFORMATION_SCHEMA.COLUMNS 
    WHERE TABLE_NAME = '%s' 
    AND COLUMN_NAME IN (%s)",
    gsub("dbo.", "", table),
    paste(sprintf("'%s'", additional_columns), collapse = ",")
  )
  
  existing_columns <- dbGetQuery(conn, check_columns_query)$COLUMN_NAME
  selected_columns <- paste(c(base_columns, existing_columns), collapse = ", ")
  
  # Build WHERE clause
  where_clause <- "WHERE data_writer_synced = 1.0"
  if (!is.null(selected_date)) {
    date_serial_start <- as.numeric(as.Date(selected_date) - as.Date("1899-12-30"))
    where_clause <- sprintf(
      "%s AND DateTimeNum >= %f AND DateTimeNum < %f",
      where_clause, date_serial_start, date_serial_start + 1
    )
  } else if (!is.null(date_range)) {
    start_serial <- as.numeric(as.Date(date_range$start) - as.Date("1899-12-30"))
    end_serial <- as.numeric(as.Date(date_range$end) - as.Date("1899-12-30")) + 1
    where_clause <- sprintf(
      "%s AND DateTimeNum >= %f AND DateTimeNum < %f",
      where_clause, start_serial, end_serial
    )
  }
  
  # Construct final query
  query <- sprintf(
    "SELECT %s%s FROM %s %s ORDER BY DateTimeNum DESC",
    if (!is.null(num_entries)) paste0("TOP ", num_entries, " ") else "",
    selected_columns,
    table,
    where_clause
  )
  
  # Execute query and process results using DBI
  tryCatch({
    data <- dbGetQuery(conn, query)
    
    if (!is.null(data) && nrow(data) > 0 && !is.null(data$DateTimeNum)) {
      # Convert dates using base R date conversion
     origin <- as.POSIXct("1899-12-30", tz = "America/Los_Angeles")
data$DateTimeNum <- format(
  origin + as.numeric(data$DateTimeNum) * 86400,
  "%Y-%b-%d %H:%M:%S"
)
    }
    
    return(data)
    
  }, error = function(e) {
    message("Error fetching data from table ", table, ": ", e$message)
    # Create empty dataframe with correct columns
    empty_df <- data.frame(matrix(ncol = length(strsplit(selected_columns, ",")[[1]]), nrow = 0))
    colnames(empty_df) <- trimws(strsplit(selected_columns, ",")[[1]])
    return(empty_df)
  })
}

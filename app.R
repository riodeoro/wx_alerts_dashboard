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

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(shiny, dplyr, lubridate, plotly, bslib, jsonlite, DT, zoo, viridis, ggplot2, odbc, DBI)


# ---------- Database Connection ----------
db_params <- list(
    driver = "SQL Server",
    server = "HITCHIE2022\\PROD1",
    database = "AutoCaller",
    trusted_connection = "Yes"
)

# Function to create database connection
create_db_connection <- function() {
    tryCatch({
        dbConnect(odbc(),
                 Driver = db_params$driver,
                 Server = db_params$server,
                 Database = db_params$database,
                 Trusted_Connection = db_params$trusted_connection)
    }, error = function(e) {
        message("Error connecting to database: ", e$message)
        NULL
    })
}

# ---------- Source Files ----------

# Core files
source("ui_definition.R")
source("global_functions.R")
source("global_resources.R")

addResourcePath("www", "www")

# Module function files
source("r/db_module/db_functions.R")
source("r/temperature_module/temperature_functions.R")
source("r/rh_module/rh_functions.R")
source("r/wind_module/wind_functions.R")
source("r/power_module/power_functions.R")
source("r/crmp_module/crmp_functions.R")
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

# ---------- Server Function Definition ----------
server <- function(input, output, session) {
    # Initialize reactive values
    wx_data <- reactiveVal(NULL)
    WX_stations <- reactiveVal(NULL)
    active_crmp_sensors <- reactiveVal(NULL)
    available_stations <- reactiveVal(NULL)
    
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

    # Modify the data fetching section
    observeEvent(input$fetch_data, {
        req(input$fire_centre)
        
        withProgress(message = "Fetching data...", {
            # Create database connection
            conn <- create_db_connection()
            
            if (is.null(conn)) {
                output$missing_entries_output <- renderPrint("Database connection failed")
                return()
            }
            
            # Ensure connection is closed when done
            on.exit({
                if (!is.null(conn)) {
                    dbDisconnect(conn)
                }
            })
            
            selected_fire_centre <- input$fire_centre
            
            # Get tables for the selected fire centre
            zones <- fire_center_mapping[[selected_fire_centre]]
            available_stations <- unique(unlist(sapply(zones, function(zone) {
                fire_zone_mapping[[zone]]
            })))

        
        # Create mapping between display names and table names
        station_to_table <- setNames(
            paste0("dbo.", tables),
            gsub("_", " ", tables)
        )
        
        # Determine which tables to fetch
        tables_to_fetch <- if (input$station_selection_type == "specific") {
    if (length(input$selected_stations) > 0) {
        station_to_table[input$selected_stations]
    } else {
        # Fallback to all stations if none selected in specific mode
        station_to_table[available_stations]
    }
} else {
    # When "All Stations" is selected
    station_to_table[available_stations]
}
        
        # Fetch weather station data
        stations_data <- lapply(tables_to_fetch, function(table) {
            if (input$fetch_type == "last_n") {
                fetch_data_from_db(conn, table, num_entries = input$num_entries, 
                                 fire_centre = selected_fire_centre)
            } else if (input$fetch_type == "by_date") {
                fetch_data_from_db(conn, table, selected_date = input$select_date, 
                                 fire_centre = selected_fire_centre)
            } else if (input$fetch_type == "date_range") {
                fetch_data_from_db(conn, table, 
                               date_range = list(
                                   start = input$start_date,
                                   end = input$end_date
                               ),
                               fire_centre = selected_fire_centre)
            }
        })
        names(stations_data) <- names(tables_to_fetch)
        
        # Remove NULL entries
        stations_data <- stations_data[!sapply(stations_data, is.null)]
        
        # Update reactive values
        WX_stations(stations_data)
        wx_data(stations_data)
        
        # Process and cache active CRMP sensors
        active_sensors <- scan_active_crmp_sensors(stations_data)
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
          
          station_data$Vbat <- as.numeric(station_data$Vbat)
          station_data$Ibat <- as.numeric(station_data$Ibat)
          station_data$Vslr <- as.numeric(station_data$Vslr)
          station_data$Islr <- as.numeric(station_data$Islr)
          station_data$Temp <- as.numeric(station_data$Temp)
          
          stats <- list(
            mean_battery_voltage = if(any(!is.na(station_data$Vbat))) mean(station_data$Vbat, na.rm = TRUE) else NA,
            min_battery_voltage = if(any(!is.na(station_data$Vbat))) min(station_data$Vbat, na.rm = TRUE) else NA,
            max_battery_voltage = if(any(!is.na(station_data$Vbat))) max(station_data$Vbat, na.rm = TRUE) else NA,
            min_battery_voltage_time = if(any(!is.na(station_data$Vbat))) {
              station_data$DateTimeNum[which.min(replace(station_data$Vbat, is.na(station_data$Vbat), Inf))]
            } else NA,
            max_battery_voltage_time = if(any(!is.na(station_data$Vbat))) {
              station_data$DateTimeNum[which.max(replace(station_data$Vbat, is.na(station_data$Vbat), -Inf))]
            } else NA,
            mean_battery_current = if(any(!is.na(station_data$Ibat))) mean(station_data$Ibat, na.rm = TRUE) else NA,
            mean_solar_voltage = if(any(!is.na(station_data$Vslr))) mean(station_data$Vslr, na.rm = TRUE) else NA,
            max_solar_voltage = if(any(!is.na(station_data$Vslr))) max(station_data$Vslr, na.rm = TRUE) else NA,
            max_solar_voltage_time = if(any(!is.na(station_data$Vslr))) {
              station_data$DateTimeNum[which.max(replace(station_data$Vslr, is.na(station_data$Vslr), -Inf))]
            } else NA,
            mean_solar_current = if(any(!is.na(station_data$Islr))) mean(station_data$Islr, na.rm = TRUE) else NA,
            max_solar_current = if(any(!is.na(station_data$Islr))) max(station_data$Islr, na.rm = TRUE) else NA,
            max_solar_current_time = if(any(!is.na(station_data$Islr))) {
              station_data$DateTimeNum[which.max(replace(station_data$Islr, is.na(station_data$Islr), -Inf))]
            } else NA
          )
          
          list(data = station_data, stats = stats)
        })
        cached_power_data(power_data[!sapply(power_data, is.null)])
        
        # Cache other calculations
        cached_missing_entries(check_for_missing_entries(stations_data))
        cached_rn1_outliers(lapply(stations_data, identify_outliers, k = 6))
        cached_constant_temp(lapply(stations_data, find_constant_temp_periods))
        cached_zero_wspd(lapply(stations_data, find_zero_wspd_periods))
        cached_consecutive_rh(check_consecutive_rh(stations_data))
        
        # Cache temperature extremes
        temp_extremes <- data.frame(
          Station = character(),
          Temp = numeric(),
          DateTimeNum = character()
        )
        for(station_name in names(stations_data)) {
          station_data <- stations_data[[station_name]]
          if (!is.null(station_data) && "Temp" %in% names(station_data) && nrow(station_data) > 0) {
            station_trends <- data.frame(
              Station = sub("^dbo\\.", "", station_name),
              Temp = as.numeric(station_data$Temp),
              DateTimeNum = station_data$DateTimeNum
            )
            temp_extremes <- rbind(temp_extremes, station_trends)
          }
        }
        cached_temp_extremes(temp_extremes)
        
        # Cache temperature trends
        temp_trends <- list()
        for(station_name in names(stations_data)) {
          trends <- identify_temp_trends(stations_data[[station_name]])
          if(length(trends) > 0) {
            temp_trends[[station_name]] <- trends
          }
        }
        cached_temp_trends(temp_trends)
        
        # Cache precipitation changes
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
        
        # Cache Vbat trends
        cached_vbat_trends(prepare_trend_data(cached_power_data()))
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
      zero_wspd_stations <- names(zero_wspd)[sapply(zero_wspd, function(x) nrow(x) > 0)]
      updateSelectizeInput(session, "zero_wspd_station_select",
                         choices = c("All Stations", zero_wspd_stations))
      
      constant_temp <- cached_constant_temp()
      constant_temp_stations <- names(constant_temp)[sapply(constant_temp, function(x) nrow(x) > 0)]
      updateSelectizeInput(session, "constant_temp_station_select",
                         choices = c("All Stations", constant_temp_stations))
      
      consecutive_rh <- cached_consecutive_rh()
      if (is.data.frame(consecutive_rh)) {
        rh_stations <- unique(consecutive_rh$Station)
        updateSelectizeInput(session, "rh_station_select",
                           choices = c("All Stations", rh_stations))
      }
      
      rn1_outliers <- cached_rn1_outliers()
      rn1_outliers_stations <- names(rn1_outliers)[sapply(rn1_outliers, function(x) !is.null(x) && nrow(x) > 0)]
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
  

observe({
  req(input$fire_centre)
  
  # Get zones for selected fire centre
  zones <- fire_center_mapping[[input$fire_centre]]
  
  # Get available stations for these zones
  available_stations <- unique(unlist(sapply(zones, function(zone) {
    fire_zone_mapping[[zone]]
  })))
  
  # Update the selectizeInput choices
  updateSelectizeInput(session, "selected_stations",
    choices = available_stations,
    selected = NULL,
    options = list(
      placeholder = 'Select stations to include',
      plugins = list('remove_button')
    )
  )
})
  # ---------- Last Entry Time Check ----------
  last_entry_time_check <- eventReactive(input$fetch_data, {
    req(WX_stations())
    
    # Get current time in Pacific Time
    current_time <- with_tz(Sys.time(), "America/Los_Angeles")
    
    # Flag to determine if we're fetching historical data
    is_historical <- input$fetch_type %in% c("by_date", "date_range")
    
    results <- lapply(names(WX_stations()), function(station_name) {
      clean_station_name <- sub("^dbo\\.", "", station_name)
      station_data <- WX_stations()[[station_name]]
      
      if (is.null(station_data) || nrow(station_data) == 0) {
        return(data.frame(
          Station = clean_station_name,
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
          Station = clean_station_name,
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
  
  # Space weather data doesn't need caching as it's fetched separately
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
}



# ---------- Run Application ----------
shinyApp(ui = ui, server = server)

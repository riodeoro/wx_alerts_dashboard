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

# temperature_module/temperature_charts.R

create_temp_change_heatmap <- function(wx_data, selected_station = NULL) {
  if (is.null(wx_data) || length(wx_data) == 0) {
    return(plot_ly() %>% layout(title = "No temperature data available"))
  }
  
  # Process data for all stations or selected station
  temp_changes <- data.frame()
  station_start_times <- data.frame(station = character(),
                                  start_time = as.POSIXct(character()),
                                  reading_count = numeric())
  
  stations_to_process <- if (!is.null(selected_station) && selected_station != "All Stations") {
    selected_station
  } else {
    names(wx_data)
  }
  
  for (station_name in stations_to_process) {
    station_data <- wx_data[[station_name]]
    
    if (!is.null(station_data) && "Temp" %in% names(station_data) && nrow(station_data) > 0) {
      # Convert DateTimeNum to POSIXct if it's character
      if (is.character(station_data$DateTimeNum)) {
        station_data$DateTimeNum <- as.POSIXct(station_data$DateTimeNum, 
                                              format = "%Y-%b-%d %H:%M:%S", 
                                              tz = "UTC")
      }
      
      # Store station start time and reading count
      station_start_times <- rbind(station_start_times, data.frame(
        station = station_name,
        start_time = min(station_data$DateTimeNum, na.rm = TRUE),
        reading_count = nrow(station_data)
      ))
      
      changes <- station_data %>%
        arrange(DateTimeNum) %>%
        mutate(
          OriginalDateTime = DateTimeNum,
          DateTimeNum = ceiling_date(DateTimeNum, unit = "minute"),
          Temp = as.numeric(Temp),
          temp_change = lead(Temp) - Temp,
          Station = sub("^dbo\\.", "", station_name)
        ) %>%
        select(Station, DateTimeNum, OriginalDateTime, temp_change, Temp) %>%
        filter(!is.na(temp_change))
      
      temp_changes <- rbind(temp_changes, changes)
    }
  }
  
  if (nrow(temp_changes) == 0) {
    return(plot_ly() %>% layout(title = "No temperature change data available"))
  }
  
  # Sort stations in reverse alphabetical order (A at top)
  station_levels <- sort(unique(temp_changes$Station), decreasing = TRUE)
  temp_changes$Station <- factor(temp_changes$Station, levels = station_levels)
  
  # Calculate weighted average start time based on number of readings
  total_readings <- sum(station_start_times$reading_count)
  weighted_start_time <- sum(as.numeric(station_start_times$start_time) * 
                           (station_start_times$reading_count / total_readings))
  common_start_date <- as.POSIXct(weighted_start_time, origin = "1970-01-01", tz = "UTC")
  
  # Round to nearest hour
  common_start_date <- ceiling_date(common_start_date, unit = "hour")
  
  # Create custom diverging color scale
  colors <- colorRampPalette(c("#313695", "#4575B4", "#74ADD1", "#ABD9E9", 
                              "#FFFFFF",
                              "#FDAE61", "#F46D43", "#D73027", "#A50026"))(100)
  
  max_abs_change <- max(abs(temp_changes$temp_change), na.rm = TRUE)
  
  # Create the heatmap
  p <- plot_ly(
    x = ~DateTimeNum,
    y = ~Station,
    z = ~temp_change,
    data = temp_changes,
    type = "heatmap",
    colors = colors,
    zmin = -max_abs_change,
    zmax = max_abs_change,
    hoverongaps = FALSE,
    hovertemplate = paste(
      "Station: %{y}<br>",
      "Time: %{customdata}<br>",
      "Temperature: %{text}°C<br>",
      "Change: %{z:.1f}°C<br>",
      "<extra></extra>"
    ),
    customdata = ~format(OriginalDateTime, "%Y-%b-%d %H:%M:%S"),
    text = ~sprintf("%.1f", Temp)
  ) %>%
    layout(
      title = list(
        text = "Hourly Temperature Changes by Station",
        font = list(size = 16)
      ),
      xaxis = list(
        title = "Time",
        tickfont = list(size = 10),
        tickangle = -45,
        range = c(common_start_date, max(temp_changes$DateTimeNum)),
        rangemode = "tozero"
      ),
      yaxis = list(
        title = "Station",
        tickfont = list(size = 10)
      ),
      margin = list(t = 50, b = 80, l = 120, r = 50),
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor = "#FFFFFF"
    )
  
  return(p)
}
create_temp_range_plot <- function(temp_data) {
  if (is.null(temp_data) || nrow(temp_data) == 0) {
    return(plot_ly() %>% 
           layout(title = "No temperature data available"))
  }
  
  # Set fixed temperature range
  x_min <- -40
  x_max <- 50
  
  # Calculate station statistics with timestamps
  station_stats <- temp_data %>%
    group_by(Station) %>%
    filter(complete.cases(Temp)) %>%  # Only include rows where Temp is not NA
    summarise(
      mean_temp = if(n() > 0) mean(Temp, na.rm = TRUE) else NA,
      max_temp = if(n() > 0) max(Temp, na.rm = TRUE) else NA,
      min_temp = if(n() > 0) min(Temp, na.rm = TRUE) else NA,
      sd_temp = if(n() > 0) sd(Temp, na.rm = TRUE) else NA,
      max_temp_time = if(n() > 0) DateTimeNum[which.max(Temp)] else NA,
      min_temp_time = if(n() > 0) DateTimeNum[which.min(Temp)] else NA,
      data_count = n()
    ) %>%
    filter(data_count > 0) %>%  # Only include stations with valid readings
    arrange(desc(mean_temp))
  
  station_order <- station_stats$Station
  
  # Initialize plot
  p <- plot_ly()
  
  # Enhanced spacing parameters
  y_step <- 5
  station_positions <- seq_along(station_order)
  names(station_positions) <- rev(station_order)
  
  # Create color scale based on mean temperatures
  mean_temp_range <- range(station_stats$mean_temp, na.rm = TRUE)
  
  # Custom color palette for mean temperature gradient
  temp_colors <- colorRampPalette(c(
    "#313695", # Deep blue (coldest)
    "#4575B4", # Blue
    "#74ADD1", # Light blue
    "#ABD9E9", # Very light blue
    "#E0F3F8", # Pale blue
    "#FFFFBF", # Pale yellow
    "#FEE090", # Light orange
    "#FDAE61", # Orange
    "#F46D43", # Dark orange
    "#D73027"  # Deep red (warmest)
  ))(100)
  
  # Process each station's data
  for (i in seq_along(station_order)) {
    station_name <- station_order[i]
    station_data <- temp_data %>% 
      filter(Station == station_name) %>%
      filter(complete.cases(Temp))  # Filter out NA values
    
    base_y <- station_positions[station_name] * y_step
    
    if (nrow(station_data) > 0) {
      clean_temps <- station_data$Temp
      
      if (length(clean_temps) > 0) {
        adjust_factor <- 0.8
        
        dens <- try(density(clean_temps,
                       adjust = adjust_factor,
                       n = 1024,
                       kernel = "gaussian",
                       from = x_min,
                       to = x_max), silent = TRUE)
        
        if (!inherits(dens, "try-error")) {
          scale_factor <- 1.8
          normalized_density <- dens$y / max(dens$y) * scale_factor
          
          y_values <- base_y + normalized_density * y_step
          y_baseline <- rep(base_y, length(dens$x))
          
          stats <- station_stats %>% filter(Station == station_name)
          
          # Calculate color index based on mean temperature
          color_pos <- (stats$mean_temp - mean_temp_range[1]) / 
                      (mean_temp_range[2] - mean_temp_range[1])
          color_index <- 1 + floor(color_pos * 99)
          station_color <- temp_colors[color_index]
          
          # Create semi-transparent version for fill
          fill_color <- sprintf("rgba(%d, %d, %d, 0.3)",
                              col2rgb(station_color)[1],
                              col2rgb(station_color)[2],
                              col2rgb(station_color)[3])
          
          # Simplified hover text without valid readings count
          hover_text <- sprintf(
            "Station: %s<br>Mean: %.1f°C<br>Max: %.1f°C at %s<br>Min: %.1f°C at %s<br>Std Dev: %.1f°C",
            station_name, 
            stats$mean_temp,
            stats$max_temp,
            stats$max_temp_time,
            stats$min_temp,
            stats$min_temp_time,
            stats$sd_temp
          )
          
          p <- p %>% add_trace(
            x = dens$x,
            y = y_baseline,
            type = 'scatter',
            mode = 'lines',
            line = list(
              width = 0.25,
              color = sprintf("rgba(%d, %d, %d, 0.05)",
                            col2rgb(station_color)[1],
                            col2rgb(station_color)[2],
                            col2rgb(station_color)[3])
            ),
            showlegend = FALSE,
            hoverinfo = 'skip'
          )
          
          p <- p %>% add_trace(
            x = dens$x,
            y = y_values,
            type = 'scatter',
            mode = 'lines',
            fill = 'tonexty',
            fillcolor = fill_color,
            line = list(
              color = station_color,
              width = 2,
              shape = 'spline'
            ),
            showlegend = FALSE,
            text = hover_text,
            hoverinfo = 'text'
          )
        }
      }
    }
  }
  
  # Add a colorbar to show temperature scale
  p <- p %>% add_trace(
    x = c(x_min),
    y = c(0),
    type = "scatter",
    mode = "markers",
    marker = list(
      colorscale = list(
        list(0, temp_colors[1]),
        list(1, temp_colors[length(temp_colors)])
      ),
      showscale = TRUE,
      colorbar = list(
        title = "Mean Temperature (°C)",
        len = 0.5,
        y = 0.5
      ),
      cmin = mean_temp_range[1],
      cmax = mean_temp_range[2]
    ),
    showlegend = FALSE,
    hoverinfo = "none",
    visible = FALSE
  )
  
  p %>% layout(
    title = list(
      text = "Temperature Distribution by Station",
      font = list(size = 18)
    ),
    xaxis = list(
      title = "Temperature (°C)",
      range = c(x_min, x_max),
      gridcolor = '#E2E2E2',
      showgrid = TRUE,
      zeroline = FALSE,
      tickfont = list(size = 12),
      dtick = 10
    ),
    yaxis = list(
      title = "",
      showgrid = FALSE,
      zeroline = FALSE,
      ticktext = rev(station_order),
      tickvals = seq_along(station_order) * y_step,
      tickmode = "array",
      tickfont = list(size = 12)
    ),
    showlegend = FALSE,
    hovermode = "closest",
    plot_bgcolor = "#f8f8f8",
    paper_bgcolor = "#f8f8f8",
    margin = list(t = 50, b = 50, l = 120, r = 50)
  )
}
# Create summary bar chart for constant temperature periods
create_constant_temp_summary <- function(constant_temp_data, selected_station = NULL) {
  if (is.null(constant_temp_data) || length(constant_temp_data) == 0) {
    return(plot_ly() %>% layout(title = "No temperature data available"))
  }
  
  # Filter data based on selected station
  if (!is.null(selected_station) && selected_station != "All Stations") {
    if (!selected_station %in% names(constant_temp_data)) {
      return(plot_ly() %>% layout(title = "No data available for selected station"))
    }
    constant_temp_data <- constant_temp_data[selected_station]
  }
  
  # Calculate summary statistics for each station and each constant temperature span
  summary_data <- lapply(names(constant_temp_data), function(station_name) {
    results <- constant_temp_data[[station_name]]
    if (!is.null(results) && nrow(results) > 0) {
      clean_station_name <- sub("^dbo\\.", "", station_name)
      
      # Create a data frame with individual spans
      spans <- data.frame(
        Station = clean_station_name,
        Temperature = results$temp,
        Percentage = results$percentage_constant
      )
      
      # Sort by percentage contribution (descending)
      spans <- spans[order(-spans$Percentage), ]
      
      # Add cumulative sum for stacking
      spans$cumsum <- cumsum(spans$Percentage)
      spans$cumsum_prev <- c(0, head(spans$cumsum, -1))
      
      return(spans)
    }
  })
  
  # Remove NULL entries and combine all summaries
  summary_df <- do.call(rbind, summary_data[!sapply(summary_data, is.null)])
  
  if (is.null(summary_df) || nrow(summary_df) == 0) {
    return(plot_ly() %>% layout(title = "No temperature data available"))
  }
  
  # Sort stations by total constant temperature percentage
  station_totals <- aggregate(Percentage ~ Station, summary_df, sum)
  station_order <- station_totals$Station[order(-station_totals$Percentage)]
  summary_df$Station <- factor(summary_df$Station, levels = station_order)
  
  # Create the horizontal stacked bar chart
  p <- plot_ly(height = 400) %>%
    add_trace(
      data = summary_df,
      y = ~Station,
      x = ~Percentage,
      customdata = ~Temperature,
      type = "bar",
      orientation = 'h',
      marker = list(
        color = ~Temperature,
        colorscale = 'Viridis',
        showscale = TRUE,
        colorbar = list(
          title = "Temperature (°C)",
          len = 0.5,
          y = 0.5
        )
      ),
      text = ~sprintf("%.1f°C (%.1f%%)", Temperature, Percentage),
      textposition = "auto",
      hovertemplate = paste(
        "Station: %{y}<br>",
        "Temperature: %{customdata}°C<br>",
        "Contribution: %{x:.1f}%<br>",
        "<extra></extra>"
      )
    ) %>%
    layout(
      barmode = "stack",
      showlegend = FALSE,
      margin = list(t = 50, b = 80, l = 120, r = 20),
      xaxis = list(
        title = list(
          text = "Percentage of Time (%)",
          standoff = 25
        ),
        range = c(0, 100)
      ),
      yaxis = list(
        title = "",
        automargin = TRUE
      ),
      title = list(
        text = "Constant Temperature Spans by Station",
        font = list(size = 14)
      ),
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor = "#FFFFFF"
    )
  
  return(p)
}


create_constant_temp_plot <- function(constant_temp_data, selected_station) {
  # Input validation
  if (is.null(constant_temp_data) || length(constant_temp_data) == 0) {
    return(plot_ly() %>% layout(title = "No temperature data available"))
  }
  
  # Filter data based on selected station
  temp_data <- constant_temp_data
  if (!is.null(selected_station) && selected_station != "All Stations") {
    if (!selected_station %in% names(temp_data)) {
      return(plot_ly() %>% layout(title = "No data available for selected station"))
    }
    temp_data <- temp_data[selected_station]
  }
  
  # Initialize data frame for plot data
  plot_data <- data.frame(
    station = character(),
    start_time = as.POSIXct(character()),
    datetime_str = character(),
    percentage = numeric(),
    temperature = numeric(),
    duration = numeric(),
    angle = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Process data for each station
  for (table_name in names(temp_data)) {
    results <- temp_data[[table_name]]
    if (!is.null(results) && nrow(results) > 0) {
      clean_table_name <- sub("^dbo\\.", "", table_name)
      
      for (i in seq_len(nrow(results))) {
        # Convert start time safely
        start_time <- if (inherits(results$period_start[i], "POSIXct")) {
          results$period_start[i]
        } else {
          as.POSIXct(results$period_start[i], format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
        }
        
        if (!is.na(start_time)) {
          # Calculate angle for polar plot (24-hour clock)
          hour <- as.numeric(format(start_time, "%H"))
          minute <- as.numeric(format(start_time, "%M"))
          angle <- (90 - (hour + minute/60) * 360 / 24) %% 360
          
          # Add row to plot data
          new_row <- data.frame(
            station = clean_table_name,
            start_time = start_time,
            datetime_str = format(start_time, "%Y-%b-%d %H:%M:%S"),
            percentage = results$percentage_constant[i],
            temperature = results$temp[i],
            duration = results$duration_entries[i],
            angle = angle,
            stringsAsFactors = FALSE
          )
          plot_data <- rbind(plot_data, new_row)
        }
      }
    }
  }
  
  # Check if we have any data to plot
  if (nrow(plot_data) == 0) {
    return(plot_ly(height = 400) %>% 
           layout(title = "No constant temperature periods found"))
  }
  
  # Create hover text
  plot_data$hover_text <- with(plot_data, 
    sprintf(
      "Station: %s<br>Start Time: %s<br>Temperature: %.2f°C<br>Duration: %d entries<br>Percentage: %.1f%%",
      station, datetime_str, temperature, duration, percentage
    )
  )
  
  # Create the polar plot
  p <- plot_ly(
    data = plot_data,
    r = ~percentage,
    theta = ~angle,
    type = "scatterpolar",
    mode = "markers",
    text = ~hover_text,
    hoverinfo = "text",
    marker = list(
      color = ~temperature,
      colorscale = "Viridis",
      colorbar = list(
        title = "Temperature (°C)",
        len = 0.5,
        y = 0.5
      ),
      showscale = TRUE,
      size = 12,
      symbol = "circle",
      line = list(
        color = '#000000',
        width = 1
      )
    )
  ) %>%
    layout(
      polar = list(
        radialaxis = list(
          title = "Percentage of Total Readings",
          range = c(0, max(plot_data$percentage, na.rm = TRUE) * 1.1),
          ticksuffix = "%",
          gridcolor = '#E2E2E2',
          tickfont = list(size = 10)
        ),
        angularaxis = list(
          ticktext = c("00:00", "06:00", "12:00", "18:00"),
          tickvals = c(90, 0, 270, 180),
          direction = "counterclockwise",
          rotation = 0,
          gridcolor = '#E2E2E2',
          tickfont = list(size = 10)
        )
      ),
      showlegend = FALSE,
      title = list(
        text = "Constant Temperature Periods by Time of Day",
        font = list(size = 16)
      ),
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor = "#FFFFFF",
      margin = list(t = 50, b = 50, l = 50, r = 50)
    )
  
  return(p)
}

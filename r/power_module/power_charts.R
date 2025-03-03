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

# r/power_module/power_charts.R

create_vbat_waterfall_plot <- function(power_data, selected_station) {
  if (is.null(power_data) || selected_station == "All Stations") {
    return(plot_ly() %>% 
             layout(title = "Select a station to view battery voltage changes"))
  }

  station_data <- power_data[[selected_station]]$data

  if (is.null(station_data) || 
      !all(c("Vbat", "Vslr", "Temp") %in% names(station_data)) || 
      nrow(station_data) == 0 ||
      all(is.na(station_data$Vbat))) {
    return(plot_ly() %>% 
             layout(title = "No battery voltage data available for this station"))
  }

  # Convert DateTimeNum to POSIXct if needed
  if (!inherits(station_data$DateTimeNum, "POSIXct")) {
    station_data$DateTimeNum <- as.POSIXct(station_data$DateTimeNum, 
                                          format = "%Y-%b-%d %H:%M:%S",
                                          tz = "UTC")
  }

  # Sort data by time and remove NA values
  station_data <- station_data[order(station_data$DateTimeNum), ]
  valid_data <- !is.na(station_data$Vbat) & !is.na(station_data$DateTimeNum)
  station_data <- station_data[valid_data, ]

  if (nrow(station_data) < 2) {
    return(plot_ly() %>% 
             layout(title = "Insufficient valid data points for visualization"))
  }

  # Initialize plot data
  initial_value <- station_data$Vbat[1]
  plot_data <- data.frame(
    measure = station_data$DateTimeNum[-1],
    value = diff(station_data$Vbat),
    vslr = station_data$Vslr[-1],
    temp = station_data$Temp[-1]
  )

  # Add type column for coloring
  plot_data$type <- ifelse(plot_data$value >= 0, "charging", "discharging")

  # Calculate cumulative sums starting from initial value
  plot_data$cumulative <- initial_value + cumsum(plot_data$value)

  # Create text labels for the bars
  plot_data$bar_text <- sprintf("%+.2fV", plot_data$value)

  # Calculate y-axis ranges for battery voltage
  battery_values <- c(plot_data$cumulative - plot_data$value, plot_data$cumulative)
  batt_y_min <- min(battery_values, na.rm = TRUE)
  batt_y_max <- max(battery_values, na.rm = TRUE)
  batt_y_range <- batt_y_max - batt_y_min
  batt_y_padding <- batt_y_range * 0.1
  
  # Separate y-axis range for solar voltage
  solar_y_min <- if(all(is.na(plot_data$vslr))) 0 else min(plot_data$vslr, na.rm = TRUE)
  solar_y_max <- if(all(is.na(plot_data$vslr))) 20 else max(plot_data$vslr, na.rm = TRUE)
  solar_y_range <- solar_y_max - solar_y_min
  solar_y_padding <- solar_y_range * 0.1

  # Create hover text
  plot_data$hover_text <- sprintf(
    "Time: %s\nBattery Voltage: %.2f V\nChange: %+.2f V\nSolar Voltage: %.2f V",
    format(plot_data$measure, "%Y-%m-%d %H:%M:%S"),
    plot_data$cumulative,
    plot_data$value,
    ifelse(is.na(plot_data$vslr), 0, plot_data$vslr)
  )

  # Create the plot with dual y-axes
  p <- plot_ly()
  
  # Add 12V reference line using battery voltage axis
  p <- p %>% add_trace(
    x = range(plot_data$measure),
    y = c(12, 12),
    type = "scatter",
    mode = "lines",
    line = list(
      color = "black",
      width = 0.5
    ),
    showlegend = FALSE,
    hoverinfo = "none",
    yaxis = "y"
  )

  # Add background shading for solar voltage
  for(i in 1:(nrow(plot_data) - 1)) {
    norm_vslr <- if (is.na(plot_data$vslr[i])) {
      0
    } else {
      (plot_data$vslr[i] - solar_y_min) / solar_y_range
    }
    
    color <- if (is.na(norm_vslr)) {
      "rgba(200, 200, 200, 0.1)"
    } else {
      r <- round(30 + (255 - 30) * norm_vslr)
      g <- round(144 + (255 - 144) * norm_vslr)
      b <- round(255 - 220 * norm_vslr)
      opacity <- 0.3
      sprintf("rgba(%d, %d, %d, %f)", r, g, b, opacity)
    }
    
    p <- p %>% add_trace(
      x = c(plot_data$measure[i], plot_data$measure[i],
            plot_data$measure[i+1], plot_data$measure[i+1], 
            plot_data$measure[i]),
      y = c(batt_y_min - batt_y_padding, batt_y_max + batt_y_padding, 
            batt_y_max + batt_y_padding, batt_y_min - batt_y_padding, 
            batt_y_min - batt_y_padding),
      type = "scatter",
      mode = "lines",
      fill = "toself",
      fillcolor = color,
      line = list(width = 0, color = "transparent"),
      showlegend = FALSE,
      hoverinfo = "none",
      name = ""
    )
  }

  # Add solar voltage trace on secondary y-axis
  if (!all(is.na(plot_data$vslr))) {
    p <- p %>% add_trace(
      data = plot_data,
      x = ~measure,
      y = ~vslr,
      type = "scatter",
      mode = "lines",
      line = list(
        color = "rgba(255, 215, 0, 0.8)",  # Golden yellow for solar
        width = 2,
        dash = "dot"  
      ),
      name = "Solar Voltage",
      yaxis = "y3",  # Use separate y-axis for solar voltage
      hovertemplate = paste0(
        "Time: %{x|%Y-%m-%d %H:%M:%S}<br>",
        "Solar Voltage: %{y:.2f}V",
        "<extra></extra>"
      )
    )
  }

  # Add temperature trace if temperature data exists
  if (!all(is.na(plot_data$temp))) {
    p <- p %>% add_trace(
      data = plot_data,
      x = ~measure,
      y = ~temp,
      type = "scatter",
      mode = "lines",
      line = list(
        color = "rgba(255, 165, 0, 0.5)",
        width = 1,
        dash = "dash"
      ),
      name = "Temperature",
      yaxis = "y2",
      hovertemplate = paste0(
        "Time: %{x|%Y-%m-%d %H:%M:%S}<br>",
        "Temperature: %{y:.1f}°C<br>",
        "Solar Voltage: ", sprintf("%.2f V", plot_data$vslr),
        "<extra></extra>"
      )
    )
  }

  # Add the main voltage change bars
  p <- p %>% add_trace(
    data = plot_data,
    x = ~measure,
    y = ~value,
    base = ~cumulative - value,
    type = "bar",
    marker = list(
      color = ~case_when(
        type == "charging" ~ "#00CC00",
        type == "discharging" ~ "#FF4444"
      ),
      opacity = 1
    ),
    text = ~bar_text,
    textposition = "inside",
    insidetextanchor = "middle",
    textangle = 0,
    textfont = list(
      color = "white",
      size = 10
    ),
    hovertext = ~hover_text,
    hoverinfo = "text",
    name = "Battery Change",
    yaxis = "y"
  )

  # Complete the layout with separate y-axes
  p <- p %>% layout(
    title = list(
      text = paste("Battery and Solar Voltage -", selected_station),
      font = list(size = 18)
    ),
    xaxis = list(
      title = "Time",
      tickangle = -45,
      tickformat = "%Y-%m-%d %H:%M:%S",
      showgrid = TRUE,
      gridcolor = "#E2E2E2"
    ),
    yaxis = list(
      title = "Battery Voltage (V)",
      zeroline = FALSE,
      showgrid = TRUE,
      gridcolor = "#E2E2E2",
      range = c(batt_y_min - batt_y_padding, batt_y_max + batt_y_padding)
    ),
    yaxis2 = list(
      title = "Temperature (°C)",
      overlaying = "y",
      side = "right",
      showgrid = FALSE,
      zeroline = FALSE
    ),
    yaxis3 = list(
      title = "Solar Voltage (V)",
      overlaying = "y",
      side = "right",
      position = 0.85,  # Position the axis slightly left of the temperature axis
      showgrid = FALSE,
      zeroline = FALSE,
      range = c(solar_y_min - solar_y_padding, solar_y_max + solar_y_padding)
    ),
    showlegend = TRUE,
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    margin = list(b = 120, l = 80, r = 120),
    legend = list(
      orientation = "h",
      x = 0.5,
      y = -0.2,
      xanchor = "center",
      yanchor = "top"
    )
  )

  return(p)
}

# Function to create normalized voltage trends plot
create_vbat_trends_plot <- function(trend_data, selected_stations = NULL) {
  if (is.null(trend_data) || length(trend_data) == 0) {
    return(plot_ly() %>%
             layout(title = "No trend data available",
                    xaxis = list(title = "Time"),
                    yaxis = list(title = "Battery Voltage (V)"),
                    plot_bgcolor = "#f8f8f8",
                    paper_bgcolor = "#f8f8f8"))
  }
  
  # Handle station selection
  plot_stations <- if (is.null(selected_stations) || "All Stations" %in% selected_stations) {
    names(trend_data)
  } else {
    intersect(selected_stations, names(trend_data))
  }
  
  if (length(plot_stations) == 0) {
    return(plot_ly() %>%
             layout(title = "No stations selected",
                    xaxis = list(title = "Time"),
                    yaxis = list(title = "Battery Voltage (V)"),
                    plot_bgcolor = "#f8f8f8",
                    paper_bgcolor = "#f8f8f8"))
  }
  
  # Initialize plot
  p <- plot_ly()
  
  # Enhanced color palette with better contrast
  colors <- c("#1f77b4", "#d62728", "#2ca02c", "#ff7f0e", "#9467bd",
              "#8c564b", "#e377c2", "#17becf", "#bcbd22", "#7f7f7f")
  
  # Find overall time range for all data
  all_times <- vector("list", length(plot_stations))
  for (station_idx in seq_along(plot_stations)) {
    station <- plot_stations[station_idx]
    station_data <- trend_data[[station]]
    
    if (!is.null(station_data) && nrow(station_data) > 0) {
      times <- if (!inherits(station_data$DateTimeNum, "POSIXct")) {
        as.POSIXct(station_data$DateTimeNum, format = "%Y-%b-%d %H:%M:%S", tz = "UTC")
      } else {
        station_data$DateTimeNum
      }
      all_times[[station_idx]] <- times
    }
  }
  
  # Calculate overall time range
  all_times <- do.call(c, all_times)
  time_range <- range(all_times, na.rm = TRUE)
  
  # Add critical voltage reference line with proper time range but hidden from legend
  p <- p %>% add_trace(
    x = time_range,
    y = c(12, 12),
    type = "scatter",
    mode = "lines",
    line = list(color = "red", width = 1, dash = "dash"),
    showlegend = FALSE,
    hoverinfo = "skip"
  )
  
  # Process and plot data for each station
  for (station_idx in seq_along(plot_stations)) {
    station <- plot_stations[station_idx]
    station_data <- trend_data[[station]]
    
    if (!is.null(station_data) && nrow(station_data) > 0) {
      # Split data by trend_id
      trend_splits <- split(station_data, station_data$trend_id)
      
      for (trend_id in names(trend_splits)) {
        trend_subset <- trend_splits[[trend_id]]
        
        if (nrow(trend_subset) < 2) next
        
        # Ensure proper datetime format
        if (!inherits(trend_subset$DateTimeNum, "POSIXct")) {
          trend_subset$plot_time <- as.POSIXct(trend_subset$DateTimeNum, 
                                              format = "%Y-%b-%d %H:%M:%S",
                                              tz = "UTC")
        } else {
          trend_subset$plot_time <- trend_subset$DateTimeNum
        }
        
        # Sort by time
        trend_subset <- trend_subset[order(trend_subset$plot_time), ]
        
        # Calculate metrics for the trend
        initial_voltage <- trend_subset$Vbat[1]
        final_voltage <- trend_subset$Vbat[nrow(trend_subset)]
        time_diff_hours <- as.numeric(difftime(max(trend_subset$plot_time), 
                                             min(trend_subset$plot_time), 
                                             units = "hours"))
        voltage_change <- final_voltage - initial_voltage
        rate_of_change <- if(time_diff_hours > 0) voltage_change / time_diff_hours else 0
        
        # Calculate line width based on rate of change
        normalized_rate <- abs(rate_of_change) / 0.5  # Normalize against 0.5V/hr
        line_width <- 1 + (normalized_rate * 2)  # Scale from 1 to 3 pixels
        
        # Create hover text with original information
        hover_text <- sprintf(
          "Station: %s<br>Start Time: %s<br>End Time: %s<br>Initial Voltage: %.2f V<br>Final Voltage: %.2f V<br>Total Change: %.1f%%<br>Rate: %.3f V/hr",
          station,
          format(min(trend_subset$plot_time), "%Y-%m-%d %H:%M"),
          format(max(trend_subset$plot_time), "%Y-%m-%d %H:%M"),
          initial_voltage,
          final_voltage,
          (voltage_change / initial_voltage) * 100,
          rate_of_change
        )
        
        # Add trace for this trend with enhanced styling
        color_index <- ((station_idx - 1) %% length(colors)) + 1
        p <- p %>% add_trace(
          data = trend_subset,
          x = ~plot_time,
          y = ~Vbat,
          type = 'scatter',
          mode = 'lines+markers',
          name = sprintf("%s - Trend %s (%.3f V/hr)", station, trend_id, rate_of_change),
          text = hover_text,
          hoverinfo = 'text',
          line = list(
            color = colors[color_index],
            width = line_width,
            shape = 'spline'
          ),
          marker = list(
            size = 6,
            color = colors[color_index]
          ),
          showlegend = TRUE
        )
      }
    }
  }
  
  # Calculate time padding (5% of total range)
  time_padding <- as.numeric(difftime(time_range[2], time_range[1], units = "secs")) * 0.05
  x_min <- time_range[1] - time_padding
  x_max <- time_range[2] + time_padding
  
  # Enhanced layout with proper time range
  p %>% layout(
    title = list(
      text = "Decreasing Vbat Trends",
      font = list(size = 18)
    ),
    xaxis = list(
      title = "Time",
      gridcolor = '#E2E2E2',
      showgrid = TRUE,
      zeroline = FALSE,
      tickformat = "%Y-%m-%d %H:%M",
      tickangle = -45,
      range = c(x_min, x_max)
    ),
    yaxis = list(
      title = "Battery Voltage (V)",
      gridcolor = '#E2E2E2',
      showgrid = TRUE,
      zeroline = TRUE,
      zerolinecolor = '#969696'
    ),
    showlegend = TRUE,
    hovermode = "closest",
    legend = list(
      orientation = "v",
      x = 1.02,
      y = 1,
      xanchor = "left",
      yanchor = "top",
      bgcolor = "rgba(255, 255, 255, 0.9)",
      bordercolor = "rgba(0, 0, 0, 0.2)",
      borderwidth = 1
    ),
    plot_bgcolor = "#f8f8f8",
    paper_bgcolor = "#f8f8f8",
    margin = list(t = 50, b = 50, l = 60, r = 150)
  )
}

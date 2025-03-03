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

# crmp_module/crmp_charts.R

# Create a precipitation plot for CRMP stations
create_precip_plot <- function(wx_stations, selected_station) {
  if (is.null(selected_station) || selected_station == "All Stations") {
    return(plot_ly(height = 300) %>% 
             layout(
               title = list(
                 text = "Select a station to view precipitation measurements",
                 font = list(size = 18),
                 y = 0.95
               ),
               margin = list(t = 100, b = 40, l = 60, r = 40),
               xaxis = list(title = "Date and Time"),
               yaxis = list(title = "Precipitation (mm)"),
               plot_bgcolor = "#f8f8f8",
               paper_bgcolor = "#f8f8f8"
             ))
  }
  
  station_data <- wx_stations[[selected_station]]
  
  # Determine which precipitation columns have data
  has_pc2_data <- any(!is.na(station_data$PrecipPC2))
  has_op2_data <- any(!is.na(station_data$PrecipOP2))
  has_op1_data <- any(!is.na(station_data$PrecipOP1))
  has_rit_data <- any(!is.na(station_data$PrecipRIT))
  has_pc_data <- any(!is.na(station_data$PC))
  has_raw_data <- any(!is.na(station_data$Pcp_raw))
  
  # Check if any precipitation data is available
  if (!has_pc2_data && !has_op2_data && !has_op1_data && 
      !has_rit_data && !has_pc_data && !has_raw_data) {
    return(plot_ly(height = 300) %>% 
             layout(
               title = list(
                 text = paste("No precipitation data available for", selected_station),
                 font = list(size = 18),
                 y = 0.95
               ),
               margin = list(t = 100, b = 40, l = 60, r = 40),
               xaxis = list(title = "Date and Time"),
               yaxis = list(title = "Precipitation (mm)"),
               plot_bgcolor = "#f8f8f8",
               paper_bgcolor = "#f8f8f8"
             ))
  }
  
  # Sort data chronologically by the original DateTimeNum string
  station_data <- station_data %>%
  mutate(DateTimeNum = as.POSIXct(DateTimeNum, format = "%Y-%b-%d %H:%M:%S", tz = "UTC")) %>%
  arrange(DateTimeNum)
  
  # Initialize plot
  p <- plot_ly(height = 300)
  
  # Generate colors for all possible sensors
  sensor_colors <- viridis::viridis(6)
  
  # Add PC2 data if available
  if (has_pc2_data) {
    p <- p %>% add_trace(
      data = station_data,
      x = ~DateTimeNum,
      y = ~PrecipPC2,
      type = 'scatter',
      mode = 'lines+markers',
      name = 'PC2',
      line = list(color = sensor_colors[1], width = 2),
      marker = list(size = 6)
    )
  }
  
  # Add OP2 data if available
  if (has_op2_data) {
    p <- p %>% add_trace(
      data = station_data,
      x = ~DateTimeNum,
      y = ~PrecipOP2,
      type = 'scatter',
      mode = 'lines+markers',
      name = 'OP2',
      line = list(color = sensor_colors[2], width = 2),
      marker = list(size = 6)
    )
  }
  
  # Add OP1 data if available
  if (has_op1_data) {
    p <- p %>% add_trace(
      data = station_data,
      x = ~DateTimeNum,
      y = ~PrecipOP1,
      type = 'scatter',
      mode = 'lines+markers',
      name = 'OP1',
      line = list(color = sensor_colors[3], width = 2),
      marker = list(size = 6)
    )
  }
  
  # Add RIT data if available
  if (has_rit_data) {
    p <- p %>% add_trace(
      data = station_data,
      x = ~DateTimeNum,
      y = ~PrecipRIT,
      type = 'scatter',
      mode = 'lines+markers',
      name = 'RIT',
      line = list(color = sensor_colors[4], width = 2),
      marker = list(size = 6)
    )
  }
  
  # Add PC data if available
  if (has_pc_data) {
    p <- p %>% add_trace(
      data = station_data,
      x = ~DateTimeNum,
      y = ~PC,
      type = 'scatter',
      mode = 'lines+markers',
      name = 'PC',
      line = list(color = sensor_colors[5], width = 2),
      marker = list(size = 6)
    )
  }
  
  # Add Pcp_raw data if available
  if (has_raw_data) {
    p <- p %>% add_trace(
      data = station_data,
      x = ~DateTimeNum,
      y = ~Pcp_raw,
      type = 'scatter',
      mode = 'lines+markers',
      name = 'Raw',
      line = list(color = sensor_colors[6], width = 2),
      marker = list(size = 6)
    )
  }
  
  # Create a list of available sensors for the title
  available_sensors <- c(
    if(has_pc2_data) "PC2" else NULL,
    if(has_op2_data) "OP2" else NULL,
    if(has_op1_data) "OP1" else NULL,
    if(has_rit_data) "RIT" else NULL,
    if(has_pc_data) "PC" else NULL,
    if(has_raw_data) "Raw" else NULL
  )
  
  # Configure layout
  p %>% layout(
    title = list(
      text = paste(
        "Cumulative precip -", 
        selected_station,
        "\nwith", 
        paste(available_sensors, collapse = ", ")
      ),
      y = 0.95
    ),
    margin = list(t = 100, b = 40, l = 60, r = 40),
    xaxis = list(
      title = "Date and Time",
      gridcolor = "#e6e6e6"
    ),
    yaxis = list(
      title = "Precipitation (mm)",
      gridcolor = "#e6e6e6"
    ),
    hovermode = "closest",
    plot_bgcolor = "#f8f8f8",
    paper_bgcolor = "#f8f8f8"
  ) %>%
  config(
    displayModeBar = TRUE,
    scrollZoom = TRUE
  )
}

#' Create a snow depth plot
create_sdepth_plot <- function(WX_stations, selected_station) {
  if (is.null(selected_station) || selected_station == "All Stations") {
    return(plot_ly() %>% 
      layout(title = list(
        text = "Select a station to view snow depth measurements",
        font = list(size = 18)
      ),
      xaxis = list(title = "Date and Time"),
      yaxis = list(title = "Snow Depth (cm)"),
      plot_bgcolor = "#f8f8f8",
      paper_bgcolor = "#f8f8f8"))
  }
  
  station_data <- WX_stations[[selected_station]]
  
  if (is.null(station_data)) {
    return(plot_ly() %>% 
      layout(title = list(
        text = paste("No data available for", selected_station),
        font = list(size = 18)
      ),
      xaxis = list(title = "Date and Time"),
      yaxis = list(title = "Snow Depth (cm)"),
      plot_bgcolor = "#f8f8f8",
      paper_bgcolor = "#f8f8f8"))
  }
  
  # Check for both types of snow depth data
  has_sdepth <- "SDepth" %in% names(station_data) && any(!is.na(station_data$SDepth))
  has_sd <- "SD" %in% names(station_data) && any(!is.na(station_data$SD))
  
  if (!has_sdepth && !has_sd) {
    return(plot_ly() %>% 
      layout(title = list(
        text = paste("No snow depth data available for", selected_station),
        font = list(size = 18)
      ),
      xaxis = list(title = "Date and Time"),
      yaxis = list(title = "Snow Depth (cm)"),
      plot_bgcolor = "#f8f8f8",
      paper_bgcolor = "#f8f8f8"))
  }
  
  # Sort data chronologically by the original DateTimeNum string
  station_data <- station_data %>%
  mutate(DateTimeNum = as.POSIXct(DateTimeNum, format = "%Y-%b-%d %H:%M:%S", tz = "UTC")) %>%
  arrange(DateTimeNum)
  
  # Initialize plot
  p <- plot_ly()
  
  # Generate distinct colors for the two sensors
  sensor_colors <- viridis::viridis(2)
  
  # Add SDepth data if available
  if (has_sdepth) {
    p <- p %>% add_trace(
      data = station_data,
      x = ~DateTimeNum, 
      y = ~SDepth, 
      type = 'scatter', 
      mode = 'lines+markers',
      name = 'SDepth',
      line = list(color = sensor_colors[1], width = 2),
      marker = list(size = 6)
    )
  }
  
  # Add SD data if available
  if (has_sd) {
    p <- p %>% add_trace(
      data = station_data,
      x = ~DateTimeNum, 
      y = ~SD, 
      type = 'scatter', 
      mode = 'lines+markers',
      name = 'SD',
      line = list(color = sensor_colors[2], width = 2),
      marker = list(size = 6)
    )
  }
  
  # Create sensor list for title
  available_sensors <- c(
    if(has_sdepth) "SDepth" else NULL,
    if(has_sd) "SD" else NULL
  )
  
  # Configure layout
  p %>% layout(
    title = list(
      text = paste(
        "Snow Depth Measurements -", 
        selected_station,
        "\nwith",
        paste(available_sensors, collapse = ", ")
      )
    ),
    xaxis = list(title = "Date and Time", gridcolor = "#e6e6e6"),
    yaxis = list(title = "Snow Depth (cm)", gridcolor = "#e6e6e6"),
    hovermode = "closest",
    plot_bgcolor = "#f8f8f8",
    paper_bgcolor = "#f8f8f8"
  ) %>%
  config(displayModeBar = TRUE, scrollZoom = TRUE)
}

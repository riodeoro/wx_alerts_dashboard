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

# r/precipitation_module/precipitation_charts.R

create_rn1_outliers_plot <- function(WX_stations, all_rn1_outliers, selected_station) {
  # Handle case when no station is selected
  if (is.null(selected_station) || selected_station == "All Stations") {
    return(plot_ly(height = 400) %>% 
      layout(
        title = list(
          text = "Select a station to view precipitation outliers",
          font = list(size = 18)
        ),
        xaxis = list(title = "Time"),
        yaxis = list(title = "Value"),
        plot_bgcolor = "#ffffff",
        paper_bgcolor = "#ffffff",
        showlegend = FALSE
      ))
  }
  
  # Check if data exists for selected station
  if (is.null(WX_stations) || is.null(WX_stations[[selected_station]])) {
    return(plot_ly(height = 400) %>% 
      layout(
        title = list(
          text = "No data available for selected station",
          font = list(size = 18)
        ),
        xaxis = list(title = "Time"),
        yaxis = list(title = "Value"),
        plot_bgcolor = "#ffffff",
        paper_bgcolor = "#ffffff",
        showlegend = FALSE
      ))
  }
  
  # Get data for selected station and arrange chronologically
  station_data <- WX_stations[[selected_station]]
  
  # Verify station_data is not empty
  if (nrow(station_data) == 0) {
    return(plot_ly(height = 400) %>% 
      layout(
        title = list(
          text = "No data available for selected station",
          font = list(size = 18)
        ),
        xaxis = list(title = "Time"),
        yaxis = list(title = "Value"),
        plot_bgcolor = "#ffffff",
        paper_bgcolor = "#ffffff",
        showlegend = FALSE
      ))
  }
  
 station_data <- station_data %>%
  mutate(DateTimeNum = as.POSIXct(DateTimeNum, format = "%Y-%b-%d %H:%M:%S", tz = "UTC")) %>%
  arrange(DateTimeNum)
  
  outliers <- all_rn1_outliers[[selected_station]]
  
  # Handle case when no outliers exist
  if (is.null(outliers) || nrow(outliers) == 0) {
    return(plot_ly(height = 400) %>% 
      layout(
        title = list(
          text = paste("No precipitation outliers to display for", selected_station),
          font = list(size = 18)
        ),
        xaxis = list(title = "Time"),
        yaxis = list(title = "Value"),
        plot_bgcolor = "#ffffff",
        paper_bgcolor = "#ffffff",
        showlegend = FALSE
      ))
  }
  
outliers <- outliers %>%
  mutate(DateTimeNum = as.POSIXct(DateTimeNum, format = "%Y-%b-%d %H:%M:%S", tz = "UTC")) %>%
  arrange(DateTimeNum)
  
  # Calculate ranges for y-axes
  rh_range <- range(station_data$Rh, na.rm = TRUE)
  temp_range <- range(station_data$Temp, na.rm = TRUE)
  precip_range <- range(station_data$Rn_1, na.rm = TRUE)
  
  # Add padding to ranges
  add_padding <- function(range, padding = 0.1) {
    range_size <- diff(range)
    c(range[1] - range_size * padding, range[2] + range_size * padding)
  }
  
  rh_range <- add_padding(rh_range)
  temp_range <- add_padding(temp_range)
  precip_range <- add_padding(precip_range)
  
  # Create the multi-trace plot
  plot_ly(height = 400) %>%
    # Rest of the plotting code remains the same
    add_trace(
      data = station_data,
      x = ~DateTimeNum, 
      y = ~Temp, 
      type = 'scatter', 
      mode = 'lines', 
      name = 'Temperature',
      line = list(color = viridis::viridis(3)[1], width = 2),
      text = ~paste0("Time: ", DateTimeNum,
                    "<br>Temperature: ", sprintf("%.1f°C", Temp)),
      hoverinfo = 'text'
    ) %>%
    add_trace(
      data = station_data,
      x = ~DateTimeNum, 
      y = ~Rh, 
      type = 'scatter', 
      mode = 'lines', 
      name = 'Relative Humidity',
      yaxis = 'y2', 
      line = list(color = viridis::viridis(3)[2], width = 2),
      text = ~paste0("Time: ", DateTimeNum,
                    "<br>RH: ", sprintf("%.1f%%", Rh)),
      hoverinfo = 'text'
    ) %>%
    add_trace(
      data = station_data,
      x = ~DateTimeNum, 
      y = ~Rn_1, 
      type = 'scatter', 
      mode = 'lines', 
      name = 'Precipitation',
      yaxis = 'y3',
      line = list(color = viridis::viridis(3)[3], width = 2),
      text = ~paste0("Time: ", DateTimeNum,
                    "<br>Precipitation: ", sprintf("%.2f mm", Rn_1)),
      hoverinfo = 'text'
    ) %>%
    add_trace(
      data = outliers,
      x = ~DateTimeNum, 
      y = ~Rn_1, 
      type = 'scatter', 
      mode = 'markers', 
      name = 'Outliers',
      yaxis = 'y3',
      marker = list(
        color = "red",
        size = 10,
        symbol = 'diamond',
        line = list(color = "black", width = 1)
      ),
      text = ~paste0("Time: ", DateTimeNum,
                    "<br>Precipitation: ", sprintf("%.2f mm", Rn_1),
                    "<br>6-hour Average: ", sprintf("%.2f mm", RunningAvg)),
      hoverinfo = 'text'
    ) %>%
    layout(
      title = list(
        text = paste("Weather Conditions and Precipitation Outliers -", selected_station),
        font = list(size = 18),
        x = 0.02,
        xanchor = "left"
      ),
      xaxis = list(
        title = "Time",
        tickangle = -45,
        tickformat = "%Y-%m-%d %H:%M",
        showgrid = TRUE,
        gridcolor = "#E2E2E2",
        zeroline = FALSE,
        nticks = 10
      ),
      yaxis = list(
        title = "Temperature (°C)",
        range = temp_range,
        side = "left",
        showgrid = TRUE,
        gridcolor = "#E2E2E2",
        zeroline = TRUE,
        zerolinecolor = "#969696"
      ),
      yaxis2 = list(
        title = "Relative Humidity (%)",
        range = rh_range,
        side = "right",
        overlaying = "y",
        showgrid = FALSE,
        zeroline = FALSE
      ),
      yaxis3 = list(
        title = "Precipitation (mm)",
        range = precip_range,
        side = "right",
        overlaying = "y",
        showgrid = FALSE,
        zeroline = FALSE,
        anchor = "free",
        position = 0.85
      ),
      legend = list(
        orientation = "h",
        y = -0.2,
        x = 0.5,
        xanchor = "center",
        bgcolor = "rgba(255, 255, 255, 0.9)",
        bordercolor = "rgba(0, 0, 0, 0.2)",
        borderwidth = 1
      ),
      margin = list(
        l = 60,
        r = 60,
        b = 100,
        t = 60,
        pad = 4
      ),
      hovermode = "closest",
      plot_bgcolor = "#ffffff",
      paper_bgcolor = "#ffffff",
      showlegend = TRUE
    ) %>%
    config(
      displayModeBar = TRUE,
      scrollZoom = TRUE,
      modeBarButtonsToRemove = c(
        "select2d", "lasso2d", "autoScale2d",
        "hoverClosestCartesian", "hoverCompareCartesian"
      )
    )
}

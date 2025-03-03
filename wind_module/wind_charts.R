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

# r/wind_module/wind_charts.R

create_zero_wspd_plot <- function(zero_wspd_data, selected_station) {
  # Return empty plot if no data is available
  if (is.null(zero_wspd_data)) {
    return(plot_ly() %>% 
           layout(title = "No data available - please fetch data first",
                 xaxis = list(title = "Date and Time"),
                 yaxis = list(title = "Station")))
  }
  
  # Filter stations based on selection
  stations_to_display <- if (!is.null(selected_station) && selected_station != "All Stations") {
    selected_station
  } else {
    names(zero_wspd_data)
  }
  
  # Handle case where no stations are available
  if (length(stations_to_display) == 0) {
    return(plot_ly() %>% 
           layout(title = "No stations available",
                 xaxis = list(title = "Date and Time"),
                 yaxis = list(title = "Station")))
  }
  
  # Combine data from selected stations
  plot_data <- do.call(rbind, lapply(stations_to_display, function(station) {
    if (is.null(zero_wspd_data[[station]])) return(NULL)
    results <- zero_wspd_data[[station]]
    if (nrow(results) > 0) {
      results$Station <- sub("dbo\\.", "", station)
      return(results)
    }
    return(NULL)
  }))
  
  if (is.null(plot_data) || nrow(plot_data) == 0) {
    return(plot_ly() %>% 
           layout(title = "No hourly zero Wspd blocks detected",
                 xaxis = list(title = "Date and Time"),
                 yaxis = list(title = "Station")))
  }
  
  # Check if each period is ongoing
  current_time <- Sys.time()
  plot_data$is_ongoing <- abs(difftime(plot_data$period_end, current_time, units="mins")) < 60
  
  # Calculate dynamic color scale range based on filtered data
  color_range <- range(plot_data$dir_changes, na.rm = TRUE)
  
  p <- plot_ly() %>%
    add_trace(
      data = plot_data,
      x = ~period_start,
      y = ~Station,
      type = 'scatter',
      mode = 'markers',
      marker = list(
        size = ~count_consecutive/2 + 7,
        color = ~dir_changes,
        colorscale = 'Viridis',
        colorbar = list(
          title = "Direction Changes",
          # Set dynamic range for color scale
          cmin = color_range[1],
          cmax = color_range[2]
        )
      ),
      name = "Zero Wind Speed Periods",
      text = ~paste(
        "Station:", Station,
        "<br>Start:", format(period_start, "%Y-%m-%d %H:%M"),
        "<br>End:", ifelse(is_ongoing, "Currently ongoing", format(period_end, "%Y-%m-%d %H:%M")),
        "<br>Duration:", count_consecutive, "hours",
        "<br>Direction Changes:", dir_changes,
        "<br>Max Dir Change:", round(max_dir_change, 1), "°",
        ifelse(anomalous, "<br>*** Flagged ***", "")
      ),
      hoverinfo = 'text'
    ) %>%
    layout(
      title = paste0(
        "Hourly Zero Wspd with Dir Changes",
        if (!is.null(selected_station) && selected_station != "All Stations") 
          paste0(" - ", sub("dbo\\.", "", selected_station))
        else ""
      ),
      xaxis = list(title = "Date and Time"),
      yaxis = list(title = "Station"),
      showlegend = FALSE,
      hoverlabel = list(bgcolor = "white")
    )
  
  if (any(plot_data$anomalous)) {
    anomalous_data <- plot_data[plot_data$anomalous, ]
    p <- p %>%
      add_trace(
        data = anomalous_data,
        x = ~period_start,
        y = ~Station,
        type = 'scatter',
        mode = 'markers',
        marker = list(
          size = ~count_consecutive/2 + 10,
          line = list(color = 'red', width = 2),
          symbol = 'circle-open'
        ),
        name = "Flagged",
        showlegend = FALSE
      )
  }
  
  return(p)
}

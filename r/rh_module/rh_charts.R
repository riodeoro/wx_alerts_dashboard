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

# r/rh_module/rh_charts.R

create_consecutive_rh_plot <- function(wx_stations, selected_station = "All Stations") {
  consecutive_rh_results <- check_consecutive_rh(wx_stations)
  
  if (!is.data.frame(consecutive_rh_results) || nrow(consecutive_rh_results) == 0) {
    return(plot_ly(height = 400) %>%
      add_annotations(
        text = "No stations found with 12 or more consecutive hourly RH 100% readings",
        showarrow = FALSE,
        font = list(size = 14)
      ) %>%
      layout(margin = list(l = 60, r = 60, b = 100, t = 60, pad = 4)))
  }
  
  if (selected_station != "All Stations") {
    consecutive_rh_results <- consecutive_rh_results[consecutive_rh_results$Station == selected_station, ]
    if (nrow(consecutive_rh_results) == 0) {
      return(plot_ly(height = 400) %>%
        add_annotations(
          text = paste("No consecutive RH 100 readings found for station:", selected_station),
          showarrow = FALSE,
          font = list(size = 14)
        ))
    }
  }
  
  consecutive_rh_results$duration_hrs <- consecutive_rh_results$Consecutive_Count / 1
  
  # Create datasets for different conditions
  red_points <- consecutive_rh_results[consecutive_rh_results$Min_Temp > 0 & consecutive_rh_results$Total_Rn_1 == 0, ]
  orange_points <- consecutive_rh_results[consecutive_rh_results$Total_Rn_1 == 0 & consecutive_rh_results$Min_Temp <= 0, ]
  blue_points <- consecutive_rh_results[consecutive_rh_results$Total_Rn_1 > 0, ]
  
  # Calculate plot ranges
  max_duration <- max(consecutive_rh_results$duration_hrs, na.rm = TRUE)
  max_rain <- max(consecutive_rh_results$Total_Rn_1, na.rm = TRUE)
  
  if (!is.finite(max_duration)) max_duration <- 24
  if (!is.finite(max_rain)) max_rain <- 10
  
  y_range_min <- -max_rain * 0.1
  y_range_max <- max_rain * 1.1
  
  plot <- plot_ly(height = 400)
  
  # Function to add points with proper legend entries
  add_category_points <- function(plot, data, color, name) {
    if (nrow(data) > 0) {
      plot %>% add_trace(
        data = data,
        x = ~duration_hrs,
        y = ~Total_Rn_1,
        type = 'scatter',
        mode = 'markers+text',
        marker = list(size = 12, color = color),
        text = ~Station,
        textposition = "top center",
        name = name,
        hovertext = ~sprintf(
          "Station: %s<br>Min Temp: %.1f°C<br>Avg Temp: %.1f°C<br>Max Temp: %.1f°C<br>Total Rain: %.1f mm<br>Start Time: %s<br>End Time: %s",
          Station, Min_Temp, Avg_Temp, Max_Temp, Total_Rn_1, Start_Time, End_Time
        ),
        hoverinfo = "text",
        showlegend = TRUE,
        legendgroup = name
      )
    } else {
      # Add an invisible trace for legend when no points exist
      plot %>% add_trace(
        x = numeric(0),
        y = numeric(0),
        type = 'scatter',
        mode = 'markers',
        marker = list(size = 12, color = color),
        name = name,
        showlegend = TRUE,
        legendgroup = name
      )
    }
  }
  
  # Add all categories with proper legend entries
  plot <- plot %>%
    add_category_points(red_points, 'red', "Continuous positive temps (>0°C), no Rn_1") %>%
    add_category_points(orange_points, 'orange', "Mixed or negative temps, no Rn_1") %>%
    add_category_points(blue_points, 'blue', "Rn_1 recorded")
  
  # Configure layout with legend
  plot %>% layout(
    title = "100% RH Duration vs Rn_1",
    xaxis = list(
      title = "Duration of 100% RH (hours)",
      zeroline = TRUE,
      zerolinecolor = 'black',
      zerolinewidth = 2,
      range = c(0, max_duration * 1.1)
    ),
    yaxis = list(
      title = "Total Precipitation (mm)",
      zeroline = TRUE,
      zerolinecolor = 'black',
      zerolinewidth = 2,
      range = c(y_range_min, y_range_max)
    ),
    showlegend = TRUE,
    legend = list(
      x = 1.02,
      y = 1,
      xanchor = 'left',
      yanchor = 'top',
      bgcolor = 'rgba(255, 255, 255, 0.9)',
      bordercolor = 'rgba(0, 0, 0, 0.5)',
      borderwidth = 1
    ),
    margin = list(r = 150)  # Add right margin to accommodate legend
  )
}

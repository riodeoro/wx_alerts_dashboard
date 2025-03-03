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

# r/db_module/db_charts.R

#' Create a plot showing missing entries across stations
#' @param missing_entries_data Data frame containing missing entries information
#' @param selected_station Selected station name (optional)
#' @return Plotly object showing missing entries over time
create_missing_entries_plot <- function(missing_entries_data, selected_station = NULL) {
  # Check for valid data
  if (!is.data.frame(missing_entries_data) || nrow(missing_entries_data) == 0) {
    return(plot_ly() %>% 
      layout(
        title = "No missing entries to display",
        xaxis = list(title = "Date and Time"),
        yaxis = list(title = "Station")
      ))
  }
  
  # Filter by selected station if specified
  missing_entries <- missing_entries_data
  if (!is.null(selected_station) && selected_station != "All Stations") {
    missing_entries <- missing_entries %>% filter(Table == selected_station)
  }
  
  # Ensure data after filtering
  if (nrow(missing_entries) == 0) {
    return(plot_ly() %>% 
      layout(
        title = "No data gaps to display for selected station",
        xaxis = list(title = "Date and Time"),
        yaxis = list(title = "Station")
      ))
  }
  
  # Convert datetime and filter out NA values
  missing_entries$MissingDateTime <- as.POSIXct(missing_entries$MissingDateTime, tz="UTC")
  missing_entries <- missing_entries %>% filter(!is.na(MissingDateTime))
  
  # Check for valid entries after datetime processing
  if (nrow(missing_entries) == 0) {
    return(plot_ly() %>% 
      layout(
        title = "No valid datetime entries to display",
        xaxis = list(title = "Date and Time"),
        yaxis = list(title = "Station")
      ))
  }
  
  # Create the plot
  plot_ly(
    missing_entries, 
    x = ~MissingDateTime, 
    y = ~Table, 
    type = 'scatter', 
    mode = 'markers',
    marker = list(size = 5, color = 'black'),
    hoverinfo = 'text',
    text = ~paste(
      "Station:", Table, 
      "<br>Missing Hour:", format(MissingDateTime, "%Y-%m-%d %H:00:00")
    )
  ) %>%
    layout(
      title = "Missing Hourly Entries by Station",
      xaxis = list(
        title = "Date and Time", 
        rangeslider = list(visible = TRUE)
      ),
      yaxis = list(title = "Station"),
      hoverlabel = list(bgcolor = "white"),
      showlegend = FALSE
    )
}

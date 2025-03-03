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

# ui_definition.R

library(shiny)
library(bslib)
library(plotly)
library(DT)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css"),
    
    tags$script(HTML("
      $(document).ready(function() {
        $('#fetch_controls_btn').click(function(e) {
          e.stopPropagation();
          $('.fetch-controls-dropdown').toggleClass('active');
        });

        $(document).click(function(e) {
          if (!$(e.target).closest('.fetch-controls-dropdown').length) {
            $('.fetch-controls-dropdown').removeClass('active');
          }
        });

        $('.fetch-controls-content').click(function(e) {
          e.stopPropagation();
        });
      });
    "))
  ),
  # Navbar
   tags$nav(class = "navbar navbar-default navbar-fixed-top",
    div(class = "container-fluid",
      div(class = "navbar-header",
        img(src = "www/gov3_bc_logo.png", class = "nav-logo"),
        span(class = "navbar-title", "WX Station Alerts Dashboard")
      )
    )
  ),

  # Main Container
  div(class = "container-fluid",
    # Header with controls
    div(class = "header-container",
      div(class = "header-left",
        # Main Navigation Tabs
        tabsetPanel(
          id = "tabs",
          type = "pills",
          tabPanel("DB check", value = "DB check"),
          tabPanel("Power", value = "Power"),
          tabPanel("Wind", value = "Wind"),
          tabPanel("Temp", value = "Temp"),
          tabPanel("RH", value = "RH"),
          tabPanel("Precip", value = "Precip"),
          tabPanel("CRMP", value = "CRMP")
        )
      ),
      
      # Data Controls Dropdown
      div(class = "fetch-controls-dropdown",
        actionButton("fetch_controls_btn", "Data Controls", class = "btn-primary"),
        div(class = "fetch-controls-content",
          radioButtons("fetch_type", "Select Time Range:",
            choices = list(
              "Recent Time" = "last_n",
              "By Date" = "by_date",
              "Date Range" = "date_range"
            ),
            selected = "last_n"
          ),
          selectInput("fire_centre", "Select Fire Centre:",
            choices = names(fire_center_mapping),
            selected = names(fire_center_mapping)[1]
          ),
          radioButtons("station_selection_type", "Station Selection:",
            choices = list(
              "All Stations" = "all",
              "Select Specific Stations" = "specific"
            ),
            selected = "all"
          ),
          conditionalPanel(
            condition = "input.station_selection_type == 'specific'",
            selectizeInput("selected_stations", "Select Stations:",
              choices = NULL,
              multiple = TRUE,
              options = list(
                placeholder = 'Select stations to include',
                plugins = list('remove_button')
              )
            )
          ),
          conditionalPanel(
            condition = "input.fetch_type == 'last_n'",
            div(class = "time-preset-container",
              radioButtons("time_preset", "Select Time Period:",
                choices = list(
                  "Last 24 Hours" = "24",
                  "Last 3 Days" = "72",
                  "Last Week" = "168",
                  "Last Month" = "720",
                  "Custom" = "custom"
                ),
                selected = "24"
              ),
              conditionalPanel(
                condition = "input.time_preset == 'custom'",
                numericInput("num_entries", "Enter Custom Hours:", 
                  value = 24, min = 1, step = 1
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.fetch_type == 'by_date'",
            dateInput("select_date", "Select Date:", value = Sys.Date())
          ),
          conditionalPanel(
            condition = "input.fetch_type == 'date_range'",
            dateInput("start_date", "Start Date:", value = Sys.Date() - 7),
            dateInput("end_date", "End Date:", value = Sys.Date())
          ),
          actionButton("fetch_data", "Retrieve Data", class = "btn-primary")
        )
      )
    ),

    # Tab Content
    # DB check tab
    conditionalPanel(
      condition = "input.tabs == 'DB check'",
      div(class = "station-select-container", 
        selectInput("db_station_select", "Select Station", choices = NULL)
      ),
      div(class = "output-card",
        h4("Station Report Status"),
        DTOutput("last_entry_time_check")
      ),
      div(class = "row",
        div(class = "col-md-7",
          div(class = "output-card",
            h4("Check for Hourly Data Gaps"),
            plotlyOutput("missing_entries_plot", height = "200px"),
            verbatimTextOutput("missing_entries_output") %>% 
              tagAppendAttributes(class = "scrollable-output")
          )
        ),
        div(class = "col-md-5",
          div(class = "output-card",
            h4("Check for Blanks"),
            verbatimTextOutput("original_output") %>% 
              tagAppendAttributes(class = "scrollable-output")
          )
        )
      ),
      div(class = "row",
        div(class = "col-md-5",
          div(class = "output-card",
            h4("NOAA Alerts"),
            DTOutput("space_weather_table")
          )
        ),
        div(class = "col-md-7",
          div(class = "output-card",
            h4("Recent Entries"),
            DTOutput("recent_entries")
          )
        )
      )
    ),



    # Power tab
    conditionalPanel(
      condition = "input.tabs == 'Power'",
      div(class = "row",
        div(class = "col-md-12",
          div(class = "output-card",
            h4("Battery Voltage Alerts"),
            verbatimTextOutput("battery_voltage_alerts") %>% 
              tagAppendAttributes(class = "scrollable-output")
          )
        )
      ),
      div(class = "row",
        div(class = "col-md-6",
          div(class = "output-card",
            h4("Power Status Details"),
            selectInput("power_station_select", "Select Station", choices = NULL),
            plotlyOutput("vbat_waterfall_plot", height = "400px"),
            verbatimTextOutput("power_status_output") %>% 
              tagAppendAttributes(class = "scrollable-output")
          )
        ),
        div(class = "col-md-6",
          div(class = "output-card",
            h4("Vbat Discharge Trends Comparison"),
            selectizeInput("vbat_trends_station_select", "Select Stations to Display",
              choices = NULL,
              multiple = TRUE,
              options = list(
                plugins = list('remove_button'),
                placeholder = 'Select stations...'
              )
            ),
            plotlyOutput("vbat_trends_plot", height = "400px"),
            verbatimTextOutput("vbat_trends_output") %>% 
              tagAppendAttributes(class = "scrollable-output")
          )
        )
      )
    ),

    # Wind tab
    conditionalPanel(
      condition = "input.tabs == 'Wind'",
      div(class = "row",
        div(class = "col-md-12",
          div(class = "output-card",
            h4("High Wspd Check"),
            verbatimTextOutput("high_wspd_output") %>% 
              tagAppendAttributes(class = "scrollable-output")
          )
        )
      ),
      div(class = "row",
        div(class = "col-md-8",
          div(class = "output-card",
            h4("Wind Analysis"),
            selectInput("zero_wspd_station_select", "Select Station", choices = NULL),
            plotlyOutput("zero_wspd_plot", height = "400px"),
            verbatimTextOutput("zero_wspd_output") %>% 
              tagAppendAttributes(class = "scrollable-output")
          )
        ),
        div(class = "col-md-4",
          div(class = "output-card",
            h4("Constant Dir Check"),
            verbatimTextOutput("dir_check") %>% 
              tagAppendAttributes(class = "scrollable-output")
          )
        )
      )
    ),

    # Temperature tab
    conditionalPanel(
      condition = "input.tabs == 'Temp'",
      div(class = "row",
        div(class = "col-md-12",
          div(class = "output-card",
            h4("Temperature Change Analysis"),
            selectInput("temp_analysis_station_select", "Select Station", choices = NULL),
            plotlyOutput("temp_change_heatmap", height = "400px"),
            verbatimTextOutput("erratic_temp_output") %>% 
              tagAppendAttributes(class = "scrollable-output")
          )
        )
      ),
      div(class = "row",
        div(class = "col-md-6",
          div(class = "output-card",
            h4("Constant Temperature Analysis"),
            selectInput("constant_temp_station_select", "Select Station", choices = NULL),
            verbatimTextOutput("temp_check") %>% 
              tagAppendAttributes(class = "scrollable-output tall-output")
          )
        ),
        div(class = "col-md-6",
          div(class = "output-card",
            h4("Time Distribution of Constant Periods"),
            plotlyOutput("constant_temp_plot", height = "400px")
          ),
          div(class = "output-card",
            h4("Overall Station Summary"),
            plotlyOutput("constant_temp_summary_plot", height = "400px")
          )
        )
      ),
      div(class = "row",
        div(class = "col-md-12",
          div(class = "output-card",
            h4("Temperature Distribution"),
            plotlyOutput("temp_range_plot", height = "550px"),
            verbatimTextOutput("extreme_temp_output") %>% 
              tagAppendAttributes(class = "scrollable-output")
          )
        )
      )
    ),

    # RH tab
    conditionalPanel(
      condition = "input.tabs == 'RH'",
      div(class = "row",
        div(class = "col-md-12",
          div(class = "output-card",
            h4("RH 0%"),
            verbatimTextOutput("low_rh_output") %>% 
              tagAppendAttributes(class = "scrollable-output")
          )
        )
      ),
      div(class = "row",
        div(class = "col-md-12",
          div(class = "output-card",
            h4("100% RH Analysis"),
            selectInput("rh_station_select", "Select Station", choices = NULL),
            plotlyOutput("consecutive_rh_plot", height = "400px"),
            verbatimTextOutput("consecutive_rh_output") %>% 
              tagAppendAttributes(class = "scrollable-output")
          )
        )
      )
    ),

    # Precipitation tab
    conditionalPanel(
      condition = "input.tabs == 'Precip'",
      div(class = "row",
        div(class = "col-md-12",
          div(class = "output-card",
            h4("Rn_1 Outliers"),
            selectInput("rn1_outliers_station_select", "Select Station", choices = NULL),
            plotlyOutput("rn1_outliers_plot", height = "400px"),
            verbatimTextOutput("rn1_outliers_output") %>% 
              tagAppendAttributes(class = "scrollable-output")
          )
        )
      ),
      div(class = "row",
        div(class = "col-md-4",
          div(class = "output-card",
            h4("Total Rn_1"),
            verbatimTextOutput("station_rainfall_output") %>% 
              tagAppendAttributes(class = "scrollable-output")
          )
        ),
        div(class = "col-md-8",
          div(class = "output-card",
            h4("Decreasing Temperature Trends with High RH"),
            selectInput("temp_trends_station_select", "Select Station", choices = NULL),
            verbatimTextOutput("decreasing_trends_output") %>% 
              tagAppendAttributes(class = "scrollable-output")
          )
        )
      )
    ),

    # CRMP tab
    conditionalPanel(
      condition = "input.tabs == 'CRMP'",
      div(class = "row",
        div(class = "col-md-6",
          div(class = "output-card",
            h4("Snow Depth Sensor Alerts"),
            verbatimTextOutput("sdepth_concerns_output") %>% 
              tagAppendAttributes(class = "scrollable-output")
          )
        ),
        div(class = "col-md-6",
          div(class = "output-card",
            h4("Cumulative Precipitation Monitoring"),
            verbatimTextOutput("crmp_precip_changes") %>% 
              tagAppendAttributes(class = "scrollable-output")
          )
        )
      ),
      div(class = "output-card",
        div(class = "station-select-container",
          selectInput("crmp_station_select", "Select Station", choices = NULL)
        ),
        div(class = "row",
          div(class = "col-md-3",
            h4("CRMP Recent Entries"),
            verbatimTextOutput("crmp_entries") %>% 
              tagAppendAttributes(class = "scrollable-output")
          ),
          div(class = "col-md-9",
            div(class = "row",
              div(class = "col-md-12",
                h4("Snow Depth Measurements"),
                plotlyOutput("sdepth_plot", height = "250px")
              )
            ),
            div(class = "row",
              div(class = "col-md-12",
                h4("Cumulative Precipitation Measurements"),
                plotlyOutput("precip_plot", height = "250px")
              )
            )
          )
        )
      )
    )
  ),

  # Footer with Back to Top
  tags$footer(id = "footer",
    div(id = "footerWrapper",
      div(id = "footerAdminSection",
        div(class = "container",
          div(id = "footerAdminLinks",
            tags$ul(class = "inline",
              tags$li(tags$a(href = "https://www2.gov.bc.ca/gov/content/home", "Home", target = "_blank")),
              tags$li(tags$a(href = "https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", target = "_blank")),
              tags$li(tags$a(href = "https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", target = "_blank")),
              tags$li(tags$a(href = "https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", target = "_blank")),
              tags$li(tags$a(href = "https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", target = "_blank")),
              tags$li(tags$a(href = "https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", target = "_blank"))
            )
          )
        )
      )
    ),
    tags$img(src = "www/back-to-top.png", class = "back-to-top", onclick = "window.scrollTo({top: 0, behavior: 'smooth'});")
  ),
  
  # JavaScript for back-to-top functionality
  tags$script(HTML("
    $(document).ready(function() {
      $(window).scroll(function() {
        if ($(this).scrollTop() > 100) {
          $('.back-to-top').fadeIn();
        } else {
          $('.back-to-top').fadeOut();
        }
      });

      // Check initial scroll position
      if ($(window).scrollTop() > 100) {
        $('.back-to-top').show();
      }
    });
  "))
) # End fluidPage

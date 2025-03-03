<!-- 
Add a project state badge

See <https://github.com/BCDevExchange/Our-Project-Docs/blob/master/discussion/projectstates.md> 
If you have bcgovr installed and you use RStudio, click the 'Insert BCDevex Badge' Addin.
-->

<div align="center">
<img src="https://github.com/user-attachments/assets/e55ee554-8469-432d-8e3d-dba1b0e50194" width="50%" alt="WX Station Alerts Dashboard">
</div>

WX Station Alerts Dashboard
============================
A web application built with R Shiny for monitoring and analyzing data from BC Wildfire Service weather stations.

## Features

- Real-time weather station monitoring across multiple fire centers and zones
- Visualization tools for weather metrics including:
  - Temperature analysis and trend detection
  - Wind speed and direction monitoring
  - Relative humidity tracking
  - Precipitation measurement
  - Power system analysis (battery voltage, solar charging)
- Interactive data filtering by:
  - Fire centre and zone
  - Individual or multiple stations
  - Time periods (last N hours, specific date, or date range)
- Automated alerts for:
  - Missing data entries
  - Sensor anomalies
  - Power system status
  - Environmental conditions
  - CRMP (Climate Related Monitoring Program) sensor support

## Requirements

- R (latest stable version recommended)
- Required R packages:
  - shiny
  - RODBC
  - dplyr
  - lubridate
  - plotly
  - bslib
  - jsonlite
  - DT
  - zoo
  - viridis
  - ggplot2
- Microsoft SQL Server with appropriate database access
- Network access to weather station data sources

## Project Status

Active development - The system is currently in production and actively maintained.


### How to Contribute

If you would like to contribute, please see the [CONTRIBUTING](CONTRIBUTING.md) guidelines.


### License

```
Copyright 2025 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an &quot;AS IS&quot; BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and limitations under the License.
```
---
*This project was created using the [bcgovr](https://github.com/bcgov/bcgovr) package.* 

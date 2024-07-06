# Data Product to Visualize Environmental and Economic Data

Description:
This repo shows the work for a Data to Product submission. We focused specifically on creating a tool to visualise environmental and economic changes to countries over the years. This visualisation tool focuses on trying to be responsive and allow users to easily analyse data through a combination of choropleth plots and time graphs. 

The application leverages R for data processing and visualization, with a user-friendly interface styled using CSS.

# Project Structure
global.R: This script handles the global setup and configuration for the Shiny app.
app.R: This is the main script for the Shiny app, containing the server and UI logic.
style.css: Contains custom CSS for styling the Shiny app interface.

DATASETS:
emissions.csv: Contains emissions data for various countries.
info.csv: Contains additional information related to the data.
sus.csv: Contains data on sustainability metrics.
gdp.csv: Contains GDP data for various countries.

world_files: Contains the information required to load and generate global choropleth plots

# Installation and Running the app
To run this project, you need to have R and several R packages installed. You can install the required packages from CRAN using the following commands:
install.packages(c("shiny", "tidyverse", "leaflet", "DT", "ggplot2", "plotly", "dplyr"))


To run the app, you can use the following command in R:
shiny::runApp("path_to_your_app_directory")


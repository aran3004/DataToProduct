################################################################################
################################ Libraries #####################################
################################################################################

library(sf)         # 'sf' (Simple Features) is used for handling and manipulating spatial data.
library(shiny)      # 'shiny' is a web application framework for R.
library(shinyjs)    # 'shinyjs' allows you to use JavaScript functions and interactions within Shiny
library(leaflet)    # 'leaflet' is a package for creating interactive maps.
library(scales)     # 'scales' provides functions for scaling data and creating legends for visualizations.
library(stringr)    # 'stringr' is a package for data manipulation and string processing.
library(dplyr)      # 'dplyr' is a part of the tidyverse, and it's used for data manipulation.
library(ggplot2)    # 'ggplot2' is a widely-used data visualization package.
library(tidyr)      # 'tidyr' is used for data tidying.
library(shinycssloaders) # 'shinycssloaders' adds loading animations to Shiny outputs

################################################################################
############################ Data cleaning #####################################
################################################################################

em <- read.csv("datasets/emissions.csv", head=TRUE)
gdp <- read.csv("datasets/gdp.csv", head=TRUE)
info <- read.csv("datasets/info.csv", head=TRUE)
sus <- read.csv("datasets/sus.csv", head=TRUE, check.names = FALSE)

#Generates a list of all the countries and one for the country codes
ISO <- unique(em$ISO)           # The list of countries by ISO code
len_I = 1:length(ISO)           # The number of countries
COUNTRY <- unique(em$Country)   # The list of countries by name
len_c = 1:length(COUNTRY)       # The number of countries

# Create the lists of the year ranges used
# For the CO2 Emissions data
years1750 <- 1 : (2021-1750 + 1)    # List of increasing numbers, long as the year span
# years_CO2 <- 1750 : 2022
len_y_CO2 <- length(years1750)

# For the GDP data
years1960 <- 1 : (2021-1960 + 1)    # List of increasing numbers, long as the year span
# years_gdp <- 1960 : 2022
len_y_gdp <- length(years1960)

# For the Renewable Energy data
years2000 <- 1 : (2020-2000 + 1)    # List of increasing numbers, long as the year span
years_sus <- 2000 : 2020
len_y_sus <- length(years2000)

# Create a template data frame
template <- expand.grid(ISO = ISO, Year = years_sus)
# Merge the 'sus' data frame with the 'template'.
sus <- merge(template, sus, by = c("ISO", "Year"), all.x = TRUE)


# Create a List of all the dataframes used
dataframes <- list(em = em, gdp = gdp, info = info, sus = sus)

# For loop to process each dataframe
for (name in names(dataframes)) {
  
  # Order by ISO
  dataframes[[name]] <- dataframes[[name]][order(dataframes[[name]]$ISO), ]
  
  # Replace NA with 0
  dataframes[[name]][is.na(dataframes[[name]])] <- 0
  
  # Convert all columns to character
  # dataframes[[name]] <- data.frame(lapply(dataframes[[name]], as.character), stringsAsFactors = FALSE)
}

# Extracting the dataframes back from the list
em <- dataframes$em
gdp <- dataframes$gdp
info <- dataframes$info
sus <- dataframes$sus

################################################################################
######################### For Map Visualization ################################
################################################################################

# List of emission types
emission_types <- c("Total", "Coal", "Oil", "Gas", "Cement", "Flaring", "Other")

# Pivot the dataframe for each type - emission
for (type in emission_types) {
  # Create the pivoted data frame
  emissions_pivoted <- em %>%
    pivot_wider(names_from = Year, values_from = type, id_cols = ISO)
  
  # Save the pivoted dataframe to the global environment
  assign(paste0("emissions_", type), emissions_pivoted)
}

# List of GDP types
gdp_types <- c("GDP_USD", "GDP_per_capita_USD")

# Pivot the dataframe for each type - GDP
for (type in gdp_types) {
  # Create the pivoted data frame
  gdp_pivoted <- gdp %>%
    pivot_wider(names_from = Year, values_from = type, id_cols = ISO)
  
  # Save the pivoted dataframe to the global environment
  assign(type, gdp_pivoted)
}

# List of specified sustainability metrics with corrected column names
sus_metrics <- c("Access to electricity (% of population)",
                 "Renewable-electricity-generating-capacity-per-capita",
                 "Renewable energy share in the total final energy consumption (%)",
                 "Electricity from fossil fuels (TWh)",
                 "Electricity from nuclear (TWh)",
                 "Value_co2_emissions_kt_by_country", # Assuming this is correct
                 "Renewables (% equivalent primary energy)",
                 "Electricity from renewables (TWh)")


# Pivot the dataframe for each specified metric and save as CSV
for (metric in sus_metrics) {
  # Create the pivoted data frame - Sus
  sus_pivoted <- sus %>%
    pivot_wider(names_from = Year, values_from = metric, id_cols = "ISO")
  
  # Save the pivoted dataframe to the global environment
  assign(paste0("sus_", metric), sus_pivoted)
}

################################################################################
############################ Format Function ###################################
################################################################################

# Format the numbers by converting them into thousands (k), millions (M) and Billions (B)
formatNumber <- function(num, dig = 1) {
  # Limit to two decimal places
  formatted_num <- formatC(num, format = "f", digits = dig)
  
  # If the number is in the thousands (>=1000 and <1,000,000)
  if (num >= 1000 && num < 1000000) {
    # Add thousands separator
    formatted_num <- paste(formatC(num / 1000, format = "f", big.mark = ",",digits = dig), "k")
  }
  
  # If the number is in the millions or more
  if (num >= 1000000 && num < 1000000000) {
    # Format as millions with one decimal place
    formatted_num <- paste(formatC(num / 1000000, format = "f", big.mark = ",",digits = dig), "M")
  }
  
  if (num >= 1000000000) {
    # Format as millions with one decimal place
    formatted_num <- paste(formatC(num / 1000000000, format = "f", big.mark = ",",digits = dig), "B")
  }
  
  return(formatted_num)
}


################################################################################
############################ Create country data ###############################
################################################################################

# Generates the large data structure with all the information for each country
# Create an empty list
data <- list()

# Loop through all countries and gather information
for (n in len_c) {
  data[[COUNTRY[n]]] <- list(
    
    # Add a bunch of parameters with the relevant data from the info dataset
    name = info[n,"COUNTRY_NAME"],
    `ISO code` = info[n,"ISO"],
    ISO_number = info[n,"ISO_NUM"],
    population  = formatNumber(info[n,"POP_CNTRY"]),
    SQKM = formatNumber(info[n,"SQKM"]), # Area in km^2
    SQMI = formatNumber(info[n,"SQMI"]), # Area in Mi^2
    currency = info[n,"CURR_TYPE"],
    landlocked = info[n,"LANDLOCKED"], # If the country is landlocked
    X = info[n,"X"], # X cCoordinate of the country
    Y = info[n,"Y"], # Y Coordinate of the country
    
    # Add a list to contain the more complex data (other lists)
    select = list(
      
      # Create a list for the data regarding the GDP of a country
      GDP = list (
        `Total GDP ($)` = gdp[years1960 + ((len_y_gdp)*(n-1)) , "GDP_USD"],
        `GDP per capita ($)` = gdp[years1960 + ((len_y_gdp)*(n-1)), "GDP_per_capita_USD"]),
      
      # Create a list for the data regarding the CO2 Emissions of a country
      `CO2 emissions` = list(
        `Total CO2 Emissions (Mt)` = em[years1750 + ((len_y_CO2)*(n-1)),"Total"],
        `CO2 Emissions from coal (Mt)` =  em[years1750 + ((len_y_CO2)*(n-1)),"Coal"],
        `CO2 Emissions from oil (Mt)` = em[years1750 + ((len_y_CO2)*(n-1)),"Oil"],
        `CO2 Emissions from gas (Mt)` = em[years1750 + ((len_y_CO2)*(n-1)),"Gas"],
        `CO2 Emissions from cement (Mt)` = em[years1750 + ((len_y_CO2)*(n-1)),"Cement"],
        `CO2 Emissions from flaring (Mt)` = em[years1750 + ((len_y_CO2)*(n-1)),"Flaring"],
        `CO2 Emissions from other (Mt)` = em[years1750 + ((len_y_CO2)*(n-1)),"Other"]),
      # `CO2 Emissions per capita (metric tons)` = em[years1750 + ((len_y_CO2)*(n-1)),"Per.Capita"]),
      
      # Create a list for the data regarding the Renewable Sources of a country
      `Renewable Energy` = list(
        `Access to electricity (% of pop)` = sus[years2000 + ((len_y_sus)*(n-1)),4],
        `Renewable electricity generating capacity per capita` = sus[years2000 + ((len_y_sus)*(n-1)),6],
        `Renewable energy share (%)` = sus[years2000 + ((len_y_sus)*(n-1)),8],
        `Electricity from fossil fuels (TWh)` = sus[years2000 + ((len_y_sus)*(n-1)),9],# Electricity from fossil fuels (TWh)
        `Electricity from nuclear (TWh)` = sus[years2000 + ((len_y_sus)*(n-1)),10],
        `Electricity from renewables (TWh)` = sus[years2000 + ((len_y_sus)*(n-1)),11],
        `Renewables (% equivalent primary energy)` = sus[years2000 + ((len_y_sus)*(n-1)),16]
      )
    )
  )
}

################################################################################
####################### Plot support functions #################################
################################################################################

# Define a function named 'plot_datay' that takes a list as an argument.
plot_datay <- function(list) {
  # Start a while loop that continues as long as the list has elements
  # and the first element of the list is zero.
  while(length(list) > 0 && list[1] == 0) {
    # Remove the first element of the list.
    list <- list[-1]
  }
  # Return the modified list after removing all leading zeros.
  return(list)
}

# Define a function named 'plot_datax' that takes a list as an argument.
plot_datax <- function(list) {
  # Start a while loop that continues as long as the list has elements
  # and the first element of the list is zero.
  while(length(list) > 0 && list[1] == 0) {
    # Remove the first element of the list.
    list <- list[-1]
  }
  # Calculate the number of elements in the modified list.
  n <- length(list)
  # Create a sequence of years starting from 2021 going backwards.
  # The length of this sequence is equal to the number of elements in the list.
  years <- 2021:(2021 - n + 1)
  # Sort the years in increasing order.
  years <- sort(years, decreasing = FALSE)
  # Return the sorted years.
  return(years)
}


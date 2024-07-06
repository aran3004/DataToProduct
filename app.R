################################################################################
############################ Import Source #####################################
################################################################################

source("global.R") # Import the "global.R" file

################################################################################
ui <- fluidPage(################################################################
################################################################################
  
  # Initialize shinyjs for JavaScript interactivity and other resources
  useShinyjs(),
  
  # Include additional resources in the HTML head section
  tags$head(
    # Add a jQuery UI script for additional UI features
    tags$script(src = "https://code.jquery.com/ui/1.12.1/jquery-ui.js"),
    
    # Include a custom CSS file for styling
    includeCSS("www/style.css")
  ),
  
  # Create a div container for the application title
  # 'tags$h2' creates an HTML <h2> element for the title "World Map"
  div(id = "appTitle", tags$h2("Sustainability World Map")),  # Custom title panel
  
  # Add a help button to the UI
  # 'actionButton' creates a clickable button, here labeled "Help"
  actionButton("helpButton", "How to Use", class = "help-button"),
  
  # Create a fluid row layout to organize UI components horizontally
  fluidRow(
    
    # Define a column with a width of 3 (out of 12) for side panels
    column(
      width = 3,
      class = "panelsContainer",
      
      # Dynamic UI output for options panel
      uiOutput("optionsPanel", class = "optionsPanel"),
      
      # To separate the two panels
      uiOutput("emptyBox"),
      
      # Dynamic UI output for the plot
      uiOutput("smallPlot", class = "smallPlot")
    ),
    
    # Define another column with a width of 9 for the main map display
    column(
      width = 9,
      class = "mapContainer",
      
      # Dynamic UI output for an information panel
      div(uiOutput("infoPanel"), class = "infoPanel"),
      
      # Dynamic UI element for the year slider
      (div(uiOutput("yearSlider"), class = "sliderPanel")),
      
      # Output for a Leaflet map with a specified height
      # 'leafletOutput' is used to display Leaflet maps in Shiny apps
      withSpinner(leafletOutput("map", height = '85vh'), color = "#B40404")
    ),

  ) # Close fluidRow 
) # Close ui fluidPage


################################################################################
server <- function(input, output, session) {####################################
################################################################################
  
  # IMPORTANT: kills the process when closing the app
  session$onSessionEnded(function() { stopApp() })
  
  ########################## REACTIVE EXPRESSIONS ##############################
  ##############################################################################
  
  #  Variable to get the selected country's data
  selectedCountryData <- reactive({data[[input$country]]})
  
  
  # Variable to get the selected parameter
  selectedParameter <- reactive(data[[input$country]]$select[[input$var1]][[input$var2]])
  
  
  # Reactive list containing the messages to print in the plot panel
  plotMessage <- reactive(list("Select a Sub-section!", "No Data Available"))
  
  
  # Boolean variable to switch between the messages
  values <- reactiveValues(updateDueToCountry = TRUE)
  
  
  # Define a reactive expression based on user input
  selectedData <- reactive({
    # Check if the selected category is 'CO2 emissions'
    if(input$var1 == "CO2 emissions") {
      
      # Return the data set corresponding to the selected 'CO2 emissions' type
      switch(input$var2,
             "CO2 Emissions from cement (Mt)" = emissions_Cement,
             "CO2 Emissions from coal (Mt)" = emissions_Coal,
             "CO2 Emissions from flaring (Mt)" = emissions_Flaring,
             "CO2 Emissions from gas (Mt)" = emissions_Gas,
             "CO2 Emissions from oil (Mt)" = emissions_Oil,
             "CO2 Emissions from other (Mt)" = emissions_Other,
             "Total CO2 Emissions (Mt)" = emissions_Total)
    } 
    # Check if the selected category is 'GDP'
    else if(input$var1 == "GDP") {
      
      # Return the dataset corresponding to the selected 'GDP' type
      switch(input$var2,
             "Total GDP ($)" = GDP_USD,
             "GDP per capita ($)" = GDP_per_capita_USD)
    } 
    # If the selected category is neither 'CO2 emissions' nor 'GDP'
    else {
      
      # Return the dataset corresponding to various other selected types
      switch(input$var2,
             "Access to electricity (% of pop)" = `sus_Access to electricity (% of population)`,
             "Electricity from fossil fuels (TWh)" = `sus_Electricity from fossil fuels (TWh)`,
             "Electricity from nuclear (TWh)" = `sus_Electricity from nuclear (TWh)`,
             "Renewable electricity generating capacity per capita" = `sus_Renewable-electricity-generating-capacity-per-capita`,
             "Renewable energy share (%)" = `sus_Renewable energy share in the total final energy consumption (%)`,
             "Renewables (% equivalent primary energy)" = `sus_Renewables (% equivalent primary energy)`,
             "Electricity from renewables (TWh)" = `sus_Electricity from renewables (TWh)`)
    }
  }) # Close selectedData()
  
  
  ############# RENDER THE OUTPUTS (MAP-OPTION PANEL-INFO PANEL) ###############
  ##############################################################################
  
  
  
  # Render the panel for parameter selection
  output$optionsPanel <- renderUI({
    # Using a list to organize the UI elements
    list(
      # Dropdown for selecting a country
      # 'choices' are populated with the names from 'data'
      selectizeInput("country", "Choose a Country:", choices = names(data), selected = "Aruba"),
      
      # Dropdown for selecting a sub-section
      # Initially, 'choices' is set to NULL
      selectInput("var1", "Choose a Sub-section:", choices = names(data$Aruba$select), selected = "GDP"),
      
      # Dropdown for selecting a parameter
      # Initially, 'choices' is set to NULL
      selectInput("var2", "Choose a parameter:", choices = names(data$Aruba$select$GDP), selected = "Total GDP ($)")
    )
  })
  
  
  # Render the panel for displaying country information
  output$infoPanel <- renderUI({
    # Create a div container to hold various text outputs
    tags$div(
      tags$h5("Country Information", style = "font-weight: bold;"),  # You can customize the style as needed
      # List of text outputs for different country attributes
      list(
        textOutput("name"),         # Display the country's name
        textOutput("ISO code"),     # Display the country's ISO code
        textOutput("population"),   # Display the country's population
        textOutput("currency"),     # Display the country's currency
        uiOutput("area"),           # Display the country's area
        textOutput("landlocked"),   # Display if the country is landlocked
        textOutput("parameter")     # Display additional parameter
      ),
      id = "infoPanel"  # Assign an ID to the div for styling or referencing
    )
  })
  
  
  # Dynamic UI for the year slider
  output$yearSlider <- renderUI({
    data <- selectedData()
    # print(selectedData())
    if (!is.null(data) && ncol(data) > 1) {
      colHeaders <- colnames(data)
      secondCol <- as.numeric(colHeaders[2])
      lastCol <- as.numeric(tail(colHeaders, n=1))
      
      if (!is.na(secondCol) && !is.na(lastCol)) {
        # UI elements, including the title and slider
        tagList(
          # Title for the year slider
          tags$h5("Select Year", style = "font-weight: bold;"),  # Customizable title
          
          # Year slider
          sliderInput("year", "",
                      min = secondCol, max = lastCol,
                      value = lastCol, sep = ""))
      } else {
        return(NULL)  # Hide slider if columns are not numeric
      }
    } else {
      return(NULL)  # Hide slider if data is not available or insufficient
    }
  })
  
  
  # Render the map
  output$map <- renderLeaflet({
    
    # Load the TM_WORLD_BORDERS shapefile with sf
    worldBorders <- st_read("world_files/TM_WORLD_BORDERS_SIMPL-0.3.shp", quiet = TRUE)
    
    # Correct some names that don't match between the datasets
    worldBorders$NAME[209] <- "USA"
    worldBorders$NAME[84] <- "Iran"
    worldBorders$NAME[205] <- "Tanzania"
    worldBorders$NAME[94] <- "North Korea"
    worldBorders$NAME[96] <- "South Korea"
    worldBorders$NAME[107] <- "Libya"
    
    missing_in_data <- setdiff(worldBorders["NAME"][[1]], names(data))

    # Merge your data with the shapefile
    if (!is.null(selectedData())) {
      mergedData <- merge(worldBorders, selectedData(), by.x = "ISO3", by.y = "ISO", all.x = TRUE)
      
      # Convert to a SpatialDataFrame for Leaflet
      mergedDataSpatial <- st_as_sf(mergedData)
      
      pal <- colorNumeric(palette = "Reds", domain = mergedDataSpatial[[as.character(input$year)]])
      
      leaflet(mergedDataSpatial) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lat = 10, lng = 0, zoom = 2) %>%  # Set the initial view of the map
        setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90) %>%  # Limit map panning
        addPolygons(
          fillColor = ~pal(mergedDataSpatial[[as.character(input$year)]]), # Set the fill color of the polygons
          stroke = FALSE,  # Disable the stroke around polygons
          label = ~NAME,  # Use the NAME attribute for the label
          layerId = ~NAME,  # Assign the country name as the layerId
          weight = 1,
          color = "#444444",
          fillOpacity = 0.7,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),  # Style options for the label
            textsize = "13px",  # Text size for the label
            direction = "auto"  # Auto direction for the label
          )
        ) %>%
        addLegend(
          position = "topright",  # Can be 'topright', 'bottomright', 'bottomleft', or 'topleft'
          pal = pal,  # The color palette function
          values = ~mergedDataSpatial[[as.character(input$year)]][!is.na(mergedDataSpatial[[as.character(input$year)]])],  # Exclude NA values
          title = str_wrap(input$var2, width = 5),  # Title for the legend
          opacity = 1.0,
          na.label = "No Available Data"
        )
    } # Close if
  }) # Close output$map
  
  ############################# OBSERVE EVENTS #################################
  ##############################################################################
  
  # Observe the Help button click
  observeEvent(input$helpButton, {
    # Show a modal dialog when the Help button is clicked
    showModal(modalDialog(
      title = "How to use", # Title of the modal dialog
      # Modal content with instructions on how to use the visualization
      "Welcome to our visualization. Start by selecting a country, either by
      selecting or searching in the dropdown menu, or by clicking on the country
      in the map. Pick then from the Sub-section dropdown the desired parameter
      group you wish to visualize. Finally, pick the specific parameter. The graph
      of the evolution of the selected parameter over the years for the selected
      country is displayed at the bottom left. By selecting another country, you
      can compare the data. The small panel on top of the map displays some
      relevant information for the selected country, feel free to drag that around,
      as well as the map itself (don't wrap around tho!).
      Enjoy!",
      easyClose = TRUE, # Allows closing the modal by clicking outside of it
      footer = modalButton("Close") # Button in the modal footer to close the dialog
    ))
  })
  
  
  # Observe changes in the UI
  observe({
    # Execute JavaScript within the Shiny app
    runjs('
      // Make elements with the "infoPanel" class draggable
      $(".infoPanel").draggable({
        containment: ".mapContainer", // Constrain movement within the element with class "mapContainer"
        scroll: false,               // Disable scrolling of the window/container while dragging
        padding: 50                  // Add padding around the draggable area
      });
      // Make elements with the "infoPanel" class draggable
      $(".sliderPanel").draggable({
        containment: ".mapContainer", // Constrain movement within the element with class "mapContainer"
        scroll: false,               // Disable scrolling of the window/container while dragging
        padding: 50                  // Add padding around the draggable area
      });
    ')
  })
  
  
  # Check what subsection is selectedand update the parameter
  observeEvent(input$var1, {
    choices <- names(data[[input$country]]$select[[input$var1]])
    updateSelectInput(session, "var2", choices = choices)
  })
  
  
  # Check what country is selected and update the other parameters
  observeEvent(input$country, {
    # Store current selections
    current_var1 <- input$var1
    current_var2 <- input$var2
    
    # Update var1 choices
    var1_choices <- names(data[[input$country]]$select)
    updateSelectInput(session, "var1", choices = var1_choices, selected = current_var1)
    
    # Update var2 choices
    var2_choices <- names(data[[input$country]]$select[[input$var1]])
    updateSelectInput(session, "var2", choices = var2_choices, selected = current_var2)
  })
  
  
  # Update the hovering label when a country is hovered over
  observeEvent(input$map_shape_mouseover, {
    hoveredCountry <- input$map_shape_mouseover$id
  }, ignoreNULL = TRUE)
  
  
  # Update the info panel when a country is clicked
  observeEvent(input$map_shape_click, {
    clickedCountry <- input$map_shape_click$id
    if (!is.null(clickedCountry)&& clickedCountry %in% names(data)) {
      updateSelectizeInput(session, "country", selected = clickedCountry)
    }
  }, ignoreNULL = TRUE)
  
  
  ############## ACCESS ALL THE "EASY" DATA FOR THE INFO PANEL #################
  ##############################################################################
  
  # Output for country name
  output$name <- renderText({
    paste("Name:", selectedCountryData()$name)
  })
  
  
  # Output for ISO code of the country
  output$`ISO code` <- renderText({
    paste("ISO code:", selectedCountryData()$`ISO code`)
  })
  
  
  # Output for the population of the country
  output$population <- renderText({
    paste("Population:", selectedCountryData()$population)
  })
  
  
  # Output for the currency used in the country
  output$currency <- renderText({
    paste("Currency:", selectedCountryData()$currency)
  })
  
  
  # Output for the area of the country in square kilometers
  output$area <- renderUI({
    HTML(paste("Area (km<sup>2</sup>):", selectedCountryData()$SQKM))
  })
  
  
  # Output indicating whether the country is landlocked or not
  output$landlocked <- renderText({
    paste("Is it landlocked:", selectedCountryData()$landlocked)
  })
  
  ################# ACCESS THE PARAMETER OF THE LAST YEAR#######################
  ##############################################################################
  
  output$parameter <- renderText({
    
    # If no country is selected, print "No Country Selected"
    if (length(data[[input$country]]) == 0) {
      print("No Country Selected")
    } else {
      # If a country is selected
      # Define the year index to access the most recent value
      param <- data[[input$country]]$select[[input$var1]][[input$var2]]
      og_index <- length(param)
      # Define the value of the parameter
      value <- param[og_index]
      
      # If there isn't a recorded value for the most recent year
      if (!is.null(value) && !is.na(value) && value == 0) {
        index <- og_index  # Create new variable for the year index
        
        # While the recorded value for the selected year is 0, go back one year
        while (!is.null(value) && !is.na(value) && value == 0) {
          # Decrease the year index and reassign the parameter value
          index <- index - 1
          value <- data[[input$country]]$select[[input$var1]][[input$var2]][index]
          
          # If the year index reaches 0, there is no available data for the selected parameter
          if (index == 0) {
            return(paste("There is no data for", input$var2))
            break # Break the while loop
          }
          
          # If the index reaches a year where the value is not 0
          if (!is.null(value) && !is.na(value) && value != 0) {
            value <- formatNumber(value)  # Format the number
            results <- paste("The last recorded value of", input$var2,
                             "is from", as.character(2021 - (og_index - index)),
                             "and is", value)
            results <- str_wrap(results, width = 35)
            return(results)  # Return the formatted results
            break # Break the while loop
          }
        }
      } else if (!is.null(value) && !is.na(value) && value != 0) {
        # If there is a recorded value for the most recent year
        value <- formatNumber(value)  # Format the number
        values$updateDueToCountry <- FALSE
        return(paste("The", input$var2, "is", value))
      }
    }
  })
  
  ############################### PLOT #########################################
  ##############################################################################
  
  # Setting up a small plot in the UI with a specified width and height
  output$smallPlot <- renderUI({
    plotOutput("plot", width = "100%", height = "250px")
  })
  
  # Main plotting function
  output$plot <- renderPlot({
    # Defining labels for the axes
    xlab <- "Year" # Label for the x-axis
    ylab <- ""     # Label for the y-axis (left blank)
    
    # Retrieving the selected parameter and corresponding data
    a <- selectedParameter()
    x <- plot_datax(a) # Data for the x-axis
    y <- plot_datay(a) # Data for the y-axis
    y[y == 0] <- NA    # Replacing zeros with NA
    
    # Checking if there's valid data to plot
    if (any(!sapply(y, is.na))) {
      # Constructing the title based on user input
      title <- paste(input$country, ":", input$var2)
      
      # Creating a dataframe for ggplot
      df <- data.frame(Column1 = x, Column2 = y)
      
      # Building the plot using ggplot
      ggplot(df, aes(x = x, y = y)) +
        geom_line() + # Adding a line graph
        geom_area(fill = "#934C54", alpha = 0.3) + # Filling the area under the line
        theme_minimal() + # Applying a minimal theme for a cleaner look
        theme(axis.line = element_line(color = "darkgreen"), # Customizing axis lines
              axis.ticks = element_line(color = "black"), # Customizing axis ticks
              axis.text = element_text(color = "black", size = 14), # Customizing axis text
              plot.title = element_text(hjust = 0.5, size = 16), # Centering and styling the title
              axis.title = element_text(color = "black", size = 16)) + # Customizing axis titles
        scale_y_continuous(labels = label_number_si()) + # Formatting y-axis labels
        labs(title = str_wrap(title, width = 35), x = xlab, y = '') # Setting labels and title
      
    } else if (values$updateDueToCountry){
      # Displaying a custom message when the plot is updated due to country selection
      plot(0, 0, type = "n", xlim = c(-1, 1), ylim = c(-1, 1),
           xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
      text(-0.1, 0, plotMessage()[1], cex = 1.5) # Adding the message to the plot
      
    } else {
      # Creating an empty plot as a default state
      plot(0, 0, type = "n", xlim = c(-1, 1), ylim = c(-1, 1),
           xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
      text(-0.1, 0, plotMessage()[2], cex = 1.5) # Adding a different message for this state
    }
  })
}

shinyApp(ui, server, options = list(launch.browser = TRUE))
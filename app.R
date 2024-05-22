# Load required libraries
library(shiny)  
library(shinythemes)  # Provides app themes
library(tidyverse)  
library(DT)
library(dplyr)  
library(readr)  
library(leaflet)  
library(leaflet.extras2)  # Additional features for Leaflet maps
library(plotly)  

# Load data from an RDS file
final_data <- readRDS("data:wrangling/final_data.Rds")

############
#    ui    #
############
# The UI created using navbarPage function, which creates a navigation bar with multiple tabs
ui <- navbarPage(
  theme = shinytheme("flatly"),  # Set the theme for the app from the shinythemes package
  title = "Most Expensive Footballers 2021",  # Title of the app
  
  # First tab: Nationality
  # This tab displays a map and a table of football players based on their nationality
  tabPanel("Nationality",
           fluidRow(
             column(4,
                    # Add sliders for filtering players based on market value, age, and minutes played
                    # The sliderInput function creates a slider widget for each filter criteria
                    # The min, max, and default values are set based on the corresponding columns in the 'final_data' dataset
                    sliderInput("valueSlider", "Minimum Market Value:", min = 0, max = max(final_data$Value), value = 0),
                    sliderInput("ageSlider", "Minimum Age:", min = min(final_data$Age), max = max(final_data$Age), value = min(final_data$Age)),
                    sliderInput("mpSlider", "Minimum Minutes Played:", min = 0, max = max(final_data$Mins), value = 0)
             ),
             column(8,
                    # Display a map of players' nationalities
                    # The leafletOutput function creates a placeholder for rendering the interactive map
                    leafletOutput("playerMap")
             )
           ),
           fluidRow(
             column(12,
                    # Display a table of player information
                    # The dataTableOutput function creates a placeholder for rendering the interactive table
                    dataTableOutput("playerTable")
             )
           )
  ),
  
  # Second tab: League
  # This tab displays a map and a table of football players based on their league
  tabPanel("League",
           fluidRow(
             column(4,
                    # Add a dropdown to select a league
                    # The selectInput function creates a dropdown widget for selecting a league
                    # The choices are populated with the unique values from the 'League' column in the 'final_data' dataset
                    # The default selected league is set to "Premier League"
                    selectInput(inputId = "leagueSelect",
                                label = "Choose a league:",
                                choices = unique(final_data$League),
                                selected = "Premier League")
             ),
             column(8,
                    # Display a map of the selected league and player nationalities
                    # The leafletOutput function creates a placeholder for rendering the interactive map
                    leafletOutput("leagueMap")
             )
           ),
           fluidRow(
             column(12,
                    # Display a table of player information for the selected league
                    # The dataTableOutput function creates a placeholder for rendering the interactive table
                    dataTableOutput("playerTableLeague")
             )
           )
  ),
  
  # Third tab: Clustering
  # This tab displays a scatter plot and a table of football players clustered by performance metrics
  tabPanel("Clustering",
           fluidRow(
             # Display a scatter plot of players clustered by G+A, Value, and Age
             # The plotlyOutput function creates a placeholder for rendering the interactive scatter plot
             plotlyOutput(outputId = "clusterPlot")
           ),
           fluidRow(
             column(6,
                    # Add a dropdown to select a cluster
                    # The selectInput function creates a dropdown widget for selecting a cluster
                    # The choices are initially set to NULL and will be updated dynamically based on the available clusters
                    selectInput("clusterSelect", "Select a Cluster:",
                                choices = NULL)
             )
           ),
           fluidRow(
             column(12,
                    # Display a table of player information for the selected cluster
                    # The DTOutput function creates a placeholder for rendering the interactive table
                    DTOutput("clusterTable")
             )
           )
  )
)

############
# server   #
############
# The server function contains the reactive logic and computation for the app
server <- function(input, output, session) {
  
  # Tab 1: Nationality
  
  # Filter players based on user input from sliders
  # The reactive expression 'filteredPlayers' updates automatically whenever the input values change
  # It filters the 'final_data' dataset based on the selected minimum market value, age, and minutes played
  filteredPlayers <- reactive({
    final_data %>%
      filter(
        Value >= input$valueSlider,
        Age >= input$ageSlider,
        Mins >= input$mpSlider
      )
  })
  
  # Render the player map using Leaflet
  output$playerMap <- renderLeaflet({
    # Group players by nationality and summarize their information
    # The 'groupedPlayers' variable aggregates player information by nationality
    # It creates a string with player details (name, market value, age, minutes played) for each nationality
    groupedPlayers <- filteredPlayers() %>%
      group_by(Nation, Nation_Long, Nation_Lat) %>%
      summarise(
        Players = paste0("<b>", Player, "</b><br>", "Market Value: ", Value, "<br>", "Age: ", Age, "<br>", "Minutes Played: ", Mins, "<br><br>", collapse = ""),
        .groups = "drop"
      )
    
    # Create a leaflet map with markers for each country
    # The leaflet function initializes the map
    # The addTiles function adds the base map tiles
    # The addMarkers function adds markers for each country, with the nationality as the popup content and layer ID
    leaflet(groupedPlayers) %>%
      addTiles() %>%
      addMarkers(
        lng = ~Nation_Long,
        lat = ~Nation_Lat,
        popup = ~paste0("<b>Country:</b> ", Nation),
        layerId = ~Nation
      )
  })
  
  # Display player information when a marker is clicked
  # The observeEvent function triggers an action when a marker is clicked on the player map
  observeEvent(input$playerMap_marker_click, {
    # Get the ID of the clicked country from the marker click event
    clickedCountry <- input$playerMap_marker_click$id
    # Filter the player data based on the clicked country
    playerData <- filteredPlayers() %>%
      filter(Nation == clickedCountry) %>%
      select(Player, Value, Age, Mins)
    
    # Render the filtered player data in a DataTable
    # The renderDataTable function renders an interactive table
    output$playerTable <- renderDataTable({
      datatable(playerData, options = list(pageLength = 5))
    })
  })
  
  # Tab 2: League
  
  # Filter players based on the selected league
  # The reactive expression 'filteredLeaguePlayers' updates automatically whenever the selected league changes
  # It filters the 'final_data' dataset based on the selected league from the dropdown
  filteredLeaguePlayers <- reactive({
    final_data %>% filter(League == input$leagueSelect)
  })
  
  # Get distinct player nationalities for the selected league
  # The reactive expression 'playerNationalitiesReactive' updates automatically whenever the filtered league players change
  # It retrieves the distinct nationalities of players in the selected league, along with their longitude and latitude coordinates
  # It filters out any missing or invalid coordinates
  playerNationalitiesReactive <- reactive({
    filteredLeaguePlayers() %>%
      distinct(Nation, Nation_Long, Nation_Lat) %>%
      filter(!is.na(Nation_Long) & !is.na(Nation_Lat))
  })
  
  # Render the league map
  # The renderLeaflet function renders an interactive map using the Leaflet library
  output$leagueMap <- renderLeaflet({
    # Ensure that a league is selected before rendering the map
    req(input$leagueSelect)
    
    # Calculate the average location of the selected league
    # The 'leagueLocation' variable computes the mean longitude and latitude of the selected league
    # It uses the summarise function to calculate the average coordinates and selects the first row of the result
    leagueLocation <- filteredLeaguePlayers() %>%
      summarise(Country_Long = mean(Country_Long, na.rm = TRUE),
                Country_Lat = mean(Country_Lat, na.rm = TRUE)) %>%
      slice(1)
    
    # Ensure valid league coordinates are available before rendering the map
    # The req function checks if the 'leagueLocation' has exactly one row and valid longitude and latitude values
    req(nrow(leagueLocation) == 1, !is.na(leagueLocation$Country_Long), !is.na(leagueLocation$Country_Lat))
    
    # Get the player nationalities for the selected league
    playerNationalities <- playerNationalitiesReactive()
    
    # Create a leaflet map centered on the league location
    # The leaflet function initializes the map
    # The addTiles function adds the base map tiles
    # The addMarkers function adds a marker for the league location, with the league name as the popup content and layer ID
    map <- leaflet() %>%
      addTiles() %>%
      addMarkers(
        lng = leagueLocation$Country_Long,
        lat = leagueLocation$Country_Lat,
        popup = paste0("<b>League:</b> ", input$leagueSelect),
        layerId = "leagueLocation"
      )
    
    # Add markers and lines for each player's nationality
    # The for loop iterates over each player's nationality
    # It generates a unique ID for each nationality layer
    # The addMarkers function adds a marker for each nationality, with the country name as the popup content and layer ID
    # The addPolylines function adds a line connecting the league location to each player's nationality, with the same layer ID as the marker
    for (i in 1:nrow(playerNationalities)) {
      natId <- paste0("nation", i)  # Generate a unique ID for the layer
      map <- map %>%
        addMarkers(
          lng = playerNationalities$Nation_Long[i],
          lat = playerNationalities$Nation_Lat[i],
          popup = playerNationalities$Nation[i],
          layerId = natId
        ) %>%
        addPolylines(
          lng = c(leagueLocation$Country_Long, playerNationalities$Nation_Long[i]),
          lat = c(leagueLocation$Country_Lat, playerNationalities$Nation_Lat[i]),
          color = "blue",
          opacity = 0.5,
          layerId = natId  # Use the same ID for the line
        )
    }
    
    map  # Return the map
  })
  
  # Display player information when a marker or line is clicked on the league map
  # The observeEvent function triggers an action when a marker or line is clicked on the league map
  observeEvent(c(input$leagueMap_marker_click, input$leagueMap_shape_click), {
    # Get the ID of the clicked marker or line from the click event
    # The %||% operator returns the first non-null value (marker click ID or shape click ID)
    clickedId <- input$leagueMap_marker_click$id %||% input$leagueMap_shape_click$id
    
    # Ensure a valid click ID is available before proceeding
    req(!is.null(clickedId))
    
    # Get the filtered players from the selected league
    playersFromLeague <- filteredLeaguePlayers()
    
    # Check if the clicked ID is not the league location marker
    if (clickedId != "leagueLocation") {
      # Extract the nationality index from the clicked ID
      nationIndex <- as.numeric(gsub("nation", "", clickedId))
      # Get the selected nationality based on the index
      selectedNation <- playerNationalitiesReactive()$Nation[nationIndex]
      # Filter the players from the selected league by the selected nationality
      playersFromNation <- playersFromLeague %>%
        filter(Nation == selectedNation) %>%
        select(Player, Nation, Age, Value, `G+A`)
      
      # Render the filtered player data in a DataTable
      # The renderDataTable function renders an interactive table using the DT package
      output$playerTableLeague <- renderDataTable({
        datatable(playersFromNation, options = list(pageLength = 5))
      })
    }
  })
  
  # Tab 3: Clustering
  
  # Prepare data for clustering
  # The reactive expression 'plot_data' updates automatically whenever the underlying data changes
  plot_data <- reactive({
    # Include Age in the distance matrix calculation
    # The dist function computes the Euclidean distance matrix based on the "G+A", "Value", and "Age" columns
    dist_mat <- dist(final_data[, c("G+A", "Value", "Age")])
    
    # Perform hierarchical clustering using the Ward's method
    # The hclust function performs hierarchical clustering on the distance matrix using the ward.D2 method
    hclust_res <- hclust(dist_mat, method = "ward.D2")
    # Cut the hierarchical clustering tree into 4 clusters
    # The cutree function assigns cluster labels to each observation based on the specified number of clusters (k = 4)
    clusters <- cutree(hclust_res, k = 4)
    
    # Add cluster labels and ensure numeric data types for plotting
    # The mutate function adds a new column "Cluster" with the assigned cluster labels
    # The Value, G+A, and Age columns are converted to numeric data type
    final_data %>%
      mutate(Cluster = as.factor(clusters),
             Value = as.numeric(Value),
             `G+A` = as.numeric(`G+A`),
             Age = as.numeric(Age))  # Ensure Age is numeric if not already
  })
  
  # Render the cluster plot
  # The renderPlotly function renders an interactive scatter plot using the plotly library
  output$clusterPlot <- renderPlotly({
    # Get the prepared data for plotting
    plot_data <- plot_data()
    # Create a scatter plot using the plot_ly function
    # The x-axis represents the "G+A" (Goals + Assists) values
    # The y-axis represents the "Value" (Market Value) values
    # The text argument specifies the hover text for each point, including player name, age, G+A, and value
    # The color argument assigns different colors to each cluster using the RColorBrewer palette
    plot_ly(plot_data, x = ~`G+A`, y = ~Value, text = ~paste("Name:", Player, "<br>Age:", Age, "<br>G+A:", `G+A`, "<br>Value:", Value),
            mode = "markers", color = ~Cluster, colors = RColorBrewer::brewer.pal(4, "Dark2")) %>%
      # Customize the plot layout
      # The title argument sets the title of the plot
      # The xaxis and yaxis arguments set the labels for the x-axis and y-axis, respectively
      # The hovermode argument sets the behavior of the hover tooltip to "closest" point
      layout(title = "Hierarchical Clustering of Players by G+A, Value, and Age",
             xaxis = list(title = "Goals + Assists (G+A)"),
             yaxis = list(title = "Value, in millions of pounds(£)"),
             hovermode = "closest")
  })
  
  # Update the cluster selection dropdown based on the available clusters
  # The observe function is used to update the choices of the cluster selection dropdown
  # It is triggered whenever the plot_data reactive expression updates
  observe({
    # Update the choices of the "clusterSelect" dropdown input
    # The levels function extracts the unique cluster labels from the "Cluster" column of the plot_data
    # The updateSelectInput function updates the choices of the dropdown input with the cluster labels
    updateSelectInput(session, "clusterSelect", choices = levels(plot_data()$Cluster))
  })
  
  # Render the cluster table
  # The renderDT function renders an interactive table using the DT package
  output$clusterTable <- renderDT({
    # Ensure that a cluster is selected before rendering the table
    req(input$clusterSelect)
    
    # Filter the plot_data based on the selected cluster
    # The filter function selects only the rows corresponding to the selected cluster
    # The select function chooses the columns to be displayed in the table (Player, Age, G+A, Value, Cluster)
    filtered_data <- plot_data() %>%
      filter(Cluster == input$clusterSelect) %>%
      select(Player, Age, `G+A`, Value, Cluster)
    
    # Create an interactive table using the datatable function
    # The options argument sets the number of rows per page to 10 and disables search highlighting
    datatable(filtered_data, options = list(pageLength = 10, searchHighlight = FALSE))
  })
  
}

# Run the Shiny app
shinyApp(ui, server)
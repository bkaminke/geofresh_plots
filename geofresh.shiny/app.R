# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# Find out more about building applications with Shiny here: http://shiny.rstudio.com/

library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(leaflet)

# Ui
ui <- fluidPage(
  theme = shinytheme("cerulean"),

  # Navbar
  navbarPage(
    "GeoFresh Tutorial Points",

    # Homepage
    tabPanel("Home", "Welcome to the GeoFresh Tutorial Points App!",
      "For this example, we will use a random subset of 100 occurrence points,
         drawn from the Harmonised freshwater fish occurrence and abundance data for 12 federal states in Germany, downloaded from GBIF.",
      "The points are visualized on the map.", "Select a specific tab to explore the environmental variables for the tutorial points.",
      "For more Information please see: http://geofresh.org/.",
      h3("Map"),
      fluidPage(leafletOutput("map")),
      div(style = "height: 10px;"),
      h3("Points"),
      dataTableOutput("table"),
      value = "home"
    ),

    # Tab 1 (Climate)
    tabPanel(
      "Climate",
      sidebarLayout(
        sidebarPanel(
          sliderInput("localBins",
            "Number of Bins (local):",
            min = 1,
            max = 50,
            value = 30
          ),
          selectInput("localColumn", "Select Column for Local Data:",
            choices = named_climate
          ),
          sliderInput("upstreamBins", "Number of Bins (upstream):",
            min = 1,
            max = 50,
            value = 30
          ),
          selectInput("upstreamColumn", "Select Column for Upstream Data:",
            choices = named_climate
          )
        ),
        mainPanel(
          plotOutput("climateLocalDistPlot"),
          plotOutput("climateUpstreamDistPlot")
        )
      )
    ),

    # Tab 2 (Topography)
    tabPanel(
      "Topography",
      sidebarLayout(
        sidebarPanel(
          sliderInput("topographyLocalBins", "Number of Bins (local):",
            min = 1,
            max = 50,
            value = 30
          ),
          selectInput("topographyLocalColumn", "Select Column for Local Data:",
            choices = named_topography
          ),
          sliderInput("topographyUpstreamBins", "Number of Bins (upstream):",
            min = 1,
            max = 50,
            value = 30
          ),
          selectInput("topographyUpstreamColumn", "Select Column for Upstream Data:",
            choices = named_topography
          ),
        ),
        mainPanel(
          plotOutput("topographyLocalDistPlot"),
          plotOutput("topographyUpstreamDistPlot"),
          plotOutput("strahlerOrderPlot")
        )
      )
    ),

    # Tab 3 (Soil)
    tabPanel(
      "Soil",
      sidebarLayout(
        sidebarPanel(
          sliderInput("soilLocalBins", "Number of Bins (local):",
            min = 1,
            max = 50,
            value = 30
          ),
          selectInput("soilLocalColumn", "Select Column for Local Data:",
            choices = named_soil
          ),
          sliderInput("soilUpstreamBins", "Number of Bins (upstream):",
            min = 1,
            max = 50,
            value = 30
          ),
          selectInput("soilUpstreamColumn", "Select Column for Upstream Data:",
            choices = named_soil
          )
        ),
        mainPanel(
          plotOutput("soilLocalDistPlot"),
          plotOutput("soilUpstreamDistPlot")
        )
      )
    ),

    # Tab 4 (Landcover)
    tabPanel(
      "Landcover",
      sidebarLayout(
        sidebarPanel(
          tags$style(HTML(".checkbox-inline { display: inline-block; margin-right: 100px; }")),
          checkboxGroupInput("Landcoverlocal", "Choose columns for Landcover local data:",
            choices = named_landcover,
            selected = colnames(LandcoverLo),
            inline = TRUE
          ),
          checkboxGroupInput("Landcoverupstream",
            "Choose columns for Landcover upstream data:",
            choices = named_landcover,
            selected = colnames(LandcoverUp),
            inline = TRUE
          )
        ),
        mainPanel(
          plotOutput("landcoverLocalDistPlot"),
          plotOutput("landcoverUpstreamDistPlot")
        )
      )
    )
  ),
  # Citation
  verbatimTextOutput("citation"),
  tags$style(HTML("
    #citation {
      font-family: 'Arial', sans-serif;
      margin-top: 40px;
    }
  ")
)
)

# Server
server <- function(input, output) {
  # Leaflet Map
  output$map <- renderLeaflet({
    Map <- leaflet(TutorialPoints)
    Map <- addTiles(Map)
    Map <- Map %>%
      addMarkers(
        lng = ~decimalLongitude,
        lat = ~decimalLatitude,
        popup = ~ paste("gbifID ", gbifID)
      )
    Map
  })
  # Point Table
  output$table <- renderDataTable({
    datatable(TutorialPoints)
  })

  # Output Histograms
  # Climate
  output$climateLocalDistPlot <- renderPlot({
    x <- ClimateLo[[input$localColumn]]
    x <- na.omit(x)
    bins <- seq(min(x), max(x), length.out = input$localBins + 1)

    hist(x,
      breaks = bins, col = "#75AADB", border = "black",
      xlab = input$localColumn,
      main = paste("Histogram of", names(named_climate)[named_climate == input$localColumn], "(local)")
    )
  })

  output$climateUpstreamDistPlot <- renderPlot({
    x <- ClimateUp[[input$upstreamColumn]]
    x <- na.omit(x)
    bins <- seq(min(x), max(x), length.out = input$upstreamBins + 1)

    hist(x,
      breaks = bins, col = "#75AADB", border = "black",
      xlab = input$upstreamColumn,
      main = paste("Histogram of", names(named_climate)[named_climate == input$upstreamColumn], "(upstream)")
    )
  })

  # Soil
  output$soilLocalDistPlot <- renderPlot({
    x <- SoilLo[[input$soilLocalColumn]]
    x <- na.omit(x)
    bins <- seq(min(x), max(x), length.out = input$soilLocalBins + 1)

    hist(x,
      breaks = bins, col = "#75AADB", border = "black",
      xlab = input$soilLocalColumn,
      main = paste("Histogram of", names(named_soil)[named_soil == input$soilLocalColumn], "(local)")
    )
  })

  output$soilUpstreamDistPlot <- renderPlot({
    x <- SoilUp[[input$soilUpstreamColumn]]
    x <- na.omit(x)
    bins <- seq(min(x), max(x), length.out = input$soilUpstreamBins + 1)

    hist(x,
      breaks = bins, col = "#75AADB", border = "black",
      xlab = input$soilUpstreamColumn,
      main = paste("Histogram of", names(named_soil)[named_soil == input$soilUpstreamColumn], "(upstream)")
    )
  })

  # Topography
  output$topographyLocalDistPlot <- renderPlot({
    x <- TopographyLo[[input$topographyLocalColumn]]
    x <- na.omit(x)
    bins <- seq(min(x), max(x), length.out = input$topographyLocalBins + 1)

    hist(x,
      breaks = bins, col = "#75AADB", border = "black",
      xlab = input$topographyLocalColumn,
      main = paste("Histogram of", names(named_topography)[named_topography == input$topographyLocalColumn], "(local)")
    )
  })

  output$topographyUpstreamDistPlot <- renderPlot({
    x <- TopographyUp[[input$topographyUpstreamColumn]]
    x <- na.omit(x)
    bins <- seq(min(x), max(x), length.out = input$topographyUpstreamBins + 1)

    hist(x,
      breaks = bins, col = "#75AADB", border = "black",
      xlab = input$topographyUpstreamColumn,
      main = paste("Histogram of", names(named_topography)[named_topography == input$topographyUpstreamColumn], "(upstream)")
    )
  })

  # Strahler-Diagram
  output$strahlerOrderPlot <- renderPlot({
    x <- TopographyLo$strahler
    x <- na.omit(x)

    barplot(table(x),
      col = "#75AADB",
      xlab = "Strahler Order",
      ylab = "Frequency",
      main = "Frequency of the Flow Order Numbers"
    )
  })

  # Output Landcover Boxplots
  output$landcoverLocalDistPlot <- renderPlot({
    selected_columns <- input$Landcoverlocal
    boxplot(LandcoverLo[, selected_columns], main = "Landcover local")
  })

  output$landcoverUpstreamDistPlot <- renderPlot({
    selected_columns <- input$Landcoverupstream
    boxplot(LandcoverUp[, selected_columns], main = "Landcover upstream")
  })
  
  #Citation
  output$citation <- renderPrint({
    cat("The data in this app is based on the GeoFRESH platform, published by: Domisch, S., Bremerich, V., Torres-Cambas, Y., Grigoropoulou, A., Garcia Marquez, J.R., Amatulli, G., De Meester, L., Grossart, H.-P. , Gessner, M., Mehner, T. & Adrian, R. (2023). Raumzeitliche Süßwasserdaten auf den richtigen Weg bringen – hin zur Interoperabilität süßwasserspezifischer Daten. Zenodo. https://doi.org/10.5281/zenodo.7888389.")
  })
}
# Create the Shiny App
shinyApp(ui = ui, server = server)

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(DT)

TutorialPoints <- read.csv("U:/R-Studio/geofresh_plots/data/tutorial_points.csv")
ClimateLo <- read.csv("U:/R-Studio/geofresh_plots/data/geofresh-2023-11-13-env-var-climate-local.csv")
ClimateUp <- read.csv("U:/R-Studio/geofresh_plots/data/geofresh-2023-11-13-env-var-climate-upstream.csv")
LandcoverLo <- read.csv("U:/R-Studio/geofresh_plots/data/geofresh-2023-11-13-env-var-land-cover-local.csv")
LandcoverUp <- read.csv("U:/R-Studio/geofresh_plots/data/geofresh-2023-11-13-env-var-land-cover-upstream.csv")
SoilLo <- read.csv("U:/R-Studio/geofresh_plots/data/geofresh-2023-11-13-env-var-soil-local.csv")
SoilUp <- read.csv("U:/R-Studio/geofresh_plots/data/geofresh-2023-11-13-env-var-soil-upstream.csv")
TopographyLo <-read.csv("U:/R-Studio/geofresh_plots/data/geofresh-2023-11-13-env-var-topography-local.csv")
TopographyUp <- read.csv("U:/R-Studio/geofresh_plots/data/geofresh-2023-11-13-env-var-topography-upstream.csv")
EnvironmentalVariables <- read.csv("U:/R-Studio/geofresh_plots/data/geofresh_environmental_variables - env_var_for_plots.csv")

ui <- fluidPage(theme = shinytheme('cerulean'),
                
# Navbar 
navbarPage("GeoFresh Tutorial Points",

# Homepage
tabPanel("Home","Welcome to the GeoFresh Tutorial Points App!",
         'For this example, we will use a random subset of 100 occurrence points, 
         drawn from the Harmonised freshwater fish occurrence and abundance data for 12 federal states in Germany, downloaded from GBIF.',
         'The points are visualized on the map.','Select a specific tab to explore various environmental variables for the tutorial points.','You can find more information on the GeoFresh website.',
         h3("Map"),
         fluidPage(leafletOutput("map")),
         div(style = "height: 10px;"),
         h3("Points"),
         dataTableOutput("table"),
         value = "home"), 

# Tab 1 (Climate)
 tabPanel("Climate",
  sidebarLayout(
   sidebarPanel(
    sliderInput("localBins",
    "Number of Bins (local):",
    min = 1,
    max = 50,
    value = 30),
    selectInput("localColumn","Select Column for Local Data:",
    choices = colnames(ClimateLo)[endsWith(colnames(ClimateLo), "_mean")],
    selected = "bio1_mean"),
    sliderInput("upstreamBins","Number of Bins (upstream):",
    min = 1,
    max = 50,
    value = 30),
    selectInput("upstreamColumn","Select Column for Upstream Data:",
    choices = colnames(ClimateUp)[endsWith(colnames(ClimateUp), "_mean")],
    selected = "bio1_mean")),
 mainPanel(
    plotOutput("climateLocalDistPlot"),
    plotOutput("climateUpstreamDistPlot")))),
                  
# Tab 2 (Topography)
 tabPanel("Topography", 
  sidebarLayout(
   sidebarPanel(
    sliderInput("topographyLocalBins","Number of Bins (local):",
    min = 1,
    max = 50,
    value = 30),
    selectInput("topographyLocalColumn","Select Column for Local Data:",
    choices = colnames(TopographyLo)[endsWith(colnames(TopographyLo), "_mean")],
    selected = "some_topography_column"),  
    sliderInput("topographyUpstreamBins","Number of Bins (upstream):",
    min = 1,
    max = 50,
    value = 30),
    selectInput("topographyUpstreamColumn","Select Column for Upstream Data:",
    choices = colnames(TopographyUp)[endsWith(colnames(TopographyUp), "_mean")],
    selected = "some_topography_column")),
 mainPanel(
    plotOutput("topographyLocalDistPlot"),
    plotOutput("topographyUpstreamDistPlot"),
    plotOutput("strahlerOrderPlot")))),
                  
# Tab 3 (Soil)
 tabPanel("Soil", 
  sidebarLayout(
   sidebarPanel(
    sliderInput("soilLocalBins","Number of Bins (local):",
    min = 1,
    max = 50,
    value = 30),
    selectInput("soilLocalColumn",
                "Select Column for Local Data:",
                choices = c("awcts_mean", "wwp_mean", "orcdrc_mean", "phihox_mean", "bldfie_mean", "cecsol_mean",
                            "crfvol_mean", "acdwrb_mean", "bdricm_mean", "bdrlog_mean", "histpr_mean"),
                selected = "awcts_mean"),    
    sliderInput("soilUpstreamBins","Number of Bins (upstream):",
    min = 1,
    max = 50,
    value = 30),
    selectInput("soilUpstreamColumn",
                "Select Column for Upstream Data:",
                choices = c("awcts_mean", "wwp_mean", "orcdrc_mean", "phihox_mean", "bldfie_mean", "cecsol_mean",
                            "crfvol_mean","acdwrb_mean", "bdricm_mean", "bdrlog_mean", "histpr_mean"),
                selected = "awcts_mean")),
 mainPanel(
    plotOutput("soilLocalDistPlot"),
    plotOutput("soilUpstreamDistPlot"),
    plotOutput("soilBoxplot")))),
                  
# Tab 4 (Landcover)
tabPanel("Landcover", 
 sidebarLayout(
  sidebarPanel(
   tags$style(HTML('.checkbox-inline { display: inline-block; margin-right: 60px; }')),
   checkboxGroupInput('Landcoverlocal', 'Choose columns for Landcover local data:', 
   choices = colnames(LandcoverLo)[grepl("^c", colnames(LandcoverLo))],
   selected = colnames(LandcoverLo),
   inline = TRUE),  
             
   checkboxGroupInput('Landcoverupstream', 
   'Choose columns for Landcover upstream data:', 
   choices = colnames(LandcoverUp)[grepl("^c", colnames(LandcoverUp))],
   selected = colnames(LandcoverUp),
   inline = TRUE)),
  mainPanel
   (plotOutput("landcoverLocalDistPlot"),
   plotOutput("landcoverUpstreamDistPlot"))))))

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
        popup = ~paste("gbifID ", gbifID)
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
    x    <- ClimateLo[[input$localColumn]]
    x    <- na.omit(x)
    bins <- seq(min(x), max(x), length.out = input$localBins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "black",
         xlab = input$localColumn,
         main = paste("Histogram of", input$localColumn, 'local'))
  })
  
  output$climateUpstreamDistPlot <- renderPlot({
    x    <- ClimateUp[[input$upstreamColumn]]
    x    <- na.omit(x)
    bins <- seq(min(x), max(x), length.out = input$upstreamBins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "black",
         xlab = input$upstreamColumn,
         main = paste("Histogram of", input$upstreamColumn, 'upstream'))
  })
  
# Soil
  output$soilLocalDistPlot <- renderPlot({
    x <- SoilLo[[input$soilLocalColumn]]
    x <- na.omit(x)
    bins <- seq(min(x), max(x), length.out = input$soilLocalBins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "black",
         xlab = input$soilLocalColumn,
         main = paste("Histogram of", input$soilLocalColumn, 'local'))
  })
  
  output$soilUpstreamDistPlot <- renderPlot({
    x <- SoilUp[[input$soilUpstreamColumn]]
    x <- na.omit(x)
    bins <- seq(min(x), max(x), length.out = input$soilUpstreamBins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "black",
         xlab = input$soilUpstreamColumn,
         main = paste("Histogram of", input$soilUpstreamColumn, 'upstream'))
  })
  
  output$soilBoxplot <- renderPlot({
    selected_columns <- c("clyppt_mean", "sndppt_mean", "sltppt_mean", "slgwrb_mean")
    boxplot(SoilLo[, selected_columns], main = "Boxplot of selected Soil columns")
  })
  
# Topography
  output$topographyLocalDistPlot <- renderPlot({
    x    <- TopographyLo[[input$topographyLocalColumn]]
    x    <- na.omit(x)
    bins <- seq(min(x), max(x), length.out = input$topographyLocalBins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "black",
         xlab = input$topographyLocalColumn,
         main = paste("Histogram of", input$topographyLocalColumn, 'local'))
  })
  
  output$topographyUpstreamDistPlot <- renderPlot({
    x    <- TopographyUp[[input$topographyUpstreamColumn]]
    x    <- na.omit(x)
    bins <- seq(min(x), max(x), length.out = input$topographyUpstreamBins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "black",
         xlab = input$topographyUpstreamColumn,
         main = paste("Histogram of", input$topographyUpstreamColumn, 'upstream'))
  })

# Strahler-Diagram
  output$strahlerOrderPlot <- renderPlot({
    x <- TopographyLo$strahler  
    x <- na.omit(x)
    
  barplot(table(x), 
            col = "#75AADB",
            xlab = "Strahler Order",
            ylab = "Frequency",
            main = "Frequency of the Flow Order Numbers")
  })
  
# Output Landcover Boxplots 
  output$landcoverLocalDistPlot <- renderPlot({
    selected_columns <- input$Landcoverlocal
    boxplot(LandcoverLo[, selected_columns], main = "Landcover local ")
  })
  
  output$landcoverUpstreamDistPlot <- renderPlot({
    selected_columns <- input$Landcoverupstream
    boxplot(LandcoverUp[, selected_columns], main = "Landcover upstream")
  })
}
# Create the Shiny App
shinyApp(ui = ui, server = server)
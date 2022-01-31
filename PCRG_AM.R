library(tidyverse)
library(sf)
library(mapview)
library(shiny)
library(leaflet)
library(shinyTree)

# Read shapefile and filter out overlaps in catch areas
ps_crab_sg <- st_read( "./Input/Puget Sound_Hexagonal Survey Grid_2000ft.shp") %>%
  st_make_valid() %>%
  filter(Region == "1" & CatchArea == "20A" |
           Region == "1" & CatchArea == "20B" |
           Region == "1" & CatchArea == "21A" |
           Region == "1" & CatchArea == "21B" |
           Region == "1" & CatchArea == "22A" |
           Region == "1" & CatchArea == "22B" |
           Region == "3" & Subregion == "3-1" & CatchArea == "23A" |
           Region == "3" & Subregion == "3-1" & CatchArea == "23B" |
           Region == "3" & Subregion == "3-2" & CatchArea == "23D" |
           Region == "3" & Subregion == "3-2" & CatchArea == "25A" |
           Region == "3" & Subregion == "3-2" & CatchArea == "25E" |
           Region == "3" & Subregion == "3-3" & CatchArea == "23C" |
           Region == "3" & Subregion == "3-4" & CatchArea == "29" |
           Region == "2W" & CatchArea == "25B" |
           Region == "2W" & CatchArea == "25D" |
           Region == "2W" & CatchArea == "26A-W" |
           Region == "2E" & CatchArea == "24A" |
           Region == "2E" & CatchArea == "24B" |
           Region == "2E" & CatchArea == "24C" |
           Region == "2E" & CatchArea == "24D" |
           Region == "2E" & CatchArea == "26A-E" |
           Region == "4" & CatchArea == "26B" |
           Region == "4" & CatchArea == "26C" |
           Region == "5" & CatchArea == "25C" |
           Region == "5" & CatchArea == "27A" |
           Region == "5" & CatchArea == "27B" |
           Region == "5" & CatchArea == "27C" |
           Region == "6" & CatchArea == "26D" |
           Region == "6" & CatchArea == "28A" |
           Region == "6" & CatchArea == "28B" |
           Region == "6" & CatchArea == "28C" |
           Region == "6" & CatchArea == "28D")

# Create mapview object from shapefile
ps.map <- mapview(ps_crab_sg,
                  zcol =  "Region",
                  cex = NULL,
                  alpha = 0.25,
                  legend = TRUE,
                  label = "ID",
                  pane = NULL)

# Define shiny UI
ui <- fluidPage(
  titlePanel("PCRG Fishery-Independent Survey Tool"),
  sidebarLayout(
    sidebarPanel(
      shinyTree("tree", checkbox = TRUE),
    numericInput("pots", "Number of pots:", 20),
    actionButton("button", "Generate points")
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Define shiny server function
server <- function(input, output, session) {
  
  # Create reactive container to receive pot number (button) and catch area (tree) inputs and filter shapefile
  filteredData <- reactive({
    if (input$button == FALSE) {
    ps_crab_sg %>%
      filter(CatchArea %in% get_selected(input$tree))
    } else {
      ps_crab_sg %>%
        filter(CatchArea %in% get_selected(input$tree))  %>%
        sample_n(input$pots)
    }
  })
  
  # Load leaflet map with unfiltered Puget Sound data 
  output$map <- renderLeaflet({
    mapview:::mapview2leaflet(ps.map)
  })
  
  # Create hierarchical shinyTree object to allow user selection of regions/ catch areas
  output$tree <- renderTree({ 
    list("Puget Sound" = structure(list(
      "Region 1" = structure(list("20A"=structure("", sticon = ""), 
                                  "20B"=structure("", sticon = ""), 
                                  "21A"=structure("", sticon = ""), 
                                  "22A"=structure("", sticon = "")), sticon = ""),
      "Region 2W" = structure(list("25B"=structure("", sticon = ""), 
                                   "25D"=structure("", sticon = ""), 
                                   "26A-W"=structure("", sticon = "")), sticon = ""),
      "Region 2E" = structure(list("24A"=structure("", sticon = ""), 
                                   "24B"=structure("", sticon = ""), 
                                   "24C"=structure("", sticon = ""), 
                                   "24D"=structure("", sticon = ""), 
                                   "26A-E"=structure("", sticon = "")), sticon = ""),
      "Region 3" = structure(list(
        "3-1" = structure(list("23A"=structure("", sticon = ""), 
                               "23B"=structure("", sticon = "")), sticon = ""),
        "3-2" = structure(list("23D"=structure("", sticon = ""), 
                               "25A"=structure("", sticon = ""), 
                               "25E"=structure("", sticon = "")), sticon = ""),
        "3-3" = structure(list("23C"=structure("", sticon = "")), sticon = ""),
        "3-4" = structure(list("29"=structure("", sticon = "")), sticon = "")), sticon = ""),
      "Region 4" = structure(list("26B"=structure("", sticon = ""), 
                                  "26C"=structure("", sticon = "")), sticon = ""),
      "Region 5" = structure(list("25C"=structure("", sticon = ""), 
                                  "27A"=structure("", sticon = ""), 
                                  "27B"=structure("", sticon = ""), 
                                  "27C"=structure("", sticon = "")), sticon = ""),
      "Region 6" = structure(list("26D"=structure("", sticon = ""), 
                                  "28A"=structure("", sticon = ""), 
                                  "28B"=structure("", sticon = ""), 
                                  "26C"=structure("", sticon = ""), 
                                  "28D"=structure("", sticon = "")), sticon = "")), stopened = TRUE, sticon = ""))
  })
  
# Filter data based on inputs 
  observe({
    req(input$tree)
    
    # Conditional reverts to Puget Sound map if all regions are unselected
    if (length(get_selected(input$tree)) != 0) {
      filteredMap <- mapview(filteredData(),
                             zcol =  "CatchArea",
                             cex = NULL,
                             alpha = 0.25,
                             legend = TRUE,
                             label = "ID",
                             pane = NULL)
      
      output$map <- renderLeaflet({
        mapview:::mapview2leaflet(filteredMap)
      })
    } else {
      output$map <- renderLeaflet({
        mapview:::mapview2leaflet(ps.map)
      })
    }
    
  })
  
}


# Execture shiny app
shinyApp(ui, server)

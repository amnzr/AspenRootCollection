#load required libraries
library(DT)
library(leaflet)
library(magrittr)
library(Polychrome)
library(RColorBrewer)
library(shiny)
library(shinydashboard)
library(stringr)
library(tidyverse)

#the server logic
shinyServer(function(input, output, session) {
  #reading input
  data <- read.csv("data/NordAsp_root_coordinates.csv", strip.white = TRUE) %>%
    type.convert(as.is = TRUE) %>%
    as_tibble()

  #convert coordinates to numeric
  data$Longitude <- as.numeric(data$Longitude) %>% suppressWarnings()
  data$Latitude <- as.numeric(data$Latitude) %>% suppressWarnings()

  #trimming the wrong coordinates. Kiping only if they are in this format: dd.ddddd
  data <- data[grepl("^\\d+\\.\\d+$", data$Latitude) | grepl("^\\d+\\.\\d+$", data$Longitude), ]

  #remove the wrong and strange coordinates
  data %<>% filter(!Tree %in% c("R32", "K6","K9", "E26"))

  #extract the section and set as new column
  data %<>% mutate(Section = str_match(Tree, "^([A-Za-z]+)")[,1], .after = Tree)

  # filter data based on the sidebar selection
  data_filter <- reactive({
    data %>% filter(Section %in% input$section,
                    Sampled_by %in% input$sampled_by)
  })

  #create a color palette
  pal <- colorFactor(palette = glasbey.colors(length(unique(data$Section))),
                     domain = unique(data$Section))


  output$map <- renderLeaflet({
    leaflet(data) %>%
      addCircles(lng = ~Longitude, lat = ~Latitude) %>%
      addTiles() %>%
      addCircleMarkers(data = data, lat =  ~Latitude, lng =~Longitude,
                       radius = 5, popup = ~as.character(Tree),
                       color = ~pal(Section),
                       stroke = FALSE, fillOpacity = 0.8) %>%
      addLegend(pal=pal, values=data$Section, opacity=1,
                na.label = "Not Available", title = "Section") %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="ME",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
  })

  output$data <- renderDataTable(datatable(data,filter = 'top'))
})



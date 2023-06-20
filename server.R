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

  #trimming the wrong format coordinates. Kipping only if they are in this format: dd.ddddd
  data <- data[grepl("^\\d+\\.\\d+$", data$Latitude) | grepl("^\\d+\\.\\d+$", data$Longitude), ]

  #remove the wrong and strange coordinates
  data %<>% filter(!Tree %in% c("R32", "K6","K9", "E26"))

  #extract the section and set as new column
  data %<>% mutate(Section = str_match(Tree, "^([A-Za-z]+)")[,1], .after = Tree)

  #create a color palette
  pal <- reactive({
    colorFactor(palette = glasbey.colors(length(unique(data$Section))),
                domain = unique(data$Section))
  })

  output$map <- renderLeaflet({
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(data = data, lat =  ~Latitude, lng =~Longitude,
                       radius = 5,
                       label = ~Tree,
                       popup = ~paste("<strong>", Tree, "</strong>",
                                      "<br>Sampled_by:", Sampled_by,
                                      "<br>Sampling_date:", Sampling_date),
                       color = ~pal()(data_filter()$Section),
                       stroke = FALSE, fillOpacity = 0.8) %>%
      addLegend(pal=pal(),
                values=unique(data_filter()$Section),
                opacity=1, na.label = "Not Available", title = "Section") %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="ME",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
  })


  # filter data based on the sidebar selection
  data_filter <- reactive({
    if (length(input$section) > 0 ) {
      data <- data[data$Section %in% input$section,]
    }
    if (length(input$sampled_by) > 0 ) {
      data <- data[grepl(paste(input$sampled_by, collapse = "|"), data$Sampled_by), ]
    }
    # else {
    #   data <- data()
    # }
    # print(data)
    return(unique(data))
  })

  output$table <- renderDataTable(datatable(data_filter(), filter = 'top'))
  # print(data_filter())

  observe({
    leafletProxy("map") %>%
      clearMarkers() %>%
      addCircleMarkers(data = data_filter(), lat =  ~Latitude, lng =~Longitude,
                       radius = 5,
                       label = ~Tree,
                       popup = ~paste("<strong>", Tree, "</strong>",
                                      "<br>Sampled_by:", Sampled_by,
                                      "<br>Sampling_date:", Sampling_date),
                       color = ~pal()(data_filter()$Section),
                       stroke = FALSE, fillOpacity = 0.8)

  })
})



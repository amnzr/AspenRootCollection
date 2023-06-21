#load required libraries
library(DT)
library(leaflet)
library(magrittr)
library(Polychrome)
library(RColorBrewer)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
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

  #sending the sidebar to UI
  output$sidebar <- renderUI({
    dashboardSidebar(
      pickerInput("section", "Section(s)",
                  choices = c(data$Section %>%
                                unique()),
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE)),
      pickerInput("sampled_by", "Sampled By",
                  choices = data$Sampled_by %>%
                    str_split("_") %>%
                    unlist() %>%
                    unique(),
                  selected = NULL,
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE)),
      dateRangeInput("dates", "Sampling dates:",
                     start = min(data$Sampling_date),
                     end = max(data$Sampling_date)),
      prettySwitch("dark_mode", "Dark mode")
    )
  })

  #create a color palette
  pal <- reactive({
    colorFactor(palette = glasbey.colors(length(unique(data$Section))),
                domain = unique(data$Section))
  })

  output$map <- renderLeaflet({
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(data = data_filter(), lat =  ~Latitude, lng =~Longitude,
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
    if (length(input$dates) > 0 ) {
      data <- subset(data, data$Sampling_date >= input$dates[1] & data$Sampling_date <= input$dates[2])
    }

    # else {
    #   data <- data()
    # }
    # print(data)
    return(unique(data))
  })

  #rendering the value boxes
  output$selected_trees <- renderText(
    nrow(data_filter())
  )

  output$collectors <- renderText(
    data_filter()$Sampled_by %>%
      str_split("_") %>%
      unlist() %>%
      unique() %>%
      length()
  )

  #rendering the data for table
  output$table <- renderDataTable(datatable(data_filter(), filter = 'top'))

  # #update the map based on filtering
  # observe({
  #   leafletProxy("map") %>%
  #     clearMarkers() %>%
  #     addCircleMarkers(data = data_filter(), lat =  ~Latitude, lng =~Longitude,
  #                      radius = 5,
  #                      label = ~Tree,
  #                      popup = ~paste("<strong>", Tree, "</strong>",
  #                                     "<br>Sampled_by:", Sampled_by,
  #                                     "<br>Sampling_date:", Sampling_date),
  #                      color = ~pal()(data_filter()$Section),
  #                      stroke = FALSE, fillOpacity = 0.8)
  #
  # })

  #updating the selectInputs section and sampled_by based on each other
  observe({
    if(!is.null(input$section))
      updatePickerInput(session, "sampled_by",
                        choices = data[grepl(paste(input$section, collapse = "|"),
                                             data$Section), "Sampled_by"]$Sampled_by %>%
                          str_split("_") %>%
                          unlist() %>%
                          unique(),
                        selected = isolate(input$sampled_by))
    else
      updatePickerInput(session, "sampled_by",
                        choices = data$Sampled_by %>%
                          str_split("_") %>%
                          unlist() %>%
                          unique(),
                        selected = NULL)
  })

  observe({
    if(!is.null(input$sampled_by))
      updatePickerInput(session, "section",
                        choices = data[grepl(paste(input$sampled_by, collapse = "|"),
                                             data$Sampled_by), "Section"]$Section %>% unique(),
                        selected = isolate(input$section))
    else
      updatePickerInput(session, "section",
                        choices = data$Section %>%
                          unique(),
                        selected = NULL)
  })

  observe({
    if(!is.null(input$section) || !is.null(input$sampled_by))
      updateDateRangeInput(session, "date",
                           start = min(data_filter()$Sampling_date),
                           end = max(data_filter()$Sampling_date))
    else
      updateDateRangeInput(session, "date",
                           start = min(data$Sampling_date),
                           end = max(data$Sampling_date))
  })


  observe(session$setCurrentTheme(
    if (isTRUE(input$dark_mode))
      bs_theme(bg = "black", fg = "grey", primary = "grey")
    else
      bs_theme(primary = "red", secondary = "#abd1de")
  ))
})



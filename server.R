#load required libraries
library(bsTools)
library(DT)
library(leaflet)
library(lubridate)
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
  data <- read.csv("data/NordAsp_root_coordinates.csv") %>%
    type.convert(as.is = TRUE) %>%
    as_tibble()

  #convert coordinates to numeric
  data$Longitude <- as.numeric(data$Longitude) %>% suppressWarnings()
  data$Latitude <- as.numeric(data$Latitude) %>% suppressWarnings()
  data$Sampling_date <- as.Date(data$Sampling_date)

  # #trimming the wrong format coordinates. Kipping only if they are in this format: dd.ddddd
  # data <- data[grepl("^\\d+\\.\\d+$", data$Latitude) | grepl("^\\d+\\.\\d+$", data$Longitude), ]
  #
  # #remove the wrong and strange coordinates
  # data %<>% filter(!Tree %in% c("R32", "K6","K9", "E26"))

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
      airDatepickerInput("dates", "Sampling dates:",
                         minDate = min(data$Sampling_date),
                         maxDate = max(data$Sampling_date),
                         range = TRUE,
                         clearButton = TRUE,
                         update_on = 'close'
      ),
      sliderInput("radius", "Radius(m)", min = 100, max = 1000, value = 500, step = 100),
      prettySwitch("dark_mode", "Dark mode"),
      checkboxInput("blind", "Color Blind Mode", value = FALSE),
      conditionalPanel(condition = "input.blind",
                       uiOutput("blind_down"))
      )
  })

  output$blind_down <- renderUI({
    selectInput("blind_pal", "Color Palette",
                choices = c("Red Blind", "Green Blind", "Blue Blind"),
                selected = "deut"
    )
  })

  #create a color palette
  pal <- reactive({
    if(isTRUE(input$blind)) {
      target <- switch(input$blind_pal,
                       "Red Blind" = "deut",
                       "Green Blind" = "prot",
                       "Blue Blind" = "trit"
      )
      p <- colorFactor(palette = createPalette(length(unique(data$Section)),
                                                       "#111111", target = target),
                               domain = unique(data$Section)) }

    else {
      p <- colorFactor(palette = dark.colors(length(unique(data$Section))),
                                domain = unique(data$Section)) }
    return(p)
  })

  leafy <- reactive({
    leaflet(data) %>%
      addTiles() %>%
      addCircles(data = data_filter(), lat =  ~Latitude, lng =~Longitude,
                 radius = 500,
                 opacity = 0.2,
                 color = ~pal()(data_filter()$Section)) %>%
      addCircleMarkers(data = data_filter(), lat =  ~Latitude, lng =~Longitude,
                       radius = 5,
                       label = ~Tree,
                       popup = ~paste("<strong>", Tree, "</strong>",
                                      "<br>Sampled_by:", Sampled_by,
                                      "<br>Sampling_date:", Sampling_date,
                                      "<br>Shipping_date:", Shipping_date),
                       color = ~pal()(data_filter()$Section),
                       fillOpacity = 0.8) %>%
      addLegend(pal=pal(),
                values=unique(data_filter()$Section),
                opacity=0.8, na.label = "Not Available", title = "Section(s)") %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="ME",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
  })

  output$map <- renderLeaflet({
    leafy()
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
      grep(pattern = paste(input$sampled_by,collapse = "|"), value = TRUE) %>%
      unique() %>%
      length()
  )

  #rendering the data for table
  output$table <- renderDataTable(datatable(data_filter(), filter = 'top'))

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

  observeEvent(input$radius, {
    leafletProxy("map") %>%
      clearShapes() %>%
      addCircles(data = data_filter(), lat =  ~Latitude, lng =~Longitude,
                 radius = input$radius,
                 opacity = 0.2,
                 color = ~pal()(data_filter()$Section)) %>%
      addCircleMarkers(data = data_filter(), lat =  ~Latitude, lng =~Longitude,
                       radius = 5,
                       label = ~Tree,
                       popup = ~paste("<strong>", Tree, "</strong>",
                                      "<br>Sampled_by:", Sampled_by,
                                      "<br>Sampling_date:", Sampling_date,
                                      "<br>Shipping_date:", Shipping_date),
                       color = ~pal()(data_filter()$Section),
                       fillOpacity = 0.8)

  })

  observeEvent(input$dark_mode, {
    if(input$dark_mode) {
      leafletProxy("map") %>%
        clearTiles() %>%
        addProviderTiles("CartoDB.DarkMatter")
    } else {
      leafletProxy("map") %>%
        clearTiles() %>%
        addTiles()
    }
  })

  observe(session$setCurrentTheme(
    if (isTRUE(input$dark_mode))
      bs_theme(bg = "black", fg = "grey", primary = "grey")
    else
      bs_theme(primary = "red", secondary = "#abd1de")
  ))
})



function(input, output, session) {
  #reading input
  data <- read.csv("data/NordAsp_root_coordinates.csv", strip.white = TRUE) %>%
    type.convert(as.is = TRUE) %>%
    as_tibble()

  #trimming the wrong coordinates
  data <- data[grepl("^\\d+\\.\\d+$", data$Latitude) | grepl("^\\d+\\.\\d+$", data$Longitude), ]

  #convert coordinates to numeric
  data$Longitude <- as.numeric(data$Longitude)
  data$Latitude <- as.numeric(data$Latitude)

  output$map <- renderLeaflet({
    leaflet(data) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(lng = ~Longitude, lat = ~Latitude)
  })
}

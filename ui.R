library(DT)
library(leaflet)
library(shiny)
library(shinydashboard)
library(stringr)

# ui parts
## header board
header <- dashboardHeader(
  title = 'Aspen Root Sampling')

## Side bar
sidebar <- dashboardSidebar(
  selectInput("section", "Section(s)",
              choices = c(data$Section %>%
                            unique()),
              multiple = TRUE),
  selectInput("sampled_by", "Sampled By",
              choices = data$Sampled_by %>%
                str_split("_") %>%
                unlist() %>%
                unique(),
              selected = NULL,
              multiple = TRUE),
  dateRangeInput("dates", "Sampling dates:", start = NULL, end = NULL)
)

## body
body <- dashboardBody(tags$head(
  tags$style(HTML(".skin-blue .main-header .logo"))
),
tabsetPanel(
  tabPanel("Map", leafletOutput("map", height=1000)),
  tabPanel("Data", dataTableOutput("table")),
  tabPanel("About",includeMarkdown("README.md")))
)

shinyUI(
# putting ui parts together
dashboardPage(
  title = "Aspen root collection",
  header = header,
  sidebar = sidebar,
  body = body
)
)

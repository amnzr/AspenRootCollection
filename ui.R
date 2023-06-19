library(DT)
library(shiny)

# navbarPage(title = "Sampling locations", id="main",
#            tabPanel("Map", leafletOutput("map", height=1000)),
#            tabPanel("Data", dataTableOutput("data")),
#            tabPanel("About",includeMarkdown("README.md")))


# ui parts
## header board
header <- dashboardHeader(
  title = 'Aspen Root Sampling')

## Side bar
sidebar <- dashboardSidebar(
  selectInput("sampled_by", "Sampled By",
              choices = c("Aman", "Tiggy", "Nazeer", "Miko"),
              multiple = TRUE),
  selectInput("section", "Section(s)",
              choices = c("A","B","D"),
              multiple = TRUE),
  dateRangeInput("dates", "Sampling dates:", start = NULL, end = NULL)
)

## body
body <- dashboardBody(tags$head(
  tags$style(HTML(".skin-blue .main-header .logo"))
),
tabsetPanel(
  tabPanel("Map", leafletOutput("map", height=1000)),
  tabPanel("Data", dataTableOutput("data")),
  tabPanel("About",includeMarkdown("README.md")))
)


# putting ui parts together
dashboardPage(
  title = "Aspen root collection",
  header = header,
  sidebar = sidebar ,
  body = body
)

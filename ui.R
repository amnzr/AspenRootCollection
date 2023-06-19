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
  selectInput("section", "Section(s)",
              choices = c(data$Section %>%
                            unique()),
              multiple = TRUE),
  # conditionalPanel(condition = "input.section.length > 0",
  selectInput("sampled_by", "Sampled By",
              choices = data$Sampled_by %>%
                unique() %>%
                str_split("_") %>%
                unlist() %>%
                unique(),
              selected = NULL,
              multiple = FALSE),
  dateRangeInput("dates", "Sampling dates:", start = NULL, end = NULL)
)
# )

## body
body <- dashboardBody(tags$head(
  tags$style(HTML(".skin-blue .main-header .logo"))
),
tabsetPanel(
  tabPanel("Map", leafletOutput("map", height=1000)),
  tabPanel("Data", dataTableOutput("data")),
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

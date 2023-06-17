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
body <- dashboardBody(
  leafletOutput("map")
)

# putting ui parts together
dashboardPage(
  title = "Aspen root collection",
  header = header,
  sidebar = sidebar ,
  body = body
)

library(bsicons)
library(bslib)
library(DT)
library(leaflet)
library(shiny)
library(shinydashboard)
library(stringr)

page_sidebar(theme = bs_theme(secondary = "#21719c"),
             sidebar = uiOutput("sidebar"),
             title = "Aspen root collection",
             layout_column_wrap(
               width = "250px", fill = FALSE,height = "100px",
               value_box(
                 title = "Number of seleted Trees",
                 value = 34,
                 showcase = bs_icon("tree"),
                 theme_color = "secondary"
               ),
               value_box(
                 title = "Number of Collcetors",
                 value = 2,
                 showcase = bs_icon("person-heart"),
                 theme_color = "secondary"
               )
             ),

             tabsetPanel(
               tabPanel("Map", leafletOutput("map", height=1000)),
               tabPanel("Data", dataTableOutput("table")),
               tabPanel("About",includeMarkdown("README.md"))
             )


)

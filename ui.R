library(bsicons)
library(bslib)
library(DT)
library(leaflet)
library(shiny)
library(shinydashboard)
library(stringr)

#   navset_card_tab(
#   sidebar = uiOutput("sidebar"),
#   nav_panel("Map", leafletOutput("map", height=600)),
#   nav_panel("Data", dataTableOutput("table")),
#   nav_panel("About",includeMarkdown("README.md"))
# )

page_sidebar(theme = bs_theme(secondary = "#21719c"),
             sidebar = uiOutput("sidebar"),
             title = "Aspen root collection",
             layout_column_wrap(
               width = "250px", fill = FALSE,height = "100px",
               value_box(
                 title = "Number of seleted Trees",
                 value = textOutput("selected_trees"),
                 showcase = bs_icon("tree"),
                 theme_color = "secondary"
               ),
               value_box(
                 title = "Number of Collcetors",
                 value = textOutput("collectors"),
                 showcase = bs_icon("person-heart"),
                 theme_color = "secondary"
               )
             ),

             tabsetPanel(
               nav_panel("Map", leafletOutput("map", height=600)),
               nav_panel("Data", dataTableOutput("table")),
               nav_panel("About",includeMarkdown("README.md"))
             )


)

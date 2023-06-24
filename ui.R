library(bsicons)
library(bslib)
library(DT)
library(leaflet)
library(shiny)
library(shinydashboard)
library(stringr)

#value boxes
boxes <- list(
  value_box(
    title = "Number of seleted Trees",
    value = textOutput("selected_trees"),
    showcase = bs_icon("tree"),
    theme_color = "secondary"
  ),
  value_box(
    title = "Number of Collcetors",
    value = textOutput("collectors"),
    showcase = bs_icon("person"),
    theme_color = "secondary"
  )
)

#ui elements
page_navbar(
  title = "NordAsp",
  sidebar = uiOutput("sidebar"),
  nav_panel("Map", icon = icon("map"),
            layout_columns(
              col_widths = c(6, 6, 12),
              row_heights = c(1, 5),
              !!!boxes,
              card(full_screen = TRUE,
                   leafletOutput("map", height=600)
              )
            )
  ),
  nav_panel("Data", icon = icon("table"),
            dataTableOutput("table")),
  nav_panel("About",includeMarkdown("README.md"))
)

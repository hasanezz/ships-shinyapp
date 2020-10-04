library(shiny)
library(shinyjs)
library(maps)
library(DT)
library(plotly)
library(geosphere)
library(leaflet)
library(magrittr)
library(tidyverse)
library(tibble)
library(shiny.semantic)
library(shinycssloaders)
source("functions.R")
#setwd("Desktop/appsilon/appsilon")

ui <- shinyUI(semanticPage(
  includeCSS("apps.css"),
  div(
    class = "ui container",
    style = "width: 100%",
    render_header(),
    render_dropdowns_ui(),
    render_map_ui(),
    div(class = "ui one column divided grid",
        div(class = "stretched row",
            div(
              class = "column",
              div(class = "ui table",
                  dataTableOutput("ships_table"))
            )))
  )
))
server <- shinyServer(function(input, output) {
  output$dropdowns <- render_both_dropdowns()
  output$ship_names <-render_ship_names_dropdown(ships_type_names, input)
  output$ship_map <- render_profile_ship_map(ships_data, input)
  output$info_box <- render_info_box_text(ships_data, input)
  output$ships_table  <-   render_table_ships(ships_data, input)})

shinyApp(ui, server)

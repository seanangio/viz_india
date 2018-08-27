library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinycssloaders)
library(markdown)
source("global.R")

shinyUI(fluidPage(
    # line below to fix bug in ggiraph when deploying app
    tags$head( tags$style(type = "text/css", "text {font-family: sans-serif}")),
    theme = shinytheme("yeti"),
    titlePanel("Visualising India's States"),
    
    sidebarLayout(
        sidebarPanel(
            pickerInput(
                inputId = "regions", 
                label = "Select/deselect regions to plot", 
                choices = levels(proj_sf$region), 
                selected = levels(proj_sf$region),
                options = list(`actions-box` = TRUE), 
                multiple = TRUE
            ),
            switchInput(
                inputId = "uts",
                label = "Union Territories",
                value = TRUE, 
                onLabel = "IN", offLabel = "OUT",
                labelWidth = '130px'
            ),
            br(),
            pickerInput(
                inputId = "variable",
                label = "Choose a variable to map", 
                choices = c("Population", 
                            "Urban Population",
                            "Rural Population",
                            "Population Growth",
                            "Population Density",
                            "Sex Ratio",
                            "Nominal GDP",
                            "Per Capita GDP"),
                selected = "Population",
                width = '210px'
            ),
            pickerInput(
                inputId = "representation",
                label = "Choose a visual representation", 
                choices = c("Geographic", "Continuous Cartogram", 
                            "Non-continuous Cartogram", "Dorling Cartogram",
                            "Hexbin"),
                selected = "Geographic",
                width = '210px'
            ),
            pickerInput(
                inputId = "color_scheme",
                label = "Choose a colour scheme", 
                choices = c("magma", "inferno","plasma", "viridis", "cividis"),
                selected = "viridis",
                multiple = FALSE,
                width = '210px'
            ),
            switchInput(
                inputId = "abbs", 
                label = "State Abbreviations",
                value = FALSE,
                labelWidth = '130px'
            ),
            switchInput(
                inputId = "graticules",
                label = "Graticules", 
                value = FALSE,
                labelWidth = '130px'
            ),
            br(),
            h6(em("N.B.: Please allow longer compute time for cartograms."))
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel(
                    "Choropleth", 
                    withSpinner(ggiraphOutput("mymap", height = "600px")),
                    h5("Mouse over map for hover and zoom effects.")
                ),
                tabPanel(
                    "Dotplot", 
                    ggiraphOutput("dotplot", height = "650px")
                ),
                tabPanel(
                    "Table", 
                    DT::dataTableOutput("table")
                ),
                tabPanel(
                    "Documentation", 
                    includeMarkdown("about.md"),
                    br()
                )
            )
        )
    )
))

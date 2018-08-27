library(shiny)
library(DT)
source("global.R")

shinyServer(function(input, output) {
    
    # make dataset a reactive object
    dataInput <- reactive({
        choose_area(input$regions, input$uts)
    })

    # make new_sf a reactive object
    second_sf <- reactive({
        adjust_geometry(dataInput(), input$representation, input$variable)
    })
    
    # make args reactive
    args <- reactive({
        get_args(second_sf(), input$variable)
    })
    
    # make a choropleth
    output$mymap <- renderggiraph({
        make_choropleth(second_sf(), args(), input$color_scheme,
                        input$graticules, input$representation, input$abbs)
    })

    # make a dotplot
    output$dotplot <- renderggiraph({
        make_dotplot(second_sf(), args())
    })
    
    # make a table
    output$table <- DT::renderDataTable({
        DT::datatable(
            second_sf() %>% 
                st_set_geometry(NULL) %>% 
                select(-c(18:21)),
            caption = 'N.B.: All data pulled from Wikipedia. See Documentation for further information.',
            options = list(lengthMenu = c(10, 20, 36), pageLength = 10) 
        ) %>% 
            formatPercentage('decadal_growth', 2) %>% 
            formatCurrency(c('nominal_gdp_usd', 'per_capita_gdp_usd'), 
                           digits = 0) %>% 
            formatCurrency(c('nominal_gdp_inr', 'per_capita_gdp_inr'), 
                           digits = 0, currency = '₹') %>% 
            formatRound(c('pop_2011', 'rural_pop', 'urban_pop', 
                          'area_km2', 'density_km2', 'sex_ratio'), digits = 0)
    })
    
})

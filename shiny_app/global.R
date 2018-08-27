library(sf)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(geogrid)
library(cartogram)

# define css for tooltips
tooltip_css <- "background-color:gray;color:white;padding:5px;border-radius:5px;font-family:sans-serif;font-size:12px;"

# load input data with a projected CRS
proj_sf <- readRDS("simp_sf.rds") %>% 
    st_transform(crs = 24343) %>% 
    mutate(
        region = factor(
            region, 
            levels = c("Northern", "Western", "Southern", "Central", 
                       "Eastern", "Northeastern", "Bay of Bengal", 
                       "Arabian Sea"),
            ordered = TRUE)
    )

# function that receives an sf object and returns a modified sf object
# according to user selections of region and uts
choose_area <- function(regions, uts) {
    
    data <- proj_sf %>% 
        filter(region %in% regions)
    
    if (!uts) {
        data <- data %>%
            filter(!type == "Union Territory")
    }
    
    data
}

# function that receives a (possibly filtered) sf object and
# outputs the same sf object with geometry modified depending on 
# user-selected representation and variable
adjust_geometry <- function(data, representation, variable) {
    
    var <- switch(variable,
                  "Population" = "pop_2011", 
                  "Urban Population" = "urban_pop",
                  "Rural Population" = "rural_pop",
                  "Population Growth" = "decadal_growth",
                  "Population Density" = "density_km2",
                  "Sex Ratio" = "sex_ratio",
                  "Nominal GDP" = "nominal_gdp_usd",
                  "Per Capita GDP" = "per_capita_gdp_usd"
    )
    
    if (representation == "Geographic") {
        new_sf <- data
    } else if (representation == "Continuous Cartogram") {
        new_sf <- cartogram_cont(data, var)
    } else if (representation == "Non-continuous Cartogram") {
        new_sf <- cartogram_ncont(data, var)
    } else if (representation == "Dorling Cartogram") {
        new_sf <- cartogram_dorling(data, var)
    } else if (representation == "Hexbin") {
        new_cells_hex <- calculate_grid(shape = data, 
                                        grid_type = "hexagonal", seed = 1)
        new_sf <- assign_polygons(data, new_cells_hex)
    } else {
        print("Representation not found.")
    }
    
    # calculate x, y coordinates for geom_text labels
    if (representation == "Hexbin") {
        # hexbin's assign_polygons calculates new coords as V1, V2
        new_sf <- new_sf %>% 
            rename(COORDS_X = V1, COORDS_Y = V2)
        
    } else {
        new_sf <- new_sf %>% 
            mutate(
                CENTROID = purrr::map(geometry, st_centroid),
                COORDS = purrr::map(CENTROID, st_coordinates),
                COORDS_X = purrr::map_dbl(COORDS, 1),
                COORDS_Y = purrr::map_dbl(COORDS, 2)
            )
    }
    
    new_sf
    
}

# function that receives the existing sf object and user-selected variable
# and outputs a list of matching arguments to be used in plots
get_args <- function(new_sf, variable) {
    
    args <- switch(variable, 
                   "Population" = list(
                       plot_var = new_sf$pop_2011 / 1e6, 
                       legend_title = "2011 Population\n(Millions)",
                       legend_labels = scales::comma
                   ),
                   "Urban Population" = list(
                       plot_var = new_sf$urban_pop / 1e6, 
                       legend_title = "2011 Urban\nPopulation (Millions)",
                       legend_labels = scales::comma
                   ),
                   "Rural Population" = list(
                       plot_var = new_sf$rural_pop / 1e6, 
                       legend_title = "2011 Rural\nPopulation (Millions)",
                       legend_labels = scales::comma
                   ),
                   "Population Growth" = list(
                       plot_var = new_sf$decadal_growth, 
                       legend_title = "Population Growth Rate\n2001-2011",
                       legend_labels = scales::percent
                   ),
                   "Population Density" = list(
                       plot_var = new_sf$density_km2, 
                       legend_title = "Population Density\n(No. Persons Per Sq Km)",
                       legend_labels = scales::comma
                   ),
                   "Sex Ratio" = list(
                       plot_var = new_sf$sex_ratio, 
                       legend_title = "Sex Ratio",
                       legend_labels = scales::comma
                   ),
                   "Nominal GDP" = list(
                       plot_var = new_sf$nominal_gdp_usd / 1e9,
                       legend_title = "Nominal GDP\n(USD$ Billion)",
                       legend_labels = scales::dollar
                   ),
                   "Per Capita GDP" = list(
                       plot_var = new_sf$per_capita_gdp_usd,
                       legend_title = "Per Capita\nGDP (USD$)",
                       legend_labels = scales::dollar
                   )
    )
    
    args
}

# function that receives previous sf object, list of args, and
# user-defined plot settings and returns the appropriate choropleth
make_choropleth <- function(new_sf, args, color_scheme, 
                            graticules, representation, abbs) {
    
    new_sf <- new_sf %>% 
        mutate(
            tip = paste0(
                "<b>", state_ut, " : ", args$plot_var, "</b>",
                "</span></div>")
        )
    
    p <- ggplot(
        data = new_sf
    ) +
        geom_sf_interactive(
            aes(fill = args$plot_var,
                tooltip = tip,
                data_id = state_ut), 
            lwd = 0
        ) +
        geom_sf(
            fill = NA, color = "lightgrey", lwd = 0.5
        ) +    
        scale_fill_viridis_c(
            args$legend_title, 
            option = color_scheme,
            labels = args$legend_labels
        )
    
    if (!graticules) {
        p <- p +
            coord_sf(datum = NA) +
            theme_void()
    }
    
    if (abbs) {
        p <- p +
            geom_text(
                mapping = aes(x = COORDS_X, y = COORDS_Y, label = abb),
                color = "white", size = 2
            ) +
            scale_y_continuous(NULL) +
            scale_x_continuous(NULL)
    }
    
    p <- ggiraph(
        ggobj = p, 
        hover_css = "cursor:pointer;stroke-width:5px;fill-opacity:0.8;",
        tooltip_extra_css = tooltip_css, tooltip_opacity = 0.75,
        zoom_max = 5
    )    
    
    p
    
}

# function that receives sf object and list of args for plotting
# returns appropriate dotplot
make_dotplot <- function(new_sf, args) {
    
    p <- ggplot(
        data = new_sf,
        mapping = aes(
            x = reorder(state_ut, args$plot_var), 
            y = args$plot_var,
            color = region)
    ) +
        geom_point_interactive(
            aes(tooltip = paste0(state_ut, ": ", args$plot_var), 
                data_id = state_ut)
        ) +
        coord_flip() +
        scale_y_continuous(
            args$legend_title, 
            labels = args$legend_labels
        ) +
        scale_x_discrete("") +
        scale_color_discrete("Region") +
        theme(legend.position = "bottom")
    
    ggiraph(
        code = print(p), 
        hover_css = "cursor:pointer;stroke-width:5px;fill-opacity:0.8;"
    )
    
}

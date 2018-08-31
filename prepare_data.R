library(tidyverse)
library(sf)
library(googlesheets)
library(rvest)
library(rmapshaper)

# read in geospatial data
ind_sf <- st_read("india_states_2014/india_states.shp")

# mutate type variable to define union territories
uts <- c("Delhi", "Andaman & Nicobar Islands", "Puducherry", 
         "Lakshadweep", "Dadra & Nagar Haveli", "Daman & Diu",
         "Chandigarh")

ind_sf <- ind_sf %>% 
    select(name, abbr) %>% 
    mutate(
        type = ifelse(name %in% uts, "Union Territory", "State")
    ) %>% 
    rename(abb = abbr, state_ut = name)

# paste data from Wikipedia into a google sheet
# population data: https://en.wikipedia.org/wiki/List_of_states_and_union_territories_of_India_by_population
# gdp data: https://en.wikipedia.org/wiki/List_of_Indian_states_and_union_territories_by_GDP
india_sheets <- gs_title('india_states')
pop <- india_sheets %>% gs_read(ws = 'population')
gdp <- india_sheets %>% gs_read(ws = 'gdp')

# wrangle population data
tidy_pop <- pop %>%
    select(-1) %>% 
    slice(2:37) %>% 
    rename_at(vars(names(.)), ~ c("state_ut", "pop_2011", 
                                  "decadal_growth", "rural_pop", 
                                  "urban_pop", "area_km2", "density_km2", 
                                  "sex_ratio")) %>%
    mutate(
        pop_2011 = sub("\n.*", "", pop_2011) %>% 
            str_replace_all(",", "") %>% 
            as.numeric(),
        decadal_growth = as.numeric(decadal_growth),
        rural_pop = sub("\n.*", "", rural_pop) %>% 
            str_replace_all(",", "") %>% 
            as.numeric(),
        urban_pop = sub("\n.*", "", urban_pop) %>% 
            str_replace_all(",", "") %>% 
            as.numeric(),
        area_km2 = sub(" .*", "", area_km2) %>% 
            str_replace_all(",", "") %>%
            as.numeric(),
        density_km2 = sub("/.*", "", density_km2) %>% 
            str_replace_all(",", "") %>%
            as.numeric(),
        state_ut = replace(state_ut, state_ut == 'Manipurβ', 'Manipur')
    ) %>% 
    arrange(state_ut)

# wrangle gdp data
tidy_gdp <- gdp %>% 
    select(-2) %>% 
    slice(3:nrow(.)) %>% 
    rename_at(vars(names(.)), ~ c("state_ut", "nominal_gdp_inr", 
                                  "nominal_gdp_usd", "data_year", 
                                  "comparable_economy")) %>% 
    separate(nominal_gdp_usd, into = c('usd_value', 'usd_unit'), sep = " ") %>%
    separate(nominal_gdp_inr, into = c('inr_value', 'inr_unit'), sep = " ") %>%
    mutate(
        usd_value = sub('.*\\$', '', usd_value) %>% 
            as.numeric(),
        inr_value = sub('.*\\₹', '', inr_value) %>% 
            str_replace(",", "") %>% 
            as.numeric(),
        nominal_gdp_usd = ifelse(usd_unit == "billion", usd_value * 1e9,
                                 usd_value * 1e6),
        nominal_gdp_inr = ifelse(inr_unit == "lakh", inr_value * 1e12,
                                 inr_value * 1e7),
        data_year = sub("( |\\[).*", "", data_year)
    ) %>%
    select(-c(2:5)) %>% 
    arrange(state_ut)

# wrangle region data
region_url <- "https://en.wikipedia.org/wiki/List_of_states_and_union_territories_of_India_by_area"
region_scrape <- read_html(region_url) %>% 
    html_nodes("td") %>% 
    html_text(trim = TRUE)

regions_mat <- matrix(region_scrape[4:length(region_scrape)], 
                      ncol = 7, byrow = TRUE)

tidy_region <- data.frame(regions_mat) %>% 
    select(2, 4) %>%
    slice(1:36) %>% 
    rename_at(vars(names(.)), ~ c("state_ut", "region")) %>% 
    mutate_if(is.factor, as.character) %>% 
    arrange(state_ut)

# join attribute data together
attributes_df <- tidy_gdp %>% 
    left_join(tidy_pop) %>% 
    left_join(tidy_region) %>% 
    mutate(
        state_ut = replace(state_ut, state_ut == "Andaman and Nicobar Islands", 
                           "Andaman & Nicobar Islands"),
        state_ut = replace(state_ut, state_ut == "Daman and Diu", 
                           "Daman & Diu"),
        state_ut = replace(state_ut, state_ut == "Dadra and Nagar Haveli", 
                           "Dadra & Nagar Haveli"),
        state_ut = replace(state_ut, state_ut == "Jammu and Kashmir", 
                           "Jammu & Kashmir")
    )

# output attributes file
saveRDS(attributes_df, "attributes.rds")

# join attribute data to sf object
ind_sf <- ind_sf %>% 
    left_join(attributes_df, by = "state_ut") %>%
    mutate(
        per_capita_gdp_inr = nominal_gdp_inr / pop_2011,
        per_capita_gdp_usd = nominal_gdp_usd / pop_2011
    )

# simplify geometry
simp_sf <- ms_simplify(ind_sf, keep = 0.01, keep_shapes = TRUE)

# output file
saveRDS(simp_sf, "simp_sf.rds")

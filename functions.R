### Function to compare proportions between zipcodes for chosen variable of interest
compare_proportions_by_zip <- function(rate_var, zip1, zip2) {
  
  counts <- tbl %>% 
    dplyr::filter(zipcode %in% c(zip1, zip2)) %>% 
    dplyr::select(!!sym(rate_var), population_in_2020) %>% 
    dplyr::mutate(numerator = round((!!sym(rate_var) / 100) * population_in_2020))
  
  pval <- prop.test(x = counts$numerator, y = counts$population_in_2020) %>% 
    broom::tidy() %>% 
    purrr::pluck("p.value") %>% 
    round(3)
  
  pval <- ifelse(pval < 0.001, "< 0.001", as.character(pval))
  
  return(pval)
}

### Function to plot leaflet map showing variables by zipcode 
plot_var_by_zip <- function(var, subtitle) {
  
  measure <- char_zips %>% pluck(var)
  
  pal <- colorNumeric(
    palette = "Blues",
    domain = measure)
  
  if ( grepl("median", "var") ) {
    labels <- paste0(
      "Zip Code: ",
      char_zips$GEOID10, "<br/>",
      subtitle, ": ",
      scales::dollar(measure, scale = 1)) %>% # dollar sign
      lapply(htmltools::HTML)
  } else {
    labels <- paste0(
      "Zip Code: ",
      char_zips$GEOID10, "<br/>",
      subtitle, ": ",
      scales::percent(measure, scale = 1)) %>% # percent
      lapply(htmltools::HTML)
  }
  
  char_zips %>% 
    leaflet %>% 
    addProviderTiles("CartoDB") %>% 
    addPolygons(fillColor = ~pal(measure),
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                highlight = highlightOptions(weight = 2,
                                             color = "#666",
                                             dashArray = "",
                                             fillOpacity = 0.7,
                                             bringToFront = TRUE),
                label = labels) %>%
    addLegend(pal = pal, 
              values = ~measure,
              opacity = 0.7, 
              title = htmltools::HTML(paste0(
                subtitle,
                " <br> 
                by Zip Code <br>
                2020")),
              position = "bottomright")
}
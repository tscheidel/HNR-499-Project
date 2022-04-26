### Function to compare proportions between zipcodes for chosen variable of interest
compare_proportions_by_zip <- function(rate_var, subtitle, zip1, zip2) {
  
  if ( subtitle %in% c("Total Population", "Median Household Income",
                       "Average Number of Jobs Per Household", "Life Expectancy",
                       "Median Age", "Average Out-of-Pocket Spending on Medical Expenses",
                       "Number of People with a Disability Living in Poverty") | zip1 == zip2 ) {
    msg <- "Please select a rate variable and two different zip codes to conduct an
    unadjusted difference in proportions test between the two selected zip codes."
  } else {
    counts <- tbl %>% 
      dplyr::filter(zipcode %in% c(zip1, zip2)) %>% 
      dplyr::select(zipcode, !!sym(rate_var), population_in_2020) %>% 
      dplyr::mutate(numerator = round((!!sym(rate_var) / 100) * population_in_2020))
    
    if ( any(is.na(counts$numerator)) ) {
      msg <- "Data not available for at least one of the zip codes selected."
    } else {
      pval <- prop.test(x = counts$numerator, n = counts$population_in_2020) %>% 
        broom::tidy() %>% 
        purrr::pluck("p.value") %>% 
        round(3)
      
      pct1 <- counts %>% dplyr::filter(zipcode == zip1) %>% dplyr::pull(!!sym(rate_var))
      pct2 <- counts %>% dplyr::filter(zipcode == zip2) %>% dplyr::pull(!!sym(rate_var))
      
      sig  <- ifelse(pval < 0.05, "is", "is not")
      pval <- ifelse(pval < 0.001, "< 0.001", paste("=", as.character(pval)))
      
      msg <- paste(paste0("There ", sig, " a significant difference in the ", subtitle,
                          " variable between zip codes ", zip1, " and ", zip2, "."),
                   paste0("(", pct1, "% vs. ", pct2, "%, p-value ", pval, ")"),
                   sep = "\n")
    }
  }
  
  return(msg)
}

### Function to plot leaflet map showing variables by zipcode 
plot_var_by_zip <- function(var, subtitle) {
  
  measure <- char_zips %>% pluck(var)
  
  pal <- colorNumeric(
    palette = "Blues",
    domain = measure)
  
  if ( grepl("income|dollars", var) ) {
    labels <- paste0(
      "Zip Code: ",
      char_zips$GEOID10, "<br/>",
      subtitle, ": ",
      scales::dollar(measure, scale = 1)) %>% # dollar sign
      lapply(htmltools::HTML)
  } else if ( grepl("life_expectancy|number_of_jobs|median_age|estimated_number_of_people_with_a_disability|population_in_2020", var) ){
    labels <- paste0(
      "Zip Code: ",
      char_zips$GEOID10, "<br/>",
      subtitle, ": ",
      scales::number(measure, scale = 1, big.mark = ",")) %>% # whole number
      lapply(htmltools::HTML)
  } else {
    labels <- paste0(
      "Zip Code: ",
      char_zips$GEOID10, "<br/>",
      subtitle, ": ",
      scales::percent(measure, scale = 1, accuracy = 0.01)) %>% # percent
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

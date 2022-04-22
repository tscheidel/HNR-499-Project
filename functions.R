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

# example
compare_proportions_by_zip(
  rate_var <- "estimated_percent_of_all_people_that_are_living_in_poverty_as_of", 
  zip1     <- "49301",
  zip2     <- "49321"
)


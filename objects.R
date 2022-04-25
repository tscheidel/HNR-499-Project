tbl <- read_csv("kent_county_2020_data.csv") %>% 
  mutate(zipcode = as.character(zipcode))

zip_codes <- tbl %>% pull("zipcode")

char_zips <- zctas(cb = TRUE, starts_with = zip_codes, year = 2019) %>% 
  geo_join(tbl, by_sp = "GEOID10", by_df = "zipcode", how = "left")

vmapping <- read_csv("variable-mapping.csv")

zip_choices <- list("49301", "49302", "49306", "49315",
                    "49316", "49319", "49321", "49326",
                    "49330", "49331", "49341",
                    "49345", "49418", "49503", "49504", 
                    "49505", "49506", "49507", "49508",
                    "49509", "49512", "49519", "49525",
                    "49534", "49544", "49546", "49548")

demographic_choices <- list("Total Population",
                            "Percent Asian",
                            "Percent Black/African American",
                            "Percent Hispanic",
                            "Percent Native American/Alaska Native", 
                            "Percent Native Hawaiian/Pacific Islander",
                            "Percent White",
                            "Median Age",
                            "Percent of All People With One or More Disability",
                            "Percent of All People Who Are Foreign Born", 
                            "Percent of All People Who Are Non-English Speaking")

education_choices <- list("Percent of All Adults with Less than a 9th Grade Education",
                          "Graduation Rate",
                          "Percent of All Adults with a High School Diploma but No College Education",
                          "Percent of All Black/African American People Who Have At Least A High School Diploma",
                          "Percent of All Hispanic People Who Have At Least A High School Diploma",
                          "Percent of All White People Who Have At Least A High School Diploma",
                          "Percent of All Adults Who Have A Bachelors Degree",
                          "Percent of Households with Access to Internet",
                          "Percent of Households with Access to Computers")

economic_choices <- list("Median Household Income",
                         "Average Number of Jobs Per Household",
                         "Percent of All People Living in Poverty", 
                         "Percent of All Asian People Who Live In Poverty",
                         "Percent of All Black/African American People Who Live in Poverty",
                         "Percent of All Hispanic People Who Live in Poverty",
                         "Percent of All White People Living in Poverty",
                         "Number of People with a Disability Living in Poverty")

healthaccess_choices <- list("Percent of All Adults with a Primary Care Physician",
                             "Percent of All Adults Who Have An Annual Physical",
                             "Average Out-of-Pocket Spending on Medical Expenses",
                             "Percent of All People Without Health Insurance",
                             "Percent of All Adults Ever Tested for HIV" )

outcomes_choices <- list("Life Expectancy",
                         "Percent of All Adults Diagnosed with COPD",
                         "Percent of All Adults Diagnosed with Depression",
                         "Percent of All Adults Diagnosed with Diabetes",
                         "Percent of All Adults Diagnosed with Heart Disease",
                         "Percent of All Adults Diagnosed with High Blood Pressure (Hypertension)",
                         "Percent of All Adults Diagnosed with High Cholesterol",
                         "Percent of All Adults Diagnosed with a Stroke")
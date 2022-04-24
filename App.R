library(shiny)
library(shinyalert)
library(tidyverse)
library(tigris)
library(leaflet)
library(bslib)
options(tigris_use_cache = TRUE)
source("functions.R")

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
                          "Percent of All Asian People Who Have At Least A High School Diploma ",
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
                             "Percent of All People With Health Insurance ",
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

# Define UI for app----
ui <- fluidPage(theme = bs_theme(version = 4, bootswatch = "minty"),
                          div(style = "padding: 1px 0px; width: '100%'",
                              titlePanel(
                                title = "",
                                windowTitle = "Art Explorer"
                              )
                          ),
    
      navbarPage(
        #title of the project
        "How Does Your Zip Code Affect Your Health?",
        tabPanel("Home",
                 sidebarLayout(
                   sidebarPanel(img(src = "Healthy People 2030 SDOH Graphic Domain Labels.png", 
                                    height = 375, width = 375),
                                br(),
                                p("To further explore the social determinants of health and the current goals
                                  of the U.S Department of Health and Human Services Healthy People 2030 campaign,
                                  visit the following resources:"), 
                                 HTML("<a href='https://www.cdc.gov/socialdeterminants/index.htm'>CDC Social Determinants of Health</a></p>"), 
                                 HTML("<a href='https://health.gov/healthypeople/priority-areas/social-determinants-health'>USDHHS Healthy People 2030 Campaign</a>")),
                   mainPanel(h3("Social Determinants of Health"),
                             p("The social determinants of health (SDOH) are the prevalent, 
                             but often overlooked factors that play into an individual’s personal health. 
                             Often, we are aware of how our own actions are affecting our health, such as eating right, exercising regularly, 
                             and avoiding risky behaviors such as smoking or excessive drinking. However, the SDOH play 
                             an almost equal part in our lifestyle and individual health outcomes. Broadly, the social 
                             determinants of health fall into five categories: Neighborhood and Built Environment, Social and Economic
                            Context, Economic Stability, Education Access and Quality, Healthcare Access and Quality."),
                             br(),
                             h4("Neighborhood and Built Environment"),
                             p("An individual’s neighborhood and built environment incorporates a large range of factors,
                             including rates of violence and violent crimes, prevalence of unsafe water or air and 
                             accessibility to safe spaces for exercise and environmental play. Often, racial and 
                             ethnic minorities, as well as those with low incomes, live in neighborhoods with high 
                             rates of these health risks and share an unproportionate burden of the negative health 
                             outcomes associated with them. Interventions and changes at the governmental level are 
                             the most needed, as they have the highest impact on infrastructure changes."),
                             br(),
                             h4("Social and Community Context"),
                             p("A person’s social support system and relationships can have a big impact on their health.
                             Living, learning, and working in environments that make an individual feel safe promotes
                             better overall health. However, some may face difficulties and discrimination in their lifetime
                             which correlates with more negative health outcomes. Cultivation of positive relationships within
                             a community, as well as those of an interpersonal nature, is very important to living a healthy life. "),
                             br(),
                             h4("Economic Stability"),
                             p("An individual’s economic stability plays a huge role in overall health. 
                             Those with steady employment are more likely to have better access to healthier foods, pay for 
                             healthcare expenses and live in safer neighborhoods—all of which play an important role in health. 
                             According to US Census data, 1 in every 10 people in the United States lived in poverty in 2018, 
                             which means 10 percent of the population was at increased risk for greater negative health outcomes. 
                             Government assistance and aid can decrease the health discrepancies we see in the population living 
                             in poverty and those unemployed."),
                             br(),
                             h4("Education Access and Quality"),
                             p("It is known that education is highly correlated with health and that individuals
                             with higher levels of education are more likely to lead to longer, healthier lives.
                             An individual’s success in school is often tied to the socioeconomic status of their
                             family. Some families live in areas with underperforming schools and others cannot
                             afford to send their children to college. These barriers to quality and continuing
                             education can effect an individual’s overall health later in their lifetimes. "),
                             br(),
                             h4("Healthcare Access and Quality"),
                             p("This social determinant of health is often the first to come to mind when considering
                               someone’s overall health as an individual’s ability to seek and obtain quality health 
                               services is imperative for a healthy life. However, there are some major barriers to 
                               achieving this goal for certain populations and demographics. Rural communities often 
                               have limited access to healthcare facilities and specialization clinics due to physical 
                               distance. Often, those living in poverty cannot afford health insurance or pricey 
                               operations/treatments. Possible solutions to these issues could include greater insurance 
                               coverage for at risk population, increased utilization of telehealth or alternative access 
                               to health services for those who cannot easily access health facilities, and increased 
                               communication and health literacy programs for communities.")
                   )
                 )),
        tabPanel("Zip code Demographics",
                 sidebarLayout(
                   sidebarPanel(
                     tags$p(HTML("<b>Zip code Demographics</b>")),
                     tags$p(span("Use the controls below to select one or more zip codes to explore and compare their demographics.")),
                     selectInput("demo_variable", h3("Choose a Demographic to Explore"), choices = demographic_choices),
                     selectInput("demo_zipcode_1", h3("Zipcode 1"), choices = zip_choices),
                     selectInput("demo_zipcode_2", h3("Zipcode 2"), choices = zip_choices)
                   ),
                   mainPanel(leafletOutput("demo_plot", height = "90%"),
                             textOutput("demo_test"))
                 )),
        tabPanel("Neighborhood and Built Environment", 
                 sidebarLayout(
                   sidebarPanel(
                     tags$p(HTML("<b>Neighborhood and Built Environment</b>")),
                     tags$p(span("Use the controls below to explore and compare some of the factors that 
                               make up the neighborhoods and built environments of Kent County zipcodes.")),
                     selectInput("nbe_variable", h3("Select a Variable of Interest"),choices = education_choices),
                     selectInput("nbe_zipcode_1", h3("Zipcode 1"), choices = zip_choices),
                     selectInput("nbe_zipcode_2", h3("Zipcode 2"), choices = zip_choices)
                   ),
                   mainPanel(leafletOutput("nbe_plot", height = "90%"),
                             textOutput("nbe_test"))
                 )),
        tabPanel("Social and Community Context", 
                 sidebarLayout(
                   sidebarPanel(
                     tags$p(HTML("<b>Social and Community Context</b>")),
                     tags$p(span("Use the controls below to explore and compare some of the factors that make up the 
                                social and community context in of Kent County zipcodes.")),
                     selectInput("scc_variable", h3("Select a Variable of Interest"), choices = education_choices),
                     selectInput("scc_zipcode_1", h3("Zipcode 1"), choices = zip_choices),
                     selectInput("scc_zipcode_2", h3("Zipcode 2"), choices = zip_choices)
                   ),
                   mainPanel(leafletOutput("scc_plot", height = "90%"),
                             textOutput("scc_test"))
                 )),
        tabPanel("Economic Stability", 
                 sidebarLayout(
                   sidebarPanel(
                     tags$p(HTML("<b>Economic Stability</b>")),
                     tags$p(span("Use the controls below to explore and compare some of the factors that determine the
                               economic stability of residents in different Kent County zipcodes.")),
                     selectInput("es_variable", h3("Select a Variable of Interest"), choices = economic_choices),
                     selectInput("es_zipcode_1", h3("Zipcode 1"), choices = zip_choices),
                     selectInput("es_zipcode_2", h3("Zipcode 2"), choices = zip_choices)
                   ),
                   mainPanel(leafletOutput("es_plot", height = "90%"),
                             textOutput("es_test"))
                 )),
        tabPanel("Education Access and Quality", 
                 sidebarLayout(
                   sidebarPanel(
                     tags$p(HTML("<b>Education Access and Quality</b>")),
                     tags$p(span("Use the controls below to explore and compare some of the factors and statistics related to the 
                                 education of the residents of various Kent County zip codes.")),
                     selectInput("eaq_variable", h3("Select a Variable of Interest"), choices = education_choices),
                     selectInput("eaq_zipcode_1", h3("Zipcode 1"), choices = zip_choices),
                     selectInput("eaq_zipcode_2", h3("Zipcode 2"), choices = zip_choices)
                   ),
                   mainPanel(leafletOutput("eaq_plot", height = "90%"),
                             textOutput("eaq_test"))
                 )),
        tabPanel("Healthcare Access and Quality", 
                 sidebarLayout(
                   sidebarPanel(
                     tags$p(HTML("<b>Healthcare Access and Quality</b>")),
                     tags$p(span("Use the controls below to explore the factors related to healthcare access for residents of various
                                 Kent County zip codes.")),
                     selectInput("haq_variable", h3("Select a Variable of Interest"), choices = healthaccess_choices),
                     selectInput("haq_zipcode_1", h3("Zipcode 1"), choices = zip_choices),
                     selectInput("haq_zipcode_2", h3("Zipcode 2"), choices = zip_choices)
                   ),
                   mainPanel(leafletOutput("haq_plot", height = "90%"),
                             textOutput("haq_test"))
                 )),
      tabPanel("Health Outcomes", 
               sidebarLayout(
                 sidebarPanel(
                   tags$p(HTML("<b>Health Outcomes</b>")),
                   tags$p(span("Use the controls below to explore the different health outcomes of residents
                               of various Kent County zip codes.")),
                   selectInput("ho_variable", h3("Health Outcome of Interest"), choices = outcomes_choices),
                   selectInput("ho_zipcode_1", h3("Zipcode 1"), choices = zip_choices),
                   selectInput("ho_zipcode_2", h3("Zipcode 2"), choices = zip_choices)
                 ),
                 mainPanel(leafletOutput("ho_plot", height = "90%"),
                           textOutput("ho_test"))
               )),
      tabPanel("Notes and Future Directions",
               sidebarLayout(
                 sidebarPanel(
                   tags$p(HTML("<b>About this Project</b>")),
                   tags$p(span("This Shiny Application was made as the culminating project for my undergraduate career
                               in Grand Valley State University's Frederick Meijer Honors College. As graduating seniors,
                               students had to complete an independet project on a topic, and in a medium, of their choice.
                               I chose to synthesize my love for public health and my background in statistics to analyze
                               the social determinants of health and their impacts on the health outcomes of those living
                               in Kent County. That being said, this application can definitely be expanded upon and it is
                               my hope and my intention to continue to grow this application in the future to include more 
                               analysis options and data points.")),
                    br(),
                    tags$p(span("I would like to thank Dr. Alisha Davis, who first introduced me to the topic global health
                                and the social determinants. Thank you for being my mentor and guide throughout this project.")),
                   tags$p(span("I would also like to thank Dr. Julia VanderMolen for introducing me to the PolicyMaps Database,
                               where I was able to download all of the data used for this project."))
                ),
                mainPanel(h3("Notes and Future Directions"),
                          p("This application is very close to the final product I set out to create at the start of my project;
                              however, there are some notable areas I would like to expand upon in the future."),
                          br(),
                          h4("Finding Applicable Data for Neighborhood and Built Environment"),
                          p("The PolicyMap Database had an extensive list of possibly applicable variables/data points that could
                            have fallen into the 'Neighborhood and Built Environment' category. However, many of these variables only
                            had data available on the county or city level and therefore, cold not be easily analyzed by zip code. In
                            the future, I would like to explore alternate data sources to see if I can find any that would allow me to
                            analyze crime rate, available green space, etc. by zip code."),
                          br(),
                          h4("Finding Applicable Data for Social and Community Context"),
                          p("Similarly, I would like to explore other data sources in an attempt to find more extensive data on the 'Social
                            and Community Context' SDOH for Kent County. Interpersonal relationships at work, school and in an individual's
                            community, as well as support structures, are not regularly asked about in large-scale questionnaires, like the 
                            Census. Collection of this data may have to be completed by an area wide survey in the future or found in an
                            alternative database."),
                          br(),
                          h4("Inclusion of More Health Outcomes"),
                          p("The health outcomes currently listed in this application focus largely on chronic diseases and disabilities. Many
                            of the typical health indicators of a community, such as mortality rate or disability-adjusted life years (DALYs),
                            were not available at the zip code level via the PolicyMaps Database. To truly to the differences in health outcomes
                            based on zip code, it would have been ideal to have had that data for analysis. Moving forward, I would like to explore
                            other data sources or organizations, such as the Kent County Health Department, to hopefully find this information broken
                            down by zip code."),
                          br(),
                          h4("Future Statistical Analysis"),
                          p("While I am happy I was able to include tests of proportion within this application, there are more tests
                            that I would love to include in the further development of this application. At the moment, statistical tests
                            can only be run on data points and variables that were reported as percentages on the PolicyMap Database. For
                            the variables that are reported as means or medians, I was not able to program tests for. In the future, I will
                            work to include tests for the difference in means or medians."),
                          br(),
                          p("Further, it would be great to include modeling of health outcomes as a future aspect of this application. As of
                            now, the PolicyMap Database only has aggregate data available, meaning overall percentages or means for variables,
                            not data on individuals. If at some point in the future I am able to obtain a large data set with information on
                            the social determinants of health and other health risk factors for individuals, I would add another tab to model
                            predicted health outcomes based an individual's information.")
                          )
                ))
      ))

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  shinyalert(
    title = "Explore Health Outcomes Based on Kent County Zip Codes",
    text = "<p style='text-align:left'>This Shiny application will allow users to explore the different social facets 
                that are linked to an individual's health, known most commonly as the Social Determinants of Health (SDOH). </p>
            <br/>
            <p style='text-align:left'> To do so, this application has been split up into tabs which will each highlight one
                of the social determinants.</p>
            <br/>
            <p style='text-align:left'> Each tab will be divided into two portions. On the left, the side panel will display
                a few drop down menus, where users will be able to choose which variable to compare. This side panel will also
                have a space for users to choose 2 zipcodes to compare statistically via an unadjusted difference in proportions
                test. The main panel will display a map outlining the individual zipcodes within Kent County, shaded based on the
                descriptive variable selected. Below the map, users will find the outcome of the proportion test. </p>
            <br/>
            <p style='text-align:left'> The data used for this project was downloaded from the PolicyMap database. To explore
                further zipcodes and counties please visit the <a href=\"https://gvsu-policymap-com.ezproxy.gvsu.edu/maps\">PolicyMap database</a>.</p> 
                           
    ",
    size = "m", 
    closeOnEsc = TRUE,
    closeOnClickOutside = FALSE,
    html = TRUE,
    type = "info",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "Let's Go!",
    confirmButtonCol = "#AEDEF4",
    timer = 0,
    imageUrl = "",
    animation = TRUE        
  )
  
  # demographics
  output$demo_plot <- renderLeaflet({
    plot_var_by_zip(
      var      = vmapping %>% filter(label == input$demo_variable) %>% pull(value),
      subtitle = input$demo_variable
    )
  })
  
  output$demo_test <- renderText({
    compare_proportions_by_zip(
      rate_var = vmapping %>% filter(label == input$demo_variable) %>% pull(value),
      subtitle = input$demo_variable,
      zip1     = input$demo_zipcode_1,
      zip2     = input$demo_zipcode_2
    )
  })
  
  # neighborhood and built environment
  output$nbe_plot <- renderLeaflet({
    plot_var_by_zip(
      var      = vmapping %>% filter(label == input$nbe_variable) %>% pull(value),
      subtitle = input$nbe_variable
    )
  })
  
  output$nbe_test <- renderText({
    compare_proportions_by_zip(
      rate_var = vmapping %>% filter(label == input$nbe_variable) %>% pull(value),
      subtitle = input$nbe_variable,
      zip1     = input$nbe_zipcode_1,
      zip2     = input$nbe_zipcode_2
    )
  })
  
  # social and community context
  output$scc_plot <- renderLeaflet({
    plot_var_by_zip(
      var      = vmapping %>% filter(label == input$scc_variable) %>% pull(value),
      subtitle = input$scc_variable
    )
  })
  
  output$scc_test <- renderText({
    compare_proportions_by_zip(
      rate_var = vmapping %>% filter(label == input$scc_variable) %>% pull(value),
      subtitle = input$scc_variable,
      zip1     = input$scc_zipcode_1,
      zip2     = input$scc_zipcode_2
    )
  })
  
  # economic stability
  output$es_plot <- renderLeaflet({
    plot_var_by_zip(
      var      = vmapping %>% filter(label == input$es_variable) %>% pull(value),
      subtitle = input$es_variable
    )
  })
  
  output$es_test <- renderText({
    compare_proportions_by_zip(
      rate_var = vmapping %>% filter(label == input$es_variable) %>% pull(value),
      subtitle = input$es_variable,
      zip1     = input$es_zipcode_1,
      zip2     = input$es_zipcode_2
    )
  })
  
  # educational access and quality
  output$eaq_plot <- renderLeaflet({
    plot_var_by_zip(
      var      = vmapping %>% filter(label == input$eaq_variable) %>% pull(value),
      subtitle = input$eaq_variable
    )
  })
  
  output$eaq_test <- renderText({
    compare_proportions_by_zip(
      rate_var = vmapping %>% filter(label == input$eaq_variable) %>% pull(value),
      subtitle = input$eaq_variable,
      zip1     = input$eaq_zipcode_1,
      zip2     = input$eaq_zipcode_2
    )
  })
  
  # healthcare access and quality
  output$haq_plot <- renderLeaflet({
    plot_var_by_zip(
      var      = vmapping %>% filter(label == input$haq_variable) %>% pull(value),
      subtitle = input$haq_variable
    )
  })
  
  output$haq_test <- renderText({
    compare_proportions_by_zip(
      rate_var = vmapping %>% filter(label == input$haq_variable) %>% pull(value),
      subtitle = input$haq_variable,
      zip1     = input$haq_zipcode_1,
      zip2     = input$haq_zipcode_2
    )
  })
  
  # healthcare outcomes
  output$ho_plot <- renderLeaflet({
    plot_var_by_zip(
      var      = vmapping %>% filter(label == input$ho_variable) %>% pull(value),
      subtitle = input$ho_variable
    )
  })
  
  output$ho_test <- renderText({
    compare_proportions_by_zip(
      rate_var = vmapping %>% filter(label == input$ho_variable) %>% pull(value),
      subtitle = input$ho_variable,
      zip1     = input$ho_zipcode_1,
      zip2     = input$ho_zipcode_2
    )
  })
}

shinyApp(ui = ui, server = server)

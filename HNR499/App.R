library(shiny)
library(shinyalert)
library(tidyverse)
library(tigris)
library(leaflet)
options(tigris_use_cache = TRUE)

# Define UI for app that draws a histogram ----
ui <- fluidPage(theme = "style.css",
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
                 sidebarPanel(img(src = "Healthy People 2030 SDOH Graphic Domain Labels.png", 
                                  height = 375, width = 375)),
                 mainPanel(h3("Social Determinants of Health", style = "font-family: 'times'; font-si16pt"),
                           p("The social determinants of health (SDOH) are the prevalent, 
                             but often overlooked factors that play into an individual’s personal health. 
                             Often, we are aware of how our own actions are affecting our health, such as eating right, exercising regularly, 
                             and avoiding risky behaviors such as smoking or excessive drinking. However, the SDOH play 
                             an almost equal part in our lifestyle and individual health outcomes. Broadly, the social 
                             determinants of health fall into five categories: Neighborhood and Built Environemnt, Social and Economic
                            Context, Economic Stability, Education Access and Quality, Healthcare Access and Quality.",
                             style = "font-family: 'times'; font-si16pt"),
                           br(),
                           h4("Neighborhood and Built Environment", style = "font-family: 'times'; font-si16pt" ),
                           p("Neighborhood and Built Environment", style = "font-family: 'times'; font-si16pt"),
                           br(),
                           h4("Social and Community Context", style = "font-family: 'times'; font-si16pt"),
                           p("Social and Community Context", style = "font-family: 'times'; font-si16pt"),
                           br(),
                           h4("Economic Stability", style = "font-family: 'times'; font-si16pt"),
                           p("Economic Stability", style = "font-family: 'times'; font-si16pt"),
                           br(),
                           h4("Education Access and Quality", style = "font-family: 'times'; font-si16pt"),
                           p("Education Access and Quality", style = "font-family: 'times'; font-si16pt"),
                           br(),
                           h4("Healthcare Access and Quality", style = "font-family: 'times'; font-si16pt"),
                           p("Healthcare Access and Quality", style = "font-family: 'times'; font-si16pt")
                           
                           
                           
                 )),
        tabPanel("Zipcode Demographics",
                 sidebarPanel(
                   tags$p(HTML("<b>Zip Codes</b>")),
                   tags$p(span("Use the controls below to select one or more zip codes to explore and compare their demographics.")),
                   selectInput("selected_demographic", h3("Choose a Demographic to Explore"), 
                               choices = list("Percent Asian" = 1, "Percent Black" = 2,
                                              "Percent Hispanic/Latino" = 3,
                                              "Median Income" = 4,
                                              "Median Age" = 5,
                                              "Percent Am Indian/Alaskan Native" = 6,
                                              "Number of People with a Disability Living in Poverty" = 7,
                                              "Percent Native Hawaiian/Other Pacific" = 8,
                                              "Percent with 1 or More Disabilities" = 9,
                                              "Percent Foreign Born" = 10,
                                              "Percent Non-English (Age > 5)" = 11,
                                              "Percent Living in Poverty" = 12)),
                   mainPanel(
                     leafletOutput("demographics", width = "400px")
                   )
                 )),
        tabPanel("Neighborhood and Built Environment", 
                 sidebarPanel(
                   tags$p(HTML("<b>Zip Codes</b>")),
                   tags$p(span("Use the controls below to explore and compare some of the factors that 
                               make up the neighborhoods and built environments of Kent County zipcodes.")),
                   selectInput("selected_health_outcome", h3("Health Outcome of Interest"), 
                               choices = list("Life Expectancy" = 1, "Percent COPD" = 2,
                                              "Percent Depression" = 3,
                                              "Percent Diabetes" = 4,
                                              "Percent Heart Disease" = 5,
                                              "Percent High Cholesterol" = 6,
                                              "Percent High Blood Pressure" = 7,
                                              "Percent Stroke" = 8,
                                              "Percent Overweight" = 9,
                                              "Percent Obese" = 10,
                                              "Percent Childhood Obesity" = 11,
                                              "Percent Binge-Drinkers" = 12))
                 )),
        tabPanel("Social and Community Context", 
                 sidebarPanel(
                   tags$p(HTML("<b>Zip Codes</b>")),
                   tags$p(span("Use the controls below to explore and compare some of the factors that make up 
                                social and community context in of Kent County zipcodes.")),
                   selectInput("selected_health_outcome", h3("Health Outcome of Interest"), 
                               choices = list("Life Expectancy" = 1, "Percent COPD" = 2,
                                              "Percent Depression" = 3,
                                              "Percent Diabetes" = 4,
                                              "Percent Heart Disease" = 5,
                                              "Percent High Cholesterol" = 6,
                                              "Percent High Blood Pressure" = 7,
                                              "Percent Stroke" = 8,
                                              "Percent Overweight" = 9,
                                              "Percent Obese" = 10,
                                              "Percent Childhood Obesity" = 11,
                                              "Percent Binge-Drinkers" = 12))
                 )),
        tabPanel("Economic Stability", 
                 sidebarPanel(
                   tags$p(HTML("<b>Zip Codes</b>")),
                   tags$p(span("Use the controls below to explore and compare some of the factors that determine the
                               economic stability of residents in different Kent County zipcodes.")),
                   selectInput("selected_health_outcome", h3("Health Outcome of Interest"), 
                               choices = list("Life Expectancy" = 1, "Percent COPD" = 2,
                                              "Percent Depression" = 3,
                                              "Percent Diabetes" = 4,
                                              "Percent Heart Disease" = 5,
                                              "Percent High Cholesterol" = 6,
                                              "Percent High Blood Pressure" = 7,
                                              "Percent Stroke" = 8,
                                              "Percent Overweight" = 9,
                                              "Percent Obese" = 10,
                                              "Percent Childhood Obesity" = 11,
                                              "Percent Binge-Drinkers" = 12))
                 )),
        tabPanel("Education Access and Quality", 
                 sidebarPanel(
                   tags$p(HTML("<b>Zip Codes</b>")),
                   tags$p(span("Use the controls below to explore and compare some of the factors that 
                               mtheir residents.")),
                   selectInput("selected_health_outcome", h3("Health Outcome of Interest"), 
                               choices = list("Life Expectancy" = 1, "Percent COPD" = 2,
                                              "Percent Depression" = 3,
                                              "Percent Diabetes" = 4,
                                              "Percent Heart Disease" = 5,
                                              "Percent High Cholesterol" = 6,
                                              "Percent High Blood Pressure" = 7,
                                              "Percent Stroke" = 8,
                                              "Percent Overweight" = 9,
                                              "Percent Obese" = 10,
                                              "Percent Childhood Obesity" = 11,
                                              "Percent Binge-Drinkers" = 12))
                 )),
        tabPanel("Healthcare Access and Quality", 
                 sidebarPanel(
                   tags$p(HTML("<b>Zip Codes</b>")),
                   tags$p(span("Use the controls below select two zip codes to compare the health outcomes of their residents.")),
                   selectInput("selected_health_outcome", h3("Health Outcome of Interest"), 
                               choices = list("Life Expectancy" = 1, "Percent COPD" = 2,
                                              "Percent Depression" = 3,
                                              "Percent Diabetes" = 4,
                                              "Percent Heart Disease" = 5,
                                              "Percent High Cholesterol" = 6,
                                              "Percent High Blood Pressure" = 7,
                                              "Percent Stroke" = 8,
                                              "Percent Overweight" = 9,
                                              "Percent Obese" = 10,
                                              "Percent Childhood Obesity" = 11,
                                              "Percent Binge-Drinkers" = 12))
                 )),
      tabPanel("Health Outcomes", 
               sidebarPanel(
                 tags$p(HTML("<b>Zip Codes</b>")),
                 tags$p(span("Use the controls below select two zip codes to compare the health outcomes of their residents.")),
                 selectInput("selected_zipcode_1", h3("Zipcode 1"), 
                             choices = list("49301" = 1, "49302" = 2, "49306" = 3, "49315" = 4,
                                            "49316" = 5, "49319" = 6, "49321" = 7, "49326" = 8,
                                            "49330" = 9, "49331" = 10, "49341" = 11,
                                            "49345" = 12, "49418" = 13, "49503" = 14, "49504" = 15, 
                                            "49505" = 16, "49506" = 17, "49507" = 18, "49508" = 19,
                                            "49509" = 20, "49512" = 21, "49519" = 22, "49525" = 23,
                                            "49534" = 24, "49544" = 25, "49546" = 26, "49548" = 27)),
                 selectInput("selected_zipcode_2", h3("Zipcode 2"), 
                             choices = list("49301" = 1, "49302" = 2, "49306" = 3, "49315" = 4,
                                             "49316" = 5, "49319" = 6, "49321" = 7, "49326" = 8,
                                             "49330" = 9, "49331" = 10, "49341" = 11,
                                             "49345" = 12, "49418" = 13, "49503" = 14, "49504" = 15, 
                                             "49505" = 16, "49506" = 17, "49507" = 18, "49508" = 19,
                                             "49509" = 20, "49512" = 21, "49519" = 22, "49525" = 23,
                                             "49534" = 24, "49544" = 25, "49546" = 26, "49548" = 27)),
                 selectInput("selected_health_outcome", h3("Health Outcome of Interest"), 
                             choices = list("Life Expectancy" = 1, "Percent COPD" = 2,
                                            "Percent Depression" = 3,
                                            "Percent Diabetes" = 4,
                                            "Percent Heart Disease" = 5,
                                            "Percent High Cholesterol" = 6,
                                            "Percent High Blood Pressure" = 7,
                                            "Percent Stroke" = 8,
                                            "Percent Overweight" = 9,
                                            "Percent Obese" = 10,
                                            "Percent Childhood Obesity" = 11,
                                            "Percent Binge-Drinkers" = 12))
                 
               ))
      ))

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  shinyalert(
    title = "Explore Health Outcomes Based on Kent County Zipcodes",
    text = "<p style='text-align:left'>This Shiny application will allow users to explore the different social facets 
                that are linked to an individual's health, known most commonly as the social determinants of Health. </p>
            <br/>
            <p style='text-align:left'> To do so, this application has been split up into tabs which will each highlight
                one of the five different broad categories of social determinants.</p>
            <br/>
            <p style='text-align:left'> Each tab will be divided into two portions. The top will display a map outlining
                the individual zipcodes within Kent County, shaded based on the descriptive variable selected. Below,
                users will be able to choose two zip codes to compare statistically, to see if there are any significant
                differences between the zipcodes. </p>
            <br/>
            <p style='text-align:left'> The data used for this project was downloaded from the PolicyMap database. To explore
                further zipcodes and counties please visit the <a href=\"https://gvsu-policymap-com.ezproxy.gvsu.edu/maps\">PolicyMap database</a>
                for further details about the dataset.</p> 
                           
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
}

shinyApp(ui = ui, server = server)

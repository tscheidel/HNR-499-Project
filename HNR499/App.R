library(shiny)

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
                 sidebarPanel(img(src = "Healthy People 2030 SDOH Graphic Domain Labels.png", height = 375, width = 375)
                              
                   
                 ),
                 mainPanel(
                 )),
        tabPanel("Community and Demographics",
                 sidebarPanel(
                   tags$p(HTML("<b>Zip Codes</b>")),
                   tags$p(span("Use the controls below to select one or more zip codes to explore and compare their demographics.")),
                   checkboxGroupInput("checkgroup", h3("Checkbox group"), 
                               choices = list("49301" = 1, "49302" = 2, "49306" = 3, "49315" = 4,
                                              "49316" = 5, "49319" = 6, "49321" = 7, "49326" = 8,
                                              "49330" = 9, "49331" = 10, "49341" = 11, "49343" = 12,
                                              "49345" = 13, "49418" = 14, "49503" = 15, "49504" = 16, 
                                              "49505" = 17, "49506" = 18, "49507" = 19, "49508" = 20,
                                              "49509" = 21, "49512" = 22, "49519" = 23, "49525" = 24,
                                              "49534" = 25, "49544" = 26, "49546" = 27, "49548" = 28)),
                 )),
        tabPanel("Education Access and Quality", 
                 sidebarPanel(
                   tags$p(HTML("<b>Zip Codes</b>")),
                   tags$p(span("Use the controls below to select one or more zip codes to explore and compare their residents' access to education.")),
                   checkboxGroupInput("checkgroup", h3("Checkbox group"), 
                               choices = list("49301" = 1, "49302" = 2, "49306" = 3, "49315" = 4,
                                              "49316" = 5, "49319" = 6, "49321" = 7, "49326" = 8,
                                              "49330" = 9, "49331" = 10, "49341" = 11, "49343" = 12,
                                              "49345" = 13, "49418" = 14, "49503" = 15, "49504" = 16, 
                                              "49505" = 17, "49506" = 18, "49507" = 19, "49508" = 20,
                                              "49509" = 21, "49512" = 22, "49519" = 23, "49525" = 24,
                                              "49534" = 25, "49544" = 26, "49546" = 27, "49548" = 28)),
                   
      )),
      tabPanel("Quality of Life", 
               sidebarPanel(
                 tags$p(HTML("<b>Zip Codes</b>")),
                 tags$p(span("Use the controls below to select one or more zip codes to explore and compare their residents' quality of life.")),
                 checkboxGroupInput("checkgroup", h3("Checkbox group"), 
                             choices = list("49301" = 1, "49302" = 2, "49306" = 3, "49315" = 4,
                                            "49316" = 5, "49319" = 6, "49321" = 7, "49326" = 8,
                                            "49330" = 9, "49331" = 10, "49341" = 11, "49343" = 12,
                                            "49345" = 13, "49418" = 14, "49503" = 15, "49504" = 16, 
                                            "49505" = 17, "49506" = 18, "49507" = 19, "49508" = 20,
                                            "49509" = 21, "49512" = 22, "49519" = 23, "49525" = 24,
                                            "49534" = 25, "49544" = 26, "49546" = 27, "49548" = 28)),
                 
               )),
      tabPanel("Health Access and Quality", 
               sidebarPanel(
                 tags$p(HTML("<b>Zip Codes</b>")),
                 tags$p(span("Use the controls below to select one or more zip codes to explore and compare their residents' ability to access quality health care.")),
                 checkboxGroupInput("checkgroup", h3("Checkbox group"), 
                             choices = list("49301" = 1, "49302" = 2, "49306" = 3, "49315" = 4,
                                            "49316" = 5, "49319" = 6, "49321" = 7, "49326" = 8,
                                            "49330" = 9, "49331" = 10, "49341" = 11, "49343" = 12,
                                            "49345" = 13, "49418" = 14, "49503" = 15, "49504" = 16, 
                                            "49505" = 17, "49506" = 18, "49507" = 19, "49508" = 20,
                                            "49509" = 21, "49512" = 22, "49519" = 23, "49525" = 24,
                                            "49534" = 25, "49544" = 26, "49546" = 27, "49548" = 28)),
                 
               )),
      tabPanel("Health Outcomes", 
               sidebarPanel(
                 tags$p(HTML("<b>Zip Codes</b>")),
                 tags$p(span("Use the controls below select one or more zip codes to explore and compare the health outcomes of their residents.")),
                 checkboxGroupInput("checkgroup", h3("Checkbox group"), 
                             choices = list("49301" = 1, "49302" = 2, "49306" = 3, "49315" = 4,
                                            "49316" = 5, "49319" = 6, "49321" = 7, "49326" = 8,
                                            "49330" = 9, "49331" = 10, "49341" = 11, "49343" = 12,
                                            "49345" = 13, "49418" = 14, "49503" = 15, "49504" = 16, 
                                            "49505" = 17, "49506" = 18, "49507" = 19, "49508" = 20,
                                            "49509" = 21, "49512" = 22, "49519" = 23, "49525" = 24,
                                            "49534" = 25, "49544" = 26, "49546" = 27, "49548" = 28)),
                 
               ))
      ))

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "orange",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
  
}

shinyApp(ui = ui, server = server)

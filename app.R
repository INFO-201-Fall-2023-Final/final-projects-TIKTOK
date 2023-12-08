library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
# Sunny can edited it!

songs_df <- read.csv("tiktok.csv") 

about_view <- fluidPage(h1("What makes Tiktok so addictive?"),
                        p(strong(em("Revealing the dynamic relationship between information quality and system quality.")),
  )
)

search_view <- fluidPage(sidebarLayout(position = "right",
                                       sidebarPanel(style = "background: black",
                                                    wellPanel(style = "background: white",
                                                              checkboxGroupInput("love_list",
                                                                                 "How much Love?:",
                                                                                 choices = love_words,
                                                                                 selected = "love",
                                                                                 inline = TRUE)),
                                                    wellPanel(style = "background: white",
                                                              h4("Info:"),
                                                              p("The words appearing before or after the word 'love' (and variants if chosen above), are shown in this network."),
                                                              p("Words appearing after 'love' have a red line, and words appearing before 'love' have a black line.")),
                                                    wellPanel(style = "background: white",
                                                              h4("Interact:"),
                                                              p("Zoom in, drag, hover and select nodes to reveal the strength of the connection."),
                                                              p("For example, common combinations such as 'in love' and 'love you' have thicker lines."))),
                                       
                                       mainPanel( 
                                         p(strong(em("\"I'm for free love, and I'm in free fall. This could be love or nothing at all.\""), "1.4 - A Chicken With Its Head Cut Off")),
                                         br(),
                                         p("Let's put all this love into context. Explore the network of love below:"),
                                         visNetworkOutput("lovenetwork", width = "100%", height = "565px")
                                       )
)
)





ui <- navbarPage(inverse = TRUE, "Final Project INFO201",
  tabPanel("Intro", includeCSS("styles.css"), about_view),
  
  tabPanel("Top Search",
           
))
#server stuff goes here 
server <- function(input, output){
  #output$testing <- renderText({
  #return(input$char_name)
  #})

}
#Make the app 
shinyApp(ui, server)

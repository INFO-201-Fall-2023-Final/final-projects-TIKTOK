library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
# Sunny can edited it!

tiktok_df <- read.csv("tiktok.csv") 


about_view <- fluidPage(h1("What makes Tiktok so addictive?"),
                        p(strong(em("Revealing the dynamic relationship between information quality and system quality.")),
  )
)

algorithm_view <- fluidPage(h1("What kinds of video gets promoted by Tiktok?"),
                            sidebarLayout(
                              sidebarPanel( 
                                h2("Control Panel"),
                                selectInput(
                                  inputId = "tiktok_ele",
                                  label = "Different elements of a video",
                                  choices = tiktok_df$song
                                )
                              ),
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Plot", plotOutput(outputId = "barchart")),
                                )
                              )
                            )
)




ui <- navbarPage(inverse = TRUE, "Final Project INFO201",
  tabPanel("Intro", includeCSS("styles.css"), about_view),
  
  tabPanel("panel",
           
))
#server stuff goes here 
server <- function(input, output){
  #output$testing <- renderText({
  #return(input$char_name)
  #})

}
#Make the app 
shinyApp(ui, server)

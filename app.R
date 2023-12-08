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


ui <- navbarPage(inverse = TRUE, "Final Project INFO201",
  tabPanel("Intro", includeCSS("styles.css"), about_view)
)
#server stuff goes here 
server <- function(input, output){
  #output$testing <- renderText({
  #return(input$char_name)
  #})

}
#Make the app 
shinyApp(ui, server)

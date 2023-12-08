library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)


songs_df <- read.csv("tiktok.csv") 

analysis_view <- fluidPage(
  titlePanel(""),
  sidebarLayout(
    sidebarPanel( 
      h2("Control Panel"),
      selectInput(
        inputId = "char_name",
        label = "Select a character",
        choices = char_df$Character
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput(outputId = "radar")),
        tabPanel("Table", tableOutput(outputId = "Table"))
      )
    )
  )
)

about_view <- fluidPage(
  h1("About Page"),
  p("This is about Mario Cart")
)

ui <- navbarPage(
  "In-class Ex",
  tabPanel("About", about_view),
  tabPanel("Analysis", analysis_view)
)
#server stuff goes here 
server <- function(input, output){
  #output$testing <- renderText({
  #return(input$char_name)
  #})
  
  make_radar_tb <- function(name){
    data_pt <- filter(char_df, Character == name)
    data_pt <- select(data_pt, -c(Character, Class))
    
    max_pt <- summarise_all(char_df, max)
    max_pt <- select(max_pt, -c(Character, Class))
    
    min_pt <- summarise_all(char_df, min)
    min_pt <- select(min_pt, -c(Character, Class))
    
    do.call("rbind", list(max_pt, min_pt, data_pt))
  }
  output$table <- renderTable({
    return(make_radar_tb(input$char_name))
  })
  output$radar <- renderPlot({
    tb <- make_radar_tb(input$char_name)
    radarchart(tb)
  })
  #output$barchart <- renderPlot({
  # ans_df <- filter(char_df, Character == input$char_name)
  #p <- ggplot(ans_df, aes(x = Class, y = Speed)) + geom_bar(stat = "identity")
  #return(p)
  #})
}
#Make the app 
shinyApp(ui, server)
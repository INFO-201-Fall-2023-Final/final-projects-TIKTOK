library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
# Sunny can edited it!

source("Final Data Wrangling.R")


about_view <-   fluidPage(
  titlePanel("How TikTok's background sound affect production traffic"),
# br(),

tags$style(HTML("
    h2 {
            background-color: #c0d6b7;
            # background-image: url('https://miro.medium.com/v2/resize:fit:1400/1*XnhCJ4DuRt_7oqwUiwjWPA.png');
            color: Black;
            }")),

mainPanel(
  h3("Introduction"),
  p("Tiktok has become one of the most young people's social media, most youth even addict to it.
      We will measure how information and system quality affect addictive behavior by thoroughly 
      examining user engagement patterns and content trends. Our goal is to educate readers with a 
      comprehensive grasp of the dynamics of addiction and encourage them to consider the role that
      social media plays in their lives by fusing real-world examples with sound statistical analysis."),
  br(),
  p(paste("Overall out of the 151 tiktok videos we looked at, there are ", like_count, "videos that have over
            10,000 likes. And in these 151 videos, ", popular_music, "has been used most.")),
  br(),
  
),
div(style = "float: right; margin-left: 50px;",
    tags$img(src = "https://miro.medium.com/v2/resize:fit:1400/1*XnhCJ4DuRt_7oqwUiwjWPA.png", 
             width = 500, height = 300, class = "right-image")),
                        
)
algorithm_view <- fluidPage(h1("What kinds of video gets promoted by Tiktok?"),
                            sidebarLayout(
                              sidebarPanel( 
                                h2("Control Panel"),
                                selectInput(
                                  inputId = "tiktok_ele",
                                  label = "Different elements of a video",
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
  
  tabPanel("panel",)
           
)
#server stuff goes here 
server <- function(input, output){
  #output$testing <- renderText({
  #return(input$char_name)
  #})

}
#Make the app 
shinyApp(ui, server)

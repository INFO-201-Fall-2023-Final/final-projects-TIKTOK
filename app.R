library(dplyr)
library(stringr)
library(ggplot2)
library(wordcloud2) 
library(shiny)
# Sunny can edited it!

source("Final Data Wrangling.R")

pal <- c("black", "#00f2ea", "#858B8E", "#ff0050")

about_view <-  fluidPage(
  titlePanel("How TikTok's background sound affect production traffic"),
  br(),

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
algorithm_view <- fluidPage(h1("What video length gets promoted the most by Tiktok?"),
                            sidebarLayout(
                              sidebarPanel( 
                                h2("Choose viedo length"),
                                selectInput(
                                  inputId = "tiktok_ele",
                                  label = "Different length of a video",
                                  choices = tiktok_df$duration_category
                                )
                              ),
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Plot", plotOutput(outputId = "chart")),
                                )
                              )
                            )
)




ui <- navbarPage(inverse = TRUE, "Final Project INFO201",
  tabPanel("Intro", includeCSS("styles.css"), about_view),
  
  tabPanel("Video Duration", includeCSS("styles.css"), algorithm_view),
  
  tabPanel("Trending Words",
           fluidPage(sidebarLayout(position = "right",
                                   sidebarPanel(style = "background: black",
                                                wellPanel(style = "background: white",
                                                          selectInput("variable",
                                                                             "Select your Variable:",
                                                                             choices = c("View Count" = "n_plays", "Likes Count" = "n_likes", "Share Count" = "n_shares"),
                                                                             selected = )),
                                                DT::dataTableOutput("counttable")),
                                   
                                   mainPanel( 
                                     p(strong(em("What are the musics that used in the top viewed, top liked and top shared music on TIKTOK?"), )),
                                     p("These are some of the music that are used in the most viewed, most liked and shared video posted on tiktok, as we can see, views sometimes don't correlate with amount of likes and shares.
                                       It is important to take note that these songs are only including song and not the original audio of the video itself."),
                                     wordcloud2Output("wordcloud", width = "100%", height = "570px")
                                   )
           )
           )
  ),
  
           
)
#server stuff goes here 
server <- function(input, output) {
  song_counts <- reactive({
    req(input$variable)
    req(tiktok_bgmSet$song)
    data <- data.frame(word = tiktok_bgmSet$song, freq = tiktok_bgmSet[[input$variable]])
    data <- data %>%
      arrange(desc(freq))
    
    data <- data %>%
      group_by(word) %>%
      filter(freq == max(freq)) %>%
      ungroup()
  })
  
  output$wordcloud <- renderWordcloud2({
    wordcloud2(song_counts(), size = 2, fontFamily = "Courier", 
               color = rep_len(pal[2:4], nrow(song_counts())), backgroundColor = "black")
  })
}
#Make the app 
shinyApp(ui, server)

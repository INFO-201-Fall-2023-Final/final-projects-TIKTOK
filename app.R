library(dplyr)
library(stringr)
library(ggplot2)
library(wordcloud2) 
library(shiny)

source("Final Data Wrangling.R")

pal <- c("black", "#00f2ea", "#858B8E", "#ff0050")

tiktok_df$n_hash <- numeric(nrow(tiktok_df))
for(i in 1:(nrow(tiktok_df)-1)){
  tiktok_df$n_hash[i+1] <- sum(strsplit(tiktok_df$hashtags.x[i], "")[[1]] == "'")/2
}

min_likes <- min(tiktok_df$n_likes.x)
max_likes <- max(tiktok_df$n_likes.x)


about_view <-  fluidPage(
  titlePanel("How TikTok's background sound affect production traffic"),
  br(),
  
  tags$style(HTML("
    h2 {
            # background-image: url('https://miro.medium.com/v2/resize:fit:1400/1*XnhCJ4DuRt_7oqwUiwjWPA.png');
            color: black;
            }")),
  
  
  mainPanel(
    h3("Introduction", style = "background-color: #D3D3D3;"),
    h4("Nowadays, examining TikTok addiction behavior is necessary because TikTok is one of the fastest-growing apps, 
      surpassing other social media platforms in terms of user numbers and usage intensity. Meanwhile, it 
      has the most advanced algorithm system, especially in terms of participation, content, and types of 
      interaction, which makes the addiction problem of TikTok more severe than the other popular social media.
      We will measure how information and system quality affect addictive behavior by thoroughly 
      examining user engagement patterns and content trends."),
    br(),
    h4(paste("Overall out of the 151 tiktok videos we looked at, there are ", like_count, "videos that have over
            10,000 likes. And in these 151 videos, ", popular_music, "has been used most.")),
    br(),
    
  ),
  div(style = "float: right; margin-left: 50px;",
      tags$img(src = "https://miro.medium.com/v2/resize:fit:1400/1*XnhCJ4DuRt_7oqwUiwjWPA.png", 
               width = 610, height = 360, class = "right-image")),
)


videolength_view <- fluidPage(
  titlePanel(
    h1("What video length gets promoted the most by Tiktok?"),
  ),
  sidebarLayout(
    sidebarPanel( 
      h2("Choose viedo length"),
      selectInput(
        inputId = "length",
        label = "Different length of a video",
        choices = tiktok_df$duration_category,
        selected = 1
      )
    ),
    mainPanel(
      p("Acoordin to the research done by Yao Qin, Bahiyah Omar and Alessandro Musetti. The timeliness and conciceness 
            are importnant measuere of the information quality in Tiktok, and these are often measured by video length. What length
            of the videos get promoted by Tiktok the most? And how well does these promoted video do in terms of their likes count?
      "),
      br(),
      plotOutput(outputId = "chart"),
      
    )
  )
)

wordcloud1 <- fluidPage(
  titlePanel(
    h1("What songs gets the most viewd?")
  ),
  sidebarLayout(position = "right",
                sidebarPanel(style = "background: black",
                             wellPanel(style = "background: white",
                                       selectInput("variable",
                                                   "Select your Variable:",
                                                   choices = c("View Count" = "n_plays.x", "Likes Count" = "n_likes.x", "Share Count" = "n_shares.x"),
                                                   selected = 1)),
                             DT::dataTableOutput("counttable")),
                
                mainPanel( 
                  p(strong(em("What are the musics that used in the top viewed, top liked and top shared music on TIKTOK?"), )),
                  p("These are some of the music that are used in the most viewed, most liked and shared video posted on tiktok, as we can see, views sometimes don't correlate with amount of likes and shares.
                                       It is important to take note that these songs are only including song and not the original audio of the video itself."),
                  wordcloud2Output("wordcloud", width = "100%", height = "570px")
                )
  )
)

hashtag_view <- fluidPage(
  titlePanel(
    h1("What hashtags gets the most viewd and liked?")
  ),
  sidebarLayout(position = "right",
                sidebarPanel(style = "background: black",
                             wellPanel(style = "background: white",
                                       sliderInput("likes",
                                                   label = "Number of Likes",
                                                   min = min_likes,
                                                   max = max_likes,
                                                   value = c(min_likes, max_likes)))
                ),
                mainPanel(
                  p("Hashtags"),
                  plotOutput(outputId = "plothash"),
                  plotOutput(outputId = "trending")
                )
  )
)

conclusion <-  fluidPage(
  titlePanel("Conclusion"),
  br(),
  
  tags$style(HTML("
    h2 {
            color: black;
            }")),
  
  mainPanel(
    h2("Summary", style = "background-color: #D3D3D3;"),
    br(),
    h3("Our goal is to educate readers with a comprehensive grasp of the dynamics of 
    addiction and encourage them to consider the role that social media plays in their
       lives by fusing real-world examples with sound statistical analysis."),
    br(),
    br(),
    br(),
    br(),
    h3("This is where we got our dataset:"),
    h3("https://github.com/datares/TikTok_Famous/blob/main/Datasets/
       TikTok%20Video%20Data%20Collection/sug_users_vids1.csv"),
    h3("https://github.com/datares/TikTok_Famous/blob/main/Datasets/
       TikTok%20Video%20Data%20Collection/top_users_vids.csv"),
    br(),
    br(),
    br(),
    br(),
    br(),
    h4(em("Thank you for your reading!"))
    
    
  ),
  
  div(style = "float: right; margin-left: 40px;",
      tags$img(src = "https://i.imgflip.com/4gu33f.jpg",
               width = 400, height = 572, class = "right-image")
  ),
  div(style = "position: relative; height: 700px;",  # Adjust height as needed
      div(h3(em("From Tony Sun, Sunny Yang, Mars Ouyang.")), 
          style = "position: absolute; bottom: 10px; right: 10px;")
  ),
)


ui <- navbarPage(inverse = TRUE, "Final Project INFO201",
                 tabPanel("Intro", includeCSS("styles.css"), about_view),
                 
                 tabPanel("Video Duration", includeCSS("styles.css"), videolength_view),
                 
                 tabPanel("Trending Songs", includeCSS("styles.css"), wordcloud1),
                 
                 tabPanel("Hashtag", includeCSS("styles.css"), hashtag_view),
                 
                 tabPanel("Conclusion", includeCSS("styles.css"), conclusion)
                 
)
#server stuff goes here 
server <- function(input, output) {
  
  length_tag <- reactive({
    tiktok_df[tiktok_df$duration_category == input$length, ]
  })
  
  output$chart <- renderPlot({
    plot <- ggplot(data = length_tag(), aes(x=video_length.x, y=n_plays.x, color = "#ff0050" )) + 
      geom_point()+
      labs(x = "Video Length (Seconds)", y = "View Count",)
    plot + theme(
      plot.background = element_rect(fill = "white"), 
      panel.background = element_rect(fill = "black")
    )
  })
  
  song_counts <- reactive({
    req(input$variable)
    req(tiktok_df$song.x)
    data <- data.frame(word = tiktok_df$song.x, freq = tiktok_df[[input$variable]])
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
  
  output$counttable = DT::renderDataTable({
    DT::datatable(song_counts(), options = list(lengthMenu = c(10, 20, 50), pageLength = 10),
                  rownames = FALSE, colnames = c("Song", input$variable), class = 'compact',
    )
  })
  
  
  
  output$plothash <- renderPlot({
    ggplot(tiktok_df, aes(x=n_hash, y=n_plays.x)) +
      geom_point() + 
      ggtitle("Number of Plays vs Number of Hashtags Used") +
      labs(color="Follower Count") + xlab("Number of Hashtags") + ylab("Number of Plays") +
      geom_hline(yintercept = mean(tiktok_df$n_plays.x)) + 
      geom_vline(xintercept = mean(tiktok_df$n_hash, na.rm = T))
  })
  
  tiktok <- reactive({
    
    tiktok_ <- filter(tiktok_df, n_likes.x < max(input$likes)) 
    tiktok_ <- filter(tiktok_df, n_likes.x > min(input$likes))
    tiktok_
  })
  
  output$trending <- renderPlot({
    if (nrow(tiktok()) > 0) {
      tiktok_ <- data.frame(table(unlist(strsplit(tolower(tiktok()$hashtags.x), ",")))) %>%top_n(15)
      colnames(tiktok_) <- c("Hashtag", "Count")
      p <-ggplot(tiktok_[0:15,], aes(Hashtag, Count))
      p + geom_bar(stat = "identity", fill = "#ff0050")
    }
  })
}
#Make the app 
shinyApp(ui, server)
library(dplyr)
library(stringr)
library(ggplot2)
library(wordcloud2) 
library(shiny)

source("Final Data Wrangling.R")

#Color Pallets
pal <- c("black", "#00f2ea", "#858B8E", "#ff0050")

#Data Set Up
tiktok_df$n_hash <- numeric(nrow(tiktok_df))
for(i in 1:(nrow(tiktok_df)-1)){
  tiktok_df$n_hash[i+1] <- sum(strsplit(tiktok_df$hashtags.x[i], "")[[1]] == "'")/2
}

min_views <- min(tiktok_df$n_plays.x)
max_views <- max(tiktok_df$n_plays.x)

#Introduction Panel
about_view <-  fluidPage(
  titlePanel("The Addiction Behavior of Short-form Video App TikTok: 
                The Information Quality and System Quality Perspective"),
  br(),
  
  tags$style(HTML("
    h2 {
            # background-image: url('https://miro.medium.com/v2/resize:fit:1400/1*XnhCJ4DuRt_7oqwUiwjWPA.png');
            color: black;
            }")),
  
      

  mainPanel(
    h2("Introduction", style = "background-color: #D3D3D3;"),
    p("Nowadays, TikTok is one of the fastest-growing apps, surpassing other social media platforms 
      in terms of user numbers and usage intensity. Meanwhile, it 
      has the most advanced algorithm system, especially in terms of participation, content, and types of 
      interaction, which makes the addiction problem of TikTok more severe than the other social media.
      We will measure how information and system quality affect addictive behavior by thoroughly 
      examining user engagement patterns and content trends."),
    br(),
    h3("The Algorithm"),
    p("The recommendation algorithm used in tiktok caters to users' needs by using a hierarchical interest 
      label tree, user persona, and partitioned data buckets strategy to recommend accurate and personalized content. 
      The algorithm also uses collaborative filtering and low-cost interaction design to create traps for users, contributing to addiction."),
    h3("Information Quality and System Quality"),
    p("There is a closed-loop relationship between Tiktok addiction and algorithm optimization. 
      The more frequently users use tiktok, the more accurate the algorithm becomes, potentially exacerbating addiction.
      In this project, we use concepts of information quality and system quality, which could be traced back to DeLone and McLean’s Information Systems Success Model,
      to look at 3 aspects of tiktok videos and see if they qualify as a measure to see why tiktok are so addictive."),
    p(paste("Given that among the 2,432 TikTok users we studied, ", like_count, " videos have garnered over 10,000 
              likes, and the most frequently used audio is the ", popular_music, "has been used most. Additionally,
             ", duration_Longvideo, "of these videos have a longer duration, and ", hasTag_exised, "include hashtags.")),
      
    p("We will be looking at video length, music, and hashtags in tiktok videos.", style = "font-weight: bold")
  ),
  div(style = "float: right; margin-right: 1px;",
      tags$img(src = "https://miro.medium.com/v2/resize:fit:1400/1*XnhCJ4DuRt_7oqwUiwjWPA.png", 
               width = 450, height = 340, class = "right-image")),
)

#Videolength Panel
videolength_view <- fluidPage(
  titlePanel(
    h1("What video length gets promoted the most by Tiktok?"),
  ),
  sidebarLayout(
    sidebarPanel( 
      h2("Control Panel"),
      p("Short: Less than 15 seconds"),
      p("Medium: Between 15 and 35 seconds"),
      p("Long: More than 35 seconds"),
      selectInput(
        inputId = "length_cat",
        label = "Choose video Length",
        choices = tiktok_df$duration_category,
        selected = 1
      ),
      span(textOutput("average_view_count"), style="color:#00f2ea;
                                                    font-size: 20px;
                                                    font-style: italic;"),
      br(),
      h2("Summary", style="color:#ff0050; font-style: italic;"),
      p("Based on the average view count and general trend in the graph, we see that Short length video, which are less than 15 seconds gets the most average view with around 
        7654364.97 views. Although this could due to that large portion of the data are in the short length categories, but it shows that the algorithm of tiktok promotes short videos 
        their users. Tiktok has a large viewer base of adolescents and young adults with short attention span, Based on the data statistics of TikTok penetration in China, the largest 
        group of users is 6–17 years old, accounting for 31.59%, followed by 18–24 years old (30.14%), 25–30 years old (20.85%), 31–35 years old (8.66%), and over 35 years old (8.76%) (Mou, 2020).
        Video creators on Tiktok also adapt to the algorithm, produce more high quality short length video and making it so much easier for their audience to lost track of time while swiping through 
        videos. This has raised serious concern as TikTok addiction affected young people seriously. They are naive and easily absorbed when exposed to a wide variety of short video contents."),
      
    ),
    mainPanel(
      br(),
      h4("According to the research done by Yao Qin, Bahiyah Omar and Alessandro Musetti. The timeliness and conciceness 
            are importnant measuere of the information quality in Tiktok, and these are often measured by video length. What video length
            gets the most viewed on Tiktok? How does these aspects of videos contribute to addiction in tiktok?" , style="font-style: italic; font-weight: bold"
      ),
      br(),
      h3("<- Choose a video length categories to see what happens to the view count", style="color:#ff0050; font-style: italic;"),
      br(),
      plotOutput("length_chart"),
      br(),
    )
  )
)

#Trending Song Panel
wordcloud1 <- fluidPage(
  titlePanel(
    h1("How does song contributes to user addiction?")
  ),
  sidebarLayout(position = "right",
                sidebarPanel(style = "background: white",
                             h2("Search your favourite song!", style = "color:#00f2ea; font-style: italic"),
                             h3("See if it is used in one of the top trending tiktok!", style = "color:#00f2ea;"),
                             wellPanel(style = "background: white",
                                       selectInput("variable",
                                                   "Select your Variable:",
                                                   choices = c("View Count" = "n_plays.x", "Likes Count" = "n_likes.x", "Share Count" = "n_shares.x"),
                                                   selected = 1)),
                             DT::dataTableOutput("counttable")),
                
                mainPanel( 
                  p("What are the musics that used in the top viewed, top liked and top shared music on TIKTOK?", style = "color:#ff0050; font-weight: bold"),
                  p("These are some of the music that are used in the most viewed, most liked and shared video posted on tiktok, as we can see, views sometimes don't correlate with amount of likes and share
                    in terms of the music use, and you may think it is not necessary to examine the music. And this is where you are wrong! In fact, the music and sound choice made in the making of these video are one of the most aspect contributes
                    to tiktok addiction. These factors are often overlooked when determining the quality of a tiktok video."),
                  wordcloud2Output("wordcloud", width = "102%", height = "530px"),
                  h2("Summary", style = "color:#ff0050; font-style: italic"),
                  p("The importance of music and sound design in a tiktok is oftern overlooked. In the context of TikTok, it had a simple operating system. The interaction was designed for immersive experiences and aimed to keep users in an extremely
                  passive state to accept the recommended videos. Users only needed to swipe up the screen with low effort to glance at short videos, therefore, indulging TikTok and extending the usage time unconsciously. The combination of visual, sound
                  and social aspect made TikTok more immersive. Thus the information quality makes has a positive influence on user's enjoyment and concentration.")
                )
  )
)

#Hashtags Panel
hashtag_view <- fluidPage(
  titlePanel(
    h1("How does hashtags has to do with user addiction?")
  ),
  sidebarLayout(position = "right",
                sidebarPanel(style = "background: white",
                             wellPanel(style = "background: white",
                                       sliderInput("views",
                                                   label = "Number of views",
                                                   min = min_views,
                                                   max = max_views,
                                                   value = c(min_views, max_views))),
                             p("This is a graph of number of views vs number of hashtags used in a video:"),
                             plotOutput(outputId = "avg_hash"),
                             p("We can see that number of hashtags is not a strong factor effecting the views, it's mostly the hashtag itself effecting the views.")
                ),
                mainPanel(
                  p("What hashtags gets the most liked?", style = "color:#ff0050; font-weight: bold"),
                  p("Hashtags, one of the most important attribute of a tiktok video, works as a keyword for a video that help TikTok's algorithm show videos to your target audience, in addition to captions and copy.
                    They are perfect examples of the information quality of tiktok. The algorithm utilize the hashtags to put the recommended video to the users. Here are the hashtags that used in some of the most viewed 
                    tiktok in 2021." ),
                  h3("You can use the slider at the right to set a view count range, and see what are the hashtags used in these videos.", style = "color:#00f2ea; font-style: italic"),
                  plotOutput(outputId = "trending"),
                  h2("Summary", style = "color:#ff0050; font-style: italic"),
                  p("For the content creators, the hashtags are great to boost their video's views count, but for the users, hashtags are tools for the algorithm to give them what they enjoy to watch. 
                    Hashtags further strength the information quality of the video that algorithm gives us, which contributes to user's addiction to tiktok.
                     There is a closed-loop relationship between Tiktok addiction and algorithm optimization. The more frequently users use tiktok, the more accurate the algorithm becomes, potentially exacerbating addiction.
")
                )
  )
)

#Conclusion Panel
conclusion <-  fluidPage(
  titlePanel("Conclusion"),
  br(),
  
  tags$style(HTML("
    h2 {
            color: black;
            }")),
  
  mainPanel(
    h2("Takeaways", style = "background-color: #D3D3D3;"),
    br(),
    p("In conclusion, we discussed three aspect of tiktok videos, the often overlooked importance
       of music and sound design. the brevity of videos and the strategic use of hashtags enhance the 
       quality of information delivered by the algorithm. These three aspects, and the platform's straightforward 
       interface, aimed at immersing users in passive engagement with recommended videos, plays a pivotal role
       in fostering addictive behaviors. Users can seamlessly swipe through short videos, 
       unintentionally prolonging their time on TikTok. This interplay underscores a symbiotic relationship
       between TikTok addiction and algorithmic optimization, highlighting the platform's unique 
       ability to captivate its users."),
    br(),
    h3("Tiktok addition can be big problems that takes way too much time form our life.
       so we want to educate the readers to better understand why tiktok is so addictive and 
       encourage them to consider the role that social media plays in their
       lives."),
    br(),
    br(),
    p("This is where we got our dataset:"),
    p("https://github.com/datares/TikTok_Famous/blob/main/Datasets/
       TikTok%20Video%20Data%20Collection/sug_users_vids1.csv"),
    p("https://github.com/datares/TikTok_Famous/blob/main/Datasets/
       TikTok%20Video%20Data%20Collection/top_users_vids.csv"),
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
  div(style = "position: relative; height: 700px;",  
      div(h3(em("From Tony Sun, Sunny Yang, Mars Ouyang.")), 
          style = "position: absolute; bottom: 10px; right: 10px;")
  ),
)

#UI
ui <- navbarPage(inverse = TRUE, "Final Project INFO201",
                 tabPanel("Intro", includeCSS("styles.css"), about_view),
                 
                 tabPanel("Video Duration", includeCSS("styles.css"), videolength_view),
                 
                 tabPanel("Trending Songs", includeCSS("styles.css"), wordcloud1),
                 
                 tabPanel("Hashtag", includeCSS("styles.css"), hashtag_view),
                 
                 tabPanel("Conclusion", includeCSS("styles.css"), conclusion)
                 
)
#Server
server <- function(input, output) {
  
  #Video Length ScatterPlot output
  length_tag <- function(tags){
    vlength <- filter(tiktok_df, duration_category == tags)
    return(vlength)
  }
  
  output$length_chart <- renderPlot({
    plot <- ggplot(data = length_tag(input$length_cat), aes(x=video_length.x, y=n_plays.x, color = "#ff0050" )) + 
      geom_point()+
      labs(x = "Video Length (Seconds)", y = "View Count", ) +
      geom_hline(yintercept = mean(length_tag(input$length_cat)$n_plays.x), color = "#00f2ea")
    plot + theme(
      plot.background = element_rect(fill = "white"), 
      panel.background = element_rect(fill = "black"),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12, face = "bold")
    )
  })
  
  average_view_count <- reactive({
    mean(length_tag(input$length_cat)$n_plays.x)
  })
  
  output$average_view_count <- renderText({
    paste("As shown by the light blue horizontal line in the graph, the average view count for the", 
          input$length_cat, "length category is around", round(average_view_count(), 2))
  })
  
  #Trending Song Wordcloud & Table output
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
  
  
  #Plots on hashtags vs likes
  output$avg_hash <- renderPlot({
    ggplot(tiktok_df, aes(x=n_hash, y=n_plays.x)) +
      geom_point() + 
      ggtitle("Number of Plays vs Number of Hashtags Used") +
      labs(color="Follower Count") + xlab("Number of Hashtags") + ylab("Number of Plays") +
      geom_hline(yintercept = mean(tiktok_df$n_plays.x)) + 
      geom_vline(xintercept = mean(tiktok_df$n_hash, na.rm = T))
  })
  
  tiktok<- reactive({
    req(input$views)
    tiktok_ <- filter(tiktok_df, n_plays.x < max(input$views), n_plays.x > min(input$views))
    return(tiktok_)
  })
  
  output$trending <- renderPlot({
    if (nrow(tiktok()) > 0) {
      tiktok_ <- data.frame(table(unlist(strsplit(tolower(tiktok()$hashtags.x), ",")))) %>%top_n(15)
      colnames(tiktok_) <- c("Hashtag", "Count")
      p <-ggplot(tiktok_[0:15,], aes(Hashtag, Count))
      p + geom_bar(stat = "identity", fill = "#ff0050") +
        labs(x = "Hashtags", y = "View Count", ) +
        theme(
          axis.title = element_text(size = 14, face = "bold"),
          axis.text = element_text(size = 12, face = "bold")
        )
    }
  })
  
}
#Make the app 
shinyApp(ui, server)
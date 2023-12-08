library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(plotly)

source("analysis.R") #loads in your analysis file

# Overview ---------------------------------------------------------------------

# Homework 5: Examining Fight Song Lyrics - Shiny App
# Before you begin, make sure you read and understand the assignment description
# on canvas first! This assignment will not make sense otherwise. 
# For each question/prompt, write the necessary code to create your shiny app. 
# Make sure you have written AND TESTED all of your code in analysis.R before 
# beginning this part of the assignment. 

songs_df <- read.csv("fight_songs.csv") #DO NOT CHANGE!


# Creating your UI -------------------------------------------------------------
#
# Part of this UI is created for you. You need to add elements to make your UI
# match the example provided in the instruction. 
#
#-------------------------------------------------------------------------------

ui <- fluidPage(
    titlePanel("Examining Lyrics in College Fights Songs"),
    
    br(), # the br() function adss line breaks
    
    p("Let's examine the fight songs from 65 different universities across the US 
      (i.e. all those in the Power Five conferences (the ACC, Big Ten, Big 12, Pac-12 and SEC), plus Notre Dame). 
      Our analysis looks at how many times certain words appear in the lyrics to see how each song compares. 
      We also look at song length and speed of each song based on the official versions availible on spotify"),
    br(),
    
    p(paste("Overall out of the 65 songs we looked at,", fight_count," songs 
      contain the word 'fight' -- usually more than once. In fact, the word 'fight' 
      appears", total_fight, "in total throughout all the songs!", win_count, "songs contain 
      'win' or 'victory' and", rah_count, "contain the phrase 'rah'.")),
    br(),
    
    #this code just adds some custom HTML styling 
    tags$style(HTML("
    h2 {
            background-color: #ccd5d8;
            color: Black;
            }")),
    # --------------------------------------------------------------------------
    # Add code for a sidebar and maine panel here, Your sidebar should appear 
    # on the left and should contain the following elements:
    # 1) drop down menu that lets the user select a particular college from the 
    #    dataset. The inputId should be `school_name` and the prompt should be 
    #    "Choose a School"
    # 2) Three html outputs that we will use to contain the information about the
    #   lyrics for that particular school. Use the following 4 lines of code to 
    #   add these elements: 
    #htmlOutput(outputId = "song_name"),
    #htmlOutput(outputId = "composer"),
    #htmlOutput(outputId = "lyrics"),
    #br()
    #
    # The main panel should appear on the right and should contain the following
    # elelemnts:
    # 1) An h3 header with the text "Comparing Lyrics"
    # 2) a plotlyOutput with the outputID set as `scatter`
    #---------------------------------------------------------------------------

    
    
    titlePanel("Filter All Songs by Specific Words"),
    textInput(
      inputId = "word",
      label = "Enter text to search for song lyrics that contain that word"
    ), 
    htmlOutput(outputId = "songs"),
)


# Creating your Server ---------------------------------------------------------
#
# In this section you will add the logic to make your UI fully interactive. Make
# sure you name all your input and output elements correctly! 
#
#-------------------------------------------------------------------------------
server <- function(input, output) {
  
    output$song_name <- renderUI({
      # Here is where you should add code to set the UI element "song_name" to the
      # name of the song for the university selected by the user from the drop down
      # menu you created. You should use your function get_song_name that you wrote
      # earlier in your analysis.r file to help with this. 
      
      
    })
    
    
    output$composer <- renderUI({
      # Here is where you should add code to set the UI element "composer" to the
      # composer and year info for the song for the university selected by the 
      # user from the drop down menu you created. You should use your function 
      # get_info that you wrote earlier in your analysis.r file to help with this. 
     
      
    })
    
    output$lyrics <- renderUI({
      
      # Here is where you should add code to set the UI element "lyrics" to the
      # lyrics the song for the university selected by the  user from the drop 
      # down menu you created. You should use your function get_lyrics that you 
      # wrote earlier in your analysis.r file to help with this.

      
    })

    output$scatter <- renderPlotly({
      
      # Here is where you will create a ggplot scatterplot that you need to store
      # in a variable called `p`. The scatterplot you create should have the 
      # following characteristics: 
      #   x axis & Label: Song Duration 
      #   y axis & Label: Beats per Minute
      #   text: school name (NOTE: setting the text will not do anything here, but you 
      #       need to create this aesthetic text mapping in order for the tooltips
      #       to appear)
      #   circle color should be determined based on which university it is
      #   a geom_text label that appears for only the university selected by the user!
      
    
     
      # Once you have created your scatterplot, uncomment the following two lines.
      # This code will add interactive tooltips to your scatterplot that display the 
      # school name and then returns the resulting plot so that the renderPlotly 
      # function works as intended. 
      
      #p <- ggplotly(p, tooltip = "text")
      #return(p)
    })
    

    output$songs <- renderUI({
      # create a variable called `filtered_df` that stores a dataframe that contains
      # only the rows in the overall dataset for the songs that contain the word
      # provided by the user per the prompt "Enter text to search for song lyrics that contain that word"
      # You should use the get_relevant songs function you create earlier to make
      # this dataframe.
      
      
      
      # When you are done creating filtered_df, uncomment the following line to 
      # create the code to make the text in three columns like in the example: 
      #return(make_cols(filtered_df))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

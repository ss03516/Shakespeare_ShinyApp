library(shiny)
library(tidytext)
library(RColorBrewer)
library(tidyverse)
library(wordcloud)
library(shinythemes)

## Rshiny.io link: https://suffwan.shinyapps.io/Problem_Set_3_Suffwan/


# The list of valid books
books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")


# task4: add in getFreq function for pre-processing

getFreq <- function(book, stopwords = TRUE) {
  # check that only one of three books is selected
  if (!(book %in% books))
    stop("Unknown book")
  
  text <-  tibble(text = readLines(sprintf("./data/%s.txt", book), encoding="UTF-8"))
  
  # could also pass column of text/character instead
  text <- text %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE) 
  
  if(stopwords){
    text <- text %>%
      anti_join(stop_words)
  }
  
  return(text)
}


# task6: add in shinythemes function

ui <- fluidPage( 
  
  #theme = shinytheme("cerulean"),
  
  titlePanel("Shakespeare's Plays Word Frequencies"), # Application title
  
  # task1: add in the sidebarLayout with sidebarPanel and mainPanel
  sidebarLayout(
    
    sidebarPanel(
      
      #Select your book from dropdown
      selectInput(inputId = 'book',
                  label = 'Choose a book:',
                  choices = books,
                  selected = NULL
                  ),
      
      #Check box for stopword 
      checkboxInput(inputId = 'stopwords',
                    label = 'Stop words',
                    value = TRUE
                    ),
      
      #Action button 'Rerun'
      actionButton(inputId = 'run',
                   label = 'Rerun'
                   ),
      
      ## seperate sections ---------
      hr(),
      
      # Header
      h3("Word Cloud Settings"),
      
      #sliderInput for Max words
      sliderInput(inputId = 'maxwords',
                  label = 'Max # of Words:',
                  min = 10,
                  max = 200,
                  value = 100,
                  step = 10
                  ),
      #sliderInput for Max word size
      sliderInput(inputId = 'largestwords',
                  label = 'Size of largest words:',
                  min = 1,
                  max = 8,
                  value = 4
                  ),
      #sliderInput for min word size
      sliderInput(inputId = 'smallestwords',
                  label = 'Size of smallest words:',
                  min = 0.1,
                  max = 4,
                  value = 0.5
                  ),
      
      ## seperate sections ---------
      hr(),
      
      # Header
      h3("Word Count Settings"),
      
      #sliderInput for Max words
      sliderInput(inputId = 'mincount',
                  label = 'Minimum words for Counts Chart:',
                  min = 10,
                  max = 100,
                  value = 25
                  ),
      
      #sliderInput for Max words
      sliderInput(inputId = 'wordsize',
                  label = 'Word size for Counts Chart:',
                  min = 8,
                  max = 30,
                  value = 14
                  ),
      
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel(title = "Word Cloud",
                            plotOutput(outputId = "cloud", height = "600px", width = "800px" )),
                  tabPanel(title = "Word Counts",
                            plotOutput(outputId = "freq", height = "600px", width = "800px")),
                  )
                  
    )
    
  )

  
  
)

server <- function(input, output) {
  
  # task5: add in reactivity for getFreq function based on inputs
  freq <- eventReactive(input$run, {
    withProgress({
      setProgress(message = "Processing corpus...")
      getFreq(input$book,input$stopwords)
    })
    
  })

#  Creating Word cloud
  output$cloud <- renderPlot({
    v <- freq()
    pal <- brewer.pal(8,"Dark2")

    v %>%
      with(
        wordcloud(
          word,
          n,
          scale = c(input$largestwords, input$smallestwords),
          random.order = FALSE,
          max.words = input$maxwords,
          colors=pal))
  })

#Creating Word count

  output$freq <- renderPlot({
    v <- freq()
    pal <- brewer.pal(8,"Dark2")

    v %>%
      filter(n > input$mincount) %>%
      ggplot(aes(x = reorder(word, +n), y = n)) +
      geom_col(state = 'identity') +
      coord_flip()+
      theme(axis.text=element_text(size = input$wordsize),
             axis.title = element_blank()
            )
    
  })
  
  
}

shinyApp(ui = ui, server = server)

# word prediction

library(shiny)
library(NLP)
library(tm)
#library(data.table)
ngramtable <- read.csv(file="ngram.csv", header=TRUE, sep=",")

oneword <- ngramtable[is.na(ngramtable$word2),]
oneword <- oneword[order(oneword$frequency, decreasing = TRUE),]
oneword <- as.vector(oneword$word1)[1:5]

cleanthetext <- function(text){
  cleanedtext <- tolower(text)
  cleanedtext <- stripWhitespace(cleanedtext)
  cleanedtext <- gsub("[^\\p{L}\\s]+", "", cleanedtext, perl=T)
  return(cleanedtext)
}

splittheword <- function(text){
  cleanedtext <- cleanthetext(text)
  splittedtext <- unlist(strsplit(cleanedtext," "))
  return(splittedtext)
}

singlewordprediction <- function(cleanedtextlist){

  df <- ngramtable[ngramtable$word1 == cleanedtextlist[1],]
  df <- df[!is.na(df$word2),]
  df <- df[order(df$frequency, decreasing = TRUE),]
  dup <- duplicated(subset(df,select = c("word1","word2")))
  df <- df[!dup,]
  
  n <- 5
  if (length(df$word2)==0){
    return(vector())
  }
  if ( n > length(df$word2)){
    n <- length(df$word2)
  }

  nextword <- as.vector(df$word2)[1:n]

  return(nextword)
}

twowordprediction <- function(cleanedtextlist){
  df <- ngramtable[ngramtable$word1 == cleanedtextlist[1],]
  df <- df[df$word2 == cleanedtextlist[2],]
  df <- df[!is.na(df$word3),]
  df <- df[order(df$frequency, decreasing = TRUE),]
  dup <- duplicated(subset(df,select = c("word1","word2","word3")))
  df <- df[!dup,]
  n <- 5
  if (length(df$word3)==0){
    return(vector())
  }
  if ( n > length(df$word3)){
    n <- length(df$word3)
  }
  
  nextword <- as.vector(df$word3)[1:n]
  
  return(nextword)
}

threewordprediction <- function(cleanedtextlist){
  df <- ngramtable[ngramtable$word1 == cleanedtextlist[1],]
  df <- df[df$word2 == cleanedtextlist[2],]
  df <- df[df$word3 == cleanedtextlist[3],]
  df <- df[!is.na(df$word4),]
  df <- df[order(df$frequency, decreasing = TRUE),]
  dup <- duplicated(subset(df,select = c("word1","word2","word3","word4")))
  df <- df[!dup,]
  n <- 5
  if (length(df$word4)==0){
    return(vector())
  }
  if ( n > length(df$word4)){
    n <- length(df$word4)
  }
  
  nextword <- as.vector(df$word4)[1:n]

  return(nextword)
}


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel(title = h6(tags$b("Predict A Next Word"), align = "center",style = "font-family: 'Lobster';font-size: 40px;
        font-weight: 1000; line-height: 2.1; 
                        color: #39487c;")),
  sidebarLayout(
    sidebarPanel(
      textInput("rinput","Please enter text here"),
      helpText("Please enter text. When space is pressed or the Next button is clicked, the possible words will show beside for prediction."),
      tags$script(HTML("$(function(){ 
      $(document).keyup(function(e) {
                       if (e.which == 32) {
                       $('#button').click()
                       }
                       });
                       })")),
      actionButton("button", "Next"),
      tags$style(".well {background-color:#c7c8cc;}")
      
    ),
    mainPanel(
      h4("Cleaned Text"),
      textOutput("cleaned"),
      br(),
      h4("Possible next word"),
      textOutput("possiblenextword")
    )
  )
)

server <- function(input, output) {
   
  output$cleaned <- renderText({
    rawinput <- input$rinput
    processedinput <- cleanthetext(rawinput)
    return(processedinput)
  })
  
  output$possiblenextword <- renderText({
    rawinput <- input$rinput
    processedinput <- cleanthetext(rawinput)
    possiblenextword <- ""
    splitinput <- splittheword(rawinput)
    noofwords <- length(splitinput)
    
    if(noofwords >= 3){
      searchforword1 <- c(splitinput[noofwords-2],
                          splitinput[noofwords-1],
                          splitinput[noofwords])
      searchforword2 <- c(splitinput[noofwords-1],
                          splitinput[noofwords])
      searchforword3 <- c(splitinput[noofwords])
      possiblenextword <- c(threewordprediction(searchforword1),
                            twowordprediction(searchforword2),
                            singlewordprediction(searchforword3),
                            oneword)
      possiblenextword <- unique(possiblenextword)
      n <- 5
      if ( n > length(possiblenextword)){
        n <- length(possiblenextword)
      }
      possiblenextword <- paste(possiblenextword[1:n])
    }
    
    if(noofwords == 2){
      searchforword2 <- c(splitinput[noofwords-1],
                          splitinput[noofwords])
      searchforword3 <- c(splitinput[noofwords])
      possiblenextword <- c(twowordprediction(searchforword2),
                            singlewordprediction(searchforword3),
                            oneword)
      possiblenextword <- unique(possiblenextword)
      n <- 5
      if ( n > length(possiblenextword)){
        n <- length(possiblenextword)
      }
      possiblenextword <- paste(possiblenextword[1:n])
    }
    
    if(noofwords == 1){
      searchforword3 <- c(splitinput[noofwords])
      possiblenextword <- c(singlewordprediction(searchforword3),
                            oneword)
      possiblenextword <- unique(possiblenextword)
      n <- 5
      if ( n > length(possiblenextword)){
        n <- length(possiblenextword)
      }
      possiblenextword <- paste(possiblenextword[1:n])
    }
    
    return(possiblenextword)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Predict The Next Word"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         textInput("phrase", "Enter a Phrase")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        h2('The Next Word in the Phrase is'),
        textOutput("phrase")
      )
   )
))

library(dplyr)
library(stringi)
library(stringr)

#setwd("I:/Capstone/en_US/Project")
setwd("~/datasciencecoursera/CAPSTONE/final/final/en_US/")
phrases4<-read.csv("Five Words.csv")
phrases3<-read.csv("Three Words.csv")
phrases2<-read.csv("Two Words.csv")
phrases1<-read.csv("One Words.csv")
predict_next_word<-function(p){
  nword<-sum(stri_count_words(p))
  k<-p
  if (nword>3) { 
    ph<-word(p,-4:-1)
    k<-paste(ph[1],ph[2],ph[3],ph[4],sep=" ")
    phrase_list<-phrases4[grep(paste("^",k,sep="",collapse=NULL),phrases4$words),]
    if(nrow(phrase_list)>0){
      phrase_list<-arrange(phrase_list,desc(freq))
      b<-word(phrase_list[1,1],-1)
    } else {
      nword=3
      ph<-word(k,-3:-1)
      k<-paste(ph[2],ph[3],ph[4],sep=" ")
    }
  } 
  if (nword==3){
    phrase_list<-phrases3[grep(paste("^",k,sep="",collapse=NULL),phrases3$words),]
    if(nrow(phrase_list)>0){
      phrase_list<-arrange(phrase_list,desc(freq))
      b<-word(phrase_list[1,1],-1)
    } else {
      nword=2
      ph<-word(k,-2:-1)
      k<-paste(ph[2],ph[3],sep=" ")
    }
  }
  if (nword==2){
    phrase_list<-phrases2[grep(paste("^",k,sep="",collapse=NULL),phrases2$words),]
    if(nrow(phrase_list)>0){
      phrase_list<-arrange(phrase_list,desc(freq))
      b<-word(phrase_list[1,1],-1)
    } else {
      nword=1
      k<-word(k,-1)
    }
  }
  if (nword==1){
    phrase_list<-phrases1[grep(paste("^",k,sep="",collapse=NULL),phrases1$words),]
    if(nrow(phrase_list)>0){
      phrase_list<-arrange(phrase_list,desc(freq))
      b<-word(phrase_list[1,1],-1)
    } else {
      nword=0
    }
  }
  if (nword==0){
    b<-"one"
  }
  b
}



# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  output$phrase<-renderPrint({predict_next_word(input$phrase)})
  
})

# Run the application 
shinyApp(ui = ui, server = server)


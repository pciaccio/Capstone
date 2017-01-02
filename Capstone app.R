#
# This is a Shiny web application for the Coursera
# Data Science Specialization Capstone Project
#
# The purpose of this app is to predict the next
# word in a phrase

library(shiny)

# Define UI for application
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Predict The Next Word"),
   
   # Text Input 
   sidebarLayout(
      sidebarPanel(
         textInput("phrase", "Enter a Phrase"),
         submitButton(text="Predict")
      ),
      
      # Show the predicted next word
      mainPanel(
        h3('The Next Word in the Phrase is'),
        strong(textOutput("phrase"))
      )
   )
))

# Most of the work being done for the prediction algorithm
library(dplyr)
library(stringi)
library(stringr)
library(RCurl)
library(tm)

# read n-gram files to use in the app
phrases4<-read.csv(text=getURL("https://raw.githubusercontent.com/pciaccio/Capstone/master/Four%20Words.csv"))
phrases3<-read.csv(text=getURL("https://raw.githubusercontent.com/pciaccio/Capstone/master/Three%20Words.csv"))
phrases2<-read.csv(text=getURL("https://raw.githubusercontent.com/pciaccio/Capstone/master/Two%20Words.csv"))
phrases1<-read.csv(text=getURL("https://raw.githubusercontent.com/pciaccio/Capstone/master/One%20Word.csv"))

# Function to predict the next word
predict_next_word<-function(p){
  p<-tolower(p)
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



# Define server logic required to output prediction
server <- shinyServer(function(input, output) {
  
  output$phrase<-renderPrint({ predict_next_word(input$phrase) })

})

# Run the application 
shinyApp(ui = ui, server = server)


library(tm)

stopw<-stopwords("en")

setwd("I:/Capstone/en_US/Project")
stop<-readLines(file("curse.txt","r"),skipNul=TRUE, encoding="UTF-8")
setwd("J:/CAPSTONE")
phrases4<-read.csv("Five Words4.386.csv")

phrases3<-phrases4
phrases3$words<-gsub("\\s*\\w*$","",phrases3$words)
phrases3<-aggregate(phrases3$freq,by=list(phrases3$words),FUN=sum,na.rm=TRUE)
names(phrases3)<-c("words","freq")
write.csv(phrases3,"Three Words.csv")

phrases2<-phrases3
phrases2$words<-gsub("\\s*\\w*$","",phrases2$words)
phrases2<-aggregate(phrases2$freq,by=list(phrases2$words),FUN=sum,na.rm=TRUE)
names(phrases2)<-c("words","freq")
write.csv(phrases2,"Two Words.csv")

phrases1<-phrases2
phrases1$words<-gsub("\\s*\\w*$","",phrases1$words)
phrases1<-aggregate(phrases1$freq,by=list(phrases1$words),FUN=sum,na.rm=TRUE)
names(phrases1)<-c("words","freq")
write.csv(phrases1,"One Words.csv")

words_only<-phrases1
words_only$words<-gsub("\\s*\\w*$","",words_only$words)
words_only<-aggregate(words_only$freq,by=list(words_only$words),FUN=sum,na.rm=TRUE)
names(words_only)<-c("words","freq")
words_only<-subset(words_only,!(words %in% stop))
words_only<-subset(words_only,!(words %in% stopw))
write.csv(words_only,"Words.csv")

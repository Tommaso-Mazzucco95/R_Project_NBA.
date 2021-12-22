
#Set Working Directory
setwd("~/Desktop/hult_NLP_student/cases/NBA Fan Engagement")
#Option
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

#tolower fucntion

tryTolower = function(x)
{
  # create missing value
  # this is where the returned value will be
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}
#Libraries
library(tm)
#library(qdap)
library(pbapply)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(stringi)

#clean and set the fucntion clean corpus
cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

#Assign stopwords

stops <- c(stopwords('SMART'), 'nba', 'amp', 'and', 'lakers','celtics','rockets'
            , "warriors","state","new york","knicks","boston","golden", "raptors",
            "nets", "bucks", "bulls", "clippers", "toronto","chicago","bulls",
           "pelicans","spurs","kings","hawks","heat","san","kings","game","star",
            "nbaallstar",'head','staff','nick','phoenix','players','sun',
            'harden','giannis','antetokounmpo','james','houston','pistons','detroit','suns',
           'mcammirepistons','milwauk')


#upload the data
text_nba <- read.csv("E_Feb2020.csv", header=TRUE)

#Random Sample
sampleids <- sample(text_nba$doc_id, 50000)
newdata_20 <- subset(text_nba, text_nba$doc_id %in% sampleids)

newdata_20$text <- gsub('http\\S+\\s*', "", newdata_20$text)
newdata_20$text <- gsub('RT|via)((?:\\b\\W*@\\w+)+)', "", newdata_20$text)
newdata_20$text <- gsub('\342\200\246', "", newdata_20$text)
newdata_20$text <- gsub('\360\237\221\216', "", newdata_20$text)
newdata_20$text <- gsub('\342\200\213', "", newdata_20$text)
newdata_20$text <- gsub('\342\200\234I\342\200\231', "", newdata_20$text)
newdata_20$text <- gsub('\342\232\241\357\270\217 ', "", newdata_20$text)
newdata_20$text <- gsub('\360\237\221\221', "", newdata_20$text)
newdata_20$text <- gsub('\342\200\223', "", newdata_20$text)
newdata_20$text <- gsub('\360\237\223\242', "", newdata_20$text)
newdata_20$text <- gsub('\342\200\234', "", newdata_20$text)
newdata_20$text <- gsub('\342\200\231', "", newdata_20$text)
newdata_20$text <- gsub('\360\237\232\250', "", newdata_20$text)
#let's select just the text 

idx <- newdata_20$text

#calcute word frequency of harden

num_harden <- grep('harden',idx,ignore.case = TRUE)
length(num_harden)
sum(num_harden) / nrow(newdata_20)

#calcute word frequency of antetokounmpo
num_ant <- grep('antetokounmpo',idx,ignore.case = TRUE)
length(num_ant)
sum(num_ant) / nrow(newdata_20)



nba_feb_20<- VCorpus(VectorSource(newdata_20$text))
clean_data <- cleanCorpus(nba_feb_20,stops)


df<- data.frame(text= unlist(sapply(clean_data, '[', "content")),
                stringsAsFactors = F)

#Let's find out  and visualize the two players' popularity

antetokounmpo  <- sum(stri_count(clean_data, fixed ='antetokounmpo'))
harden  <- sum(stri_count(clean_data, fixed ='harden '))



freq <- data.frame(terms = c('Antetokounmpo','Harden'),
                   freq  = c(antetokounmpo,harden))
#plot it
ggplot(data = freq, aes(x = reorder(terms, freq), y = freq,fill="green")) + 
  geom_bar(stat = "identity") + coord_flip() + 
  theme_gdocs() + theme(legend.position = "none")


#Let's find tweets ctiting Antetoukonmpo and Harden in the same Sentence and explore


newdata_20$players <- grepl(('antetokounmpo|harden'),newdata_20$text,ignore.case = TRUE)

playersData <- subset(newdata_20,newdata_20$players== T)

#Apply v corpus function
NBAplayers<- VCorpus(VectorSource(playersData$text))
players_clean_data <- cleanCorpus(NBAplayers, stops)

#make it into a matrix
players_dtm <- TermDocumentMatrix(players_clean_data)
playersdtm_m <- as.matrix(players_dtm)



tweetSums_players <- sort(rowSums(playersdtm_m),decreasing = TRUE)
tweetFreq_players <- data.frame(word=names(tweetSums_players),frequency=tweetSums_players)

head(tweetFreq_players, 15)

topwords_players = subset(tweetFreq_players, tweetFreq_players$frequency >= 160) 
topwords_players= topwords_players[order(topwords_players$frequency, decreasing=F),]

# factor for ggplot
topwords_players$word <- factor(topwords_players$word, 
                                levels=unique(as.character(topwords_players$word)))

#Plot the top words that have a frequency >160
topwords_players%>% 
  filter(frequency>160) %>% 
  ggplot(., aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill="lightblue") + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.20, size=1.0)




#Create a new Data Frame with tweets conncerning the All Star Game

newdata_20$all_star <- grepl(('star game'),newdata_20$text,ignore.case = TRUE)

allStarData <- subset(newdata_20,newdata_20$all_star== T)

nbaAllStar<- VCorpus(VectorSource(allStarData$text))
clean_data_NBA <- cleanCorpus(nbaAllStar, stops)

textdoc_dtm <- TermDocumentMatrix(clean_data_NBA)
NBAdtm_m <- as.matrix(textdoc_dtm)

#document matrix


tweetSums <- sort(rowSums(NBAdtm_m),decreasing = TRUE)
tweetFreq <- data.frame(word=names(tweetSums),frequency=tweetSums)

head(tweetFreq, 15)

topwords = subset(tweetFreq, tweetFreq$frequency >= 30) 
topwords= topwords[order(topwords$frequency, decreasing=F),]

#factor for ggplot
topwords$word <- factor(topwords$word, 
                        levels=unique(as.character(topwords$word)))

#Plot the top words that have a frequency >30
topwords %>% 
  filter(frequency>30) %>% 
  ggplot(., aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill="lightblue") + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.20, size=1.0)

#Plot Top 5 words
topwords %>% 
  slice_max(frequency, n=5) %>% 
  ggplot(., aes(x=word, y=frequency))+
  geom_col(stat="identity", fill="lightblue")


#Create a word could of the most popular words 
wordcloud(words=tweetFreq$word, freq= tweetFreq$freq, min.freq = 5,
          max.words=100, random.order =FALSE, rot.per=0.40,
          colors= brewer.pal(8, "Dark2"))


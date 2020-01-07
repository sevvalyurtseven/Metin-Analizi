install.packages("httpuv")
install.packages("ROAuth")
install.packages("wordcloud")
install.packages("tm")
install.packages("tidyverse")
install.packages("lubridate")

library(httpuv)
library(tidyverse)
library(ROAuth)
library(ggplot2)
library(wordcloud)
library(tm)
library(twitteR)
library(dplyr)
library(lubridate)


consumer_key<-"kTjNJbRkm9FOlD7alNfqE5OVz"
consumer_secret<-"mvpUe965F3sMSRiYGcRQj5kqnr5kMRtwVhsUweTIKxp7njqmqjNz"
access_token<-"14519511-fVpPVLuuGiWbrFyRKYzbZNxc05IQg141fQGbVZoHy"
access_secret<-"YgbM4yIUCiZBttRJA9Jm3VNF5glINY08XPuIzxC7n6VFw"
setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)

##NYTÝMES attýðý tweetler

nytimes <- userTimeline("nytimes",n=1000,since = '2019-01-01')
tweets = twitteR::twListToDF(nytimes) #data frame

save(nytimes,file ="nytimes.RData") #dosya olarak R a Kaydetmek

write.csv(tweets,file ="nytimes.csv") #excel (csv) olarak kaydetmek

nytimes <- read.csv("C:/Users/hp/Downloads/nytimes.csv") #Excel dosyasýný R a okutmak
nytimes <- nytimes %>%
  mutate_at(vars(text), as.character) 

View(tweets) #veriyi göstermek için kullanýlýr.

names(tweets)
summary(tweets)

## ETKÝLESÝM DEÐERLERÝ

nytimes_tw <- rownames_to_column(tweets) %>%
  select(created,favoriteCount,retweetCount)
View(nytimes_tw)

nytimes_tw1 <- nytimes_tw %>%
  mutate(etkilesim = (favoriteCount + retweetCount)/2) %>%
  gather(etkilesim,deger,-created)

View(nytimes_tw1)

## ETKÝLESÝM GÖRSELÝ

ggplot(nytimes_tw1,aes(created,deger,color=etkilesim)) + geom_jitter(size = 4 , alpha = 0.5) +
  labs(x= "" , y="", title = "NewYork Times'ýn Attýðý Son Tweetlerin Etkileþim Deðerleri") + 
  theme_minimal(base_size = 10 , base_family = "Helvetica") + 
  theme(legend.position = "bottom")


##EN ÇOK KÝMÝN TWEETÝ RETWEET EDÝLDÝ
rt <- tweets %>% count(replyToSN , sort=TRUE)
rt

##Corpus Oluþturmak

mycorpus <- VCorpus(VectorSource(tweets$text))

removeURL <- function(x) gsub("http[[:alnum:]]*","",x) #url
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*","",x) #ing olmayan harfler

new_stop <- c(stopwords("en"),"rt","one","news","will","year")
new_stop #istenmeyen kelimeleri silme (stopwords)

#Veriyi temizleme

clean_tweets <- tm_map(mycorpus,PlainTextDocument)
clean_tweets <- tm_map(clean_tweets,content_transformer(removeURL))
clean_tweets <- tm_map(clean_tweets, stripWhitespace)
clean_tweets <- tm_map(clean_tweets, content_transformer(tolower))
clean_tweets <- tm_map(clean_tweets, removeWords, new_stop)
clean_tweets <- tm_map(clean_tweets,content_transformer(removeNumPunct))
clean_tweets <- tm_map(clean_tweets, removePunctuation)

tweets$text[40]
clean_tweets[[40]][1]

#Terim-doküman matsisi olusturma

tweets_tdm <- TermDocumentMatrix(clean_tweets)
tweets_tdm

tweets_m <- as.matrix(tweets_tdm)
dim(tweets_m)

#Sýk kullanýlan kelimeler

term_frequency <- rowSums(tweets_m)
term_frequency <- sort(term_frequency,decreasing = TRUE)

term_frequency[1:10]

#Barplot (en sýk kullanýlan 20 kelimenin grafiði)

barplot(term_frequency[1:10],col ="tan", las = 2)

#Çizgi grafiði (frekansý 15 ten büyük olan kelimelerin)

term_frequency <- subset(term_frequency,term_frequency >= 15)
term_freq_df <- data.frame(term = names(term_frequency),freq = term_frequency)
ggplot(term_freq_df,aes(x=term,y=freq)) +
  geom_line(aes(group=1),colour="blue") +
  geom_point(size = 3,colour = "pink2") +
  xlab("Kelime") + ylab("Frekans") + coord_flip()

#WordCloud (Kelime Bulutu)
install.packages("wordcloud2")
install.packages("wordcloud")
install.packages("reshape")

library(wordcloud)
library(wordcloud2)
library(reshape)
library(tm)

wordcloud(clean_tweets,min.freq = 2,scale = c(2,0.5) , colors =brewer.pal(8,"Dark2"),
          random.color = TRUE, random.order = FALSE, max.words = 180)
##
 
wordcloud(clean_tweets,min.freq = 2,scale = c(2,0.5),colors = brewer.pal(8,"Dark2"),
          random.color = TRUE,random.order = FALSE,max.words = 180)

## 
word_freqs <- data.frame(term_frequency,term =names(term_frequency),num = term_frequency)
wordcloud(word_freqs$term,word_freqs$num,max.words = 100, colors = brewer.pal(11,"RdYlGn"),random.color = TRUE)


##Kelimeler Arasýndaki iliþkilendirmeler
findAssocs(tweets_tdm,terms = "new",corlimit = 0.2)$new
findAssocs(tweets_tdm,terms = "president",corlimit = 0.3)$president
findAssocs(tweets_tdm,terms = "opinion",corlimit = 0.24)$opinion

#Duygu Analizi
install.packages("tidytext")
install.packages("textdata")
install.packages("janeaustenr")
install.packages("stringr")
install.packages("syuzhet")
install.packages("lubridate")
install.packages("scales")
install.packages("reshape2")

library(syuzhet)
library(lubridate)
library(sclaes)
library(reshape2)
library(tidytext)
library(textdata)
library(janeaustenr)
library(stringr)
library(plyr)

tweets_sa <- iconv(tweets$text)
s <- get_nrc_sentiment(tweets_sa)
head(s)

get_nrc_sentiment('delay')

barplot(colSums(s),
        las = 2,
        col = rainbow(8),
        ylab = 'Count',
        main = 'Duygu Analizi NYTÝMES')


#Korelasyonlar
korelasyonlar <- findAssocs(tweets_tdm, "new", 0.2)

#View the venti associations
korelasyonlar

#Korelasyonlarý veri tabaný haline çevirelim
install.packages("qdap")
library(qdap)

korelasyonlar.df <- list_vect2df(korelasyonlar)[, 2:3]

#Grafiklendirme
install.packages("ggthemes")
library(ggplot2)
library(ggthemes)
install.packages("qdap")
library(qdap)

ggplot(korelasyonlar.df, aes(y = korelasyonlar.df [, 1])) + 
  geom_point(aes(x = korelasyonlar.df [, 2]), 
             data = korelasyonlar.df , size = 3) + 
  theme_gdocs()

#Kelime kümeleri
dim(tweets_tdm)
tweets_tdm1 <- removeSparseTerms(tweets_tdm, sparse = 0.95) #1. seyreklestirme islemi
tweets_tdm2 <- removeSparseTerms(tweets_tdm, sparse = 0.98) #2. seyreklestirme islemi
tweets_tdm1
tweets_tdm2

#Matrisi oluþturalým
tweets_tdm.m <- as.matrix(tweets_tdm2)

#Veri tabanýný oluþturalým
tweets_tdm.vt <- as.data.frame(tweets_tdm.m)

#Kelimeler arasý mesafeleri hesaplayalým
tweets_mesafe <- dist(tweets_tdm.vt)

#Kümeleme iþlemini yapalým
tweets_kume <- hclust(tweets_mesafe)

#Dendogramý çizelim
plot(tweets_kume)

library(rvest)
library(XML)
library(magrittr)

###########################KHUDA HAFIZ MOVIE###############################

khmovie <- NULL
rev <- NULL
url <- "https://www.imdb.com/title/tt7363104/reviews?ref_=tt_urv"
murl <- read_html(as.character(paste(url,1, sep = "")))
rev <- murl %>% html_nodes(".show-more__control") %>% html_text()
khmovie <- c(khmovie,rev)

write.table(khmovie, "khudahafiz.txt")
getwd()


###############################DIL BECHARA MOVIE###########################
dbmovie <- NULL
review <- NULL
url <- "https://www.imdb.com/title/tt8110330/reviews?ref_=tt_urv"
murl <- read_html(as.character(paste(url,1, sep = "")))
review <- murl %>% html_nodes(".show-more__control") %>% html_text()
dbmovie <- c(dbmovie,review)

write.table(dbmovie, "dilbechara.txt")
getwd()


#######################SENTIMENTAL ANALYSIS#########################

library(tm)
library(topicmodels)
library(slam)

x <- readLines(file.choose()) #import dilbechara.txt
str(x)
length(x)
x

#using tm package#
mydata.corpus <- Corpus(VectorSource(x))
mydata.corpus <- tm_map(mydata.corpus,removePunctuation)
my_stopwords <- readLines(file.choose())
mydata.corpus <- tm_map(mydata.corpus,removeWords,my_stopwords)
mydata.corpus <- tm_map(mydata.corpus,removeNumbers)
mydata.corpus <- tm_map(mydata.corpus,stripWhitespace)


#build a term document matrix
mydata.dtm3 <- TermDocumentMatrix(mydata.corpus)
matrix <- as.matrix(mydata.dtm3)
#
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)


dim(mydata.dtm3)

dtm <- t(mydata.dtm3)
dtm$ncol
dtm$nrow
rowTotals <- apply(dtm,1,sum)
?apply
dtm.new <- dtm[rowTotals>0,]

lda <- LDA(dtm.new,10)

lterm <- terms(lda,10)
lterm

tops <- terms(lda)
tb <- table(names(tops),unlist(tops))
tb <- as.data.frame.matrix(tb)
?unlist

cls <- hclust(dist(tb),method = 'ward.D2')
par(family ='HiraKakuProN-W3')
plot(cls)


#emotion mining#

library(syuzhet)
my_example_text <- readLines(file.choose()) #import dilbechara.txt
s_v <- get_sentences(my_example_text)
class(s_v)
str(s_v)
head(s_v)

sentiment_vector <- get_sentiment(s_v,method = "bing")
head(sentiment_vector)

nrc_vector <- get_sentiment(s_v,method = "nrc")
head(nrc_vector)

sum(sentiment_vector)
mean(sentiment_vector)
summary(sentiment_vector)

#plot
plot(sentiment_vector,type = "l", main = "Plot Trajectory",
     xlab = "movie", ylab = "review")
abline(h= 0, col= "red")

#to extract the sentance with most negative emotional valence
negative <- s_v[which.min(sentiment_vector)]
negative

# and to extract most positive sentence
positive <- s_v[which.max(sentiment_vector)]
positive

#more depth
poa_v <- my_example_text
poa_sent <- get_sentiment(poa_v, method = "bing")
plot(poa_sent,type = "h", main = "review using transformed Values",
     xlab = "movie", ylab = "review")


ft_values <- get_transformed_values(poa_sent,
                                    low_pass_size = 3,
                                    x_reverse_len = 100,
                                    scale_vals = TRUE,
                                    scale_range = FALSE)

plot(ft_values, type = "h", main = "LOTR using Transformed values",
     xlab = "movie", ylab = "review",
     col="red")

nrc_data <- get_nrc_sentiment(s_v)
nrc_score_sent <- get_nrc_sentiment(negative)
nrc_score_word <- get_nrc_sentiment('grim')

#subset
sad_items <- which(nrc_data$sadness>0)
head(s_v[sad_items])

barplot(sort(colSums(prop.table(nrc_data[,1:8]))), horiz = T, cex.names = 0.7,
        las= 1, main = "review", xlab="Percentage",col = 1:8)


################## Word cloud ###############
install.packages("wordcloud")
install.packages("wordcloud2")

library(tm)
library(SnowballC)
library(wordcloud)
library(RCurl)
library(XML)
library(wordcloud2)


set.seed(1234) # for reproducibility 

wordcloud(words = df$word, freq = df$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

wordcloud2(data=df, size=1.6, color='random-dark')

#################postive words################3
wordcloud(words = positive, min.freq = 1, max.words=50, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

######################negative words##############
wordcloud(words = negative, min.freq = 1, max.words=50, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))



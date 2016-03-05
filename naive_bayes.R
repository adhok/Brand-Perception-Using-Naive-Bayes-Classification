library(twitteR)
library(tm)
library(RCurl)
library(plyr)
library(pacman)
library(ggplot2)
library(sentiment)
library(stringr)
install_url("http://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz")
install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
consumer_key<-"1MipFO71s9RQ5Yrp2UVLk4IFi"
consumer_secret<-"FdtnQzFgwcoomPl3DxoDykoKkJdAI3n4xwhOqZXbrvqhxH3VYq"
access_token<-"3013177956-a1Pkor6eYwO4RaVozAd1KWp7aXfTQZso2mvViRm"
access_secret<-"pgPQx0WTgBQWHQTGcEU6VFfSiscccvciPQg6ziYD8ejf3"
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)
text<-searchTwitter("Freedom251",n=15000,lang='en',resultType="recent")
text<-sapply(text,function(x) x$getText())
#removing content with no emotions
text<- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", text)
# remove at people
text<-gsub("@\\w+", "", text)
# remove punctuation
text <- gsub("[[:punct:]]", "", text)
# remove numbers
text <- gsub("[[:digit:]]", "", text)
# remove html links
text <- gsub("http\\w+", "", text)
# remove unnecessary spaces
text <- gsub("[ \t]{2,}", "", text)
text <- gsub("^\\s+|\\s+$", "", text)
#remove texts that are not letters eg smileys
text<-str_replace_all(text,"[^[:graph:]]"," ")
text<- tolower(text)
class_emo<-classify_emotion(text,algorithm="bayes",prior=1.0)
emotion <- class_emo[,7]
emotion[is.na(emotion)] = "unknown"
class_pol<-classify_polarity(text,algorithm="bayes")
polarity <- class_pol[,4]
sent_df <- data.frame(text=text,emotion=emotion,polarity=polarity,stringAsFactors=FALSE)
sent_df = within(sent_df,
emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
png("plot1.png")
ggplot(sent_df, aes(x=emotion)) +
geom_bar(aes(y=..count.., fill=emotion)) +
scale_fill_brewer(palette="Dark2") +
labs(x="emotion categories", y="number of tweets") +
ggtitle("Sentiment Analysis of Tweets about Freedom251\n(classification by emotion)") +
theme(plot.title = element_text(size=12, face="bold"))
dev.off()
#different plot
emos = levels(factor(sent_df$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = text[emotion == emos[i]]
  emo.docs[i] = paste(tmp, collapse=" ")
}

# remove stopwords
emo.docs = removeWords(emo.docs, stopwords("english"))
# create corpus
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos
png("wordcloud.png")
# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
scale = c(3,.5), random.order = FALSE, title.size = 1.5)
dev.off()



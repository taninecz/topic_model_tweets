.libPaths("C:/Users/dell/R/win-library/3.4")
library(quanteda)
library(quanteda.dictionaries)
library(stringi)
library(readtext)
library(stringi)
library(dplyr)
library("hexbin")
library(stm)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(tidytext)
library(broom)
library(stringr)
#*********************
tweets <- read.csv("tweets.csv", stringsAsFactors = FALSE)
#*********************
#CREATED_STR MIGHT TAKE CARE OF THIS-- LOOKS FORMATTED.
troll_dfm_w_dict@docvars$created_at <- as.numeric(troll_dfm_w_dict@docvars$created_at)
#*********************
#Corpus and dicts and dfms
#*********************
troll_corpus <- corpus(tweets)
RIDdict <- dictionary(file = "RID/RID.CAT", format = "wordstat")
tweet_dict <- dictionary(RIDdict)
troll_dfm <- dfm(troll_corpus,
                 remove = c("^.{0,3}$",#could also remove 3 letter words? orig: "^.{0,3}$"
                            "https://",
                            "t.co",
                            "https",
                            "http","&amp","&lt","&gt","RT","rt","t.co",
                            "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https",
                            stopwords("english")),
                 tolower = TRUE,
                 remove_punct=TRUE,
                 remove_numbers=FALSE,
                 remove_symbols=TRUE,
                 stem = TRUE)
troll_dfm <- troll_dfm %>%
  dfm_select(min_nchar = 4) %>%
  dfm_weight('frequency') #orig did 5 here, changed to 4.
topfeatures(troll_dfm, 500)
troll_dfm5 <- troll_dfm %>%
  dfm_select(min_nchar = 5) %>%
  dfm_weight('frequency')
#*********************
#STM topic models
#*********************
stm_troll_dfm <- stm(troll_dfm, K = 0,  #”free”
                    verbose = TRUE, init.type = "Spectral") 
stm_troll_dfm20 <- stm(troll_dfm, K = 20,  
                     verbose = TRUE, init.type = "Spectral")
troll_dfm_20k_4char <- stm(troll_dfm, K = 20,  
                  verbose = TRUE, init.type = "Spectral")
troll_dfm5 <- stm(troll_dfm5, K = 20,  
                     verbose = TRUE, init.type = "Spectral")

tidy_stm_troll_dfm <- tidy(stm_troll_dfm, matrix = "beta")
#notes from this, pjnet and nra. pjnet is a weird fringe right twitter group. unknown?
#also NRA connect. both work exploring.
plot.STM(stm_troll_dfm, type = c("labels"), labeltype = c("prob"),
         main="Title", width = 150, xlim = .5,
         text.cex = .7)
plot.STM(troll_dfm_20k_4char, type = c("summary"), labeltype = c("prob"),
         frexw=0.5,
         main="Title",
         width = 90, #topic proportions
         text.cex = .8)
#***************************
#simple STM process, sans quanteda
#***************************
tweets_no_hash <- data.frame(tweets$user_id, tweets$created_str, tweets$text,
                         tweets$tweet_id, tweets$user_key)
#hashtags still in text. hm. so cant remove punct?
tweets_no_hash$tweets.text <- as.character(tweets_no_hash$tweets.text)
encoding(tweets_no_hash$tweets.text, verbose = TRUE)
tweets_no_hash$tweets.text <- iconv(texts(tweets_no_hash$tweets.text),
                         from="windows-1252", to="ASCII", sub="")
tweets_no_hash$tweets.text <- iconv(texts(tweets_no_hash$tweets.text),
                         from="ISO-8859-1", to="ASCII", sub="")
tweets_no_hash$tweets.text <- iconv(texts(tweets_no_hash$tweets.text),
                         from="ISO-8859-2", to="ASCII", sub="")
tweets_no_hash$tweets.text <- iconv(texts(tweets_no_hash$tweets.text),
                         from="ISO-8859-9", to="ASCII", sub="")
tweets_processor<-textProcessor(documents=tweets_no_hash$tweets.text,
                                  lowercase = TRUE, removestopwords = TRUE,
                                  removepunctuation = FALSE, stem = TRUE,
                                  wordLengths = c(4, Inf),
                                  language = "en", verbose = TRUE)
out <- prepDocuments(tweets_processor$documents, tweets_processor$vocab,
                     tweets_processor$meta, verbose=TRUE)
stm_free <- stm(documents = out$documents, vocab = out$vocab, K=0)
plot.STM(stm_free, type = c("labels"), labeltype = c("prob"),
         main="Title", width = 150, xlim = .5,
         text.cex = .7)
plot.STM(stm_free, type = c("summary"), labeltype = c("score"),
         frexw=0.5,
         main="Title",
         width = 90, #topic proportions
         text.cex = .8)
#*************************
#dict analysis below
#*************************
troll_dfm_w_dict <- dfm_lookup(troll_dfm, dictionary = tweet_dict) #works!?
#loses userid and created_str? no maybe not
trollDFMdict_realfreq <- dfm_weight(troll_dfm_w_dict, "relFreq")
#seems have to have userid, etc. time.
aggro <- as.vector(trollDFMdict_realfreq[, "EMOTIONS.AGGRESSION"])
aggroDF <- as.data.frame(trollDFMdict_realfreq)
#this gets you all the features as a DF
aggro <- dfm_subset(trollDFMdict_realfreq,
                    trollDFMdict_realfreq@docvars$created_at)
trolldfm_relfreq <- dfm_weight(troll_dfm, "relFreq")
trollDFMdict_realfreq@docvars$created_str <- as.character(trollDFMdict_realfreq@docvars$created_str)
aggro <- as.data.frame(trollDFMdict_realfreq[, "EMOTIONS.AGGRESSION"],
                  trollDFMdict_realfreq@docvars$created_str,
                  stringsAsFactors=TRUE)
aggro <- rownames_to_column(aggro, "time")
ggplot(aggro,
       aes(time.numeric,
           EMOTIONS.AGGRESSION,
           color=time.numeric))+ 
  geom_histogram(stat = "identity")
ggplot(aggro,
       aes(time.numeric,
           EMOTIONS.AGGRESSION,
           color=time.numeric))+ 
  geom_col()
#*************************
#NRC dict analysis within quanteda
#*************************
tweet_DFM_NRC <- dfm(troll_corpus, #worked??
                         remove = c("^.{0,3}$",
                                    "0.00", "iii", "iiii", "1-1", "1.1", "400.00", "i-i",
                                    stopwords("english")),
                         dictionary = data_dictionary_NRC,
                         # valuetype = c("glob"),
                         ngrams=1L,  
                         tolower = TRUE,
                         remove_punct=TRUE,
                         remove_numbers=TRUE,
                         remove_symbols=TRUE,
                         stem = TRUE)
tweet_DFMNRC_relfreq <- dfm_weight(tweet_DFM_NRC, "relFreq")
#have to do this to get % of doc, not just sum of specific words. what % of doc sad, etc.
tweet_nrc_sent <- as.data.frame(tweet_DFMNRC_relfreq[, tweet_DFMNRC_relfreq@Dimnames$features],
                          tweet_DFMNRC_relfreq@docvars$created_str,
                          stringsAsFactors=TRUE)
tweet_nrc_sent <- rownames_to_column(tweet_nrc_sent, "Time")
tweet_nrc_sent$Time <- as.Date(tweet_nrc_sent$Time)
graph1 <- tweet_nrc_sent %>%
  ggplot(aes(x=Time, y=anger, color=anger, fill=anger)) +
  geom_smooth(se = TRUE) +
  scale_color_brewer(palette="Dark2")
graph2 <- tweet_nrc_sent %>%
  ggplot(aes(x=Time, y=anticip, color=anticip, fill=anticip)) +
  geom_smooth(se = TRUE) +
  scale_color_brewer(palette="Dark2")
graph1 <- tweet_nrc_sent %>%
  ggplot(aes(x=Time, y=disgust, color=disgust, fill=disgust)) +
  geom_smooth(se = TRUE) +
  scale_color_brewer(palette="Dark2")
graph2 <- tweet_nrc_sent %>%
  ggplot(aes(x=Time, y=fear, color=fear, fill=fear)) +
  geom_smooth(se = TRUE) +
  scale_color_brewer(palette="Dark2")
graph1 <- tweet_nrc_sent %>%
  ggplot(aes(x=Time, y=joy, color=joy, fill=joy)) +
  geom_smooth(se = TRUE) +
  scale_color_brewer(palette="Dark2")
graph2 <- tweet_nrc_sent %>%
  ggplot(aes(x=Time, y=posit, color=posit, fill=posit)) +
  geom_smooth(se = TRUE) +
  scale_color_brewer(palette="Dark2")
graph1 <- tweet_nrc_sent %>%
  ggplot(aes(x=Time, y=negat, color=negat, fill=negat)) +
  geom_smooth(se = TRUE) +
  scale_color_brewer(palette="Dark2")
graph2 <- tweet_nrc_sent %>%
  ggplot(aes(x=Time, y=sad, color=sad, fill=sad)) +
  geom_smooth(se = TRUE) +
  scale_color_brewer(palette="Dark2")
tweet_nrc_sent %>%
  ggplot(aes(x=Time, y=surpris, color=surpris, fill=surpris)) +
  geom_smooth(se = TRUE) +
  scale_color_brewer(palette="Dark2")
tweet_nrc_sent %>%
  ggplot(aes(x=Time, y=trust, color=trust, fill=trust)) +
  geom_smooth(se = TRUE) +
  scale_color_brewer(palette="Dark2"




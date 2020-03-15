setwd("C:\\Users\\acer\\Desktop\\MABA\\Business Domain 2")
Tweets<-read.csv("Tag_En_tweet.csv")
library(lda)
library(LDAvis)
library(dplyr)
# read in some stopwords:
library(tm)
stop_words <- stopwords("SMART")

# pre-processing:
Tweets$tweet <- gsub("http.*","",  Tweets$tweet)
Tweets$tweet <- gsub("https.*","", Tweets$tweet)
Tweets$tweet <- gsub("@.*","", Tweets$tweet)
Tweets$tweet <- gsub("#.*","", Tweets$tweet)
Tweets$tweet <- gsub("'", "", Tweets$tweet)  # remove apostrophes
Tweets$tweet <- gsub("[[:punct:]]", " ", Tweets$tweet)  # replace punctuation with space
Tweets$tweet <- gsub("[[:cntrl:]]", " ", Tweets$tweet)  # replace control characters with space
Tweets$tweet <- gsub("^[[:space:]]+", "", Tweets$tweet) # remove whitespace at beginning of documents
Tweets$tweet <- gsub("[[:space:]]+$", "", Tweets$tweet) # remove whitespace at end of documents
Tweets$tweet <- tolower(Tweets$tweet)  # force to lowercase

# tokenize on space and output as a list:
doc.list <- strsplit(Tweets$tweet, "[[:space:]]+")

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

# remove terms that are stop words or occur fewer than 5 times:
del <- names(term.table) %in% stop_words | term.table < 10
term.table <- term.table[!del]
vocab <- names(term.table)

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)

# Compute some statistics related to the data set:
D <- length(documents)  # number of documents (2,000)
W <- length(vocab)  # number of terms in the vocab (14,568)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
N <- sum(doc.length)  # total number of tokens in the data (546,827)
term.frequency <- as.integer(term.table) 

# MCMC and model tuning parameters:
K <- 3
G <- 5000
alpha <- 0.02
eta <- 0.02

# Fit the model:
library(lda)
set.seed(357)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1 

theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

CovidTweets <- list(phi = phi,
                     theta = theta,
                     doc.length = doc.length,
                     vocab = vocab,
                     term.frequency = term.frequency)


# create the JSON object to feed the visualization:
json <- createJSON(phi = CovidTweets$phi, 
                   theta = CovidTweets$theta, 
                   doc.length = CovidTweets$doc.length, 
                   vocab = CovidTweets$vocab, 
                   term.frequency = CovidTweets$term.frequency)

serVis(json, out.dir = 'vis', open.browser = TRUE)

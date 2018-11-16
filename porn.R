# init
#install.packages("tm")
#install.packages("plyr")
#install.packages("e1071")

libs <- c("tm", "plyr", "class")
lapply(libs, require, character.only=TRUE)

#set options
options(stringSASFactors = FALSE)

# set paramters
target <- c("porn", "wikipedia")
pathname <- "C:/Users/user/Documents/DC/paper_content _filtering/project/data"


# clean text
cleanCorpus<- function(corpus) {
  corpus.tmp <- tm_map(corpus, PlainTextDocument) 
  corpus.tmp <- tm_map(corpus.tmp, content_transformer(tolower))
  corpus.tmp <- tm_map(corpus.tmp, removeNumbers)
  corpus.tmp <- tm_map(corpus.tmp, removePunctuation)
  #corpus.tmp <- tm_map(corpus.tmp, stemDocument, language = "english")
  #myStopwords <- c(stopwords('english'), "will", "just", "get", "make", "can", "must", "every", "#", "@")
  #corpus.tmp <- tm_map(corpus.tmp, removeWords, myStopwords)
  corpus.tmp <- tm_map(corpus.tmp, removeWords, stopwords('english'))
  corpus.tmp <- tm_map(corpus.tmp, stripWhitespace)
   
  return(corpus.tmp)
 
}

# build TDM
generateTDM <- function(cand, path) {
  s.dir <- sprintf("%s/%s", path, cand)
  s.cor <- Corpus(DirSource(directory = s.dir, encoding = "UTF-8"))
  s.cor.cl <- cleanCorpus(s.cor)
  #s.tdm <- TermDocumentMatrix(s.cor.cl)
  s.tdm <- TermDocumentMatrix(s.cor.cl, control = list(weighting =function(x) weightTfIdf(x, normalize =FALSE),stopwords = TRUE))
  s.tdm <- removeSparseTerms(s.tdm, 0.7) 
  result <- list(name = cand, tdm = s.tdm) ##note the return format
}
tdm <- lapply(target, generateTDM, path= pathname) ## to apply a function to each element in a vec or list and return a list
#str(tdm)


# attach name
bindCandidateToTDM <- function(tdm){
  s.mat <- t(data.matrix(tdm[["tdm"]]))
  s.df <- as.data.frame(s.mat, stringsAsFactors = FALSE)
  s.df <- cbind(s.df, rep(tdm[["name"]], nrow(s.df)))
  colnames(s.df)[ncol(s.df)] <- "target"
  return(s.df)   
}
candTDM <- lapply(tdm, bindCandidateToTDM)
str(candTDM)


# stack
tdm.stack <- do.call(rbind.fill, candTDM) ## rbind.fill : missing value? NA? ??
tdm.stack[is.na(tdm.stack)] <- 0          ## NA?? 0?? ??
head(tdm.stack)
nrow(tdm.stack)
ncol(tdm.stack)


# k-fold cross validation
#install.packages("carpet")

# Define train control for k fold cross validation
train_control <- trainControl(method="cv", number=10, repeats = 10)
# Fit Naive Bayes Model
model <- train(target~., data=tdm.stack, trControl=train_control, method="nb")
# summarize results
print(model)


# hold-out
##train.idx <- sample(nrow(tdm.stack), ceiling(nrow(tdm.stack) * 0.55)) 
##test.idx <- (1:nrow(tdm.stack)) [- train.idx]

# model - knn
##tdm.cand <- tdm.stack[, "target"]
##tdm.stack.n1 <- tdm.stack[, !colnames(tdm.stack) %in% "target"]


#knn.pred <- knn(tdm.stack.n1[train.idx, ], tdm.stack.n1[test.idx, ], tdm.cand[train.idx])
#conf.mat <- table("Predictions" = knn.pred, Actual = tdm.cand[test.idx])
#(accuracy <- sum(diag(conf.mat) / length(test.idx) * 100)) ## ???? ?? ??
#conf.mat


# model - Fitting Naive Bayes to the Training set
#tdm.stack.n1[train.idx, ] = scale(tdm.stack.n1[train.idx, ])
#tdm.stack.n1[test.idx, ] = scale(tdm.stack.n1[test.idx, ])

#library(e1071)
#classifier = naiveBayes(x = tdm.stack.n1[train.idx, ],
#                       y = tdm.cand[train.idx])

# Predicting the Test set results
#y_pred = predict(classifier, newdata = tdm.stack.n1[test.idx, ])
# Making the Confusion Matrix
#conf.mat = table("Predictions" = y_pred, "Actual" = tdm.cand[test.idx])
#(accuracy <- sum(diag(conf.mat) / length(test.idx) * 100)) ## ???? ?? ??
#conf.mat


# model - Fitting Support Vector Machine to the Training set

##library(e1071)
##classifier = svm(x = tdm.stack.n1[train.idx, ],
##                        y = tdm.cand[train.idx])

# Predicting the Test set results
##y_pred = predict(classifier, newdata = tdm.stack.n1[test.idx, ])
# Making the Confusion Matrix
##conf.mat = table("Predictions" = y_pred, "Actual" = tdm.cand[test.idx])
##(accuracy <- sum(diag(conf.mat) / length(test.idx) * 100)) ## ???? ?? ??
##conf.mat

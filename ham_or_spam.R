ham_or_spam <- read.csv("E:/DATA MINING/sms_spam.csv")
ham_or_spam <- as.data.frame(ham_or_spam)
View(ham_or_spam)
ham_or_spam$type <- factor(ham_or_spam$type)
str(ham_or_spam$type)
ham_or_spam$text<- as.character(ham_or_spam$text)
complete.cases(ham_or_spam)
which(!complete.cases((ham_or_spam)))
table(ham_or_spam$type)
textlength <- nchar(ham_or_spam$text)
par(mar = rep(2, 4))
hist(textlength)
library(ggplot2)

ggplot(ham_or_spam,aes(textlength,fill=type)) + geom_histogram(binwidth =3)+facet_wrap(~type)
library(tm)
sms_corpus <- Corpus(VectorSource(ham_or_spam$text))
print(sms_corpus)
inspect(sms_corpus[1:3])
sms_corpus_clean <- tm_map(sms_corpus,tolower)
sms_corpus_clean <- tm_map(sms_corpus_clean,removeNumbers)
sms_corpus_clean <- tm_map(sms_corpus_clean,removePunctuation)
sms_corpus_clean <- tm_map(sms_corpus_clean,removeWords,stopwords)
sms_corpus_clean <- tm_map(sms_corpus_clean,stripWhitespace)
sms_corpus_clean <- tm_map(sms_corpus_clean,stemDocument)
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
sms_dtm2 <- DocumentTermMatrix(sms_corpus,control = list(
  tolower = TRUE,
  removeNumbers=TRUE,
  removePunctuation= TRUE,
  stopwords= TRUE,
  stripWhitespace = TRUE
))
str(sms_dtm2)


# partitioning df,corpus,dtm
ham_or_spam_train <- sample(ham_or_spam[1:4169,]$text)
ham_or_spam_train <- as.vector(ham_or_spam_train)
length(ham_or_spam_train)
str(ham_or_spam_train)
View(ham_or_spam_train)
ham_or_spam_train_labels <- ham_or_spam[1:4169,]$type
ham_or_spam_test_labels <- ham_or_spam[4169:5559,]$type
length(ham_or_spam_train_labels)
View(ham_or_spam_train_labels)
table(ham_or_spam_train_labels)

prop.table(table(ham_or_spam_test_labels))

sms_dtm2_train <- sms_dtm2[1:4169,]
sms_dtm2_test <- sms_dtm2[4169:5559,]
str(sms_dtm2_train)

sms_corpus_clean_train <- sms_corpus_clean[1:4169]
sms_corpus_clean_test <- sms_corpus_clean[4169:5559]
View(sms)

#finding frequent term

frequent_words <- findFreqTerms(sms_dtm2_train,5)
str(frequent_words)
frequent_words[1:10]

sms_freq_words_train <- sms_dtm2_train[,frequent_words]
str(sms_freq_words_train )
sms_freq_words_test <- sms_dtm2_test[,frequent_words]
str(sms_freq_words_train)
#creating yes or no function

yes_or_no <- function(x){
  y<-ifelse(x > 0,1,0)
  y<- factor(y,levels = c(0,1),labels = c("NO","YES"))
  y
}

sms_train <- apply(sms_freq_words_train,2,yes_or_no)
sms_test <- apply(sms_freq_words_test,2,yes_or_no)
str(sms_train)
View(sms_test)
nrow(sms_train)
ncol(sms_test)
str(sms_test)
for(i in 4169:5559)
{
  ham_or_spam$type[i]== "p"
}
View(ham_or_spam)
#naive bayes classifier

library(e1071)
sms_classifier <- naiveBayes(sms_train,ham_or_spam_train_labels,laplace = 1)
class(sms_classifier)
sms_test_pred <- predict(sms_classifier,newdata=sms_test)
sms_test_pred
length(sms_test_pred)
sms_test_pred[1:10]
sms_test_pred[455]
for(i in 1:5559)
{
  if(i<4169){
    ham_or_spam$result[i] <- ham_or_spam$type[i]
  }
    else{
      
  ham_or_spam$result[i]=sms_test_pred[i-4168]
    }
}
for(i in 1:5559)
{
  if(ham_or_spam$result[i]==1)
  {
    ham_or_spam$result[i]<- "ham"
  }
  else{
    ham_or_spam$result[i]<- "spam"
  }
}

View(ham_or_spam)
table(sms_test_pred,ham_or_spam_test_labels)

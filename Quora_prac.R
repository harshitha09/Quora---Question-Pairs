#### quora data
#install.packages("readr")
#install.packages("stringr")
#install.packages("syuzhet")
#install.packages("SnowballC")
#install.packages("NLP")


library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(stringr)
library(tm)
library(syuzhet)
library(SnowballC)

# considering only the 1000 rows
quora_train <- read_csv("E://Machine Learning/My practice/kaggle_quora/train.csv", n_max = 1000)
View(quora_train)

df = data.frame()
df_new = data.frame()

for(i in 1:nrow(quora_train)){
  q1 = Corpus(VectorSource(quora_train$question1[i]))
  inspect(q1)
   
  q1 = tm_map(q1, removePunctuation)
  
  q1 = tm_map(q1, removeNumbers)

  q1 = tm_map(q1, tolower)
  
  q1 = tm_map(q1, stemDocument)
  
  q1 = tm_map(q1, removePunctuation)
  
  q1 = tm_map(q1, stripWhitespace)
  
  q1 = tm_map(q1, removeWords, stopwords("english"))
  
  q1 = tm_map(q1, PlainTextDocument)
  
  doc = TermDocumentMatrix(q1)
  
  a11 = doc$dimnames$Terms
  
  q2 = Corpus(VectorSource(quora_train$question1[i]))
  
  q2 = tm_map(q2, removePunctuation)
  
  q2 = tm_map(q2, removeNumbers)
  
  q2 = tm_map(q2, tolower)
  
  q2 = tm_map(q2, stemDocument)
  
  q2 = tm_map(q2, removePunctuation)
  
  q2 = tm_map(q2, stripWhitespace)
  
  q2 = tm_map(q2, removeWords, stopwords("english"))
  
  q2 = tm_map(q2, PlainTextDocument)
  
  doc2 = TermDocumentMatrix(q2)
  
  b11 = doc2$dimnames$Terms
  
  c11 = a11 %in% b11
  
  same_items = sum(c11)
  
  distinct_items = length(a11) + length(b11)
  
  match_count = (2*same_items)/(distinct_items)
  
  sentiment1 = get_nrc_sentiment(train$question1[i])
  sentiment2 = get_nrc_sentiment(train$question2[i])
  
  sentiment1
  sentiment2
  
  p1 =  sum(sentiment1$positive)
  p2 =  sum(sentiment2$positive)
  
  n1 =  sum(sentiment1$negative)
  n2 =  sum(sentiment2$negative)
  
  df_new = cbind(match_count, p1, p2, n1, n2)
  df = rbind(df, df_new)

}
p1
p2
n1
n2
df

tr = cbind(train, df)
tr = tr[,6:11]

tr[,c(1,3:6)] = lapply(tr[,c(1,3:6)],as.factor)
tr = na.omit(tr)

library(randomForest)
model <- randomForest(is_duplicate ~ ., data = tr)
model

pred <- predict(model, newdata = tr,  type="prob")

#bound the results, otherwise you might get infinity results
pred = apply(pred, c(1,2), function(x) min(max(x, 1E-15), 1-1E-15)) 

logLoss = function(pred, actual){
  -1*mean(log(pred[model.matrix(~ actual + 0) - pred > 0]))
}

### Logloss value = 
logLoss(pred, tr$is_duplicate)









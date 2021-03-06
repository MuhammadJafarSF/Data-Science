#Import package
library(dplyr)
library(tidyverse)
library(tm)
library(e1071)
library(caret)

features_rds_path = "model-dan-dataset/features.rds"
naive_bayes_rda_path = "model-dan-dataset/NBClassifier.rda"

# Membersihkan data dan merubah data menjadi bentuk corpus
clean_data <- function(data) {
  #Mengubah data menjadi factor
  data <- as.factor(data)
  corpus <- Corpus(VectorSource(data))
  corpus_clean <- corpus %>% 
    tm_map(content_transformer(tolower)) %>% #Mengubah menjadi huruf nonkapital
    tm_map(removePunctuation) %>% #Menghapus tanda baca
    tm_map(removeNumbers) %>% #Menghapus angka
    tm_map(removeWords, stopwords(kind = "en")) %>% #Menghapus stopwords
    tm_map(stripWhitespace) #Mengubah blank space menjadi strip
  
  #Mengecek perbedaan corpus yang sudah dibersihkan dengan yang belum
  corpus[[1]]$content
  corpus_clean[[1]]$content
  
  return(corpus_clean)
}

# Menerapkan features dan mengubah data menjadi document term matrix
apply_feature <- function(corpus, features) {
  dtm <- DocumentTermMatrix(corpus, control = list(dictionary = features))
  return(apply(dtm, 2, convert_count))
}

# Mengubah jumlah kemunculan kata menjadi "Yes" dan "No"
convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  return(y)
}

# Traning naive bayes model
train_model <- function() {
  # Membaca training dataset
  file_path <- "dataset/tripadvisor-restauran-traning-dataset.txt"
  data.source <- read_delim(file_path, delim = "\t")
  
  # Menambahkan kolom kelas pada data frame
  data.source$sentiment <-  ifelse(data.source$score > 0, "Positive", "Negative")
  # Mengubah data menjadi factor
  data.source$sentiment <- as.factor(data.source$sentiment)
  
  # Mengacak data agar tidak berurutan
  set.seed(1)
  data.source <- data.source[sample(nrow(data.source)),]
  
  # Pembersihan data
  data.corpus <- clean_data(data.source$review)
  
  # Mengubah data corpus menjadi document term matrix
  data.dtm <- DocumentTermMatrix(data.corpus)
  
  # Rasio perbandingan antara data training dengan data testing
  training_ratio = 0.8
  
  # Memecah data menjadi data training dan data testing
  data.source.total <- nrow(data.source)
  data.source.train <- data.source[1 : round(training_ratio * data.source.total),]
  data.source.test <- data.source[(round(training_ratio * data.source.total) + 1) : data.source.total,]
  
  data.corpus.total <- length(data.corpus)
  data.corpus.train <- data.corpus[1 : round(training_ratio * data.corpus.total)]
  data.corpus.test <- data.corpus[(round(training_ratio * data.corpus.total) + 1) : data.corpus.total]
  
  data.dtm.total <- nrow(data.dtm)
  data.dtm.train <- data.dtm[1 : round(training_ratio * data.dtm.total),]
  data.dtm.test <- data.dtm[(round(training_ratio * data.dtm.total) + 1) : data.dtm.total,]
  
  # Mengambil kata yang sering muncul, minimal 3 kali 
  freq_terms <- findFreqTerms(data.dtm.train, 3)
  length(freq_terms)
  
  #Save features yang sudah dibuat
  saveRDS(freq_terms, file = features_rds_path)
  #save(freq_terms, file = features_rds_path)
  
  #Mengaplikasikan fungsi convert_count untuk mendapatkan hasil training dan testing DTM
  data.dtm.train <- apply_feature(data.corpus.train, freq_terms)
  data.dtm.test <- apply_feature(data.corpus.test, freq_terms)
  
  #Membuat model naive bayes
  model <- naiveBayes(data.dtm.train, data.source.train$sentiment, laplace = 1)
  
  #Save Model yang sudah dibuat agar bisa dipakai di Shiny
  save(model, file = naive_bayes_rda_path)
  
  #Membuat prediksi
  prediction <- predict_sentiment(data.dtm.test)
  
  
  #Mengecek akurasi dari model yang telah dibuat
  result <- confusionMatrix(table(Prediction = prediction, Actual = data.source.test$sentiment))
  result
}

# Prediksi review sentiment
get_prediction <- function(review) {
  features <- readRDS(features_rds_path)
  model <- get(load(naive_bayes_rda_path))
  
  data_corpus <- clean_data(review)
  data_test <- apply_feature(data_corpus, features = features)
  prediction <- predict(model, newdata = data_test) 
  
  return(data.frame(review = review, sentiment = prediction))
}

# Hapus komentar dibawah untuk traning model
# train_model()

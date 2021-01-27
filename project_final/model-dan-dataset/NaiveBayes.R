#Import package
library(dplyr)
library(tidyverse)
library(tm)
library(e1071)
library(caret)

#Import file
df <- read.delim(file.choose(), stringsAsFactors = FALSE)

#Menambahkan kolom kelas pada data frame
df$class <-  ifelse(df$score>0,"Positive", "Negative")
#Mengacak data agar tidak berurutan
set.seed(1)
df <- df[sample(nrow(df)),]

#Mengubah data menjadi factor
df$class <- as.factor(df$class)

#mengubah data menjadi bentuk corpus sekaligus membersihkan data
corpus <- Corpus(VectorSource(df$reviews))
corpus_clean <- corpus %>% 
  tm_map(content_transformer(tolower)) %>% #Mengubah menjadi huruf nonkapital
  tm_map(removePunctuation) %>% #Menghapus tanda baca
  tm_map(removeNumbers) %>% #Menghapus angka
  tm_map(removeWords, stopwords(kind = "en")) %>% #Menghapus stopwords
  tm_map(stripWhitespace) #Mengubah blank space menjadi strip

#Mengecek perbedaan corpus yang sudah dibersihkan dengan yang belum
corpus[[1]]$content
corpus_clean[[1]]$content

#Mengubah menjadi DTM
dtm <- DocumentTermMatrix(corpus_clean)

inspect(dtm[40:50, 10:15])

dim(dtm)

#Memecah data menjadi data training dan data testing
df_train <- df[1:1200,]
df_test <- df[1201:1600,]

corpus_clean_train <- corpus_clean[1:1200]
corpus_clean_test <- corpus_clean[1201:1600]

dtm_train <- dtm[1:1200,]
dtm_test <- dtm[1201:1600,]

#Mengambil kata yang sering muncul, minimal 5 kali 
threeFreq <- findFreqTerms(dtm_train,3)
length(threeFreq)

# set direktori untuk simpan data
setwd("D:/project_final/data")
#Save features yang sudah dibuat
save(fiveFreq, file = 'features.rds')

# Sesuaikan fitur pada data train dan test dengan fitur yang sudah diseleksi sebelumnya
dtm_train_nb <- DocumentTermMatrix(corpus_clean_train, control = list(dictionary = fiveFreq))
dtm_test_nb <- DocumentTermMatrix(corpus_clean_test, control = list(dictionary = fiveFreq))

#Mengubah jumlah kemunculan kata menjadi "Yes" dan "No"
convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

#Mengaplikasikan fungsi convert_count untuk mendapatkan hasil training dan testing DTM
trainNB <- apply(dtm_train_nb, 2, convert_count)
testNB <- apply(dtm_test_nb, 2, convert_count)

view(testNB)

#Membuat model naive bayes
modelNB <- naiveBayes(trainNB, df_train$class, laplace = 1)

# set direktori untuk simpan data
setwd("D:/project_final/model-dan-dataset")
#Save Model yang sudah dibuat agar bisa dipakai di Shiny
save(modelNB, file = 'NBClassifier.rda')

#Membuat prediksi
pred <- predict(modelNB, newdata=testNB)

table("Predictions"= pred,  "Actual" = df_test$class)

#Mengecek akurasi dari model yang telah dibuat
conf_mat <- confusionMatrix(pred, df_test$class)
conf_mat$overall['Accuracy']

hasil <- confusionMatrix(table(pred, df_test$class))
hasil

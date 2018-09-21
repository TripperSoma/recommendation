plz install this packages necessary
made by juhwan
#install.packages("readr")
#install.packages("tidyverse")
#install.packages("stringr")
#install.packages("stringi")
#install.packages('recommenderlab')
library(recommenderlab)
library(tidyverse)
library(stringr)

getwd()
rating_df <- read_csv("/Users/jhmon/Downloads/juhwan/ratings.csv")
rating_df <- rating_df %>% select(-timestamp) %>% 
  mutate(userId = str_c("u", userId),
         movieId = str_c("m", movieId))


# dataframe -> Matrix -> realRatingMatrix 
rating_mat <- spread(rating_df, movieId, rating) %>% 
  remove_rownames() %>% 
  column_to_rownames(var="userId")

rating_rrm <- as(as(rating_mat, "matrix"), "realRatingMatrix")

# Reduce to meaningful data

rating_rrm <- rating_rrm[rowCounts(rating_rrm) > 50,
                         colCounts(rating_rrm) > 100]

# data size
print(object.size(rating_mat), units = "auto")

print(object.size(rating_rrm), units = "auto")

# 3. recommendation model
## 3.1. model evaluation 
#cross validation
rating_eval <- evaluationScheme(rating_rrm, method="split", train=0.7, given=2)

## 3.2. CF model
ubcf_rmse <- Recommender(getData(rating_eval, "train"), method = "UBCF", 
                         param=list(normalize = "center", method="Cosine", nn=5))

## 3.3. 
ubcf_pred <- predict(ubcf_rmse, getData(rating_eval, "known"), type="ratings")
calcPredictionAccuracy(ubcf_pred, getData(rating_eval, "unknown"))

# 4. recommendation
ubcf_pred <- predict(object = ubcf_rmse, newdata = rating_rrm,  n = 5)

recc_matrix <- sapply(ubcf_pred@items, function(x){
  colnames(rating_rrm)[x]
})
recc_matrix

#recc_matrix[,1:2] %>% DT::datatable()

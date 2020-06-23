if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(skimr)) install.packages("skimr", repos = "http://cran.us.r-project.org")
if(!require(tidymodels)) install.packages("tidymodels", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(ranger)) install.packages("ranger", repos = "http://cran.us.r-project.org")
if(!require(corrr)) install.packages("corrr", repos = "http://cran.us.r-project.org")
if(!require(httr)) install.packages("httr", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(caret)
library(data.table)
library(skimr)
library(tidymodels)
library(rpart)
library(e1071)
library(ranger)
library(corrr)
library(httr)
download.file(url = "https://github.com/OmarRam37/EDX-Choose-Your-Own/archive/master.zip",destfile = "EDXCYO.zip")
unzip(zipfile = "EDXCYO.zip")
setwd("EDX-Choose-Your-Own-master/data-files")
file_list <- list.files()
for (file in file_list){
  
# if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read_csv(file,
                        col_types = cols(X1 = col_integer(),
                                         amount = col_double(),
                                         isFlaggedFraud = col_character(),
                                         isFraud = col_character(),
                                         nameDest = col_character(),
                                         nameOrig = col_character(),
                                         newbalanceDest = col_double(),
                                         newbalanceOrig = col_double(),
                                         oldbalanceDest = col_double(),
                                         oldbalanceOrg = col_double(),
                                         step = col_integer(),
                                         type = col_character()))
  }
  
  # if the merged dataset does exist, append to it
  else{
    temp_dataset <- read_csv(file,
                             col_types = cols(X1 = col_integer(),
                                              amount = col_double(),
                                              isFlaggedFraud = col_character(),
                                              isFraud = col_character(),
                                              nameDest = col_character(),
                                              nameOrig = col_character(),
                                              newbalanceDest = col_double(),
                                              newbalanceOrig = col_double(),
                                              oldbalanceDest = col_double(),
                                              oldbalanceOrg = col_double(),
                                              step = col_integer(),
                                              type = col_character()))
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
  
}
rm(file, file_list)
setwd("..")
dataset <- dataset %>% mutate( combination = as.factor( paste(str_sub(nameOrig,end = 1),str_sub(nameDest,end = 1), sep = "")), isFraud = as.factor(isFraud), isFlaggedFraud = as.factor(isFlaggedFraud), type = as.factor(type))
head(dataset)
dataset[1,]
dataset %>% group_by(combination, isFraud) %>% summarize(count = n())
dataset_c <- dataset %>% filter(combination == "CC") %>% select(-combination,-X1,-step, -nameDest, -nameOrig)
rm(dataset)
nrow(dataset_c)
ncol(dataset_c)
skim(dataset_c)
dataset_c %>% group_by(isFlaggedFraud, isFraud) %>% summarize(count = n())
dataset_c %>% group_by(type) %>% summarize(total = sum(amount), count = n())
dataset_c %>% ggplot(aes(amount)) + geom_histogram()
dataset_c %>% select(amount, newbalanceDest, newbalanceOrig, oldbalanceDest, oldbalanceOrg) %>% correlate() %>% rplot(shape = 15, colors = c("red","green"))

set.seed(37)
data_split <- initial_split(dataset_c)
data_train <- training(data_split)
data_test <- testing(data_split)
data_rec <- recipe(isFraud ~ ., data = data_train) %>%
  step_corr(all_numeric()) %>%
  step_zv(all_numeric()) %>%
  step_dummy(all_nominal(),-isFraud,-isFlaggedFraud)
data_prep <- data_rec %>% prep()
data_juice <- juice(data_prep)
head(data_juice)
test_proc <- bake(data_prep, new_data = data_test)
control <- trainControl(method = "cv", number = 10, p = 0.9)
rm(dataset_c,data_train,data_test,data_split,data_rec,data_prep)
train_glm <- train(isFraud ~ .,
                   method = "glm",
                   data = data_juice,
                   trControl = control)
pred_glm <- predict(train_glm, test_proc)
mat_glm <- confusionMatrix(data = pred_glm , reference = test_proc$isFraud)
result <- tibble(method="glm",
                 sensitivity = sensitivity(mat_glm$table) %>% pull(.estimate) %>% round(5) %>% format(nsmall = 5),
                 specificity = specificity(mat_glm$table) %>% pull(.estimate) %>% round(5) %>% format(nsmall = 5),
                 overall = accuracy(mat_glm$table) %>% pull(.estimate) %>% round(5) %>% format(nsmall = 5))
result
rm(mat_glm, train_glm, pred_glm)
gc()
train_rpart <- train(isFraud ~ .,
                     method = "rpart",
                     data = data_juice,
                     tuneGrid = data.frame(cp = seq(0,1,0.25)),
                     trControl = control)
pred_rpart <- predict(train_rpart, test_proc)
mat_rpart <- confusionMatrix(data = pred_rpart , reference = test_proc$isFraud)
plot(train_rpart)
plot(train_rpart$finalModel, margin = 0.1)
result <- bind_rows(result, tibble(method="rpart",
                                   sensitivity = sensitivity(mat_rpart$table) %>% pull(.estimate) %>% round(5) %>% format(nsmall = 5),
                                   specificity = specificity(mat_rpart$table) %>% pull(.estimate) %>% round(5) %>% format(nsmall = 5),
                                   overall = accuracy(mat_rpart$table) %>% pull(.estimate) %>% round(5) %>% format(nsmall = 5)))
result
rm(mat_rpart, train_rpart, pred_rpart)
gc()
train_ranger<- train(isFraud ~ .,
                     method = "ranger",
                     data = data_juice,
                     tuneGrid = data.frame(mtry = 8,
                                           splitrule = "gini",
                                           min.node.size = 1
                     ),
                     trControl = control,
                     num.trees = 100)
pred_ranger <- predict(train_ranger, test_proc)
mat_ranger <- confusionMatrix(data = pred_ranger , reference = test_proc$isFraud)
plot(train_ranger)
result <- bind_rows(result, tibble(method="ranger",
                                   sensitivity = sensitivity(mat_rf$table) %>% pull(.estimate) %>% round(5) %>% format(nsmall = 5),
                                   specificity = specificity(mat_rf$table) %>% pull(.estimate) %>% round(5) %>% format(nsmall = 5),
                                   overall = accuracy(mat_rf$table) %>% pull(.estimate) %>% round(5) %>% format(nsmall = 5)))
result


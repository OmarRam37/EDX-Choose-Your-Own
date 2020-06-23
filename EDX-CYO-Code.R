##Download and use the libraries that the code needs
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
##Download dataset from github repository as a zip file
download.file(url = "https://github.com/OmarRam37/EDX-Choose-Your-Own/archive/master.zip",destfile = "EDXCYO.zip")
##Unzip Github repository
unzip(zipfile = "EDXCYO.zip")
##Change working directory to acces files of dataset. Dataset was divided into 30 files because of github space constraints
setwd("EDX-Choose-Your-Own-master/data-files")
##Get a lis of the files to merge in a loop
file_list <- list.files()
##Loop of the file list to merge them on a singel csv file
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
##With the dataset created we can procede to delete the file and file_list
rm(file, file_list)
##Change working directory out of the file list
setwd("..")
##Mutate a dataset to have a coumn that identifies if the trnasaction involved a merchant client
dataset <- dataset %>% mutate( combination = as.factor( paste(str_sub(nameOrig,end = 1),str_sub(nameDest,end = 1), sep = "")), isFraud = as.factor(isFraud), isFlaggedFraud = as.factor(isFlaggedFraud), type = as.factor(type))
##Quick look at the dataset
head(dataset)
##Quick look at just the first line as an example of the structure
dataset[1,]
##Get a summary of the transactions that are flaged as fraud
dataset %>% group_by(combination, isFraud) %>% summarize(count = n())
##Filter out of the dataset the transactions that involve a merchant and selecting the columns that are relevant
dataset_c <- dataset %>% filter(combination == "CC") %>% select(-combination,-X1,-step, -nameDest, -nameOrig)
##Remove the dataset table to begin using the celan dataset
rm(dataset)
##Count rows in clean dataset
nrow(dataset_c)
##Count columns in clean dataset
ncol(dataset_c)
##Make a quick skim of the data. Skim gives you a quick summary in general. It measures diffrent thing depending on the type of variable.
skim(dataset_c)
##Indentify how many frauds are flagged as fraud by the system
dataset_c %>% group_by(isFlaggedFraud, isFraud) %>% summarize(count = n())
##Summarize the amount of money and the total of transactions by each transaction type
dataset_c %>% group_by(type) %>% summarize(total = sum(amount), count = n())
##Generate histogram of the amount (not very helpful)
dataset_c %>% ggplot(aes(amount)) + geom_histogram()
##Graph the correlation between the variables that are numeric
dataset_c %>% select(amount, newbalanceDest, newbalanceOrig, oldbalanceDest, oldbalanceOrg) %>% correlate() %>% rplot(shape = 15, colors = c("red","green"))

##Start of the models
##Set seed to split data
set.seed(37)
##Make the intial split and save the train a test sets on different variables
data_split <- initial_split(dataset_c)
data_train <- training(data_split)
data_test <- testing(data_split)
##Generate a recipe (a set of steps) to get the data a little more clean and to eliminate correlated variables
data_rec <- recipe(isFraud ~ ., data = data_train) %>%
  step_corr(all_numeric()) %>% #Eliminate correlated variables
  step_zv(all_numeric()) %>% #Eliminate variables with zero variance
  step_dummy(all_nominal(),-isFraud,-isFlaggedFraud) #Unpivot the columns tht are factors except the ones that are already binary
##Return the object of the applied train data with the recipe
data_prep <- data_rec %>% prep()
##Get just the data out of the object
data_juice <- juice(data_prep)
##A quick lok at the data after the recipe
head(data_juice)
##Apply the recipe to the test set
test_proc <- bake(data_prep, new_data = data_test)
##Deffine the controls for the validation in the models
control <- trainControl(method = "cv", number = 10, p = 0.9)
##Remove unused tables and objects
rm(dataset_c,data_train,data_test,data_split,data_rec,data_prep)
##Train the glm model
train_glm <- train(isFraud ~ .,
                   method = "glm",
                   data = data_juice,
                   trControl = control)
##Predict values on the test set, based on the trained model
pred_glm <- predict(train_glm, test_proc)
##Save the confusion matrix of the prediction
mat_glm <- confusionMatrix(data = pred_glm , reference = test_proc$isFraud)
##Save the result of sensitivity, specificity and overall accuracy into a table
result <- tibble(method="glm",
                 sensitivity = sensitivity(mat_glm$table) %>% pull(.estimate) %>% round(5) %>% format(nsmall = 5),
                 specificity = specificity(mat_glm$table) %>% pull(.estimate) %>% round(5) %>% format(nsmall = 5),
                 overall = accuracy(mat_glm$table) %>% pull(.estimate) %>% round(5) %>% format(nsmall = 5))
##Return the values on the table
result
##Clean the working area
rm(mat_glm, train_glm, pred_glm)
gc()
##Train the rpart model
train_rpart <- train(isFraud ~ .,
                     method = "rpart",
                     data = data_juice,
                     tuneGrid = data.frame(cp = seq(0,1,0.25)),
                     trControl = control)
##Predict values on the test set, based on the trained model
pred_rpart <- predict(train_rpart, test_proc)
##Save the confusion matrix of the prediction
mat_rpart <- confusionMatrix(data = pred_rpart , reference = test_proc$isFraud)
##Plot the result based on the tuning values
plot(train_rpart)
##Plot the tree that gets generated from the model
plot(train_rpart$finalModel, margin = 0.1)
##Save the result of sensitivity, specificity and overall accuracy into a table
result <- bind_rows(result, tibble(method="rpart",
                                   sensitivity = sensitivity(mat_rpart$table) %>% pull(.estimate) %>% round(5) %>% format(nsmall = 5),
                                   specificity = specificity(mat_rpart$table) %>% pull(.estimate) %>% round(5) %>% format(nsmall = 5),
                                   overall = accuracy(mat_rpart$table) %>% pull(.estimate) %>% round(5) %>% format(nsmall = 5)))
##Return the values on the table
result
##Clean the working area
rm(mat_rpart, train_rpart, pred_rpart)
gc()
##Train the ranger model with tuning parameters set as static variables and with a small number of trees
train_ranger<- train(isFraud ~ .,
                     method = "ranger",
                     data = data_juice,
                     tuneGrid = data.frame(mtry = 8,
                                           splitrule = "gini",
                                           min.node.size = 1
                     ),
                     trControl = control,
                     num.trees = 100)
##Predict values on the test set, based on the trained model
pred_ranger <- predict(train_ranger, test_proc)
##Save the confusion matrix of the prediction
mat_ranger <- confusionMatrix(data = pred_ranger , reference = test_proc$isFraud)
##Save the result of sensitivity, specificity and overall accuracy into a table
result <- bind_rows(result, tibble(method="ranger",
                                   sensitivity = sensitivity(mat_rf$table) %>% pull(.estimate) %>% round(5) %>% format(nsmall = 5),
                                   specificity = specificity(mat_rf$table) %>% pull(.estimate) %>% round(5) %>% format(nsmall = 5),
                                   overall = accuracy(mat_rf$table) %>% pull(.estimate) %>% round(5) %>% format(nsmall = 5)))
result


library(readr)
library(corrr)

setwd(dir = "data-files/")
file_list <- list.files()
for (file in file_list){
  
# if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read_csv(file,
                        col_types = cols(X1 = col_integer(),
                                         amount = col_double(),
                                         isFlaggedFraud = col_factor(),
                                         isFraud = col_factor(),
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
                                              isFlaggedFraud = col_factor(),
                                              isFraud = col_factor(),
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
dataset <- dataset %>% mutate( combination = as.factor( paste(str_sub(nameOrig,end = 1),str_sub(nameDest,end = 1), sep = "")))
dataset %>% group_by(combination, isFraud) %>% summarize(count = n())
dataset_c <- dataset %>% filter(combination == "CC") %>% select(-combination,-X1)
rm(dataset)


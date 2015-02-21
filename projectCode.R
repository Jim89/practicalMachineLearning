################################################################################
# set up
################################################################################
# clear out environment
  rm(list = ls())

# load packages
  library(dplyr, quietly = T)
  library(ggplot2, quietly = T)
  library(caret, quietly = T)
  library(randomForest, quietly = T)


################################################################################
# get the data
################################################################################
# set destinations
  trainURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
  testURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

# make a folder to store data
  if(!file.exists("./data")){
    dir.create("./data")
  }
# train
  if(!file.exists("./data/train.csv")){
    download.file(trainURL, "./data/train.csv")
  }
# test
  if(!file.exists("./data/test.csv")){
    download.file(testURL, "./data/test.csv")
  }

  rm(testURL)
  rm(trainURL)

# read training data (save test for when we actually need it!)
  data <- read.csv("./data/train.csv", header = T, na.strings = c("#DIV/0!", "NA"))

################################################################################
# explore the data
################################################################################
# quick overview
  str(data) # first few (7) fields look like descriptions, let's ignore them
  summary(data) # looks like LOTS of missing data in multiple fields

# remove the junk fields
  data <- data[,-c(1:7)]
  data <- data[,colSums(is.na(data)) < (0.3*nrow(data))]

# check the count of missing values per field
  missing <- sapply(1:(ncol(data)-1), function(x) data[,x] %>% is.na() %>% sum())


# check what sort of data we're working with - expecting mainly numeric
  classes <- sapply(1:(ncol(data)-1), function(x) data[,x] %>% class()) # lots of factors


################################################################################
# fit a model
################################################################################
index <- createDataPartition(data$classe, p = 0.8, list = F)
train <- data[index,]
test <- data[-index,]
rf <- randomForest(classe ~ ., data = train, ntree = 100); rf

names(rf)

testOutcome <- predict(rf, test)
confusionMatrix(testOutcome, test$classe)



pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}



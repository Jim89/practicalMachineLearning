################################################################################
# set up
################################################################################
#dir <- "/media/storage/Documents/R/practicalMachineLearning/project"

# clear out environment
  rm(list = ls())

# load packages
  library(dplyr, quietly = T)
  library(ggplot2, quietly = T)
  library(caret, quietly = T)
  library(randomForest, quietly = T)
  library(rattle, quietly = T)

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
    download.file(trainURL, "./data/train.csv", method = "curl")
  }
# test
  if(!file.exists("./data/test.csv")){
    download.file(testURL, "./data/test.csv", method = "curl")
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
  data <- data[,colSums(is.na(data)) < 0.3*nrow(data)]

# check the count of missing values per field
  missing <- sapply(1:(ncol(data)-1), function(x) data[,x] %>% is.na() %>% sum())
# none! 

# check what sort of data we're working with - expecting mainly numeric
  classes <- sapply(1:(ncol(data)-1), function(x) data[,x] %>% class()) # all numeric/int


################################################################################
# fit a model
################################################################################
# partition the data into training and test
  index <- createDataPartition(data$classe, p = 0.8, list = F)
  train <- data[index,]
  test <- data[-index,]

# fit the model - try random forest first as the data are not large and it's a robust method
ptm <- proc.time()
rf <- randomForest(classe ~ ., data = train, ntree = 201, importance = T)
proc.time() - ptm

testOutcome <- predict(rf, test)
confMatrix <- confusionMatrix(testOutcome, test$classe)

# looks like the model is highly accurate, even on the held out test set. Apply to real validation set

################################################################################
# apply to validation set
################################################################################

validation <- read.csv("./data/test.csv", header = T, na.strings = c("#DIV/0!", "NA"))

predictions <- predict(rf, validation) %>% as.character()

if(!file.exists("./outputs")){dir.create("./outputs")}

pml_write_files <- function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("./outputs/problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predictions)

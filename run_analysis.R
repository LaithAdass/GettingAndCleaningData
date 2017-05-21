library(plyr)
##the following run.analysis function will work through the full script to:
##1.data download
##2. merging training and test datasets & returning merged dataframes
##3. exctarcting the mean and standard deviation of each measurment
##4. naming activities into decriptive names 
##5. descriptive column name for subjects
##6. conbining dataframes into one
##7. creating a tidy dataset
##8. writing the tidy dataset into a csv

run.analysis = function() {
  ##1.data download
  download.data()
  ##2. merging training and test datasets & returning merged dataframes
  merged <- merge.datasets()
  ##3. exctarcting the mean and standard deviation of each measurment
  cx <- extract.mean.and.std(merged$x)
  ##4. naming activities into decriptive names 
  cy <- name.activities(merged$y)
  ##5. descriptive column name for subjects
  colnames(merged$subject) <- c("subject")
  ##6. conbining dataframes into one
  combined <- bind.data(cx, cy, merged$subject)
  ##7. creating a tidy dataset
  tidy <- create.tidy.dataset(combined)
  ##8. writing the tidy dataset into a csv
  write.csv(tidy, "UCI_HAR_tidy.csv", row.names=FALSE)
  }

download.data = function() {
  ##the following function creates a folder GettingAndCleaningData after checking if it exists of not
  if (!file.exists("GettingAndCleaningData")) {
    message("Creating data directory")
    dir.create("GettingAndCleaningData")
  }
  ##the following function downloads the data into GettingAndCleaningData and unzippes the data into folder UCI HAR Dataset
  if (!file.exists("GettingAndCleaningData/UCI HAR Dataset")) {
    fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    message("Downloading data")
    download.file(fileURL, destfile="GettingAndCleaningData/UCI_HAR_data.zip", method="libcurl")
    unzip("./GettingAndCleaningData/UCI_HAR_data.zip", exdir="./GettingAndCleaningData")
  }
  list.files(path="GettingAndCleaningData")
  list.files(path = "GettingAndCleaningData/UCI HAR Dataset")
}

merge.datasets = function() {
  "Merge training and test datasets"
  ##the folloiwng functions read and stors data into variables for processing 
  message("reading X_train.txt")
  training.x <- read.table("GettingAndCleaningData/UCI HAR Dataset/train/X_train.txt")
  message("reading y_train.txt")
  training.y <- read.table("GettingAndCleaningData/UCI HAR Dataset/train/y_train.txt")
  message("reading subject_train.txt")
  training.subject <- read.table("GettingAndCleaningData/UCI HAR Dataset/train/subject_train.txt")
  message("reading X_test.txt")
  test.x <- read.table("GettingAndCleaningData/UCI HAR Dataset/test/X_test.txt")
  message("reading y_test.txt")
  test.y <- read.table("GettingAndCleaningData/UCI HAR Dataset/test/y_test.txt")
  message("reading subject_test.txt")
  test.subject <- read.table("GettingAndCleaningData/UCI HAR Dataset/test/subject_test.txt")
  ##the following function merges the X,Y and subject of both the training and test datasets 
  merged.x <- rbind(training.x, test.x)
  merged.y <- rbind(training.y, test.y)
  merged.subject <- rbind(training.subject, test.subject)
  ## following funtion returns the merger 
  list(x=merged.x, y=merged.y, subject=merged.subject)
}

extract.mean.and.std = function(df) {
  ##this fucntions reads the featres table and returns the mean and standard deviation of measurment when given a the dataset (x values)
  
  features <- read.table("GettingAndCleaningData/UCI HAR Dataset/features.txt")
  mean.col <- sapply(features[,2], function(x) grepl("mean()", x, fixed=T))
  std.col <- sapply(features[,2], function(x) grepl("std()", x, fixed=T))
  edf <- df[, (mean.col | std.col)]
  colnames(edf) <- features[(mean.col | std.col), 2]
  edf
}

name.activities = function(df) {
  ##function to name each activity in the dataset a descreptive name
  colnames(df) <- "activity"
  df$activity[df$activity == 1] = "WALKING"
  df$activity[df$activity == 2] = "WALKING_UPSTAIRS"
  df$activity[df$activity == 3] = "WALKING_DOWNSTAIRS"
  df$activity[df$activity == 4] = "SITTING"
  df$activity[df$activity == 5] = "STANDING"
  df$activity[df$activity == 6] = "LAYING"
  df
}

bind.data <- function(x, y, subjects) {
  ##function to combine mean-std values (x), activities (y) and subjects into one dataframe
  cbind(x, y, subjects)
}

create.tidy.dataset = function(df) {
  ##this fucntion takes the x,y and subjects and creates a seperate dataset with the avg of each variable for each activity and subject
  tidy <- ddply(df, .(subject, activity), function(x) colMeans(x[,1:60]))
  tidy
}

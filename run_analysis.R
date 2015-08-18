## run_analysis.R
# The purpose of this project is to demonstrate your ability to collect, work with,
# and clean a data set. The goal is to prepare tidy data that can be used for later
# analysis. You will be graded by your peers on a series of yes/no questions related
# to the project. You will be required to submit: 1) a tidy data set as described 
# below, 2) a link to a Github repository with your script for performing the
# analysis, and 3) a code book that describes the variables, the data, and any
# transformations or work that you performed to clean up the data called
# CodeBook.md. You should also include a README.md in the repo with your scripts.
# This repo explains how all of the scripts work and how they are connected.  
# 
# One of the most exciting areas in all of data science right now is wearable 
# computing - see for example this article . Companies like Fitbit, Nike, and 
# Jawbone Up are racing to develop the most advanced algorithms to attract new 
# users. The data linked to from the course website represent data collected from 
# the accelerometers from the Samsung Galaxy S smartphone. A full description is 
# available at the site where the data was obtained: 
#   
#   http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
# 
# Here are the data for the project: 
#   
#   https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 
# You should create one R script called run_analysis.R that does the following. 
# 1) Merges the training and the test sets to create one data set.
# 2) Extracts only the measurements on the mean and standard deviation for each 
#    measurement.
# 3) Uses descriptive activity names to name the activities in the data set
# 4) Appropriately labels the data set with descriptive variable names. 
# 5) From the data set in step 4, creates a second, independent tidy data set
#    with the average of each variable for each activity and each subject.
library(plyr)
library(dplyr)

# getSerData funciton
# This funciton parses the data from the specified set (test or train) and returns
# a tidy data set.
getSetData <- function(set)
{
  # Extract Subject Number
  subject.number  <- read.delim(file.path(BASE_DIR,set,paste0('subject_',set,'.txt')),
                                sep=' ',
                                header=FALSE,
                                col.names='subject')
  
  # Extract Activity number, convert to labeled levels using activity labels to
  # use descriptive activity names to name the activities in the data set
  activity <- read.delim(file.path(BASE_DIR,set,paste0('y_',set,'.txt')),
                         sep=' ',
                         header=FALSE,
                         col.names='activity')
  activity$activity <- as.factor(activity$activity)
  levels(activity$activity) <- activity.labels$name
  
  # Extract Features
  features <- read.delim(file.path(BASE_DIR,set,paste0('X_',set,'.txt')),
                         sep='',
                         header=FALSE,
                         col.names=feature.labels$name)
  
  # Subset to exclude all but mean and standard deviaiton measurements
  features <- select(features, mean.std.feature.labels)
  
  # Add in set variable to differentiate sets
  features$set <- rep_len(set, nrow(features))
  
  # Add in activity variable
  features$activity <- activity$activity
  
  # Add in subject variable
  features$subject <- subject.number$subject
  
  return(features)
}

# Set the directories
BASE_DIR  <- './UCI HAR Dataset'

# Extract the activity labels
activity.labels <- read.delim(file.path(BASE_DIR,'activity_labels.txt'),
                              sep=' ',
                              header=FALSE,
                              col.names=c('id','name'))

# Extract the feature labels
feature.labels  <- read.delim(file.path(BASE_DIR,'features.txt'),
                              sep=' ',
                              header=FALSE,
                              col.names=c('id','name'))

# Split up the labels to make the function and parameter apparent
split.feature.labels <- do.call(rbind.data.frame,
                                strsplit(as.character(feature.labels$name),split="-"))
names(split.feature.labels) <- c('feature','function.applied','parameter')
feature.labels  <- cbind(feature.labels, split.feature.labels)

# Format the label names to make them more understandable to
# appropriately label the data set with descriptive variable names
feature.labels$name <- gsub("-",".",feature.labels$name)  # Replace dashes w/dots
feature.labels$name <- gsub("\\(","",feature.labels$name) # Remove open parens
feature.labels$name <- gsub("\\)","",feature.labels$name) # Remove close parens

# Create a list of label indexes containing all means and standard deviations to
# extract only the measurements on the mean and standard deviation for each measurement
mean.std.feature.labels  <- feature.labels$id[(feature.labels$function.applied == 'mean()' |
                                                feature.labels$function.applied == 'std()')]

# Extract Test and Training set data
testFeatures  <- getSetData('test')
trainFeatures <- getSetData('train')

# Merge the training and the test sets to create one data set
data <- rbind(testFeatures, trainFeatures)

# Create a second, independent tidy data set with the average of each variable
# for each activity and each subject.
dataMeans <- ddply(out, .(activity, subject), numcolwise(mean))

# Save data in CSV format
write.csv(data, "data.csv")
write.csv(dataMeans, "dataMeans.csv")


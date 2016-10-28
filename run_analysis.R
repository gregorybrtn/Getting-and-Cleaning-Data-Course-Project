
# 1. Merge the training and test sets to create one data set.
## a. Read the training data files and compose a data frame that adds appropriate 
##    subject and activity label to observations.
## b. Read the test data files and compose a data frame that adds appropriate 
##    subject and activity label to observations.
## c. Merge the new training and test data frames.

# 2. Extract only the measurements on the mean and standard deviation for each 
#    measurement.
## a. Add names to variables in the merged data.
## b. Modify the data frame to include only variables with names describing mean or 
##    standard deviation.

# 3. Use descriptive activity names to name the activities in the data set.
## a. Create a vector of descriptive activity names corresponding to activity labels.
## b. Modify the merged data to replace activity label values with descriptive names.

# 4. Appropriately label the data set with descriptive variable names.
## a. Create a vector of clean, descriptive variable names.
## b. Replace merge data variable names with clean, descriptive variable names.

# 5. From the data set in step 4, create a second, independent tidy data set with
#    the average of each variable for each activity and each subject.
## a. Melt the merged data frame.
## b. Reshape the melted data with rows corresponding to subject and activity,
##    columns corresponding to variables, and values as variable means.

# Getting and Cleaning Data Course Project

library(plyr)
library(reshape2)
library(dplyr)

# 1. Merge the training and test sets to create one data set.

featuresData <- read.table("./UCI HAR Dataset/features.txt", stringsAsFactors = FALSE)
varNames <- featuresData$V2
rm(featuresData)

## a. Read the training data files and compose a data frame that adds appropriate 
##    subject and activity label to observations.
### i. Read training data set from 'x_train' file
trainData <- read.table("./UCI HAR Dataset/train/X_train.txt",
                        col.names = varNames)
### ii. Read 'y_train' file and make a vector of training data activity labels
ytrainData <- read.table("./UCI HAR Dataset/train/y_train.txt", stringsAsFactors = FALSE)
trainActivityLabels <- ytrainData$V1
rm(ytrainData)
### iii. Read 'subject_train' file and make a vector of training data subjects
strainData <- read.table("./UCI HAR Dataset/train/subject_train.txt", stringsAsFactors = FALSE)
trainSubjects <- strainData$V1
rm(strainData)
### iv. Use vectors to add activity label column and subject column to training data
trainData$activityLabel <- trainActivityLabels
trainData$subject <- trainSubjects
rm(trainActivityLabels)
rm(trainSubjects)

## b. Read the test data files and compose a data frame that adds appropriate 
##    subject and activity label to observations.
### i. Read test data set from 'x_test' file
testData <- read.table("./UCI HAR Dataset/test/X_test.txt",
                       col.names = varNames)
rm(varNames)
### ii. Read 'y_test' file and make a vector of test data activity labels
ytestData <- read.table("./UCI HAR Dataset/test/y_test.txt", stringsAsFactors = FALSE)
testActivityLabels <- ytestData$V1
rm(ytestData)
### iii. Read 'subject_test' file and make a vector of test data subjects
stestData <- read.table("./UCI HAR Dataset/test/subject_test.txt", stringsAsFactors = FALSE)
testSubjects <- stestData$V1
rm(stestData)
### iv. Use vectors to add activity label column and subject column to test data
testData$activityLabel <- testActivityLabels
testData$subject <- testSubjects
rm(testActivityLabels)
rm(testSubjects)

## c. Merge the training and test data frames.
### i. Merge data frames 'trainData' and 'testData'
mData = merge(trainData, testData, all = TRUE)
rm(trainData)
rm(testData)
### ii. Convert the merged data to {dplyr} data frame
mData <- tbl_df(mData)

# 2. Extract only the measurements on the mean and standard deviation for each 
#    measurement.
## a. Get indices for column names that inlcude mean or standard deviation
mNames <- names(mData)
mNamesSub <- grep("mean\\.|std", mNames)
rm(mNames)
## b. Use indices to select mean and standard columns
mData <- select(mData, activityLabel, subject, mNamesSub)
rm(mNamesSub)

# 3. Use descriptive activity names to name the activities in the data set.
## a. Create a vector of descriptive activity names corresponding to activity labels.
actLabelsData <- read.table("./UCI HAR Dataset/activity_labels.txt", stringsAsFactors = FALSE)
activities <- actLabelsData$V2
rm(actLabelsData)

## b. Modify the merged data to replace activity label values with descriptive names.
mActivityLabels <- mData$activityLabel
trainActivityFactor <- factor(mActivityLabels,
                              labels = activities)
mData$activityLabel <- trainActivityFactor
rm(activities)
rm(mActivityLabels)
rm(trainActivityFactor)

# 4. Appropriately label the data set with descriptive variable names.

## a. Create a vector of clean, descriptive variable names.
mdsNames <- names(mData)
mdsNames <- gsub('mean', 'Mean', mdsNames)
mdsNames <- gsub('std', 'Std', mdsNames)
mdsNames <- gsub('\\.\\.?\\.?', '', mdsNames)
mdsNames <- gsub('activityLabel', 'activity', mdsNames)

## b. Replace merge data variable names with clean, descriptive variable names.
names(mData) <- mdsNames
rm(mdsNames)

# 5. From the data set in step 4, create a second, independent tidy data set with
#    the average of each variable for each activity and each subject.

## a. Melt the merged data frame.
subMelt <- melt(mData, id.vars = c("subject", "activity"))
## b. Reshape the melted data with rows corresponding to subject and activity,
##    columns corresponding to variables, and values as variable means.
averageData <- dcast(subMelt, activity + subject ~ variable, mean)
rm(subMelt)

write.csv(averageData, "./tidydata.csv")



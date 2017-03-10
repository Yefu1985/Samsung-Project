#Author: Yefu Wang
#Date: March 8th, 2017
#Purpose: This code is to solve the project in Week 4.

# Part I: import all the data into the workspace. For convenience, I saved all the raw data files in the working directory.
cat('\014')  
rm(list = ls())  # make the console and workspace clear
#install.packages("plyr")
library(plyr)  # The plyr package is needed in the code, the mapvalues function is needed.

features <- read.table("features.txt") # get the features data
activity_labels <- read.table("activity_labels.txt") # get the activity labels data


subject_train <- read.table("subject_train.txt") # get the subject data in the train group
X_train <- read.table("X_train.txt") # get the X (561 activity features) data in the train group
y_train <- read.table("y_train.txt") # get the y (6 categories) data in the train group

subject_test <- read.table("subject_test.txt") # get the subject data in the test group
X_test <- read.table("X_test.txt") # get the X (561 activity features) data in the test group
y_test <- read.table("y_test.txt") # get the y (6 categories) data in the test group


# Part II: arrange the data in train group and test group separately.
# Step #1: Creat two data frames to save data from train group and test group.trainGroup, testGroup. 
# Step #2: Assign features name in features to trainGroup and testGroup
# Step #3: Keep only the columns with "mean" and std"
# Step #4: Creat a column name "Activity" to the trainGroup and testGroup
# Step #5: Add one column with subject_test and subject_train variables to testGroup and trainGroup
# Step #6: Merge two data frames, trainGroup and testGroup

# step #1 Creat trainGroup and testGroup
trainGroup <- X_train
testGroup <- X_test

# Step #2 Assign features name in the trainGroup and testGroup
features <- as.character(features[,2]) # get the features name
colnames(testGroup) <- features # assign feature names to testGroup
colnames(trainGroup) <- features # assign feature neams to trainGroup

# Step #3 Keep only the columns with "mean" and "std"
trainGroup <- trainGroup[,grep("mean|std",colnames(trainGroup))]
testGroup <- testGroup[,grep("mean|std",colnames(testGroup))]

# Step #4 Add the "Activity" column in the trainGroup and testGroup
y_test <- as.character(y_test[,1])
y_train <- as.character(y_train[,1])
labelData <- as.character(activity_labels[,1])
activityData <- as.character(activity_labels[,2])
activity_test <- mapvalues(y_test,from = labelData, to = activityData)
activity_train <- mapvalues(y_train,from = labelData, to = activityData)
testGroup <- cbind(activity = activity_test, testGroup)
trainGroup <- cbind(activity = activity_train, trainGroup)


# Step #5 Add column with subject in test and train
testGroup <- cbind(volunteerID = as.numeric(subject_test[,1]),testGroup)
trainGroup <- cbind(volunteerID = as.numeric(subject_train[,1]),trainGroup)
                   
# Step #6 Merge two data frames testGroup and trainGroup
allGroup <- rbind(testGroup, trainGroup) 
tidyData <- allGroup[order(as.numeric(allGroup$volunteerID),allGroup$activity),] # sort allGroup data frame by volunteerID and activity
write.table(tidyData,file = "tidyData.txt",quote = FALSE, row.names = FALSE) # save the tidyData as tidyData.csv


# Part III: Create a second, independent tidy data set with the average of each variable for each activity and each subject
# Initial a data frame to save averageResult
ID <- unique(allGroup$volunteerID)  # Get all the IDs of volunteers
activity <- unique(allGroup$activity) # Get all the activities
averageGroup <- data.frame(matrix(NA,nrow = 1, ncol = ncol(allGroup))) # initial the averageGroup
meanNames <- paste("MEAN_", colnames(allGroup[,3:ncol(allGroup)]), sep = "") # get all the colnames fro the new averageGroup by adding MEAN in the begining
colnames(averageGroup) <- c("volunterID","activity",meanNames)  # assign the colnames to the averageGroup

# Add the new row in the averageGroup with the following info.: volunteerID, activity, mean value of all the features from each activity and volunteer
for (IDindex in 1:length(ID)) # for each volunteer
{
  for (activityIndex in 1:length(activity)){ # for each activity
    # get the mean of each feature from each activity and each volunteer
    temp <- apply(allGroup[allGroup$volunteerID == ID[IDindex] & allGroup$activity == activity[activityIndex], 3:ncol(allGroup)], 2, mean)
    tempVector <- c(ID[IDindex],as.character(activity[activityIndex]), temp)
    averageGroup <- rbind(averageGroup,tempVector) # add the info into the new row of averageGroup
      }  
}
averageGroup <- averageGroup[2:nrow(averageGroup),]
meanGroup <- averageGroup[order(as.numeric(averageGroup$volunterID),averageGroup$activity),]  # Sort the data frame by the volunteerID and activity
write.table(meanGroup,file = "meanData.txt",quote = FALSE, row.names = FALSE)  # save the final meanData as meanData.csv


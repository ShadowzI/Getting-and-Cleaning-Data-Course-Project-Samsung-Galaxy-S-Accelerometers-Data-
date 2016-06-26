##The code should have a file run_analysis.R in the main directory that can be run as long as the Samsung data is in your working directory.
##If the file is not present the data should be downloaded from here https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip and extracted into the working directory

library(data.table)

## STEP 1: Merges the training and the test sets to create one data set

##Read the data into data frames
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")

y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")

##Read the names in the metadata
featureNames <- read.table("./UCI HAR Dataset/features.txt")
activityLabels <- read.table("./UCI HAR Dataset/activity_labels.txt")

##data is combined into three data sets: subject, features, and activity
subject <- rbind(subject_train, subject_test)
features <- rbind(X_train, X_test)
activity <- rbind(y_train, y_test)

##name the data
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
##names for the features can be pulled from featureNames
colnames(features) <- t(featureNames[2])

##merge data
MergedData <- cbind (features,activity,subject)

##Only colums with mean or std are extracted
MeanSTDcolums <- grep(pattern="Mean|Std", names(MergedData), ignore.case=TRUE)

Columns <- c(MeanSTDcolums, 562, 563)
dim(MergedData)

Data <- MergedData[,Columns]
dim(Data)

Data$Activity <- as.character(Data$Activity)
for (i in 1:6){
Data$Activity[Data$Activity == i] <- as.character(activityLabels[i,2])
}

Data$Activity <- as.factor(Data$Activity)

##Appropriately labels the data set with descriptive variable names

names(Data)<-gsub("BodyBody", "Body", names(Data))
names(Data)<-gsub("Acc", "Accelerometer", names(Data))
names(Data)<-gsub("gravity", "Gravity", names(Data))
names(Data)<-gsub("Gyro", "Gyroscope", names(Data))
names(Data)<-gsub("tBody", "TimeBody", names(Data))
names(Data)<-gsub("Mag", "Magnitude", names(Data))
names(Data)<-gsub("^t", "Time", names(Data))
names(Data)<-gsub("^f", "Frequency", names(Data))
names(Data)<-gsub("-mean()", "Mean", names(Data), ignore.case = TRUE)
names(Data)<-gsub("-std()", "STD", names(Data), ignore.case = TRUE)
names(Data)<-gsub("-freq()", "Frequency", names(Data), ignore.case = TRUE)
names(Data)<-gsub("angle", "Angle", names(Data))

Data$Subject <- as.factor(Data$Subject)
Data <- data.table(Data)

tidyDataSet <- aggregate(. ~Subject + Activity, Data, mean)
tidyDataSet <- tidyDataSet[order(tidyDataSet$Subject,tidyDataSet$Activity),]
write.table(tidyDataSet, file = "Tidy_Data_Set.txt", row.names = FALSE)

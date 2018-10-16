##Getting And Cleaning Data: Week Four Project

## Load libraries needed fore getting & cleaning data
library(plyr)

        library(data.table)

                library(dplyr)

                        library(data.table)

## Check for and create a new directory to house the data. 
if(!getwd() == "GettingandCleaningData") {
        dir.create("GettingandCleaningData")
}

## Set the wordking directory as the newly created one. 
setwd("C:/Users/Home/Desktop/Homework/Data_Science/datascience_repo/GettingandCleaningData")

##Download data files
temp <- tempfile()
        download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", temp)
        unzip(temp, list =TRUE)
        ##Created fileList.txt to keep the list availible for copying and pasting file names as neccessary. 

README <- read.delim(unzip(temp, "UCI HAR Dataset/README.txt"  ))
        View(README)

##Read all training datasets in the "train" subdirectory.
subject_train <- read.table(unzip(temp, "UCI HAR Dataset/train/subject_train.txt"),header = FALSE)
        y_train <- read.table(unzip(temp, "UCI HAR Dataset/train/y_train.txt"))
                x_train <- read.table(unzip(temp, "UCI HAR Dataset/train/X_train.txt"))


##Read all test datasets in the "test" subdirectory. 
subject_test <- read.table(unzip(temp, "UCI HAR Dataset/test/subject_test.txt"))
        x_test <- read.table(unzip(temp, "UCI HAR Dataset/test/X_test.txt"))
                y_test <- read.table(unzip(temp, "UCI HAR Dataset/test/y_test.txt"))

##Read tidying datasets
features <- read.table(unzip(temp, "UCI HAR Dataset/features.txt"), sep = " ")
        activity_labels <- read.table(unzip(temp, "UCI HAR Dataset/activity_labels.txt"), sep = " ")

##1: Merges the training and the test sets to create one data set.
merge_train <- cbind(y_train, subject_train, x_train)
        merge_test <- cbind(y_test, subject_test, x_test)
                complete_data <- rbind(merge_train, merge_test)

##4: Appropriately labels the data set with descriptive variable names.
names(complete_data) [1] <- "Activity" 
        names(complete_data) [2] <- "Subject" 
                features <- features$V2
                        features <- as.character(features)
                                names(complete_data)  [3:563] <- features
        ## Make tidy by giving human readable column names. 
        names(complete_data)[-c(1:2)]<-gsub("^t", "time", names(complete_data)[-c(1:2)])
                names(complete_data)[-c(1:2)]<-gsub("^f", "frequency", names(complete_data)[-c(1:2)])
                        names(complete_data)[-c(1:2)]<-gsub("Acc", "Accelerometer", names(complete_data)[-c(1:2)])
                                names(complete_data)[-c(1:2)]<-gsub("Gyro", "Gyroscope", names(complete_data)[-c(1:2)])
                                        names(complete_data)[-c(1:2)]<-gsub("Mag", "Magnitude", names(complete_data)[-c(1:2)])
                                                names(complete_data)[-c(1:2)]<-gsub("BodyBody", "Body", names(complete_data)[-c(1:2)])
##2: Extracts only the measurements on the mean and standard deviation for each measurement.
mean_std <- grep(pattern = "mean|std", x = colnames(complete_data), ignore.case = TRUE)
        msTable <- data.frame(c(complete_data[1:2], complete_data[mean_std]))

##3: Uses descriptive activity names to name the activities in the data set
 msTable$Activity <- factor(msTable$Activity, labels=c("Walking",
     "Walking Upstairs", "Walking Downstairs", "Sitting", "Standing", "Laying"))
        msTable <msTable[order(msTable$Subject,msTable$Activity),]
                write.table(msTable, row.names = FALSE, file = "tidyData.txt")

 ##5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidy_data<- aggregate(. ~ Subject + Activity, msTable, mean)
        write.table(tidy_data, row.names = FALSE, file = "average.txt")
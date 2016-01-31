# run_analysis.R
##You should create one R script called run_analysis.R that does the following.
##1.Merges the training and the test sets to create one data set.
##2.Extracts only the measurements on the mean and standard deviation for each measurement.
##3.Uses descriptive activity names to name the activities in the data set
##4.Appropriately labels the data set with descriptive variable names.
##5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
##Load packages
library("downloader")
library(dplyr)
library(data.table)
library(tidyr)

##1.Download the file and put the file in the folder
if(!file.exists("./data")){dir.create("./data")}else{setwd("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"


download.file(fileUrl,destfile="./data/Dataset.zip")
setwd("./data")
unzip("Dataset.zip", files = NULL, list = FALSE, overwrite = TRUE,
           junkpaths = FALSE, exdir = ".", unzip = "internal",
           setTimes = FALSE)
##1.Merges the training and the test sets to create one data set.
dataSubjectTrain<-read.table("./UCI HAR Dataset/train/subject_train.txt")
dataTrain <- read.table("./UCI HAR Dataset/train/X_train.txt")
dataActivityTrain <- read.table("./UCI HAR Dataset/train/y_train.txt")
dataSubjectTest<-read.table("./UCI HAR Dataset/test/subject_test.txt")
dataTest <- read.table("./UCI HAR Dataset/test/X_test.txt")
dataActivityTest <- read.table("./UCI HAR Dataset/test/y_test.txt")
alldataSubject <- rbind(dataSubjectTrain, dataSubjectTest)
setnames(alldataSubject, "V1", "subject")
alldataActivity<- rbind(dataActivityTrain, dataActivityTest)
setnames(alldataActivity, "V1", "activityNum")
dataTable <- rbind(dataTrain, dataTest)
dataFeatures <- tbl_df(read.table("./UCI HAR Dataset/features.txt"))
setnames(dataFeatures, names(dataFeatures), c("featureNum", "featureName"))
colnames(dataTable) <- dataFeatures$featureName
activityLabels<- tbl_df(read.table("./UCI HAR Dataset/activity_labels.txt"))
setnames(activityLabels, names(activityLabels), c("activityNum","activityName"))
alldataSubjAct<- cbind(alldataSubject, alldataActivity)
dataTable <- cbind(alldataSubjAct, dataTable)

##2.Extracts only the measurements on the mean and standard deviation for each measurement.
dataFeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",dataFeatures$featureName,value=TRUE)
dataFeaturesMeanStd <- union(c("subject","activityNum"), dataFeaturesMeanStd)
dataTable<- subset(dataTable,select=dataFeaturesMeanStd) 

##3.Uses descriptive activity names to name the activities in the data set
dataTable <- merge(activityLabels, dataTable , by="activityNum", all.x=TRUE)
dataTable$activityName <- as.character(dataTable$activityName)
dataTable$activityName <- as.character(dataTable$activityName)
dataAggr<- aggregate(. ~ subject - activityName, data = dataTable, mean) 
dataTable<- tbl_df(arrange(dataAggr,subject,activityName))

##4.Appropriately labels the data set with descriptive variable names.
head(str(dataTable),2)
names(dataTable)<-gsub("std()", "SD", names(dataTable))
names(dataTable)<-gsub("mean()", "MEAN", names(dataTable))
names(dataTable)<-gsub("^t", "time", names(dataTable))
names(dataTable)<-gsub("^f", "frequency", names(dataTable))
names(dataTable)<-gsub("Acc", "Accelerometer", names(dataTable))
names(dataTable)<-gsub("Gyro", "Gyroscope", names(dataTable))
names(dataTable)<-gsub("Mag", "Magnitude", names(dataTable))
names(dataTable)<-gsub("BodyBody", "Body", names(dataTable))
head(str(dataTable),6)

##5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
write.table(dataTable, "TidyData.txt", row.name=FALSE)

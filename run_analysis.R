##Set WD##
setwd("C:/Users/User/Desktop/Coursera/Data Science/03. Getting and Cleaning Data/CourseProject")

##Download File##
filesPath <- "C:/Users/User/Desktop/Coursera/Data Science/03. Getting and Cleaning Data/CourseProject"
setwd(filesPath)
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip",method="auto")

##Unzip file##
unzip(zipfile="./data/Dataset.zip",exdir="./data")

##Load required packages##
library(dplyr)
library(data.table)
library(tidyr)

##Read needed fille and create folder##
filesPath <- "C:/Users/User/Desktop/Coursera/Data Science/03. Getting and Cleaning Data/CourseProject/data/UCI HAR Dataset"
##Read subject files##
dataSubjectTrain <- tbl_df(read.table(file.path(filesPath, "train", "subject_train.txt")))
dataSubjectTest  <- tbl_df(read.table(file.path(filesPath, "test" , "subject_test.txt" )))

##Read activity files##
dataActivityTrain <- tbl_df(read.table(file.path(filesPath, "train", "Y_train.txt")))
dataActivityTest  <- tbl_df(read.table(file.path(filesPath, "test" , "Y_test.txt" )))

##Read data files##
dataTrain <- tbl_df(read.table(file.path(filesPath, "train", "X_train.txt" )))
dataTest  <- tbl_df(read.table(file.path(filesPath, "test" , "X_test.txt" )))


##both Activity and Subject files will merge to the training and the test sets by row binding and rename variables "subject" and "activityNum"##
alldataSubject <- rbind(dataSubjectTrain, dataSubjectTest)
setnames(alldataSubject, "V1", "subject")
alldataActivity<- rbind(dataActivityTrain, dataActivityTest)
setnames(alldataActivity, "V1", "activityNum")

##combine the DATA training and test files##
dataTable <- rbind(dataTrain, dataTest)

##name variables according to feature e.g.(V1 = "tBodyAcc-mean()-X")##
dataFeatures <- tbl_df(read.table(file.path(filesPath, "features.txt")))
setnames(dataFeatures, names(dataFeatures), c("featureNum", "featureName"))
colnames(dataTable) <- dataFeatures$featureName

##column names for activity labels##
activityLabels<- tbl_df(read.table(file.path(filesPath, "activity_labels.txt")))
setnames(activityLabels, names(activityLabels), c("activityNum","activityName"))

##Merge columns##
alldataSubjAct<- cbind(alldataSubject, alldataActivity)
dataTable <- cbind(alldataSubjAct, dataTable)


##Read "features.txt" and extracting only the mean and standard deviation##
dataFeaturesMeanStd <- grep("mean//(//)|std//(//)",dataFeatures$featureName,value=TRUE) #var name

##Take only measurements for the mean and standard deviation and add "subject","activityNum"##
dataFeaturesMeanStd <- union(c("subject","activityNum"), dataFeaturesMeanStd)
dataTable<- subset(dataTable,select=dataFeaturesMeanStd) 

##enter name of activity into dataTable##
dataTable <- merge(activityLabels, dataTable , by="activityNum", all.x=TRUE)
dataTable$activityName <- as.character(dataTable$activityName)

## create dataTable with variable means sorted by subject and Activity
dataTable$activityName <- as.character(dataTable$activityName)
dataAggr<- aggregate(. ~ subject - activityName, data = dataTable, mean) 
dataTable<- tbl_df(arrange(dataAggr,subject,activityName))


##Names before##
head(str(dataTable),2)
names(dataTable)<-gsub("std()", "SD", names(dataTable))
names(dataTable)<-gsub("mean()", "MEAN", names(dataTable))
names(dataTable)<-gsub("^t", "time", names(dataTable))
names(dataTable)<-gsub("^f", "frequency", names(dataTable))
names(dataTable)<-gsub("Acc", "Accelerometer", names(dataTable))
names(dataTable)<-gsub("Gyro", "Gyroscope", names(dataTable))
names(dataTable)<-gsub("Mag", "Magnitude", names(dataTable))
names(dataTable)<-gsub("BodyBody", "Body", names(dataTable))

##Names after##
head(str(dataTable),6)

##Write Text file##
write.table(dataTable, "TidyData.txt", row.name=FALSE)

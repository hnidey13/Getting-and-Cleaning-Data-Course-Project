#Leemos los activity_labels
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", colClasses = c("integer", "character"))
activity_labels

#Leemos los features
features <- read.table("UCI HAR Dataset/features.txt", colClasses = c("integer", "character"))[,2]
head(features)

#Leemos subject_test, y_test & X_test
subject <- read.table("UCI HAR Dataset/test/subject_test.txt", colClasses = "factor")[,1]
activity <- read.table("UCI HAR Dataset/test/y_test.txt", colClasses = "factor")[,1]
X_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features)
str(subject)
head(subject)
str(activity)
head(activity)
X_test[1:6, 1:6]
dim(X_test)

#Cambiamos los labels de y_test
levels(activity) <- activity_labels$V2
head(activity)

#Unimos subject, activity & X_test
testData <- cbind(subject, activity, X_test)
testData[1:6, 1:6]

##Leemos subject_train, y_train & X_train
subject <- read.table("UCI HAR Dataset/train/subject_train.txt", colClasses = "factor")[,1]
activity <- read.table("UCI HAR Dataset/train/y_train.txt", colClasses = "factor")[,1]
X_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features)

#Cambiamos los labels de y_test
levels(activity) <- activity_labels$V2

#Unimos subject, activity & X_test
trainData <- cbind(subject, activity, X_train)
trainData[1:6, 1:6]

#Juntamos los dos data frames
Data <- rbind(testData, trainData)
Data[1:6, 1:6]

#Acomodamos los datos
library(dplyr)
Data$subject <- factor(Data$subject, 1:30)
Data <- arrange(Data, subject, activity)
Data[1:6, 1:6]

#Extraemos columnas de media y sd
meanSD <- grep("mean[^Freq]|std", names(Data), value = T)
DataMeanSD <- select(Data, subject, activity, meanSD)
DataMeanSD[1:6, 1:6]
str(DataMeanSD)

#Data set with the average of each variable for each activity and each subject
DataMeanSD_split <- split(DataMeanSD, list(DataMeanSD$subject, DataMeanSD$activity))
names(DataMeanSD_split)
tidyData <- t(sapply(1:length(DataMeanSD_split), function(i) colMeans(DataMeanSD_split[[i]][,-c(1,2)])))
tidyData[1:180, 1:6]
dim(tidyData)

#Agregamos el subject y Activity
subjectActivity <- do.call(rbind, lapply(1:length(DataMeanSD_split), function(i) DataMeanSD_split[[i]][1,c(1,2)]))
tidyData <- cbind(subjectActivity, tidyData)
tidyData[,1:6]
str(tidyData)
names(tidyData)

#Exportamos tidyData.txt
write.table(tidyData, "tidyData.txt", row.names = F)


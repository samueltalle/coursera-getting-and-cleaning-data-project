#step 0 - Change work directory and load data sets
setwd("C:/Users/Samuel/coursera/DataScience")

testData<-read.table("X_test.txt")
testActivities <- read.table("Y_test.txt")
testSubjects <- read.table("subject_test.txt")
trainData<-read.table("X_train.txt")
trainActivities <- read.table("Y_train.txt")
trainSubjects <- read.table("subject_train.txt")
featuresData<-read.table("features.txt") 
activityLabels <- read.table("activity_labels.txt") 
activityLabels[,2] <- as.character(activityLabels[,2]) # convert activityLabel as character


#step 1 -  merge data sets
mergeData<-rbind(testData,trainData)
mergeActivities<-rbind(testActivities,trainActivities)
mergeSubjects<-rbind(testSubjects,trainSubjects)


#step 2 - Extracts only the measurements on the mean and standard deviation for each measurement
varMeanStD<-grep("mean|std",featuresData$V2) #identify ids of variables for the measurements on the mean and standard deviation
measurementsMeanStd<-mergeData[,c(varMeanStD)] # extract only data whose ids match with the ones extracted above

#step 3 and 4 - Uses descriptive activity names to name the activities in the data set, Appropriately labels the data set with descriptive variable names
namesVariable<-names(measurementsMeanStd) #extracts actual names of variables from measurementsMeanStd data
namesVariable<-sub("V","",namesVariable) #remove letter V on each variable
descriptiveName<-function(x){featuresData[featuresData$V1==x,2]} #function to retrieve descriptive names of each variable
descriptiveVarNames<-sapply(namesVariable, descriptiveName) #using the function created with sapply to change the actual names of all variables in measurementsMeanStd data set.
names(measurementsMeanStd)<-descriptiveVarNames #apply the newname on data set
measurementsMeanStd<-cbind(mergeSubjects,mergeActivities,measurementsMeanStd)
names(measurementsMeanStd)<-c("subject", "activity",colnames(measurementsMeanStd[,3:81]))
names(measurementsMeanStd)<-gsub('[-()]', '', names(measurementsMeanStd))    

#step 5- creates a second, independent tidy data set with the average (mean) of each variable for each activity and each subject 
measurementsMeanStd$activity <- factor(measurementsMeanStd$activity, levels = activityLabels[,1], labels = activityLabels[,2])
measurementsMeanStd$subject <- as.factor(measurementsMeanStd$subject)
measurementsMeanStd.melted <- melt(measurementsMeanStd, id = c("subject", "activity"))
measurementsMeanStd.mean <- dcast(measurementsMeanStd.melted, subject + activity ~ variable, mean)

#The end result is shown in the file tidy.txt
write.table(measurementsMeanStd.mean, "tidy.txt", row.names = FALSE, quote = FALSE)
